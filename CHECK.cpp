//	todo: check struct (and other ?) initializers
//	fix gen code to handle struct (and other ?) initializers
//


/*
 *
 * check.c
 * Thursday, 2/27/1997.
 * Craig Fitzgerald
 *
 *
 * decorate symbols in the symbol table
 * decorates nodes in the parse/syntax tree
 * performs type checking and type conversion
 * performs param matching
 * normalizes pointer arithmetic
 * checks syntax of some things that get by the parser
 *
 *
 * global declarations must be operating on new symbols else-error
 * param and local declarations must be operating on new symbols else-
 *   create new symbols for them
 * variable references must be to previously declared vars else-error
 *
 * global var initializers must be constants
 * param declarations cannot be fns or have initializers
 * local declarations cannot be fns
 *
 * ensure all referenced symbols are declared
 *
 * assignments must have lval on left
 * var types need to be resolved in expression trees
 * var types need to be resolved in fn calls
 *
 * on fn declaration (not defn) remove param vars from
 *   symbol table
 *
 * ----------------------------------------------------------
 * flow control checks: break, continue in loops
 * case uniqueness and case scoping
 *
 */

#include "stdafx.h"
#include <stdarg.h>
#include "symbol.h"
#include "tokenize.h"
#include "parse.h"
#include "check.h"
#include "error.h"
#include "opt.h"
#include "type.h"
#include "mem.h"
#include "opcodes.h"
#include "node.h"


void CheckStmtDecls (PNODE pStmt);
void CheckDeclarationList (PNODE pDecList);
void CheckDeclarationLists (PNODE pDecList);
void CheckActuals (PNODE pFn);
void CheckExpression (PNODE pExpr);
int CheckInitializerElements (PNODE pVar, PTLST ptlst, PNODE pnodeInit);
PNODE DecorateTypeSpecifier (PNODE pType);

UINT uSTRUCT_PACK_SIZE = 8;


//UINT uFNTYPE;
PTLST pFNTYPE;

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

/*
 *	may need work
 *
 */
static void EnsureAddressableValue (PNODE pnode)
	{
	if (TLNodeIsArray (pnode) || TLNodeIsHash (pnode) || TLNodeIsStruct (pnode))
		return;
   if (pnode->uID == TOK_VARIABLE)
      return;
   if (pnode->uID == TOK_DEREF)
      return;
	if (pnode->uID == TOK_HASHDEREF)
		return;
   if (pnode->uID == TOK_PERIOD || pnode->uID == TOK_ARROW)
      return;
   NodeError (pnode, "operand must be addressable value");
	}


/*
 * makes sure pnode is assignable
 * (i.e. an lvalue)
 *
 */
static void EnsureLValue (PNODE pnode)
   {
	// try this...
	if (TLNodeIsArray (pnode) || TLNodeIsHash (pnode) || TLNodeIsStruct (pnode))
	   NodeError (pnode, "operand must be lvalue");

   if (pnode->uID == TOK_VARIABLE && !TLNodeIsArray (pnode->pn1) && !TLNodeIsHash (pnode->pn1))
      return;
   if (pnode->uID == TOK_DEREF && !TLNodeIsArray (pnode) && !TLNodeIsHash (pnode)) // This may need work
      return;
	if (pnode->uID == TOK_HASHDEREF && !TLNodeIsArray (pnode) && !TLNodeIsHash (pnode))	// This may need work
		return;
   if (pnode->uID == TOK_PERIOD || pnode->uID == TOK_ARROW)
      return;

   NodeError (pnode, "operand must be lvalue");
   }


PSYM FindStructType (PNODE pnode)
   {
//   if (!pnode || !TLNodeIsStruct(pnode))
//      {
//		assert (!bDie); // "internal error: cannot find struct"
//      return NULL;
//      }
//   if (pnode->uID == TOK_IDENTIFIER)
//      return pnode->ps->psStruct;
//   if (pnode->pn2 && pnode->pn2->uTypeBase == DATATYPE_STRUCT)
//      return FindStructType (pnode->pn2, bDie);
//   if (pnode->pn1 && pnode->pn1->uTypeBase == DATATYPE_STRUCT)
//      return FindStructType (pnode->pn1, bDie);
//
//	// implicit casts (for example passing a NULL actual param
//	// when fn is expecting a struct ptr) would cause this error
//	// therefore we don't always die here
//	//
//	assert (!bDie); // "internal error: type inconsistency"
//   return NULL;

//	// lets allow them to call us with tok_var node as well...
//	if (pnode->uID == TOK_VARIABLE)
//		pnode = pnode->pn1;
//
//
//	if (pnode->uID == TOK_IDENTIFIER)
//		{
//		PTLST ptlst = TLGetTail (pnode->ptlst);
//		assert (TLGetBaseType (pnode->ptlst) == DATATYPE_STRUCT);
//		return ptlst->pSym;
//		}
//	assert (0);
//	return NULL;

	assert (pnode->ptlst);
	PTLST ptlstTmp = TLGetTail (pnode->ptlst);
	assert (TLIsStruct (ptlstTmp));
	return ptlstTmp->pSym;
   }


/*
 * psStruct is a: structdef symbol
 *               
 * pnode is a:  variable or identifier node containing a struct element
 * 
 * This fn checks to make sure that the var pnode is an element of the
 * psStruct struct.  This fn may change the pnode->ps symbol.
 *
 */
static PSYM FindElement (PSYM psStruct, PNODE pnode)
   {
   PSYM *pps, psElement;

   if (pnode->uID == TOK_VARIABLE)
      pnode = pnode->pn1;
	assert (pnode->uID == TOK_IDENTIFIER);

   psElement = pnode->ps;

   if (!psStruct->ppElements)
      NodeError (pnode, "struct %s was not defined", psStruct->pszLiteral);

   /*--- loop thru structure elements ---*/
   for (pps = psStruct->ppElements; *pps; pps++)
      if (!strcmp (psElement->pszLiteral, (*pps)->pszLiteral))
         break;
   if (!*pps)
      NodeError (pnode, "element '%s' not part of struct '%s'", 
                  psElement->pszLiteral, psStruct->pszLiteral);
   pnode->val.s = TRUE;	// clean this up
   return pnode->ps = *pps;
   }




//
// This fn type checks and/or type converts all elements of an expression
//	PNODE->ptlst are set for all nodes in an Expression tree.
//
//	A PSYM owns its ptlst data.
//	A PNODE's ptlst usually just points to a PSYM's ptlst data.
//	There are some instances when a PNODE will own it's own ptlst 
//	data: CAST nodes, ADDROF nodes, WTOF, FTOW.
//
static void PropagateTypes (PNODE pExpr)
   {
   int  	 iSize;
   PSYM 	 psStructType;
	PTLST  ptlst1, ptlst2;

   if (!pExpr)
      return;

   if (pExpr->uID != TOK_FUNCTION && 
       pExpr->uID != TOK_PERIOD   &&
       pExpr->uID != TOK_ARROW)
      {
      PropagateTypes (pExpr->pn1);
      PropagateTypes (pExpr->pn2);
      }
	ptlst1 = (pExpr->pn1 ? pExpr->pn1->ptlst : NULL);
	ptlst2 = (pExpr->pn2 ? pExpr->pn2->ptlst : NULL);

   switch (pExpr->uID)
      {
      case TOK_EXPR:
			NodeSetTypeList (pExpr, ptlst1, 0); // note were copying the pointer, not a malloc
         break;

      case TOK_LITERAL:
			NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_SIZEOF:

//			if (TLIsPtr (pExpr->pn1)) // all sizeof's that refer to a pointer
//            iSize = MEMSIZE_WORD;
//			else if (pExpr->pn1->uID == TOK_CAST)	// Type size: of form: sizeof(CHAR) or sizeof(INT *)
//            iSize = MemElementSize (pExpr->pn1->ptlst); // Child node parsed as a cast statement
//			else // Var Size: of form sizeof(i) or sizeof (c*)
//				// Child node parsed as an expression statement without the top TOK_EXPR node
//			----------------------
//			if (TLIsPtr (pExpr->pn1))
//            iSize = MEMSIZE_WORD; // takes care of deref casts
//			else if (pExpr->pn1->pn1->uID == TOK_IDENTIFIER)
//				...
//			else if (TLIsArray (pExpr->pn1))
//            iSize = MemNodeDerefSize (pExpr->pn1);
//         if (pExpr->pn1->val.s) // cast with a star  ie: (type *)
//            uSize = MEMSIZE_WORD;
//         /*--- simple expression or a user defined type ---*/
//         else if (pExpr->pn1->pn1->uID == TOK_IDENTIFIER)
//            uSize = MemSymSize (pExpr->pn1->pn1->ps); // datatype or variable
//         /*--- arbitrary expression -or- predefined type cast ---*/
//         else
//            uSize = MemSize (uTypeVal1, FindStructType (pExpr->pn1, FALSE));

			iSize = TLMemSize (ptlst1);

         /*--- free sizeof tree & replace with a constant ---*/
         pExpr->pn1 = NodeFreeTree (pExpr->pn1);
         pExpr->pn1 = NodeBuild1 (TOK_INTLITERAL, NULL);
         pExpr->uID = TOK_LITERAL;
         pExpr->pn1->val.s = iSize;
         PropagateTypes (pExpr->pn1);
			NodeSetTypeList (pExpr, pExpr->pn1->ptlst, 0);
         break;

      case TOK_IDENTIFIER:
         /*-- new: from CheckIdentUse ---*/
         if (!pExpr->ps->uKind)
            NodeError (pExpr, "undeclared variable or function : '%s'", pExpr->ps->pszLiteral);
        if (pExpr->ps->uScope == SCOPE_STRUCT && pExpr->val.s != TRUE) 
            NodeError (pExpr, "undeclared variable '%s' (struct element?): ", pExpr->ps->pszLiteral);
			NodeSetTypeList (pExpr, pExpr->ps->ptlst, 0);
         break;

      case TOK_FTOW:
			NodeSetTypeList (pExpr, TLCreateBaseTypeList(DATATYPE_LONG), 1);
         break;

      case TOK_WTOF:
			NodeSetTypeList (pExpr, TLCreateBaseTypeList(DATATYPE_FLOAT), 1);
         break;

      case TOK_VOID:
      case TOK_CHAR:
      case TOK_SHORT:
      case TOK_INT:
      case TOK_LONG:
      case TOK_FLOAT:
      case TOK_STRING:
      case TOK_CHARLITERAL:
      case TOK_SHORTLITERAL:
      case TOK_INTLITERAL:
      case TOK_LONGLITERAL:
      case TOK_FLOATLITERAL:
      case TOK_STRINGLITERAL:
			if (!pExpr->ptlst) // a minor efficiency if a subtree is re-propigated
				NodeSetTypeList (pExpr, TLCreateBaseTypeList(TokIDToDataType (pExpr->uID)), 1);
         break;

      case TOK_CAST:
			{
			// if 1st time called & no deref modifiers
			// set cast type to be type node's type
			// typedef owns typelist mem
			if (!pExpr->ptlst)							 
				NodeSetTypeList (pExpr, ptlst1, 0);	 

			// if 1st time called with deref modifiers
			//	combine derefs and base type
			//	cast owns new typelist mem
			else if (TLIsPtrOrArray (TLGetTail (pExpr->ptlst)))
				TLAppend (pExpr->ptlst, TLCopyTypeList (ptlst1));

			// otherwise, we've already traversed this node so do
			//	nothing
			else
				{
				// do nothing
				}
			// we dont get pn2 if it's part of a sizeof() statement
			if (pExpr->pn2) 
	         pExpr->pn2 = TypInsert (pExpr->pn2, pExpr->ptlst, 3, 1);
			}
	      break;

      case TOK_FUNCTION:
	      PropagateTypes (pExpr->pn1);
         if (pExpr->pn1->ps->uKind != KIND_FUNCTION)
            NodeError (pExpr->pn1, "Call to undeclared function '%s'", pExpr->pn1->ps->pszLiteral);
         CheckActuals (pExpr);
			NodeSetTypeList (pExpr, pExpr->pn1->ptlst, 0);
         break;

      case TOK_VARIABLE:
         if (pExpr->pn1->ps->uKind != KIND_VARIABLE)
            NodeError (pExpr->pn1, "undeclared variable '%s'", pExpr->pn1->ps->pszLiteral);
			NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_INCREMENT:
      case TOK_DECREMENT:
         EnsureLValue (pExpr->pn1 ? pExpr->pn1 : pExpr->pn2);
			NodeSetTypeList (pExpr, pExpr->pn1 ? ptlst1 : ptlst2, 0);
			if (pExpr->pn2 && TLNodeIsFloat (pExpr))
				NodeError (pExpr, "Post Increment/Decrement of floats is not supported");
         break;

      case TOK_EXCLAMATION:
			TypNoVoid (pExpr->pn1);
         pExpr->pn1 = TypInsert (pExpr->pn1, TLCreateBaseTypeList(IntTyp ()), 3, 1);
			NodeSetTypeList (pExpr, pExpr->pn1->ptlst, 0);
         break;

      case TOK_SHL1:
      case TOK_SHL2:
      case TOK_SHL3:
         TypNoVoid (pExpr->pn1);
			//todo: check pExpr->pn2 is integral or at least a number
			NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_LESSTHAN :
      case TOK_GREATERTHAN :
      case TOK_LESSOREQUAL :
      case TOK_GREATEROREQUAL:
      case TOK_EQUIVALENT :
      case TOK_NOTEQUAL :
      case TOK_LOGICALOR :
      case TOK_LOGICALAND :
      	TypResolve (pExpr, 1, 1, 0);
         break;

      case TOK_AMPERSAND :
      case TOK_OR :
      case TOK_HAT :
      case TOK_SHIFTRIGHT :
      case TOK_SHIFTLEFT :
      case TOK_TILDA :
         TypResolve (pExpr, 0, 0, 0);
         break;

      case TOK_SLASH :
      case TOK_PERCENT:
      case TOK_STAR :
         TypResolve (pExpr, 1, 0, 0);
         break;

      case TOK_PERIOD: // struct element
         PropagateTypes (pExpr->pn1);
         if (TLGetBaseType (pExpr->pn1->ptlst) != DATATYPE_STRUCT || TLNodeGetDerefCount(pExpr->pn1) > 1)
            NodeError (pExpr, "left of '.' must be struct type");
			else if (TLNodeGetDerefCount(pExpr->pn1))
            NodeError (pExpr, "left of '.' is struct ptr, use '->'");

         psStructType = FindStructType (pExpr->pn1);
         FindElement (psStructType, pExpr->pn2);
         PropagateTypes (pExpr->pn2);
			NodeSetTypeList (pExpr, pExpr->pn2->ptlst, 0); // cannot use ptlst2, FindElement might have changed the symbol
         break;

      case TOK_ARROW : // struct element
         PropagateTypes (pExpr->pn1);
         if (TLGetBaseType (pExpr->pn1->ptlst) != DATATYPE_STRUCT || TLNodeGetDerefCount(pExpr->pn1) > 1)
            NodeError (pExpr, "left of '->' must be ptr to struct");
			else if (!TLNodeGetDerefCount(pExpr->pn1))
            NodeError (pExpr, "left of '->' is a struct, use '.'");

         psStructType = FindStructType (pExpr->pn1);
         FindElement (psStructType, pExpr->pn2);
         PropagateTypes (pExpr->pn2);
			NodeSetTypeList (pExpr, pExpr->pn2->ptlst, 0); // cannot use ptlst2, FindElement might have changed the symbol
         break;

      case TOK_PLUS :
         TypResolve (pExpr, 1, 2, 1);
         break;

      case TOK_MINUS :
         if (pExpr->pn2)
            {
            TypResolve (pExpr, 1, 3, 1);
            }
         else // UNARY FORM
            {
            TypNoVoid (pExpr->pn1);
				NodeSetTypeList (pExpr, ptlst1, 0);
            }
         break;
            
      case TOK_QUESTION:
         PropagateTypes (pExpr->pn3);
         TypResolve (pExpr, 1, 1, 1);
         break;

      case TOK_EQUALS :
         EnsureLValue (pExpr->pn1);
         pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1, 2, 1);
         NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_PLUSEQUAL :
      case TOK_MINUSEQUAL :
         EnsureLValue (pExpr->pn1);
         if (TLIsPtr (ptlst1) && TLIsIntegral (ptlst2))
            ;
         else
            pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1, 0, 1);
         NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_STAREQUAL :
      case TOK_SLASHEQUAL :
      case TOK_PERCENTEQUAL :
         EnsureLValue (pExpr->pn1);
         pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1, 0, 1);
         NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_SHIFTRIGHTEQUAL:
      case TOK_SHIFTLEFTEQUAL :
      case TOK_ANDEQUAL :
      case TOK_XOREQUAL :
      case TOK_OREQUAL :
         EnsureLValue (pExpr->pn1);
			//todo: EnsureIntegral (pExpr->pn1);
			//todo: EnsureIntegral (pExpr->pn2);
         //TypResolve (pExpr, 0, 0, 0);
         pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1, 0, 1);
         NodeSetTypeList (pExpr, ptlst1, 0);
         break;

      case TOK_OBRACKET:
			if (TLNodeIsPtrOrArray (pExpr->pn1))
				{
				// convert a[n] into equivalent *(a+n) format
            PNODE pn1 = NodeBuild2 (TOK_PLUS, pExpr->pn1, pExpr->pn2);
				pExpr->uID = TOK_DEREF;
				pExpr->pn1 = pn1;
				pExpr->pn2 = NULL;

				// look at plus node type info
	         TypResolve (pExpr->pn1, 1, 2, 1);

				// finally, look at the cast node	
				if (!TLGetDerefCount(pExpr->pn1->ptlst))
	            NodeError (pExpr, "dereference of a non pointer");
	         NodeSetTypeList (pExpr, TLDeref(pExpr->pn1->ptlst), 0);
				}

#if defined (HASH_SUPPORT)
			else if (TLNodeIsHash (pExpr->pn1))
				{
				pExpr->uID = TOK_HASHDEREF;

				// make sure hash key is corrent type
	         pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1->pExpr->ptlst, 1, 1);

				// node is the hash value type				
	         NodeSetTypeList (pExpr, TLDeref(pExpr->pn1->ptlst), 0);
				}
			else
	         NodeError (pExpr->pn1, "Array or hash expected");
#else
			else
	         NodeError (pExpr->pn1, "Array expected");
#endif // defined (HASH_SUPPORT)

			break;

      case TOK_DEREF:
			if (!TLGetDerefCount(ptlst1))
            NodeError (pExpr, "dereference of a non pointer");
         NodeSetTypeList (pExpr, TLDeref(ptlst1), 0);
         break;

      case TOK_ADDROF:
			{
//			if (!TLNodeIsArray (pExpr->pn1) && !TLNodeIsHash (pExpr->pn1)) // addr of array is ok
         EnsureAddressableValue (pExpr->pn1);
			PTLST ptlstTmp = TLAddToHead (TLCopyTypeList (ptlst1), DATATYPE_PTR);
         NodeSetTypeList (pExpr, ptlstTmp, 1);
			}
         break;

      default:
         NodeError (NULL, "Internal error Token not handled in PropagateTypes: '%d'", pExpr->uID);
      }

   }

PTLST* FreeFormals (PTLST* ppFormals)
	{
	return FreeTLArray (ppFormals);
	}


//
// makes a dummy param
// used by CheckActuals
//
static PNODE MakeDummyActual (PTLST ptlst)
   {
   PNODE p1, p2;

// p1 = NodeBuildLit (uFormalType);
   p1 = NodeBuildLit (TLGetDerefCount (ptlst) ? DATATYPE_PTR : TLGetBaseType (ptlst));
	p1->ptlst = TLCopyTypeList(ptlst); // the type of this literal may not be a basic type - will this work?
	p1->bOwnsTL = TRUE; // 02/07/01 CLF MemDebug

   p2 = NodeBuild1 (TOK_EXPR, p1);
   CheckExpression (p2); // we need to propagate the types
   return p2;
   }



/*
 * This examines params used in a function call
 * It does all the normal expression checking for each parm,
 * It makes sure the params are the right type
 * and dummies up params to make sure there at least as many
 * actuals as formal params
 */
static void CheckActuals (PNODE pFn)
   {
   PNODE   pExpr, pDecl;
   PSYM    ps;
   INT     iCount;
	PTLST   ptFormal;

   /*--- 1st check each expression in the parm list ---*/
   iCount = 0;
   for (pExpr = pFn->pn2; pExpr; pExpr = pExpr->next)
      {
      CheckExpression (pExpr);
      iCount++;
      }
   pFn->val.s = iCount; // store # of actuals

   /*--- type convert parms as necessary ---*/
   ps = pFn->pn1->ps;
   iCount = 0;
   for (pExpr = pFn->pn2; pExpr; pExpr = pExpr->next)
      {
		ptFormal = ps->ppFormals[iCount];
		if (!ptFormal)
			break; // more actuals than formals

		if (!TLIsSameTypes (ptFormal, pExpr->ptlst))
			{
			if (!(pExpr->pn1 = TypInsert (pExpr->pn1, ptFormal, 2, 0)))
            NodeError (pExpr, "Incompatible types, parameter %d", iCount+1);
			NodeSetTypeList (pExpr, pExpr->pn1->ptlst, 0);
//	      CheckExpression (pExpr); // redo type propagation
			}
// whats this?? - now handled by TLIsSameType and TypInsert
//      else if (ps->pFormals[uCount].psStruct)
//         {
//         CheckStructTypes (pExpr, FindStructType (pExpr, 0), ps->pFormals[uCount].psStruct);
//         }
      iCount++;
      }

   /*--- if more formals than actuals, pad parm space ---*/
   for (; ps->ppFormals[iCount]; iCount++)
      {
		ptFormal = ps->ppFormals[iCount];
      pDecl = MakeDummyActual (ptFormal);
      pFn->pn2 = NodeAddToEnd (pFn->pn2, pDecl);

//      uFormalType = pFn->pn1->ps->pFormals[uCount].uTypeVal;
//      pDecl = MakeDummyActual (uFormalType);
//      pFn->pn2 = NodeAddToEnd (pFn->pn2, pDecl);
      }

//   for (pExpr = pFn->pn2; pExpr; pExpr = pExpr->next)
//      {
//		for (ptlst = pExpr->ptlst; ptlst && TLIsArray (ptlst); ptlst = ptlst->next)
//			ptlst->cType = DATATYPE_PTR;
//		}

   }


/***************************************************************************/

///*
// * Makes sure all referenced idents are defined.
// *
// * This uses a brute force method of finding all identifiers
// * used in the parse tree.
// */
//static void CheckIdentUse (PNODE pExpr)
//   {
//   if (!pExpr)
//      return;
//
//   CheckIdentUse (pExpr->pn1);
//   CheckIdentUse (pExpr->pn2);
//   CheckIdentUse (pExpr->next); // fn calls have params linked via next ptr
//
//   if (pExpr->uID == TOK_PERIOD)
//   if (pExpr->uID == TOK_ARROW)
//
//
//   if (pExpr->uID != TOK_IDENTIFIER)
//      return;
//
//   if (!pExpr->ps->uKind)
//      NodeError (pExpr, "undeclared variable or function : '%s'", pExpr->ps->pszLiteral);
//   }

/***************************************************************************/

///*
// * used by ConvertPointerExpressions to size constants used 
// * in ptr arithmetic, all basetypes have a size that is a
// * power of 2 - therefore we can usually use a shift operation
// */
//static PNODE InsertShift (PNODE pnode, UINT uTypeVal, PSYM psStructType)
//   {
//   UINT uSize;
//   PNODE pn1, pn2;
//
//   uSize = MemSize (TypPtrBase (uTypeVal), psStructType);
//   if (uSize < 2)
//      return pnode;
//
//   /*--- optimize if we can ---*/
//   if (pnode->uID == TOK_LITERAL && TypIntegral (pnode->pn1->uTypeVal)) 
//      {
//      OptChangeLit (pnode, OP_MUL, uSize);
//      return pnode;
//      }
//
//   switch (uSize)
//      {
//      case 2: 
//         return NodeBuild1 (TOK_SHL1, pnode);
//      case 4: 
//         return NodeBuild1 (TOK_SHL2, pnode);
//      case 8: 
//         return NodeBuild1 (TOK_SHL3, pnode);
//      default: 
//         pn1 = NodeBuild1 (TOK_SHORTLITERAL, NULL);
//         pn1->val.s    = uSize;
//         pn1->uTypeVal = DATATYPE_SHORT;
//         pn2 = NodeBuild1 (TOK_LITERAL, pn1);
//         return NodeBuild2 (TOK_STAR, pn2, pnode);
//      }
//   return NULL;
//   }

//	used by ConvertPointerExpressions to adjust for element sizes
//	when doing pointer arithmetic
//
static PNODE AddElementSizeMultiplier (PNODE pnodeInt, PNODE pnodePtr)
	{
	int iSize;
   PNODE pn1, pn2, pnHead;

//	iSize = MemNodeDerefSize (pnodePtr);
	iSize = TLMemSize (TLDeref(pnodePtr->ptlst));
	if (iSize < 2)
		return pnodeInt;

   /*--- optimize if we can ---*/
   if (pnodeInt->uID == TOK_LITERAL && TLNodeIsIntegral (pnodeInt->pn1)) 
      return OptChangeLit (pnodeInt, OP_MUL, iSize);

   switch (iSize)
      {
      case 2: 
			pnHead = NodeBuild1 (TOK_SHL1, pnodeInt);
			break;
      case 4: 
         pnHead = NodeBuild1 (TOK_SHL2, pnodeInt);
			break;
      case 8: 
         pnHead = NodeBuild1 (TOK_SHL3, pnodeInt);
			break;
      default: 
         pn1 = NodeBuild1 (TOK_SHORTLITERAL, NULL);
         pn1->val.s    = iSize;
			NodeSetTypeList (pn1, TLCreateBaseTypeList(TokIDToDataType (TOK_SHORTLITERAL)), 1);
         pn2 = NodeBuild1 (TOK_LITERAL, pn1);
			NodeSetTypeList (pn2, pn1->ptlst, 0);
         pnHead = NodeBuild2 (TOK_STAR, pn2, pnodeInt);
      }
	NodeSetTypeList (pnHead, pnodeInt->ptlst, 0);
	return pnHead;
	}


/*
 * expressions involving pointers:
 * PINT+1 really means PTR to INT + size of INT
 * pointer arithmetic must be done in terms of elements
 * not bytes, so static values must be multiplied
 * by the size of an element
 */
static void ConvertPointerExpressions (PNODE pnode)
   {
   if (!pnode || pnode->uID == TOK_FUNCTION)
      return;

   ConvertPointerExpressions (pnode->pn1);
   ConvertPointerExpressions (pnode->pn2);

   if (!pnode->pn2 || 
      (pnode->uID != TOK_PLUS && pnode->uID != TOK_MINUS))
      return;

	// ptr +/- int/short/long/char
	if (TLNodeGetDerefCount (pnode->pn1) && TLNodeIsIntegral (pnode->pn2))
		pnode->pn2 = AddElementSizeMultiplier (pnode->pn2, pnode->pn1);

	// int/short/long/char +/- ptr
	if (TLNodeGetDerefCount (pnode->pn2) && TLNodeIsIntegral (pnode->pn1))
		pnode->pn1 = AddElementSizeMultiplier (pnode->pn1, pnode->pn2);
   }


/*
 * This fn checks that all referenced variables have been defined
 * converts types in expressions and makes sure they are type compatible
 * performs expression simplification
 * adjusts pointer expressions (see ConvertPointerExpressions)
 *
 */
void CheckExpression (PNODE pExpr)
   {
   if (!pExpr)
      return; // expressions sometimes can be empty

   assert (pExpr->uID == TOK_EXPR);

   if (!pExpr->pn1)
      return;

// CheckIdentUse  (pExpr);
   PropagateTypes (pExpr);

   OptCollapseExpr (pExpr);      // static initializers must be static
   ConvertPointerExpressions (pExpr); 
   }


/* 
 *
 */
static BOOL StaticNode (PNODE pExpr)
   {
   if (!pExpr)
      return FALSE;
   if (pExpr->uID == TOK_LITERAL)
      return TRUE;
   if (pExpr->uID == TOK_CAST)
      return StaticNode (pExpr->pn2);
   return FALSE;
   }


/*
 * after OptCollapseExpr static expressions
 * reduce to a literal
 *
 * In this virtual machine environment I cannot support
 * ADDRESS OF '&' as a static value.  This is because memory
 * locations undergo a translation at run time.  (Unless
 * I am going to create relocatable code, a fixup table, 
 * and an associated loader!)
 *
 *
 *	For now I don't do much other than look for a literal...
 */
BOOL StaticExpression (PNODE pExpr)
   {
   if (!pExpr)
      return FALSE;

   return StaticNode (pExpr->pn1);

//   return (pExpr && 
//           pExpr->pn1 &&
//           pExpr->pn1->uID == TOK_LITERAL);
   }



static int StaticIntegralValue (PNODE pnode)
	{
	if (!pnode)
		return 0;

	for (;;) 	// skip past fluffyness
		{			//
		if 	  (pnode->uID == TOK_EXPR	 )	pnode = pnode->pn1;
		else if (pnode->uID == TOK_LITERAL) pnode = pnode->pn1;
		else if (pnode->uID == TOK_CAST	 )	pnode = pnode->pn2;
		else break;
		}
	if (!TLNodeIsIntegral (pnode))
		NodeError (pnode, "Array initializer must be a static int"); // this is already done isn't it?
	if (pnode->val.l < 0)
		NodeError (pnode, "Array initializer cannot be negative");
	return pnode->val.l;
	}

/*
 * local initializers generate code, 
 * this fn in essence converts 
 * 	from: int i=0;
 * 	  to: int i; i=0;
 * so that the code generator can handle it just like
 * any other assignment
 */
static PNODE FixLocalInitializer (PNODE pIdent, PNODE pExpr)
   {
   PNODE pIdentCpy, pnode, pnode2;
                      
   pIdentCpy  = NodeBuild1 (0, NULL);
   memcpy (pIdentCpy, pIdent, sizeof (NODE));

   pnode   = NodeBuild1 (TOK_VARIABLE, pIdentCpy);
   pnode2  = NodeBuild2 (TOK_EQUALS, pnode, pExpr->pn1);
   pExpr->pn1 = pnode2;
   return pExpr;
   }

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/


///*
// * Here we create an array of ints, one per function parameter.  Each int 
// * contains the typeval of the parameter to be used to match up actual 
// * params when the fn is called.  Note that this does not allow structures
// * to be passed because vars of type struct also need a psStruct val to 
// * complete its type definition and we only store an int.
// * Also, when passing structure pointers I cannot fully typecheck - all I
// * know is that there is a structure at the end of the pointer.
// *
// * I should probably replace this with a linked list of elements that have
// * space for a ps.  But I rarely pass structures ...
// *
// *
// */
//static PFORMAL MakeFormalsList (PNODE pFnDecl)
//   {
//   PNODE pDL, pV;
//   UINT  uCount = 0;
//   PFORMAL pFormals;
//
//   for (pDL = pFnDecl; pDL; pDL = pDL->next)
//      for (pV=pDL->pn2; pV; pV=pV->next)
//         uCount++;
//   pFormals = (PFORMAL)calloc (uCount+1, sizeof (FORMAL));
//
//   uCount = 0;
//   for (pDL = pFnDecl; pDL; pDL = pDL->next)
//      for (pV=pDL->pn2; pV; pV=pV->next)
//         {
//         pFormals[uCount].uTypeVal = pV->pn1->ps->uTypeVal;
//         pFormals[uCount].psStruct = pV->pn1->ps->psStruct;
//         uCount++;
//         }
//   pFormals[uCount].uTypeVal = 0xFF; // terminator
//   return pFormals;
//   }


//	a Formals list is an array of PSYM's
//	one for each function parameter symbol
//	In left-to-right order
//	Array is null terminated
//
static PTLST* MakeFormalsList (PNODE pFnDecl)
	{
   PNODE	 pDL, pV;
	PTLST *ppFormals;
   int    iCount = 0;

   for (pDL = pFnDecl; pDL; pDL = pDL->next)
      for (pV=pDL->pn2; pV; pV=pV->next)
         iCount++;

   ppFormals = (PTLST*)calloc (iCount+1, sizeof (PTLST));

   iCount = 0;
   for (pDL = pFnDecl; pDL; pDL = pDL->next)
      for (pV=pDL->pn2; pV; pV=pV->next)
			ppFormals[iCount++] = TLCopyTypeList (pV->pn1->ps->ptlst);

	ppFormals[iCount] = NULL;
	return ppFormals;
	}




/*
 * Check statement list for general validity:
 *
 */
static void CheckCmpStmtDecls (PNODE pCmpStmt)
   {
   PNODE pStmt;

   if (!pCmpStmt)
      return;
   for (pStmt = pCmpStmt->pn4; pStmt; pStmt = pStmt->next)
      CheckStmtDecls (pStmt);
   }


/*
 * Check statement for general validity:
 *
 */
static void CheckStmtDecls (PNODE pStmt)
   {
   if (!pStmt)
      return;

   switch (pStmt->uID)
      {
      case TOK_EXPR:
         CheckExpression (pStmt);
         break;

      case TOK_BREAK:
      case TOK_CONTINUE:
         break;

      case TOK_RETURN:
         {
			PNODE pExpr = pStmt->pn1;

         if (!pExpr && TLGetBaseType (pFNTYPE) != DATATYPE_VOID)
            NodeError (pStmt, "return statement must have a value");
         if (pExpr && TLGetBaseType (pFNTYPE) == DATATYPE_VOID)
            NodeError (pStmt, "value not allowed in return statement");
         if (pExpr)
            {
            CheckExpression (pExpr);
            pExpr->pn1 = TypInsert (pExpr->pn1, pFNTYPE, 2, 1); // make sure return type matches fn type
            }
         }
         break;

      case TOK_COMPOUNDSTATEMENT:
         CheckCmpStmtDecls (pStmt);
         break;
         
      case TOK_IF:
         CheckExpression (pStmt->pn1);
         CheckStmtDecls  (pStmt->pn4);
         CheckStmtDecls  (pStmt->pn3);
         break;

      case TOK_WHILE:
         CheckExpression (pStmt->pn1);
         CheckStmtDecls  (pStmt->pn4);
         break;

      case TOK_FOR:
         CheckExpression (pStmt->pn1);
         CheckExpression (pStmt->pn2);
         CheckExpression (pStmt->pn3);
         CheckStmtDecls  (pStmt->pn4);
         break;

      case TOK_SWITCH:
         CheckExpression (pStmt->pn1);
         CheckStmtDecls  (pStmt->pn4);
         break;

      case TOK_CASE:
         CheckExpression  (pStmt->pn1);
         if (!StaticExpression (pStmt->pn1))
            NodeError (pStmt, "case value must be static");
         CheckStmtDecls   (pStmt->pn4);
         break;

      case TOK_DEFAULT:
         CheckStmtDecls   (pStmt->pn4);
         break;

      case TOK_DECLARATIONLIST:
         CheckDeclarationList (pStmt);
         break;

      default:
         Error ("Internal Error: Unknown statement id: '%d'", pStmt->uID);
      }
   }


//	Array 
// cvt ptlst->pEval to ptlst->iCount
//	
//	
static void EvalArraySizeInitializers (PNODE pIdent) 
	{
	PTLST  ptlstTmp;
	PNODE  pExpr;

	for (ptlstTmp = pIdent->ptlst; ptlstTmp; ptlstTmp = ptlstTmp->next)
		{
		if (ptlstTmp->cType != DATATYPE_ARRAY)
			continue;

		pExpr = ptlstTmp->pExpr;
	   CheckExpression (pExpr);
	   if (pExpr && !StaticExpression (pExpr))
	      NodeError (pExpr, "Array initializer must be static: '%s'", pIdent->ps->pszLiteral);
		ptlstTmp->iCount = StaticIntegralValue (pExpr); // handles NULL by returning 0
		ptlstTmp->pExpr = NodeFreeTree (pExpr);
		}
	}


static void	EvalHashInitializers (PNODE pIdent)
	{
	PTLST  ptlstTmp;

	for (ptlstTmp = pIdent->ptlst; ptlstTmp; ptlstTmp = ptlstTmp->next)
		{
		if (!TLIsHash (ptlstTmp))
			continue;
#if defined (HASH_SUPPORT)
		PNODE pCast = ptlstTmp->pExpr;
		PNODE pType = pCast->pn1;
		DecorateTypeSpecifier (pType);

		PTLST ptlst = TLAppend (pCast->ptlst, TLCopyTypeList (pType->ptlst));
		NodeSetTypeList (pCast, ptlst, -1);
#else
      NodeError (pIdent, "Array initializer expected");
#endif // defined (HASH_SUPPORT)
		}
	}



//
// decorate a single function identifier symbol
//
static void CheckFnDeclaration (PNODE pDecl, PNODE pType)
   {
   PNODE pIdent, pParmDecl, pInternal, pBody;
   PSYM  ps;
   PTLST* ppFormals;
   UINT  i;
	PTLST ptlst;

   pIdent   = pDecl->pn1;
   pParmDecl= pDecl->pn2;
   pInternal= pDecl->pn3;
   pBody    = pDecl->pn4;
	ps			= pIdent->ps;

//	EvalArraySizeInitializers (pIdent); // cvt ptlst->pEval to ptlst->iCount
	ptlst = TLAppend (pIdent->ptlst, TLCopyTypeList (pType->ptlst));

   if (CurrentScope () != SCOPE_GLOBAL)
      NodeError (pIdent, "Cannot declare nested functions: %s", ps->pszLiteral);

   if (ps->uKind) // already defined or declared
      {
      if (!pBody)	// if this is a decl and there already was a decl
         NodeError (pIdent, "function already declared: '%s'", ps->pszLiteral);
      if (ps->bDefined)	// if this is a defn and there already was a defn
         NodeError (pIdent, "function already defined: '%s'", ps->pszLiteral);
		if (!TLIsSameTypes (ps->ptlst, ptlst))	// this defn, already was decl - ok if types are the same
         NodeError (pIdent, "cannot redeclare fn return type: '%s'", ps->pszLiteral);
		ps->ptlst = TLFree (ps->ptlst);
      }
	NodeSetTypeList (pIdent, ptlst, -1);
	ps->ptlst 	 = ptlst;				 
   ps->uKind    = KIND_FUNCTION;  //
   ps->bDefined = !!pBody;        //
	pFNTYPE = ptlst;					 // used when examining return stmts

   if (pInternal)
      {
      pIdent->ps->uInternal = (UINT)pInternal->val.l;
      if (pBody)
         NodeError (pIdent, "internal function cannot have a body: '%s'", pIdent->ps->pszLiteral);
      }
   PushScope (SCOPE_PARAM);
   CheckDeclarationLists (pParmDecl);
   ppFormals = MakeFormalsList (pParmDecl);
   PopScope ();

   /*--- if already declared, make sure param list hasn't changed ---*/
   if (pIdent->ps->ppFormals)
      {
		for (i=0; ppFormals[i] && pIdent->ps->ppFormals[i]; i++)
			if (!TLIsSameTypes (ppFormals[i], pIdent->ps->ppFormals[i]))
            NodeError (pIdent, "Fn '%s' declarations differ, param #%d", pIdent->ps->pszLiteral, i);
		if (ppFormals[i] || pIdent->ps->ppFormals[i])
            NodeError (pIdent, "Fn '%s' declarations differ, different # of parameters", pIdent->ps->pszLiteral);
		ppFormals = FreeFormals (ppFormals);
      }
   else
      pIdent->ps->ppFormals = ppFormals;

   PushScope (SCOPE_LOCAL);
   CheckStmtDecls (pBody);
   PopScope ();
   }


////	if bStringGlob, then a string can be used in place of a series of char literals
////	pIdent is only used for Error reporting here
////	pExpr is verified to be compatible with type ptlst.
////
////	int i=1;
////	int i[] = {1,2,3};
////	char* p = "Testing";
////
////
////	Looks like this needs work: TLIsCompatibleTypes ??
////
////
//static int CheckInitializerElement (PNODE pIdent, PNODE pExpr, PTLST ptlst, BOOL bStringGlob)
//   {
//   CheckExpression (pExpr);   // this should set the uTypeVals in pnodes
//   if (!StaticExpression (pExpr))
//      NodeError (pExpr, "Global initialization must be static: '%s'", pIdent->ps->pszLiteral);
//
//	PTLST ptlstExpr = pExpr->ptlst;
//
////	if (!TLIsCompatibleTypes (ptlst, ptlstExpr))
////      NodeError (pExpr, "Initializer incompatible types: '%s'", pIdent->ps->pszLiteral);
//
//   if (bStringGlob && TLIsString (ptlstExpr))
//      return strlen (pExpr->pn1->pn1->val.psz) + 1;
//   
//   TypConvertLiteral (pExpr->pn1->pn1, ptlstExpr, 1);
//   return 1;
//   }
//
//
////
//// Check initializer for a global var
////
//static void CheckGlobalInitializer (PNODE pDecl)
//   {
//   PNODE pIdent, pInit, pExpr;
//   PSYM  ps;
//   int   iCount;
//	PTLST ptlst;
//
//   pIdent = pDecl->pn1;
//   pInit  = pDecl->pn2;
//   ps     = pIdent->ps;
//
//	//	This does not yet allow for multi dimensional array initializers
//	ptlst = ps->ptlst;
//	if (!TLIsArray (ptlst))  // global non-array initializers
//		{							 // initializer of form 'type ident = value;'
//      CheckInitializerElement (pIdent, pInit, ptlst, 0);
//      return;
//		}
//   for (pExpr = pInit; pExpr; pExpr = pExpr->next)
//      iCount += CheckInitializerElement (pIdent, pExpr, TLDeref(ptlst), 1);
//
//	// if the type list says were an array, the first type node
//	// in the type list will be of type array.
//	if (!ptlst->iCount) // size not given - so set it to number of initializers given
//		ptlst->iCount = iCount;
//	else if (ptlst->iCount < iCount)	// verify initializers max size
//         NodeError (pInit, "More initializers (%d) than array elements (%d)", iCount, ptlst->iCount);
//   }


//
//	checks a level of static struct initializers
//
static int CheckStructInitializerElements (PNODE pVar, PTLST ptlst, PNODE pnodeInit)
	{
	assert (TLIsStruct(ptlst));

	PSYM  ps		= ptlst->pSym;
	PSYM* ppElements = ps->ppElements;
	PNODE pInit;
	int	iIdx;

	for (pInit=pnodeInit->pn1, iIdx = 0; 
	     pInit					 , ppElements[iIdx]; 
	     pInit = pInit->next , iIdx++)
		{
		CheckInitializerElements (pVar, ppElements[iIdx]->ptlst, pInit);
		}
	if (pInit)
		NodeError (pnodeInit, "Too many initializers for struct '%s'", pVar->ps->pszLiteral);
	if (ppElements[iIdx])
		NodeError (pnodeInit, "Not enough initializers for struct '%s'", pVar->ps->pszLiteral);
	return 0;
	}

//
//	checks a level of static array initializers
//
static int CheckArrayInitializerElements (PNODE pVar, PTLST ptlst, PNODE pnodeInit)
	{
	assert (TLIsArray(ptlst));
	PNODE pInit;
	int	iCount;

	for (pInit=pnodeInit->pn1, iCount = 0; 
	     pInit; 
	     pInit = pInit->next, iCount++)
		{
		CheckInitializerElements (pVar, TLDeref(ptlst), pInit);
		}
	if (!ptlst->iCount) // size not given - so set it to number of initializers given
		ptlst->iCount =  iCount;
	else if (ptlst->iCount < iCount)
	   NodeError (pnodeInit, "More initializers (%d) than array elements (%d) in array '%s'", iCount, ptlst->iCount, pVar->ps->pszLiteral);
	return 0;
	}


//
//	checks a level of static initializers, recursively checks child livels
//	pVar only here for error reporting
//	ptlst is type list required for initializer in pnodeInit
//
static int CheckInitializerElements (PNODE pVar, PTLST ptlst, PNODE pnodeInit)
	{
	BOOL  bBraced;

	if (TLIsHash (ptlst))
      NodeError (pnodeInit, "Hash initialization is not supported: '%s'", pVar->ps->pszLiteral);

	// make sure structure of initializer is correct
	if      (TLIsPtr    (ptlst)) 	bBraced = FALSE;
	else if (TLIsString (ptlst)) 	bBraced = (pnodeInit->uID == TOK_OBRACE);
	else if (TLIsArray  (ptlst)) 	bBraced = TRUE;
	else if (TLIsStruct (ptlst)) 	bBraced = TRUE;
	else								  	bBraced = FALSE;
	if (!(bBraced == (pnodeInit->uID == TOK_OBRACE)))
		NodeError (pnodeInit, "Invalid Initializer for var '%s'", pVar->ps->pszLiteral);

	// we looking at an array init?
	if (pnodeInit->uID == TOK_OBRACE && TLIsArray (ptlst))
		return CheckArrayInitializerElements (pVar, ptlst, pnodeInit);

	// we looking at a struct init?
	if (pnodeInit->uID == TOK_OBRACE && TLIsStruct (ptlst))
		return CheckStructInitializerElements (pVar, ptlst, pnodeInit);

	// a simple init or a string init
	assert (pnodeInit->uID == TOK_EXPR);
	CheckExpression (pnodeInit);
   if (!StaticExpression (pnodeInit))
      NodeError (pnodeInit, "Global initialization of '%s' must be static: ", pVar->ps->pszLiteral);

	PNODE pnode = pnodeInit->pn1; // skip EXPR node

	// converts literal iff types are different
	if (pnode->uID == TOK_CAST) 
		{
		if (TLIsSameTypes (pnode->ptlst, ptlst))
			return 0;
		if (TLMemSize (pnode->ptlst) != TLMemSize (ptlst))
			NodeError (pnode, "Initializer is incompatible type");
		else if (!TLIsVoidPtr (pnode->ptlst))
			NodeWarn (pnode, "Initializer is different type");
		}
	else if (pnode->uID != TOK_LITERAL)
		NodeError (pnode, "Invalid static initializer");
	else 
		pnodeInit->pn1 = TypConvertLiteral (pnode, ptlst, 1);
	return 0;
	}


// parsing made sure the array depth matched
//	we make sure the types match and that the
//	expressions are literal
//
static void CheckGlobalInitializer (PNODE pDecl)
	{
	PNODE pIdent =	pDecl->pn1;
	PNODE pInit	 = pDecl->pn2;

	CheckInitializerElements (pIdent, pIdent->ps->ptlst, pInit);
	}


//	given a type node, decorate the ptlst
//
static PNODE DecorateTypeSpecifier (PNODE pType)
	{
	assert (!pType->ptlst);

	//	handles	TOK_VOID,TOK_CHAR,TOK_SHORT,TOK_INT,TOK_LONG,TOK_FLOAT,TOK_STRING
	if (TokTypIsBaseType (pType->uID))
	  return NodeSetTypeList (pType, TLCreateBaseTypeList (TokIDToDataType (pType->uID)), 1);

	if (pType->uID == TOK_STRUCT || pType->uID == TOK_UNION)
		{
		PTLST ptlst = TLNewStructElement (pType->pn1->ps);
		pType->pn1->ps->ptlst = ptlst;
		return NodeSetTypeList (pType, ptlst, 0);
		}

	//	otherwise it's a typedef'ed identifier
	assert (pType->uID == TOK_IDENTIFIER);

	//if (pTyp->ps->uKind == KIND_TYPEDEF)
	return NodeSetTypeList (pType, pType->ps->ptlst, 0);
	}





//
// Decorate a single variable identifier symbol
//
static void CheckVarDeclaration (PNODE pDecl, PNODE pType)
   {
   PNODE pIdent, pInit;
	PSYM  ps;
   UINT  uScope;

   pInit  = pDecl->pn2;
   pIdent = pDecl->pn1;
   ps 	 = pIdent->ps;

	EvalArraySizeInitializers (pIdent); // cvt ptlst->pEval to ptlst->iCount
	EvalHashInitializers (pIdent); // cvt ptlst->pEval to ptlst->TYPEDEF

	//	pType has initial type info
	//	pIdent->ptlst has modifications to type info
	//	Combine this info and set the symbol entry (ps->ptlst)
	ps->ptlst = TLAppend (pIdent->ptlst, TLCopyTypeList (pType->ptlst));
   ps->uKind = KIND_VARIABLE;
	NodeSetTypeList (pIdent, ps->ptlst, -1);

   uScope = CurrentScope ();
   if (pInit)  // var has initializer?
      {
      if (TLIsHash (ps->ptlst))
         NodeError (pIdent, "hash initializers are not supported '%s'", pIdent->ps->pszLiteral);
      if (uScope == SCOPE_STRUCT)
         NodeError (pIdent, "struct elements cannot have initializers '%s'", pIdent->ps->pszLiteral);
      if (uScope == SCOPE_PARAM)
         NodeError (pIdent, "Parameter variables cannot have initializers '%s'", pIdent->ps->pszLiteral);
      if (uScope == SCOPE_GLOBAL)
         CheckGlobalInitializer (pDecl);
      if (uScope == SCOPE_LOCAL)
         {
         if (TLIsArray (ps->ptlst))
            NodeError (pIdent, "Array initializers not allowed for local variables");
         pInit = FixLocalInitializer (pIdent, pInit);
         CheckExpression (pInit);
         }
      }
   }


//
// sets variable and function type info for var/fns in a declaration 
// list chain.
// (pDecl is TOK_VARIABLE or TOK_FUNCTION)
//
static void CheckDeclarationList (PNODE pDecList)
   {
   PNODE pDecl, pType;

	assert (pDecList->uID == TOK_DECLARATIONLIST);
   pType = pDecList->pn1;
	DecorateTypeSpecifier (pType);

   for (pDecl = pDecList->pn2; pDecl; pDecl = pDecl->next)
      {
      if (pDecl->uID == TOK_VARIABLE)
         CheckVarDeclaration (pDecl, pType);
      else
         CheckFnDeclaration (pDecl, pType);
      }
   }


//
// sets variable and function type info for var/fns in a declaration 
// list chain.
//
static void CheckDeclarationLists (PNODE pDecList)
   {
   PNODE pDecL;

   if (!pDecList)  // will be NULL checking fn params when it is "(void)"
      return;

   for (pDecL = pDecList; pDecL; pDecL = pDecL->next)
      CheckDeclarationList (pDecL);
   }


//
// EXTERNAL
//
// This is the external function which 
// checks a var or fn declaration (or definition)
//
void CheckDeclaration (PNODE pnode)
   {
   PushScope (SCOPE_GLOBAL);
   CheckDeclarationLists (pnode);
   PopScope ();
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/


// EXTERNAL - Check a typedef parse tree
//		Eval Any Array Sizes
//		Build symbol's TypeList
//		This doesn't generate code so we Free the parse tree ??
//
//	Generating a symbols typelist:
//		Type specifier node has the original type info (base type, another typedef or struct)
//		Identifier has additional ptr or array info 	  (int** i[5] <- stars and array)
//		These 2 parts are combined and are given to the symbol entry
//
void CheckTypedef (PNODE pnode)
   {
   PNODE pType = pnode->pn1;

	DecorateTypeSpecifier (pType);

   for (PNODE pIdent = pnode->pn2; pIdent; pIdent = pIdent->next)
		{
   	PSYM ps = pIdent->ps;
		EvalArraySizeInitializers (pIdent); // cvt ptlst->pEval to ptlst->iCount
		EvalHashInitializers (pIdent); // cvt ptlst->pEval to ptlst->TYPEDEF

		// pType keeps his type info so copy it
		//	pIdent gives up his piece to the symbol
		//	pIdent then points to symbol's ptlst
		ps->ptlst = TLAppend (pIdent->ptlst, TLCopyTypeList (pType->ptlst));
		NodeSetTypeList (pIdent, ps->ptlst, -1);

   	ps->uKind = KIND_TYPEDEF;
		}
	//	pnode = NodeFreeTree (pnode); // caller should be responsible
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

// 
// builds element list array
// calcs element offsets
// calcs struct size
//
static void CalcStructMetrics (PNODE pStruct)
   {
   PNODE pn1, pn2, pIdent;
   int  iElementCount, iElementSize;
   int  iMaxElementSize = 0, iOffset = 0;
   PSYM  *ppElementList;

   /*--- count elements in struct definition ---*/
   iElementCount = 0;
   for (pn1 = pStruct->pn2; pn1; pn1 = pn1->next)
      for (pn2 = pn1->pn2; pn2; pn2 = pn2->next)
         iElementCount++;

   ppElementList = (PSYM*)calloc (iElementCount+1, sizeof (PSYM));

   /*--- fill struct element ptr array ---*/
   iElementCount = 0;
   for (pn1 = pStruct->pn2; pn1; pn1 = pn1->next)
      for (pn2 = pn1->pn2; pn2; pn2 = pn2->next)
         ppElementList[iElementCount++] = pn2->pn1->ps;
   ppElementList[iElementCount] = NULL;

   pStruct->pn1->ps->ppElements = ppElementList;

   /*--- calc element offsets ---*/
   for (pn1 = pStruct->pn2; pn1; pn1 = pn1->next)
      {
      for (pn2 = pn1->pn2; pn2; pn2 = pn2->next)
         {
         pIdent = pn2->pn1;

         if (pStruct->pn1->ps->bUnion)
            pIdent->ps->iAddr = 0;
         else
				{
				MemStructElementAlign (&iOffset, pIdent->ps->ptlst);
            pIdent->ps->iAddr = iOffset;
         	iElementSize = TLMemSize (pIdent->ps->ptlst);
	         iOffset += iElementSize;
				}
         }
      }
   }


//
// EXTERNAL
// Check a Structure definition parse tree
//
// decorate symbols and syntax tree nodes
// make sure structure isn't already defined
// make sure var names are unique in the structure
// calc element offsets
// calc structure size
//
void CheckStructDef (PNODE pStruct)
   {
   CheckDeclarationLists (pStruct->pn2);
   CalcStructMetrics (pStruct);
   pStruct->pn2 = NodeFreeTree (pStruct->pn2); // free definition
   }

