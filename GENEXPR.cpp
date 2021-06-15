/*
 *
 * genexpr.c
 * Monday, 3/3/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include "symbol.h"
#include "check.h"
#include "genfn.h"
#include "genglob.h"
#include "error.h"
#include "label.h"
#include "genexpr.h"
#include "genop.h"
#include "opcodes.h"
#include "type.h"
#include "mem.h"


static void _genExpr (PNODE pnode);
static void _genLVal (PNODE pnode);


static BOOL IsWord (PNODE pnode)
   {
   return (TLNodeType(pnode) != DATATYPE_FLOAT);
   }


/*
 * simple assignment if !uOP
 * op-assignment (+=, |=, etc..) if a valid uOP
 *
 */
static void _genAssign (PNODE pnode, UINT uOP)
   {
   UINT uOp2;

   if (uOP)
      _genExpr (pnode->pn1);

   _genExpr (pnode->pn2);
   if (uOP)
      AddOp (uOP, TLNodeType(pnode));

   // The else clause will also work, but it's less efficient
   if (pnode->pn1->uID == TOK_VARIABLE)
      {
	   PSYM ps = pnode->pn1->pn1->ps;

      if (TLNodeType(pnode->pn1) == DATATYPE_STRUCT)
         {
         AddOpW (OP_PUSHI, 0, TLMemSize (pnode->pn1->ptlst));	// was MemSize
         uOp2 = (ps->uScope == SCOPE_GLOBAL ? OP_SAVNG : OP_SAVNL);
         AddOpA (uOp2, 0, ps->iAddr, ps->uScope);
         }
      else
         {
         uOp2 = (ps->uScope == SCOPE_GLOBAL ? OP_SAVG : OP_SAVL);
         AddOpA (uOp2, TLNodeType(pnode->pn1), ps->iAddr, ps->uScope);
         }
      }
   else
      {
      _genLVal (pnode->pn1);
      AddOp (OP_SAVGS, TLNodeType(pnode->pn1));
      }
   }



/*
 * simple binary operations
 *
 */
static void _genBinop (PNODE pnode, UINT uOP)
   {
   _genExpr (pnode->pn1);
   _genExpr (pnode->pn2);
   AddOp (uOP, TLNodeType(pnode->pn1));
   }


/*
 * This code supports short circuit evaluation
 * if the operands are integers, we use a quicker algorithm
 * that does not conditionally convert the left result
 * (boolean logic ops always return an integer)
 */
static void _genBoolAnd (PNODE pnode)
   {
   PLBL pE, pM;

   if (IsWord (pnode->pn1))
      {
      pE = LabForward ();
      _genExpr (pnode->pn1);
      AddOpL (OP_JZK, TLNodeType(pnode->pn1), pE);
      _genExpr (pnode->pn2);
      AddOp (OP_LAND, TLNodeType(pnode->pn2));
     LabSet (pE);
      LabFree (pE);
      }
   else
      {
      pE = LabForward ();
      pM = LabForward ();
      _genExpr (pnode->pn1);
      AddOpL (OP_JNZ, TLNodeType(pnode->pn1), pM);
      AddOp (OP_PUSH0, 0);
      AddOpL (OP_JMP, 0, pE);
     LabSet (pM);
      _genExpr (pnode->pn2);
      AddOp (OP_NZ, TLNodeType(pnode->pn2));
     LabSet (pE);
      LabFree (pM);
      LabFree (pE);
      }
   }


/*
 * This code supports short circuit evaluation
 * if the operands are integers, we use a quicker algorithm
 * that does not conditionally convert the left result
 * (boolean logic ops always return an integer)
 */
static void _genBoolOr (PNODE pnode)
   {
   PLBL pE, pM;

   if (IsWord (pnode->pn1))
      {
      pE = LabForward ();
      _genExpr (pnode->pn1);
      AddOpL (OP_JNZK, TLNodeType(pnode->pn1), pE);
      _genExpr (pnode->pn2);
      AddOp (OP_LOR, TLNodeType(pnode->pn2));
      LabSet (pE);
      LabFree (pE);
      }
   else
      {
      pE = LabForward ();
      pM = LabForward ();
      _genExpr (pnode->pn1);
      AddOp (OP_NZ, TLNodeType(pnode->pn1));
      AddOpL (OP_JZ, 0, pM);
      AddOp (OP_PUSH1, 0);
      AddOpL (OP_JMP,   0, pE);                
      LabSet (pM);
      _genExpr (pnode->pn2);
      AddOp (OP_NZ, TLNodeType(pnode->pn2));
      LabSet (pE);
      LabFree (pM);
      LabFree (pE);
      }
   }


/*
 * ternary
 *
 */
static void _genQuestion (PNODE pnode)
   {
   PLBL pF, pE;

   pF = LabForward (); // false expr loc
   pE = LabForward (); // end of expr loc

   _genExpr (pnode->pn3);
   AddOpL (OP_JZ, TLNodeType(pnode->pn3), pF);

   _genExpr (pnode->pn1);
   AddOpL (OP_JMP, TLNodeType(pnode->pn1), pE);

   LabSet (pF);
   _genExpr (pnode->pn2);
   LabSet (pE);
   LabFree (pF);
   LabFree (pE);
   }


///*
// * prefix/postfix increment/decrement of a local/global
// *
// */
//static void _genCrement (PNODE pnode, UINT uOpG, UINT uOpL)
//   {
//   PSYM ps;
//   UINT uOp1, uOp2;
//
//   ps = (pnode->pn1 ? pnode->pn1->pn1->ps : pnode->pn2->pn1->ps);
//
//   uOp1 = (ps->uScope == SCOPE_GLOBAL ? uOpG  : uOpL);
//   uOp2 = (ps->uScope == SCOPE_GLOBAL ? OP_PUSHG : OP_PUSHL);
//
//   if (pnode->pn1)  // prefix increment/decrement
//      {
//      AddOpA (uOp1, TLNodeType(pnode), ps->iAddr, ps->uScope);
//      AddOpA (uOp2, TLNodeType(pnode), ps->iAddr, ps->uScope);
//      }
//   else             // postfix increment/decrement
//      {
//      AddOpA (uOp2, TLNodeType(pnode), ps->iAddr, ps->uScope);
//      AddOpA (uOp1, TLNodeType(pnode), ps->iAddr, ps->uScope);
//      }
//   }


/*
 * prefix/postfix increment/decrement of a local/global
 *
 */
static void _genCrement (PNODE pnode, UINT uOp)
   {
   if (pnode->pn1)  // prefix increment/decrement
		{
	   _genLVal (pnode->pn1);
      AddOp (uOp, TLNodeType(pnode)); // new INC DEC looks at but doesn't effect stack
      AddOp (OP_PUSHGS, TLNodeType(pnode));
		}
	else				 // postfix increment/decrement : this will not handle floats!
		{
	   _genLVal (pnode->pn2);
      AddOp (OP_DUPS, 0); 			  	  
      AddOp (OP_PUSHGS, TLNodeType(pnode));
      AddOp (OP_SWAPS, 0); 			  
      AddOp (uOp, TLNodeType(pnode)); // new INC DEC looks at but doesn't effect stack
	   AddOpS (OP_DSP, 0, 1);
		}
	}


/*
 * returns number of stack words used
 * The first entry contains type info about the 1st 10 params
 * The second entry contains info about the # of actuals
 * the number of actuals after padding, and the size in stack words
 * of the parameters
 */
static INT _generateStatusData (PNODE pFn)
   {
   PNODE pIdent, pParmL, pParm;
   ULONG ulTmp, ulTyp, ul1, ul2, ulParmSpace, ulParmCount;

   pIdent = pFn->pn1;
   pParmL = pFn->pn2;

   ul2 = ulParmSpace = ulParmCount = 0;
   for (pParm = pParmL; pParm; pParm = pParm->next)
      {
//      ulParmSpace += MemStkSize (TLNodeType(pParm), FindStructType (pParm, 0));
//      ulParmSpace += TLMemStkSize (pParm->ptlst);
      ulParmSpace += TLMemRefStkSize (pParm->ptlst);

      ulTyp = 0x00;
      switch (TLNodeType(pParm))
         {
         case DATATYPE_CHAR : ulTyp = M_BYTE;  break;
         case DATATYPE_SHORT: ulTyp = M_SHORT; break;
         case DATATYPE_LONG : ulTyp = M_WORD;  break;
         case DATATYPE_FLOAT: ulTyp = M_FLOAT; break;
         default            : ulTyp = 0x04;    break; // ptr
         }
      if (ulParmCount < 10)
         {
         ulTmp = ulTyp << (ulParmCount * 3); 
         ul2 |= ulTmp;
         }
      ulParmCount ++;
      }
   ul1 = (ULONG)(pFn->val.s) | ulParmCount<<8 | ulParmSpace<<16;

   AddOpW (OP_PUSHI, 0, ul2);
   AddOpW (OP_PUSHI, 0, ul1);
   return 2;
   }


/*
 * push actuals (function call parameters) in reverse order
 *
 */
static INT _genActuals (PNODE pParmL)
   {
   INT iSize;

   if (!pParmL)
      return 0;

   iSize = _genActuals (pParmL->next);
   GenExpression (pParmL);                    // --- push actual ---
//   iSize += MemStkSize (TLNodeType(pParmL), FindStructType (pParmL, 0));
//   iSize += TLMemStkSize (pParmL->ptlst);
   iSize += TLMemRefStkSize (pParmL->ptlst);
   return iSize;
   }


/*
 * Generates the code for an internal or user function call
 *
 */
static void _genFunctionCall (PNODE pFn)
   {
   INT   i, iRetSize, iStkSpace = 0;
   PNODE pIdent, pParmL;

   pIdent = pFn->pn1;
   pParmL = pFn->pn2;

//   if (iRetSize = MemStkSize (pIdent->ps->uTypeVal, pIdent->ps->psStruct))
   if (iRetSize = TLMemRefStkSize (pIdent->ptlst))
      AddOpS (OP_ISP, 0, iRetSize);

   iStkSpace  = _genActuals (pParmL);
   iStkSpace += _generateStatusData (pFn);

   AddOpW (OP_PUSHI, 0, -iStkSpace-2-iRetSize);  // offset to return val address

   if (!pIdent->ps->uInternal)
      {
      AddOp (OP_CALL, 0);
      for (i=0; pIdent->ps->pszLiteral[i]; i++)
         AddChar (pIdent->ps->pszLiteral[i]);
      AddChar (0);
      }
   else
      {
      AddOpS    (OP_CALLI, 0, pIdent->ps->uInternal);
      }

   /*--- cleanup after call ---*/
   AddOpS (OP_DSP, 0, iStkSpace+1);
   }

//	assumes value is on top of stack already
//
//static void _genHashSet (PNODE pnode)
//	{
//   _genExpr (pnode->pn2); 							// the key		  +1 SP
//   _genExpr (pnode->pn1); 							// the variable  +1 SP
//   AddOpW (OP_PUSHI, 0, TLNodeType (pnode));	// Type Hint	  +1 SP
//   AddOp (OP_HSET, 0);	  							// -3 SP  net 0 SP
//	}


//	puts h
//
//
//
static void _genHashGet (PNODE pnode)
	{
   _genExpr (pnode->pn2); 							  // the key
   _genExpr (pnode->pn1); 							  // the variable
	DWORD dwFlags = TLMemSize (TLDeref(pnode->pn1->ptlst)) |	
		             (TLIsString(pnode->pn1->ptlst->pExpr->ptlst) ? 0x10000 : 0);
	AddOpA (OP_HGET, 0, dwFlags, SCOPE_GLOBAL); //	2pop&1push=-1 SP
	}

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/


static void _genLVal (PNODE pnode)
   {
   switch (pnode->uID)
      {
      case TOK_VARIABLE:
         AddOpS (OP_PUSHI, DATATYPE_SHORT, pnode->pn1->ps->iAddr);
         if (pnode->pn1->ps->uScope == SCOPE_STRUCT) // struct element
            return;
         AddOp ((pnode->pn1->ps->uScope == SCOPE_GLOBAL ? OP_ADDRG : OP_ADDRL), 0);
         return;

      case TOK_DEREF:
         _genExpr (pnode->pn1);
         return;

		case TOK_HASHDEREF:
//			if (TLGetDerefCount (TLDeref(pnode->ptlst)))	//	this isn't clean
				_genHashGet (pnode);
//			else
//				_genHashSet (pnode);
         return;

      case TOK_ADDROF:
         _genLVal (pnode->pn1);
         return;

      case TOK_ARROW:
         _genExpr (pnode->pn1);
         _genLVal (pnode->pn2);
         AddOp (OP_ADD, 0);
         return;

      case TOK_PERIOD:
         _genLVal (pnode->pn1);
         _genLVal (pnode->pn2);
         AddOp (OP_ADD, 0);
         return;

      default:
         _genExpr (pnode);
         return;
      }
   }


/*
 * Generates the code represented by a pnode tree
 *
 */
static void _genExpr (PNODE pnode)
   {
   UINT uOp;

   if (!pnode)
      return;

   switch (pnode->uID)
      {
      case TOK_EQUALS:          _genAssign (pnode, 0);       return;
      case TOK_PLUSEQUAL:       _genAssign (pnode, OP_ADD);  return;
      case TOK_MINUSEQUAL:      _genAssign (pnode, OP_SUB);  return;
      case TOK_STAREQUAL:       _genAssign (pnode, OP_MUL);  return;
      case TOK_SLASHEQUAL:      _genAssign (pnode, OP_DIV);  return;
      case TOK_PERCENTEQUAL:    _genAssign (pnode, OP_MOD);  return;
      case TOK_SHIFTRIGHTEQUAL: _genAssign (pnode, OP_SHR);  return;
      case TOK_SHIFTLEFTEQUAL:  _genAssign (pnode, OP_SHL);  return;
      case TOK_ANDEQUAL:        _genAssign (pnode, OP_AND);  return;
      case TOK_XOREQUAL:        _genAssign (pnode, OP_XOR);  return;
      case TOK_OREQUAL:         _genAssign (pnode, OP_OR );  return;
                                
      case TOK_STAR:            _genBinop (pnode, OP_MUL);  return;
      case TOK_SLASH:           _genBinop (pnode, OP_DIV);  return;
      case TOK_PERCENT:         _genBinop (pnode, OP_MOD);  return;
      case TOK_PLUS:            _genBinop (pnode, OP_ADD);  return;
      case TOK_SHIFTRIGHT:      _genBinop (pnode, OP_SHR);  return;
      case TOK_SHIFTLEFT:       _genBinop (pnode, OP_SHL);  return;
      case TOK_LESSTHAN:        _genBinop (pnode, OP_LT );  return;
      case TOK_GREATERTHAN:     _genBinop (pnode, OP_GT );  return;
      case TOK_LESSOREQUAL:     _genBinop (pnode, OP_LE );  return;
      case TOK_GREATEROREQUAL:  _genBinop (pnode, OP_GE );  return;
      case TOK_EQUIVALENT:      _genBinop (pnode, OP_EQ );  return;
      case TOK_NOTEQUAL:        _genBinop (pnode, OP_NE );  return;
      case TOK_AMPERSAND:       _genBinop (pnode, OP_AND);  return;
      case TOK_HAT:             _genBinop (pnode, OP_XOR);  return;
      case TOK_OR:              _genBinop (pnode, OP_OR );  return;

      case TOK_MINUS:
         if (pnode->pn2)
            {
            _genBinop (pnode, OP_SUB);  // bianry minus
            }
         else
            {
            _genExpr (pnode->pn1);
            AddOp (OP_NEG, TLNodeType(pnode)); // unary minus
            }
         break;

      case TOK_EXCLAMATION:
         _genExpr (pnode->pn1);
         AddOp (OP_LNOT, TLNodeType(pnode->pn1)); // auto int conversion
         break;

      case TOK_TILDA:
         _genExpr (pnode->pn1);
         AddOp (OP_NOT, TLNodeType(pnode->pn1)); // auto int conversion
         break;

      case TOK_COMMA:
         _genExpr (pnode->pn1);
         _genExpr (pnode->pn2);
//         AddOp (OP_DSP, MemStkSize (TLNodeType(pnode->pn2), FindStructType (pnode->pn2, 0)));
         AddOp (OP_DSP, TLMemStkSize (pnode->pn2->ptlst));
         break;
         
      case TOK_FUNCTION:
         _genFunctionCall (pnode);
         break;

      case TOK_VARIABLE:
         if (TLNodeIsArray (pnode->pn1) || TLNodeIsHash (pnode->pn1))
            _genLVal (pnode);
         else if (TLNodeType(pnode) == DATATYPE_STRUCT)
            {
//          AddOpW (OP_PUSHI, 0, MemSize (TLNodeType(pnode), FindStructType (pnode, 1)));
            AddOpW (OP_PUSHI, 0, TLMemSize (pnode->pn1->ptlst));
            uOp = (pnode->pn1->ps->uScope == SCOPE_GLOBAL ? OP_PUSHNG : OP_PUSHNL);
            AddOpA (uOp, TLNodeType(pnode), pnode->pn1->ps->iAddr, pnode->pn1->ps->uScope);
            }
         else
            {
            uOp = (pnode->pn1->ps->uScope == SCOPE_GLOBAL ? OP_PUSHG : OP_PUSHL);
            AddOpA (uOp, TLNodeType(pnode), pnode->pn1->ps->iAddr, pnode->pn1->ps->uScope);
            }
         break;

      case TOK_LITERAL:
         {
         PNODE pLit;

         pLit = pnode->pn1;
         switch (TLNodeType(pLit))
            {
            case DATATYPE_CHAR:   
               AddOpC (OP_PUSHI, TLNodeType(pLit), pLit->val.c);
               break;

            case DATATYPE_SHORT:
               AddOpS (OP_PUSHI, TLNodeType(pLit), pLit->val.s);
               break;

            case DATATYPE_LONG:
               AddOpW (OP_PUSHI, TLNodeType(pLit), pLit->val.l);
               break;

            case DATATYPE_FLOAT:  
               AddOpF (OP_PUSHI, TLNodeType(pLit), pLit->val.bg);
               break;

            case DATATYPE_PTR:
					assert (TLIsString(pLit->ptlst));
               AddOpW (OP_PUSHI, TLNodeType(pLit), AddStatic (pLit->val.psz, strlen (pLit->val.psz)+1));
               AddOp (OP_ADDRG, 0);
               break;

            case DATATYPE_STRING: 
               AddOpW (OP_PUSHI, TLNodeType(pLit), AddStatic (pLit->val.psz, strlen (pLit->val.psz)+1));
               AddOp (OP_ADDRG, 0);
               break;

            default:
               NodeError (pLit, "internal error: unknown constant type [%d]", TLNodeType(pLit));
            }
         }
         break;

      case TOK_LOGICALAND:
         _genBoolAnd (pnode);
         break;

      case TOK_LOGICALOR:
         _genBoolOr (pnode);
         break;

      case TOK_INCREMENT:
         _genCrement (pnode, OP_INC);
         break;

      case TOK_DECREMENT:
         _genCrement (pnode, OP_DEC);
         break;

      case TOK_QUESTION:
         _genQuestion (pnode);
         break;

      case TOK_FTOW:
         _genExpr (pnode->pn1);
         AddOp (OP_FTOW, 0);
         break;

      case TOK_WTOF:
         _genExpr (pnode->pn1);
         AddOp (OP_WTOF, 0);
         break;

      case TOK_CAST:
         _genExpr (pnode->pn2);
         break;

      case TOK_DEREF:
         _genExpr (pnode->pn1);
			if (!TLNodeIsArray(pnode) && !TLNodeIsHash(pnode))  // new condition!
	         AddOp (OP_PUSHGS, TLNodeType(pnode));
         break;

      case TOK_HASHDEREF:
//         _genLVal (pnode);
			_genHashGet (pnode);
			if (!TLNodeIsArray(pnode) && !TLNodeIsHash(pnode))  // new condition!
	         AddOp (OP_PUSHGS, TLNodeType(pnode));
         break;

      case TOK_ADDROF:
         _genLVal (pnode);
         break;

      case TOK_ARROW:
         _genExpr (pnode->pn1);
         _genLVal (pnode->pn2);
         AddOp (OP_ADD, 0);
         if (!TLNodeIsArray (pnode->pn2->pn1))
            AddOp (OP_PUSHGS, TLNodeType(pnode->pn2));  // maybe  TLNodeType(pnode) ???
         break;

      case TOK_PERIOD:
         _genLVal (pnode->pn1);
         _genLVal (pnode->pn2);
         AddOp (OP_ADD, 0);
         if (!TLNodeIsArray (pnode->pn2->pn1))
            AddOp (OP_PUSHGS, TLNodeType(pnode->pn2));
         break;

      case TOK_SHL1:
         _genExpr (pnode->pn1);
         AddOp (OP_SHL1, 0);
         break;

      case TOK_SHL2:
         _genExpr (pnode->pn1);
         AddOp (OP_SHL2, 0);
         break;

      case TOK_SHL3:
         _genExpr (pnode->pn1);
         AddOp (OP_SHL3, 0);
         break;

      default:
         NodeError (pnode, "Bad expression node in _genExpr: [%d]", pnode->uID);
      }
   }



/*
 * Generates the code represented by a pnode tree
 *
 */
void GenExpression (PNODE pExpr)
   {
   _genExpr (pExpr->pn1);
   }


/*
 * a condition is an expression that always ends up being a
 * 0 or non zero word
 * if pT ! NULL, jump there if result is non zero
 * if pF ! NULL, jump there if result is zero
 *
 * if expression is a natural int expr, nothing is done before
 * the jump checks are done.
 *
 * (otherwise a NZ(C,F,S) is inserted first to get a bool result)
 *
 */
void GenCondition (PNODE pExpr, PLBL pT, PLBL pF)
   {
   GenExpression (pExpr);

   if (pT && pF)
      {
      AddOpL (OP_JNZ, TLNodeType(pExpr), pT);
      AddOpL (OP_JMP, TLNodeType(pExpr), pF);
      }
   else if (pT && !pF)
      {
      AddOpL (OP_JNZ, TLNodeType(pExpr), pT);
      }
   else if (!pT && pF)
      {
      AddOpL (OP_JZ, TLNodeType(pExpr), pF);
      }
   else if ((TLNodeType (pExpr) != DATATYPE_SHORT) && (TLNodeType (pExpr) != DATATYPE_LONG))
      {
      AddOp (OP_NZ, TLNodeType(pExpr));
      }
   }

