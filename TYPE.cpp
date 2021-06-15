/*
 *
 * type.c
 * Wednesday, 3/12/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include "symbol.h"
#include "tokenize.h"
#include "parse.h"
#include "check.h"
#include "error.h"
#include "opt.h"
#include "type.h"
#include "node.h"
#include "mem.h"



/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

BOOL TLIsVoidPtr 	 (PTLST ptlst) {return (TLIsPtr(ptlst) && TLIsVoid(ptlst->next));}

BOOL TLIsVoid 	 (PTLST ptlst) {return (ptlst->cType == DATATYPE_VOID);}

BOOL TLIsArray  (PTLST ptlst) {return (ptlst->cType == DATATYPE_ARRAY);}

BOOL TLIsHash  (PTLST ptlst) {return (ptlst->cType == DATATYPE_HASH);}

BOOL TLIsPtr    (PTLST ptlst) {return (ptlst->cType == DATATYPE_PTR);}

BOOL TLIsPtrOrArray (PTLST ptlst) {return (ptlst->cType == DATATYPE_PTR || ptlst->cType == DATATYPE_ARRAY);}

BOOL TLIsAddress (PTLST ptlst) 
	{
	return (ptlst->cType == DATATYPE_PTR   || 
			  ptlst->cType == DATATYPE_ARRAY || 
			  ptlst->cType == DATATYPE_HASH);
	}

BOOL TLIsStruct (PTLST ptlst)	{return (ptlst->cType == DATATYPE_STRUCT);}

BOOL TLIsFloat  (PTLST ptlst)	{return (ptlst->cType == DATATYPE_FLOAT);	}


BOOL TLIsIntegral (PTLST ptlst)
	{
	return (ptlst->cType == DATATYPE_CHAR  ||  ptlst->cType == DATATYPE_SHORT || ptlst->cType == DATATYPE_LONG );
	}

BOOL TLIsString (PTLST ptlst)
	{
	return ((ptlst->cType == DATATYPE_PTR  || ptlst->cType == DATATYPE_ARRAY) &&
			  ptlst->next && ptlst->next->cType == DATATYPE_CHAR);
	}


BOOL TLIsSameTypes (PTLST ptlst1, PTLST ptlst2)
	{
	for (; ptlst1 && ptlst2; ptlst1 = ptlst1->next, ptlst2 = ptlst2->next)
		{
		if ((ptlst1->cType != ptlst2->cType) &&
			 (!TLIsPtrOrArray (ptlst1) || !TLIsPtrOrArray (ptlst2)))
			return FALSE;
		if ((ptlst1->cType == DATATYPE_STRUCT) && (ptlst1->pSym != ptlst2->pSym))
			return FALSE;
		}
	if (ptlst1 || ptlst2)
		return FALSE;
	return TRUE;
	}

PTLST TLGetTail (PTLST ptlst)
	{
	while (ptlst && ptlst->next)
		ptlst = ptlst->next;
	return ptlst;
	}

UINT TLGetType (PTLST ptlst)		 //	top level type		  ***char[] is array
	{
	return ptlst->cType;
	}

UINT TLGetBaseType (PTLST ptlst)	 //	lowest level type   ***char[] is char
	{
	return TLGetTail(ptlst)->cType;
	}

UINT TLGetDerefCount (PTLST ptlst) //   ***char is 3 	     ***char[] is 4		 ???
	{
	int i;

	for (i=0; ptlst && TLIsPtrOrArray(ptlst); ptlst = ptlst->next, i++)
		;
	return i;
	}


PTLST TLAppend (PTLST ptlst, PTLST ptlstSuffix) // add ptlstSuffix on to ptlst
	{
	if (!ptlst)
		return ptlstSuffix;
	PTLST ptlstTmp = TLGetTail (ptlst);
	ptlstTmp->next = ptlstSuffix;
	return ptlst;
	}


PTLST TLNewElement (PTLST ptlstProto)
	{
	PTLST ptlstNew = (PTLST)calloc (1, sizeof (TYPLST));
	if (ptlstProto)
		memcpy (ptlstNew, ptlstProto, sizeof (TYPLST));
	ptlstNew->next = NULL;
	return ptlstNew;
	}

PTLST TLNewArrayElement (PNODE pExpr)
	{
	PTLST ptlst   = TLNewElement (NULL);
	ptlst->cType  = DATATYPE_ARRAY;
	ptlst->pExpr  = pExpr;
	return ptlst;
	}

PTLST TLNewHashElement (PNODE pType)
	{
	PTLST ptlst   = TLNewElement (NULL);
	ptlst->cType  = DATATYPE_HASH;
	ptlst->pExpr  = pType;
	return ptlst;
	}

PTLST TLNewStructElement (PSYM pSym)
	{
	PTLST ptlst  = TLNewElement (NULL);
	ptlst->cType = DATATYPE_STRUCT;
	ptlst->pSym  = pSym;
	return ptlst;
	}

PTLST TLCopyTypeList (PTLST ptlst)
	{
	PTLST ptlstPtr;
	PTLST ptlstNew;
	PTLST ptlstHead = NULL;

	for (; ptlst; ptlst = ptlst->next)
		{
		ptlstNew = TLNewElement (ptlst);
		if (!ptlstHead)
			ptlstHead = ptlstNew;
		else
			ptlstPtr->next = ptlstNew;
		ptlstPtr = ptlstNew;
		}
	return ptlstHead;	
	}

PTLST TLAddToHead (PTLST ptlst, UINT uPrimitiveType)
	{
	PTLST ptlstNew = TLNewElement (NULL);
	ptlstNew->cType = uPrimitiveType;
	ptlstNew->next = ptlst;
	return ptlstNew;
	}

PTLST TLCreateBaseTypeList(UINT uDataType)
	{
	if (uDataType == DATATYPE_STRING) // not really a base type but lets allow it
		{
		PTLST ptlst = TLAddToHead (NULL, DATATYPE_CHAR);
		return TLAddToHead (ptlst, DATATYPE_PTR);
		}
	return TLAddToHead (NULL, uDataType);
	}



//	Add a type list element at a specific position
//
PTLST TLAddAtPos (PTLST ptlst, int iIdx, PTLST ptlstNewElement)
	{
	PTLST ptlstHead	= ptlst;
	PTLST ptlstNext = ptlst;
	PTLST ptlstPrev	= NULL;

	for (int i=0; i<iIdx; i++)
		{
		assert (ptlstNext);
		ptlstPrev = ptlstNext;
		ptlstNext = ptlstNext->next;
		}
	if (ptlstPrev)
		ptlstPrev->next = ptlstNewElement;
	else
		ptlstHead = ptlstNewElement;

	ptlstNewElement->next = ptlstNext;

	return ptlstHead;
	}


PTLST TLDeref (PTLST ptlst)
	{
	assert (TLIsPtrOrArray(ptlst) || TLIsHash (ptlst));
	return ptlst->next;
	}

//	This frees the type list
//	This also free any expression node trees that could be hanging
//	of any array type elements
//
PTLST TLFree (PTLST ptlst)
	{
	PTLST ptlstNext;
	
	for (; ptlst; ptlst = ptlstNext)
		{
		ptlstNext = ptlst->next;
		if (ptlst->cType == DATATYPE_ARRAY)
			ptlst->pExpr = NodeFreeTree (ptlst->pExpr);
		free (ptlst);
		}
	return NULL;
	}


PTLST* FreeTLArray (PTLST* ppFormals)
	{
   for (int iCount=0; ppFormals[iCount]; iCount++)
		TLFree (ppFormals[iCount]);
	MemFreeData (ppFormals);
	return NULL;
	}

BOOL TLNodeIsVoid 		 (PNODE pnode)	{ return (TLIsVoid 		  (pnode->ptlst)); }
BOOL TLNodeIsFloat		 (PNODE pnode)	{ return (TLIsFloat 		  (pnode->ptlst)); }
BOOL TLNodeIsPtr 			 (PNODE pnode)	{ return (TLIsPtr 		  (pnode->ptlst)); }
BOOL TLNodeIsArray		 (PNODE pnode)	{ return (TLIsArray 		  (pnode->ptlst)); }
BOOL TLNodeIsHash		 	 (PNODE pnode)	{ return (TLIsHash 		  (pnode->ptlst)); }
BOOL TLNodeIsPtrOrArray  (PNODE pnode)	{ return (TLIsPtrOrArray  (pnode->ptlst)); }
BOOL TLNodeIsAddress     (PNODE pnode)	{ return (TLIsAddress 	  (pnode->ptlst)); }
BOOL TLNodeIsStruct 		 (PNODE pnode)	{ return (TLIsStruct 	  (pnode->ptlst)); }
BOOL TLNodeIsIntegral	 (PNODE pnode)	{ return (TLIsIntegral 	  (pnode->ptlst)); }
int  TLNodeGetDerefCount (PNODE pnode)	{ return (TLGetDerefCount (pnode->ptlst)); }
int  TLNodeType 			 (PNODE pnode)	{ return (TLGetType 		  (pnode->ptlst)); }


//	croak if datatype is void
//
//
void TypNoVoid (PNODE pnode) 
   {
   if (TLNodeIsVoid (pnode))
      NodeError (pnode, "void type in expression");
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

//	only valid for token ID's that represent basic types
//
//
UINT TokIDToDataType (UINT uTokenID)
	{
	switch (uTokenID)
		{
      case TOK_VOID:   
         return DATATYPE_VOID;

      case TOK_CHARLITERAL:
      case TOK_CHAR:   
         return DATATYPE_CHAR;

      case TOK_SHORTLITERAL:
      case TOK_SHORT:  
         return DATATYPE_SHORT;

      case TOK_INTLITERAL:
      case TOK_INT:    
         return IntTyp ();

      case TOK_LONGLITERAL:
      case TOK_LONG:   
         return DATATYPE_LONG;

      case TOK_FLOATLITERAL:
      case TOK_FLOAT:  
         return DATATYPE_FLOAT;

      case TOK_STRINGLITERAL:	 // not really a basic type ...
      case TOK_STRING: 
         return DATATYPE_STRING;

      case TOK_STRUCT: 
      case TOK_UNION: 
         return DATATYPE_STRUCT;
		}
	return Error ("internal: TokIDToDataType [%d]", uTokenID);
	}


//	looks at token ID and returns TRUE if the token ID
//	represents a Base Type
//
BOOL TokTypIsBaseType (UINT uID)
   {
   switch (uID)
      {
      case TOK_VOID  :
      case TOK_CHAR  :
      case TOK_SHORT :
      case TOK_INT   :
      case TOK_LONG  :
      case TOK_FLOAT :
      case TOK_STRING:	// not really but lets allow it
         return TRUE;
      }
   return FALSE;
   }


//	looks at the current token
//	returns TRUE is the token is a type specifier
//
BOOL TokTypIsType (void)
   {
   if (TokTypIsBaseType (Token.uID))
      return TRUE;

   if (Token.uID == TOK_STRUCT || Token.uID == TOK_UNION)
      return TRUE;
   
   if (Token.uID == TOK_IDENTIFIER)
      return (Token.ps->uKind == KIND_TYPEDEF);

   return FALSE;
   }

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

///*
// * deref partially set in parse
// */
//void TypCombineVal (PSYM ps, UINT uTypeVal)
//   {
//   ps->uTypeBase   =  uTypeVal & 0x00FF;
//   ps->uTypeDeref += (uTypeVal >> 8);
//   }

///*
// * this takes a datatype subtree 
// * and builds a TypeVal from it
// */
////UINT TypBuildVal (PNODE pnode) ...
//   {
//   switch (pnode->uID)
//      {
//      case TOK_VOID:   
//         return DATATYPE_VOID;
//
//      case TOK_CHARLITERAL:
//      case TOK_CHAR:   
//         return DATATYPE_CHAR;
//
//      case TOK_SHORTLITERAL:
//      case TOK_SHORT:  
//         return DATATYPE_SHORT;
//
//      case TOK_INTLITERAL:
//      case TOK_INT:    
//         return IntTyp ();
//
//      case TOK_LONGLITERAL:
//      case TOK_LONG:   
//         return DATATYPE_LONG;
//
//      case TOK_FLOATLITERAL:
//      case TOK_FLOAT:  
//         return DATATYPE_FLOAT;
//
//      case TOK_STRINGLITERAL:
//      case TOK_STRING: 
//         return 0x0100 + DATATYPE_CHAR;
//
//      case TOK_STRUCT: 
//      case TOK_UNION: 
//         return DATATYPE_STRUCT;
//
//      case TOK_IDENTIFIER: // a defined type
//         return pnode->ps->uTypeVal;
//
//      default: NodeError (pnode, "internal: TypBuildVal [%d]", pnode->uID);
//      }
//   return 0;
//   }


///*
// * null unless pType is struct or ptr to struct
// *
// */
//PSYM StructTypePtr (PNODE pType)
//   {
//   /*--- struct type ---*/
//   if (pType->uID == TOK_STRUCT || pType->uID == TOK_UNION)
//      {
//      return pType->pn1->ps;
//      }
//
//   if (pType->uID != TOK_IDENTIFIER)
//      return NULL;
//
//   /*--- typedef ident ---*/
//   if (pType->ps->uKind != KIND_TYPEDEF)
//      return NULL;
//
//   return pType->ps->psStruct; // null unless a struct typedef
//   }

//void TypXfer (PSYM ps, PNODE pType)
//   {
//   UINT uTypeVal;
//
//   uTypeVal = TypBuildVal (pType);
//   TypCombineVal (ps, uTypeVal);
//
//   ps->psStruct = StructTypePtr (pType); // null unless pType is struct or ptr to struct
//
//   /*--- if type is a defined type, it may have array info ---*/
//   if (pType->uID == TOK_IDENTIFIER)
//      {
//      ps->bArray     = pType->ps->bArray;
//      ps->uArraySize = pType->ps->uArraySize;
//      }
//   }
//

//UINT TypPtrBase (UINT uTypeVal)
//   {
////   if (!(uTypeVal & 0x00FF))
////      Error ("TypPtrBase: val not a pointer [%d]", uTypeVal);
//   if (!(uTypeVal & 0xFF00))
//      Error ("TypPtrBase: val not a pointer [%d]", uTypeVal);
//   return uTypeVal - 0x100;
//   }
//


//
//
//BOOL DataTypIntegral (UINT uTypeVal)
//   {
//   return (uTypeVal == DATATYPE_CHAR  ||
//           uTypeVal == DATATYPE_SHORT ||
//           uTypeVal == DATATYPE_LONG);
//   }
//
//
//BOOL DataTypFloat (UINT uTypeVal)
//   {
//   return (uTypeVal == DATATYPE_FLOAT);
//   }
//
//
///*
// * Pointy bird, so pointy pointy.
// * anoint my head, anointy nointy.
// */
//BOOL DataTypPtr (UINT uTypeVal)
//   {
//   return (uTypeVal == DATATYPE_PTR    ||
//           uTypeVal == DATATYPE_STRING ||
//           uTypeVal > 0xFF);
//   }
//
//
///*
// * Jointed bird, anointed jointly
// *
// */
//BOOL TypString (UINT uTypeVal)
//   {
//   return (uTypeVal == DATATYPE_STRING ||
//           uTypeVal == 0x100 + DATATYPE_CHAR);
//   }

////PNODE TypCast (PNODE pnode, UINT uNewType)
////	pnode is the node that neds to be changed
////	ptlst is the destination type 
////	This only adds a type change node if there needs to be a coersion
////
//PNODE TypCoersion (PNODE pnode, PTLST ptlst)
//
//   {
//   UINT uOldType;
//
//   uOldType = pnode->uTypeVal;
//
//   if (TypFloat (uOldType) && TypIntegral (uNewType))
//      return BuildCastNode (TOK_FTOW, pnode, uNewType);
//   if (TypIntegral (uOldType) && TypFloat (uNewType))
//      return BuildCastNode (TOK_WTOF, pnode, uNewType);
//
////   pnode->uTypeVal = uNewType; 
//
//   return pnode;
//   }
//

///*
// * uAllowPtrConversion: 0 - no
// *                      1 - yes with warning
// *                      2 - yes, and old type can be pvoid wo warning
// */
//PNODE TypInsert (PNODE pnode, UINT uOldType, UINT uNewType, BOOL bAllowPtrConversion, BOOL bDieOnError)
//   {
//   PNODE pTmp;
//
//   if (bAllowPtrConversion && TypPtr(uOldType) && TypPtr (uNewType))
//      {
//      TypCheckPointers (pnode, uOldType, uNewType, bAllowPtrConversion == 2);
//      return pnode;
//      }
//
//   if (uOldType == uNewType)
//      return pnode;
//
//   if (TypIntegral (uOldType) && TypFloat (uNewType))
//      return BuildCastNode (TOK_WTOF, pnode, uNewType);
//
//   if (TypFloat (uOldType) && TypIntegral (uNewType))
//      {
//      TypPrecisionWarning (pnode);
//      return BuildCastNode (TOK_FTOW, pnode, uNewType);  
//      }
//   if (TypIntegral (uOldType) && TypIntegral (uNewType))
//      {
//      if (MemSize (uOldType, NULL) > MemSize (uNewType, NULL))
//         {
//         pTmp = (pnode->uID == TOK_EXPR ? pnode->pn1 : pnode);
//         if (pTmp->uID == TOK_LITERAL)
//            {
//            if (uNewType == DATATYPE_CHAR && (pTmp->pn1->val.l > 255 || pTmp->pn1->val.l < 0) )
//               TypPrecisionWarning2 (pnode);
//            if (uNewType == DATATYPE_SHORT && (pTmp->pn1->val.l > 32767 || pTmp->pn1->val.l < -32768))
//               TypPrecisionWarning2 (pnode);
//            }
//         else
//            TypPrecisionWarning (pnode);
//         }
//      return pnode;
//      }
//   if (TypIntegral (uOldType) && TypPtr (uNewType))
//      {
//      pTmp = (pnode->uID == TOK_EXPR ? pnode->pn1 : pnode);
//      if (pTmp->uID == TOK_LITERAL && !pTmp->pn1->val.l)
//         return pnode; // 0 is a valid ptr
//      }
//
//   NodeError (pnode, "Incompatible types");
//   return pnode;
//   }
//
//

///*
// * uPtrs: 0 - no ptrs allowed                  x
// *        1 - ptrs ok
// *        2 - + rules: ptr/ptr -> invalid      x
// *        3 - - rules: ptr/ptr -> int          x
// *
// * uMix:  0 - no mixing i & p                  x
// *        1 - mixing ok
// *        2 - ptr on left only                 x
// */
//UINT TypResolve (PNODE pExpr, UINT bFloats, UINT uPtrs, UINT uMix)
//   {
//   UINT uTypeVal1, uTypeVal2, uma, umi;
//
//   uTypeVal1 = pExpr->pn1->uTypeVal;
//   uTypeVal2 = pExpr->pn2->uTypeVal;
//
//   uma = __max (uTypeVal1, uTypeVal2);
//   umi = __min (uTypeVal1, uTypeVal2);
//
//   if (!uTypeVal1 || !uTypeVal2)
//      NodeError (pExpr, "Operation on void type");
//
//   if (!bFloats && (TypFloat (uTypeVal1) || TypFloat (uTypeVal2)))
//      NodeError (pExpr, "Incompatible type for operation");
//
//   if (!uPtrs && TypPtr (uma))
//      NodeError (pExpr, "Incompatible type for operation");
//
//   if (TypPtr (uma) && TypFloat (umi))
//      NodeError (pExpr, "Incompatible types for operation");
//
//   if (TypPtr (uma) && TypIntegral (umi))
//      {
//      if (!uMix || (uMix == 2 && uma == uTypeVal2))
//         NodeError (pExpr, "Incompatible types for operation");
//      return uma;
//      }
//
//   if (TypIntegral (uma) && TypIntegral (umi))
//      return uma;
//   
//   if (TypPtr (umi)) //  && TypPtr (uma))
//      {
//      if (uPtrs == 2)
//         NodeError (pExpr, "Incompatible type for operation");
//      TypCheckPointers (pExpr, uma, umi, 0);
//      return (uPtrs == 3 ? IntTyp() : uma);
//      }
//
//   if (uma == umi)
//      return uma;
//
//   /*--- f & i  or  i & f ---*/
//   pExpr->pn1 = TypInsert (pExpr->pn1, uTypeVal1, uma, 0, 1);
//   pExpr->pn2 = TypInsert (pExpr->pn2, uTypeVal2, uma, 0, 1);
//
//   return uma;
//   }


///* 
// * uMix 0: int -> ptr not allowed
// *      1: int -> ptr allowed with a warning
// *
// */
//void TypConvertStatic (PNODE pLit, UINT uTargetType, UINT uMix)
//   {
//   UINT uLitType;
//
//   uLitType = pLit->uTypeVal;
//   if (uLitType == uTargetType)
//      return;
//
//   if (TypIntegral (uLitType) && TypIntegral (uTargetType))
//      {
//      if (MemSize (uLitType, NULL) > MemSize (uTargetType, NULL))
//         TypPrecisionWarning (pLit);
//      pLit->uTypeVal = uTargetType;
//      return;
//      }
//
//   if (TypIntegral (uLitType) && TypFloat (uTargetType))
//      {
//      if (uTargetType == DATATYPE_SHORT)
//         pLit->val.bg = (BIG) pLit->val.s; 
//      else
//         pLit->val.bg = (BIG) pLit->val.l; // char type ok here
//      pLit->uTypeVal = uTargetType;
//      return;
//      }
//
//   if (TypFloat (uLitType) && TypIntegral (uTargetType))
//      {
//      TypPrecisionWarning (pLit);
//      switch (uTargetType)
//         {
//         case DATATYPE_CHAR : pLit->val.c = (CHAR )pLit->val.bg; break;
//         case DATATYPE_SHORT: pLit->val.s = (SHORT)pLit->val.bg; break;
//         case DATATYPE_LONG : pLit->val.l = (LONG) pLit->val.bg; break;
//         }
//      pLit->uTypeVal = uTargetType;
//      return;
//      }
//
//   if (TypPtr (uLitType) && TypPtr (uTargetType))
//      {
//      TypCheckPointers (pLit, uLitType, uTargetType, 1); // 0->1 $$
//      return;
//      }
//
//   if (uMix==1 && TypIntegral (uLitType) && TypPtr (uTargetType))
//      {
//      NodeWarn (pLit, "different levels of indirection");
//      pLit->uTypeVal = 0x0100 + DATATYPE_VOID;
//      return;
//      }
//   NodeError (pLit, "invalid static type");
//   }



///*
// *
// */
//void CheckStructTypes (PNODE pnode, PSYM ps1, PSYM ps2)
//   {
//   if (!ps1 && !ps2) // neither are structures
//      return; 
//   if (ps1 && !ps2 || !ps1 && ps2) // one is structure
//      NodeError (pnode, "different basic types");
//   if (ps1 == ps2) // same structure
//      return;
//   if (ps1->uStructSize == ps2->uStructSize)
//      NodeWarn (pnode, "different structure type (of same size)");
//   else
//      NodeWarn (pnode, "different structure type (of different size!)");
//   }


//
//
//
INT IntTyp (void)
   {
   return DATATYPE_LONG;
   }


//	pnode should already be established as a literal type
//
//
//
//PNODE TypConvertLiteral (PNODE pnode, TYPLST ptlst)
//	{
//	PNODE pTmp;
//
//   pTmp = (pnode->uID == TOK_EXPR ? pnode->pn1 : pnode);
//	assert (pTmp->uID == TOK_LITERAL);
//	assert (pTmp = pTmp->pn1);
//	l = pTmp->val.l;
//	f = pTmp->val.bg;
//
//	switch (TLGetType (ptlst))
//		{
//		case DATATYPE_CHAR  :
//			if (l > 255 || l < 0) TypPrecisionWarning2 (pnode);
//			break;
//		case DATATYPE_SHORT :
//			if (l > 32767 || l < -32768) TypPrecisionWarning2 (pnode);
//			break;
//		case DATATYPE_LONG  :
//		case DATATYPE_FLOAT :
//		case DATATYPE_PTR   :
//		case DATATYPE_ARRAY :
//		case DATATYPE_VOID  :
//		case DATATYPE_STRUCT:
//		case DATATYPE_STRING:
//		}
//	}
//
//


//
//void TypCheckPointers (PNODE pnode, UINT uTypeVal1, UINT uTypeVal2, BOOL bVoidOK)
//   {
//   if (uTypeVal1 == uTypeVal2)
//      return;
//
//   if (bVoidOK && uTypeVal1 == 0x100+DATATYPE_VOID)
//      return;
//
//   if ((uTypeVal1 >> 8) != (uTypeVal2 >> 8))
//      NodeWarn (pnode, "different levels of indirection");
//   else
//      NodeWarn (pnode, "Indirection to different types");
//   }
//

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

//
// this is one precise warning
//
static void TypPrecisionWarning (PNODE pnode)
   {
   NodeWarn (pnode, "possible loss of precision");
   }

static void TypPrecisionWarning2 (PNODE pnode)
   {
   NodeWarn (pnode, "loss of precision");
   }

static void TypTruncationWarning (PNODE pnode)
   {
   NodeWarn (pnode, "Value Truncated");
   }


//
//	for adding FTOW and WTOF cast nodes
//
static PNODE BuildCastNode (PNODE pnode, int iDataType)
   {
	assert (iDataType == DATATYPE_FLOAT || iDataType == IntTyp());

	INT   iID = (iDataType == DATATYPE_FLOAT ? TOK_WTOF : TOK_FTOW);
   PNODE pn1 = NodeBuild1 (iID, pnode);
	return NodeSetTypeList (pn1, TLCreateBaseTypeList(iDataType), 1);
   }


//	returns the type list whose base size is larger
//
static PTLST LargestIntegralType (PTLST ptlst1, PTLST ptlst2)
	{
	if (TLMemSize (ptlst1) < TLMemSize (ptlst2))
		return ptlst2;
	return ptlst1;
	}

static BOOL IsALiteral (PNODE pnode)
	{
	PNODE pTmp;

	pTmp = (pnode->uID == TOK_EXPR ? pnode->pn1 : pnode);
   return (pTmp->uID == TOK_LITERAL);
	}


// perform inverse portfolio translation of the 
// cross indexed foil vectors to ensure trans gendering for
// each sub element - who the hell am I commenting this for anyway!
//
//	For me stupid.  I can't remember what the hell I did yesturday!
//
//	pnode only user for warning msgs, and it's returned
//
//
//	iLevel: 0 - Error if ptrs are not the same
//  		  1 -	Warning if ptrs are not the same
//			  2 -	no Warning if same level and ptlst1 is void
//			  3 -	warning if indirection to different levels
//			  4 -	no warning 
//
static PNODE TypCheckPointers (PNODE pnode, PTLST ptlst1, PTLST ptlst2, int iLevel)
	{
	if (iLevel > 3) return pnode;

	if (!iLevel && !TLIsSameTypes (ptlst1, ptlst2))
		NodeError (pnode, "different pointer types");

	if (TLGetDerefCount (ptlst1) != TLGetDerefCount (ptlst2))
		{
      NodeWarn (pnode, "different levels of indirection");
      return pnode;
		}

	if (iLevel > 2) return pnode;

	if (TLGetBaseType (ptlst1) == TLGetBaseType (ptlst2))
		return pnode;

	if ((iLevel > 1) && TLIsVoid (TLGetTail (ptlst1)))
		return pnode;

   NodeWarn (pnode, "Indirection to different types");
	return pnode;
	}


//
//	 pnode is the literal node to convert
//	 ptlst is the new type
//	 umix: 0: int -> ptr not allowed
//			 1: int -> ptr allowed with a warning
//			 2: int -> ptr allowed without a warning
//
PNODE TypConvertLiteral (PNODE pnodeLit, PTLST ptlstNew, UINT uMix)
	{
	assert (pnodeLit->uID == TOK_LITERAL);
	PNODE pnode = pnodeLit->pn1;  // TOK_INTLITERAL, etc...
	assert (pnode->ptlst);

	// do nothing if already the same type
	if (TLIsSameTypes (pnode->ptlst, ptlstNew))
		return pnodeLit;

	// integral -> integral: warning if truncation
	if (TLIsIntegral (pnode->ptlst) && TLIsIntegral (ptlstNew))
		{
		if (TLMemSize (pnode->ptlst) <= TLMemSize (ptlstNew))
			{
			switch (TLGetType (pnode->ptlst))
				{
				case DATATYPE_CHAR  : pnode->val.l = (LONG)pnode->val.c; break;
				case DATATYPE_SHORT : pnode->val.l = (LONG)pnode->val.c; break;
				}
			}
		else if ((TLGetType (ptlstNew) == DATATYPE_CHAR) && (pnode->val.l > 255 || pnode->val.l < 0))
			TypTruncationWarning (pnode);
		else if ((TLGetType (ptlstNew) == DATATYPE_SHORT) && (pnode->val.l > 32767 || pnode->val.l < -32768))
			TypTruncationWarning (pnode);

		NodeSetTypeList (pnode, ptlstNew, 0);
		NodeSetTypeList (pnodeLit, ptlstNew, 0);
		return pnodeLit;
		}
	// float -> integral: precision warning
	if (TLIsFloat (pnode->ptlst) && TLIsIntegral (ptlstNew))
		{
      TypPrecisionWarning (pnode);
      switch (TLGetType (ptlstNew))
         {
         case DATATYPE_CHAR : pnode->val.c = (CHAR )pnode->val.bg; break;
         case DATATYPE_SHORT: pnode->val.s = (SHORT)pnode->val.bg; break;
         case DATATYPE_LONG : pnode->val.l = (LONG) pnode->val.bg; break;
			default: assert (FALSE); // do we need to handle another type?
         }
		NodeSetTypeList (pnode, ptlstNew, 0);
		NodeSetTypeList (pnodeLit, ptlstNew, 0);
		return pnodeLit;
		}

	// integral -> float
	if (TLIsIntegral (pnode->ptlst) && TLIsFloat (ptlstNew))
		{
      switch (TLGetType (pnode->ptlst))
			{
         case DATATYPE_CHAR : pnode->val.bg =	(BIG)	pnode->val.c; break;
         case DATATYPE_SHORT: pnode->val.bg =	(BIG)	pnode->val.s; break;
         case DATATYPE_LONG : pnode->val.bg =	(BIG)	pnode->val.l; break;
			default: assert (FALSE); // do we need to handle another type?
			}
		NodeSetTypeList (pnode, ptlstNew, 0);
		NodeSetTypeList (pnodeLit, ptlstNew, 0);
		return pnodeLit;
		}

	// integral -> ptr
	if (TLIsIntegral (pnode->ptlst) && TLIsPtr (ptlstNew) && uMix)
		{
		if (uMix == 1 && pnode->val.l) // 0 is actually a valid ptr
			NodeWarn (pnode, "different levels of indirection");
//		NodeSetTypeList (pnode, ptlstNew, 0); <--- no, we want to leave lit type alone
		NodeSetTypeList (pnodeLit, ptlstNew, 0);
		return pnodeLit;
		}

	if (TLIsString (pnode->ptlst) && TLIsPtr (ptlstNew) && uMix)
		{
		if (uMix > 1)
			return pnodeLit;
		if (TLGetDerefCount (ptlstNew) > 1)
			NodeWarn (pnode, "different levels of indirection");
		return pnodeLit;
		}

	if (TLIsString (pnode->ptlst) && TLIsFloat (ptlstNew))
	   NodeError (pnode, "invalid literal conversion: char * -> Float");

	if (TLIsString (pnode->ptlst) && TLIsIntegral (ptlstNew))
		{
		if (uMix <= 1)
			NodeWarn (pnode, "different levels of indirection char * -> integral");
		return pnodeLit;
		}

	// anything else
   NodeError (pnode, "invalid literal conversion");
	return pnodeLit;
	}




//	This fn attempts to convert pnode's type into the same type as ptlst
//
//	If the type conversion is invalid it will croak or return NULL
//
//	if pnode is a literal it's base type may be changed to match
//	if pnode isn't a literal this fn may add a cast node
//	
//	uAllowPtrConversion: 0 - no
//	                     1 - yes with warning
//	                     2 - yes, and old type can be pvoid wo warning
//								3 - # -> ptr is ok
//	
//	char* foo() {return 3}	//	not ok: return checked with uAllowPtrConversion==2
//	ptr = (char *)3;			//	ok: cast checked with uAllowPtrConversion==3 
//	char* foo() {return 0}	//	ok: special case of 0
//
PNODE TypInsert (PNODE pnode, PTLST ptlst, int iAllowPtrConversion, BOOL bDieOnError)
	{
	assert (pnode && ptlst);

	if (TLIsVoid (pnode->ptlst) || TLIsVoid (ptlst))
      NodeError (pnode, "Operation on void type is not allowed");

	// we can directly change literals
	if (pnode->uID == TOK_LITERAL)
		return TypConvertLiteral (pnode, ptlst, iAllowPtrConversion);
	
	// if Same types then do nothing
	if (TLIsSameTypes (pnode->ptlst, ptlst))
		return pnode;

	// ptr -> ptr
	if (iAllowPtrConversion && TLIsAddress (pnode->ptlst) && TLIsAddress (ptlst))
		{
      TypCheckPointers (pnode, pnode->ptlst, ptlst, iAllowPtrConversion + 1);
		return NodeSetTypeList (pnode, ptlst, 0);
		}

	// ptr->integral   ie: (!psz)
	if (TLIsIntegral (pnode->ptlst) && TLIsFloat (ptlst))
		return BuildCastNode (pnode, DATATYPE_FLOAT);
	
	// float -> integral
	if (TLIsFloat (pnode->ptlst) && TLIsIntegral (ptlst))
		{
      TypPrecisionWarning (pnode);
		return BuildCastNode (pnode, IntTyp());
		}

	// if pnode is a integral
	if (TLIsIntegral (pnode->ptlst) &&	TLIsIntegral (ptlst))
		{
//		if (TLMemSize (pnode->ptlst) > TLMemSize (ptlst))
//            TypPrecisionWarning (pnode);
			
		return NodeSetTypeList (pnode, ptlst, 0);
		}

	// integral -> ptr
	if (TLIsIntegral (pnode->ptlst) && TLIsPtr (ptlst))
		{
//    pTmp = (pnode->uID == TOK_EXPR ? pnode->pn1 : pnode);
//    if (pTmp->uID == TOK_LITERAL && !pTmp->pn1->val.l)
//       return pnode; // 0 is a valid ptr

		if (!iAllowPtrConversion)
			NodeError (pnode, "Cannot convert to pointer here");
		if (iAllowPtrConversion == 1)
			NodeWarn (pnode, "different levels of indirection");
		return NodeSetTypeList (pnode, ptlst, 0);
		}

	if (TLNodeIsAddress (pnode) && TLIsIntegral (ptlst))
		{
		return NodeSetTypeList (pnode, ptlst, 0);
		}

//	This is probably incomplete.
//	Doesn't address arrays and structs for example
//
	if (!bDieOnError)
		return NULL;

   NodeError (pnode, "Incompatible types");
   return pnode;
	}


//	 Call to resolve types for a binary op
//	 This may add a cast node or change a literal type of 
//	 one of the children
//
//	 iPtrs: 0 - no ptrs/arrays allowed           x
//	        1 - ptrs/arrays ok
//	        2 - + rules: ptr/ptr -> invalid      x
//	        3 - - rules: ptr/ptr -> int          x
//
//	 iMix:  0 - no mixing i & p                  x
//	        1 - mixing ok
//
PNODE TypResolve (PNODE pExpr, BOOL bFloats, int iPtrs, int iMix)
	{
	PTLST ptlst1, ptlst2, ptlstTmp;

	ptlst1 = pExpr->pn1->ptlst;
	ptlst2 = pExpr->pn2->ptlst;

	if (TLGetDerefCount(ptlst1) > TLGetDerefCount(ptlst2))
		{
		ptlstTmp = ptlst2,
		ptlst2   = ptlst1,
		ptlst1   = ptlstTmp;
		}

	if (TLIsVoid (ptlst1) || TLIsVoid (ptlst2))
      NodeError (pExpr, "Operation on void type is not allowed");

	if (TLIsStruct (ptlst1) || TLIsStruct (ptlst2))
      NodeError (pExpr, "Operation on struct is not allowed");

//	if (TLIsArray (ptlst1) || TLIsArray (ptlst2)) what was i thinking?
//      NodeError (pExpr, "Operation on array is not allowed");

	if (!bFloats && (TLIsFloat (ptlst1) || TLIsFloat (ptlst2)))
      NodeError (pExpr, "Incompatible types for operation: float type is not allowed");

	if (!iPtrs && (TLGetDerefCount(ptlst1) + TLGetDerefCount(ptlst2)))
      NodeError (pExpr, "Incompatible types for operation: pointer type is not allowed");

	if (TLIsFloat (ptlst1) && TLGetDerefCount(ptlst2))
      NodeError (pExpr, "Incompatible types for operation: cannot combine floats and pointers");

	if (TLGetDerefCount(ptlst1) && TLGetDerefCount(ptlst2))
		{
      if (iPtrs == 2)
         NodeError (pExpr, "Incompatible type for operation: cannot mix pointer and pointer");
		if (iPtrs == 3)
			return NodeSetTypeList (pExpr, TLCreateBaseTypeList(IntTyp()), 1);

      TypCheckPointers (pExpr, ptlst1, ptlst2, 2);
		return NodeSetTypeList (pExpr, ptlst1, 0);
		}

	if (TLIsSameTypes (ptlst1, ptlst2))
		return NodeSetTypeList (pExpr, ptlst1, 0);

	if (TLIsIntegral (ptlst1) && TLGetDerefCount(ptlst2))
		{
		if (!iMix)
         NodeError (pExpr, "Incompatible types for operation: cannot mix pointer and integral value");
		return NodeSetTypeList (pExpr, ptlst2, 0);
		}

	if (TLIsIntegral (ptlst1) && TLIsIntegral (ptlst2))
		{
		if (TLMemSize (ptlst1) > TLMemSize (ptlst2))
			pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1, 0, 1);
		else if (TLMemSize (ptlst1) < TLMemSize (ptlst2))
			pExpr->pn1 = TypInsert (pExpr->pn1, ptlst2, 0, 1);
		return NodeSetTypeList (pExpr, LargestIntegralType (pExpr->pn1->ptlst, pExpr->pn2->ptlst), 0);
		}
	if (TLIsIntegral (ptlst1) && TLIsFloat (ptlst2))
		{
		pExpr->pn1 = TypInsert (pExpr->pn1, ptlst2, 0, 1);
		return NodeSetTypeList (pExpr, ptlst2, 0);
		}
	if (TLIsFloat (ptlst1) && TLIsIntegral (ptlst2))
		{
		pExpr->pn2 = TypInsert (pExpr->pn2, ptlst1, 0, 1);
		return NodeSetTypeList (pExpr, ptlst1, 0);
		}
	assert (FALSE); // what did I miss??
	return NULL;
	}


///////////////////////////////////////////////////////////////////////////////////////
//
//	Type List manipulation
//
///////////////////////////////////////////////////////////////////////////////////////

