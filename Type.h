/*
 *
 * type.h
 * Wednesday, 3/12/1997.
 * Craig Fitzgerald
 *
 * Not an external header - This header is intended for 
 * internal use by the gclib library
 */

//	only valid for token ID's that represent basic types
//
//
UINT TokIDToDataType (UINT uTokenID);

//	looks at token ID and returns TRUE if the token ID
//	represents a Base Type
//
BOOL TokTypIsBaseType (UINT uID);

//	looks at the current token
//	returns TRUE is the token is a type specifier
//
BOOL TokTypIsType (void);

//	croak if datatype is void
//
void TypNoVoid (PNODE pnode);


//	 Call to resolve types for a binary op
//	 This may add a cast node or change a literal type of 
//	 one of the children
//
//	 iPtrs: 0 - no ptrs allowed                  x
//	        1 - ptrs ok
//	        2 - + rules: ptr/ptr -> invalid      x
//	        3 - - rules: ptr/ptr -> int          x
//
//	 iMix:  0 - no mixing i & p                  x
//	        1 - mixing ok
//
PNODE TypResolve (PNODE pExpr, BOOL bFloats, int iPtrs, int iMix);


//	This fn attempts to convert pNode's type into the same type as ptlst
//
//	If the type conversion is invalid it will croak or return NULL
//
//	if pNode is a literal it's base type may be changed to match
//	if pNode isn't a literal this fn may add a cast node
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
PNODE TypInsert (PNODE pnode, PTLST ptlst, int iAllowPtrConversion, BOOL bDieOnError);



//
//	 pnode is the literal node to convert
//	 ptlst is the new type
//	 umix: 0: int -> ptr not allowed
//			 1: int -> ptr allowed with a warning
//			 2: int -> ptr allowed without a warning
//
PNODE TypConvertLiteral (PNODE pnode, PTLST ptlstNew, UINT uMix);


//	32 bit compiles return DATATYPE_LONG
//	16 bit compiles return DATATYPE_SHORT
//
INT IntTyp (void);



///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
//
//
//
//void TypCombineVal (PSYM ps, UINT uTypeVal);
//
//void TypXfer (PSYM ps, PNODE pType);
//
//UINT TypBuildVal (PNODE pnode);
//
//BOOL TypIsBaseType (UINT uID);
//
//BOOL TypIsType (void);
//
//UINT TypPtrBase (UINT uTypeVal);
//
//
///***************************************************************************/
///*                                                                         */
///*                                                                         */
///*                                                                         */
///***************************************************************************/
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
//UINT TypResolve (PNODE pExpr, UINT bFloats, UINT uPtrs, UINT uMix);
//
//
///*
// *
// *
// *
// */
//PNODE TypInsert (PNODE pnode, UINT uOldType, UINT uNewType, BOOL bAllowPtrConversion, BOOL bDieOnError);
//
//
//PNODE TypCast (PNODE pnode, UINT uNewType);
//
//
//void TypCheckPointers (PNODE pnode, UINT uTypeVal1, UINT uTypeVal2, BOOL bVoidOK);
//
//void TypPrecisionWarning (PNODE pnode);
//
//void TypNoVoid (PNODE pnode); 
//
//BOOL TypIntegral (UINT uTypeVal);
//
//BOOL TypFloat (UINT uTypeVal);
//
//BOOL TypPtr (UINT uTypeVal);
//
//BOOL TypString (UINT uTypeVal);
//
///*
// * 32 bit compiles return DATATYPE_LONG
// * 16 bit compiles return DATATYPE_SHORT
// */
//INT IntTyp (void);
//
///* 
// * uMix 0: int -> ptr not allowed
// *      1: int -> ptr allowed with a warning
// *
// *
// */
//void TypConvertStatic (PNODE pLit, UINT uTargetType, UINT uMix);
//
///*
// *
// */
//void CheckStructTypes (PNODE pnode, PSYM ps1, PSYM ps2);
//
// 
///////////////////////////////////////////////////////////////////////////////////////
//
//	TypeList fn's
//
///////////////////////////////////////////////////////////////////////////////////////

BOOL TLIsVoid (PTLST ptlst);
BOOL TLIsIntegral (PTLST ptlst);
BOOL TLIsFloat (PTLST ptlst);
BOOL TLIsStruct (PTLST ptlst);	 // also true if a union
BOOL TLIsString (PTLST ptlst); // allows ptr to char
BOOL TLIsPtr (PTLST ptlst);
BOOL TLIsArray (PTLST ptlst);
BOOL TLIsHash (PTLST ptlst);
BOOL TLIsSameTypes (PTLST ptlst1, PTLST ptlst2);
BOOL TLIsVoidPtr (PTLST ptlst);
BOOL TLIsPtrOrArray (PTLST ptlst);
BOOL TLIsAddress (PTLST ptlst);

UINT TLGetType (PTLST ptlst);		 //	top level type		  ***char[] is array
PTLST TLGetTail (PTLST ptlst);
UINT TLGetBaseType (PTLST ptlst);	 //	lowest level type   ***char[] is char
UINT TLGetDerefCount (PTLST ptlst); //   ***char is 3 	     ***char[] is 0		 ???

PTLST TLDeref (PTLST ptlst);
PTLST TLFree (PTLST ptlst); // add ptlstSuffix on to ptlst
PTLST* FreeTLArray (PTLST* ppFormals);


PTLST TLAppend (PTLST ptlst, PTLST ptlstSuffix); // add ptlstSuffix on to ptlst
PTLST TLNewElement (PTLST ptlstProto);
PTLST TLNewArrayElement (PNODE pExpr);
PTLST TLNewHashElement (PNODE pType);
//	PTLST TLCopyTypeListFromTypedef (PNODE pnode);
PTLST TLNewStructElement (PSYM ps);
PTLST TLCopyTypeList (PTLST ptlst);
PTLST TLAddToHead (PTLST ptlst, UINT uPrimitiveType);
PTLST TLCreateBaseTypeList(UINT uDataType);
PTLST TLAddAtPos (PTLST ptlst, int iIdx, PTLST ptlstNewElement);

BOOL TLNodeIsPtr (PNODE pnode);
BOOL TLNodeIsArray (PNODE pnode);
BOOL TLNodeIsHash (PNODE pnode);
BOOL TLNodeIsPtrOrArray (PNODE pnode);
BOOL TLNodeIsAddress (PTLST ptlst);
BOOL TLNodeIsStruct (PNODE pnode);
BOOL TLNodeIsIntegral (PNODE pnode);
BOOL TLNodeIsFloat (PNODE pnode);
BOOL TLNodeIsVoid (PNODE pnode);
int  TLNodeGetDerefCount (PNODE pnode);
int  TLNodeType (PNODE pnode);


//int TLMemSize    (PTLST ptlst); in mem.h
//int TLMemStkSize (PTLST ptlst); in mem.h
//PTLST TLTypeListFromNode (PNODE pnode); // from node->sym->tl
//PTLST TLAddToTail (PTLST ptlst, UINT uPrimitiveType, UINT uElements);
//PTLST TLPrependList (PTLST ptlst, PTLST ptlstPrefix);
//void TLSetSymbolTypeList (PSYM ps, PTLST ptlst);
//BOOL TLIsCompatibleTypes (PTLST ptlst1, PTLST ptlst2);
//PTLST TLFreeTypeList (PTLST ptlst);
//UINT TLDataSizeFromTC (PTLST ptlst);

