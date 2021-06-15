/*
 *
 * genglob.c
 * Thursday, 2/27/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include <GnuFile.h>
#include "symbol.h"
#include "tokenize.h"
#include "parse.h"
#include "check.h"
#include "label.h"
#include "genglob.h"
#include "gen.h"
#include "genop.h"
#include "opcodes.h"
#include "binfile.h"
#include "type.h"
#include "error.h"
#include "mem.h"
#include "node.h"


typedef struct _glob
   {
   PNODE pnode;         // ptr to symbol entry if a global, NULL if a static
   PSZ   psz;           // ptr to buffer if a static
   int   iAddr;         // offset into global buffer
   int   iLen;          // data size
   struct _glob *next;  // next ptr
   struct _glob *prev;  // next ptr

#if defined (_DEBUG)
	int iIdx;
#endif 
   } GLOB;
typedef GLOB *PGLOB;

#if defined (_DEBUG)
int iINDEX = 0;
#endif 


void GenGlobalElement (PGLOB pgl);
void GenGlobalElement (PTLST ptlst, PNODE pInit, int iAddr);


static int   iGP = 0;        // Global Area current size Index
static int   iDumpIdx = 0;   // Global Area current dump size Index
static PGLOB pglHEAD = NULL; // global vars and static data linked list head
static PGLOB pglTAIL = NULL; // global vars and static data linked list tail


/***************************************************************************/
/*                                                                         */
/* Global varibles and Static values store/write routines                  */
/*                                                                         */
/***************************************************************************/

static PGLOB FreeGLOB (PGLOB pgl)
   {
   if (!pgl)
      return NULL;

   if (pgl->pnode)
      pgl->pnode = NodeFreeTree (pgl->pnode);
   else
      MemFreeData (pgl->psz);     // a static

   return (PGLOB)MemFreeData (pgl);
   }


static PGLOB NewGlob (void)
   {
   PGLOB pglob;

   pglob = (PGLOB)calloc (sizeof (GLOB), 1);
   pglob->iAddr = iGP;
   pglob->prev  = pglTAIL;
   pglTAIL      = pglob;

#if defined (_DEBUG)
	pglob->iIdx  = iINDEX++;
#endif 

   if (pglob->prev)
      pglob->prev->next = pglob;
	if (!pglHEAD)
		pglHEAD = pglob;

   return pglob;
   }


static void AddDummyStart ()
	{
	assert (!iGP);

   PGLOB pglob = NewGlob ();
   pglob->psz  = strdup ("\x44\x44\x44\x44");
   pglob->iLen = iGP = 4;
	}


// for now I do a linear search for a dup
// change implementation to store this stuff in a tree or something
PGLOB FindStatic (PSZ pszBuff, int iLen)
	{
	PGLOB pglob;

	for (pglob = pglHEAD; pglob; pglob = pglob->next)
		if (!pglob->pnode && !memcmp (pglob->psz, pszBuff, iLen))
			return pglob;
	return NULL;
	}

/*
 * This fn adds a static buffer to the global memory list
 *
 * Memory pointers are in the form 0x0000XXXX or 0xXXXXXXXX.  If the
 * 2 high bytes are zero, the ptr is assumed to be an index into the
 * global memory area.  This could lead to a problem for the first
 * entry in the global area because it's address would be 0 which is
 * the same as NULL.  So if the first entry is a static (not a global
 * var), we must add a dummy first so that its address is offset
 * hence the if (!iGP) condition
 *
 */
int AddStatic (PSZ pszBuff, int iLen)
   {
   PGLOB pglob;

   if (!iGP)
		AddDummyStart ();

	if (pglob = FindStatic (pszBuff, iLen))
   	return pglob->iAddr;

	// else create a new one
   pglob = NewGlob ();
   pglob->psz  = strdup(pszBuff);
   pglob->iLen = iLen;
   iGP += iLen;
   return pglob->iAddr;
   }


static PNODE SkipExprAndCastAndLit (PNODE pExpr)
	{
	PNODE pn1;

	if (!pExpr)
		return NULL;

	assert (pExpr->uID == TOK_EXPR);
	for (pn1 = pExpr->pn1; pn1->uID == TOK_CAST; pn1 = pn1->pn2)
		;
	assert (pn1->uID == TOK_LITERAL);
	pn1 = pn1->pn1;
//	assert (pn1->uID == TOK_STRINGLITERAL)
	return pn1;
	}


//	recurse through initialization structure looking for string literals.
//	Add them to the global pool as statics unless they are array
//	initializers.
//	Examples:
//  	char *p = "Testing";
//		Here, the string "Testing" is added to the global pool as a 
//		static.  p is written with the address of the static.
//
//	   char q[] = "Testing";
//		Here, no static is added.  When the p aray is written it
//		is initialized with the string contents.
//
//
static int AddStringLiteralInitializers (PNODE pInit, PTLST ptlst)
	{
	PNODE pnode, pLit;

	if (!pInit)
		return 0;

	for (pnode = pInit; pnode; pnode = pnode->next)
		{
		if (pnode->uID == TOK_OBRACE)
			{
			AddStringLiteralInitializers (pnode->pn1, TLDeref(ptlst));
			continue;
			}
		pLit = SkipExprAndCastAndLit (pnode);
		if (pLit && pLit->uID == TOK_STRINGLITERAL)
			{
			PSZ psz = pLit->val.psz;
	      int uGlobAddr = AddStatic (psz, strlen (psz) + 1);
			// pLit->uLine is commandeered for a different purpose here.  A hack you say?
			pLit->uLine = uGlobAddr;
			}
		}
	return 0;
	}




//	if (pnode->uID != TOK_STRINGLITERAL)
//		AddStringLiteralInitializers (pnode->pn1, TLDeref(ptlst));
//
//	if (pnode->uID == TOK_STRINGLITERAL && !TLIsArray (ptlst))
//		{
//		PSZ psz   = pnode->val.psz;
//      int uGlobAddr = AddStatic (psz, strlen (psz) + 1);
//
//		// pnode->uLine is commandeered for a different purpose here.  A hack you say?
//		pnode->uLine = uGlobAddr;
//		}
//	if (pnode->uID != TOK_STRINGLITERAL)
//		AddStringLiteralInitializers (pnode->next, ptlst);
//	}



// 
// This fn adds a global variable to the global memory list
// Not that if the global initializer contains one or more
// static strings then there will be additional mem allocations
// in the global mem pool for them immediately following the
// variable.
// 
static void AddGlobal (PNODE pnode, PNODE pnodeType)
   {
   PNODE pIdent = pnode->pn1;
   PNODE pInit  = pnode->pn2;
   PSYM  ps 	 = pIdent->ps;

   // if first global entry is a pointer, make a dummy first
   if (!iGP && TLGetDerefCount (ps->ptlst))
		AddDummyStart ();

   PGLOB pglob  = NewGlob ();
   pglob->pnode = pnode;

	// if a struct or array of struct, align the structure in memory
    PTLST ptlst;
	for (ptlst = ps->ptlst; TLIsArray(ptlst); ptlst = TLDeref(ptlst))
		;
	if (TLIsStruct (ptlst))
		MemStructElementAlign (&iGP, ptlst);

   pglob->iAddr = iGP; // we might have adjusted iGP for alignment
   ps->iAddr    = iGP;
   pglob->iLen  = TLMemSize (ps->ptlst);
   iGP += pglob->iLen;

	AddStringLiteralInitializers (pInit, pIdent->ptlst);
	}

//-----------------------------------------------
//
//if pnodeType->uID = TOK_IDENTIFIER 	 //(a typedef type)
//if pnodeType->ps->bArray
//
//for (i=0; i<ps->uArraySize; i++)
//	{
//	iElementSize = pnodeType->ps->uArraySize * BaseSize(pnodeType->ps->TypeBase...)
//	AddStatic (, iElementSize);
//	}
//
//
//
//-----------------------------------------------
//
//	//	Look for string initializers that need thier
//	//	own memory space
//
//   if (!pInit || !TLGetDerefCount (ps->ptlst))
//      return;
//
//	// for now we only handle
//	//	 char *sz = "Hey, nice boots!";
//	//	 char *sz[] = {"Hey,", "nice", "boots!"};
//	//	but this should eventually be modified to 
//	//	recursively handle nested intitializers
//	//	with strings
//
//
//   /*--- char *sz = "Hey, nice boots!"; ---*/
//   if (!ps->bArray && pInit->pn1->pn1->uID == TOK_STRINGLITERAL)
//      {
//      psz = pInit->pn1->pn1->val.psz;
//      pInit->val.l = AddStatic (psz, strlen (psz) + 1);
//      return;
//      }
//
//   /*--- char *sz[] = {"Hey,", "nice", "boots!"}; ---*/
//   if (ps->bArray && TLGetDerefCount (ps->ptlst) == 2)
//      {
//      for (pExpr = pInit; pExpr; pExpr = pExpr->next)
//         {
//			if (pExpr->pn1->uID == TOK_LITERAL && pExpr->pn1->pn1->uID == TOK_STRINGLITERAL)
//				{
//	         psz = pExpr->pn1->pn1->val.psz;
//            pExpr->val.l = AddStatic (psz, strlen (psz) + 1);
//				}
////       if (pExpr->pn1->pn1->uID == TOK_STRINGLITERAL)
////          pExpr->val.l = AddStatic (psz, strlen (psz) + 1);
//         }
//      }
//   }


/*
 * this actually stores them until
 * GenGlobals is called
 *
 */
void StoreGlobalDecl (PNODE pnode)
   {
   PNODE pVar, pnodeNext, pnodeType, pnodeVarList;

	pnodeType	  = pnode->pn1;
	pnodeVarList  = pnode->pn2;

   for (pVar = pnodeVarList; pVar; pVar = pnodeNext)
      {
      pnodeNext = pVar->next; 	  // snip chains
      pVar->next = NULL;      	  //
      AddGlobal (pVar, pnodeType); //
      }
   }

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

static void _DumpData (PVOID p, int iLen)
	{
   GenDumpGlobData (p, iLen);
	iDumpIdx += iLen;
	}

static int WriteFiller (int uBytes, BYTE bFiller)
   {
   for (; uBytes; uBytes--)
      _DumpData (&bFiller, 1);
	return 0;
   }

static void _AlignFill (int iAddr)
	{
	if (iDumpIdx < iAddr)
		WriteFiller (iAddr - iDumpIdx, '0');
	}




static int GenGlobalSimpleElement (PTLST ptlst, PNODE pInit, int iAddr)
	{
	 _AlignFill (iAddr);

#if defined (HASH_SUPPORT)
	if (TLIsHash (ptlst))
		{		
//		DWORD dwVal = 0x0000; // special value signifying a global
//		GenDumpGlobData (&dwVal, 4);
//		return 0;

		return WriteFiller (4, '0');
		}
#endif // defined (HASH_SUPPORT)
		
	if (!pInit)
		return WriteFiller (TLMemSize(ptlst), '0');

	PNODE pLit = SkipExprAndCastAndLit (pInit);

	switch (TLGetType (ptlst))
		{
		case DATATYPE_CHAR  :
         _DumpData (&(pLit->val.c), 1);
			break;			
		case DATATYPE_SHORT :
			_DumpData (&(pLit->val.s), 2);
			break;			
		case DATATYPE_LONG  :
			_DumpData (&(pLit->val.l), 4);
			break;			
		case DATATYPE_FLOAT :
			_DumpData  (&(pLit->val.bg), 8);
			break;			
//		case DATATYPE_STRING:
		case DATATYPE_PTR	  :
			

		case DATATYPE_ARRAY :
			if (pLit->uID == TOK_STRINGLITERAL)
				_DumpData (&(pLit->uLine), 4); // pnode->uLine was commandeered for this purpose
			else
				_DumpData (&(pLit->val.l), 4);
			break;			
		default:
			assert (FALSE);
		}
	return 0;
	}


static int GenGlobalStructElement (PTLST ptlst, PNODE pInit, int iAddr)
	{
	assert (pInit == NULL || pInit->uID == TOK_OBRACE);

	//	we don't handle union initializers correctly as yet.
	//
	if (ptlst->pSym->bUnion)
		{
		NodeWarn (pInit, "Union initializers not yet handled in code generator '%s'", ptlst->pSym->pszLiteral);
		_AlignFill (iAddr);
		WriteFiller (TLMemSize(ptlst), '0');
		return 0;
		}

	PNODE pInitializer = (pInit ? pInit->pn1 : NULL);
	PSYM* ppElements = ptlst->pSym->ppElements;

	for (int i=0; ppElements[i]; i++)
		{
		int iElementAddr = iAddr + ppElements[i]->iAddr;
		GenGlobalElement (ppElements[i]->ptlst, pInitializer, iElementAddr);
		pInitializer = (pInitializer ? pInitializer->next : NULL);
		}
	return 0;
	}


static int GenGlobalArrayElement  (PTLST ptlst, PNODE pInit, int iAddr)
	{
	int i, iIdx;
	PNODE pnode = NULL;

	// ptr or array of char w/o {}
	if (TLIsString (ptlst) == 1 && (!pInit || pInit->uID != TOK_OBRACE))
		{
		pnode = SkipExprAndCastAndLit (pInit);
		assert (!pnode || pnode->uID == TOK_STRINGLITERAL);

		_AlignFill (iAddr);
		for (iIdx=i=0; i<ptlst->iCount; i++)
			{
			char c = (pnode && pnode->val.psz[iIdx] ? pnode->val.psz[iIdx++] : 0);
         _DumpData (&c, 1);
			}
		return 0;
		}
	assert (!pInit || pInit->uID == TOK_OBRACE);
	pnode = (pInit ? pInit->pn1 : NULL);
	for (i=0; i<ptlst->iCount; i++)
		{
		GenGlobalElement (TLDeref(ptlst), pnode, iAddr);
		pnode = (pnode ? pnode->next : NULL);
		iAddr += TLMemSize (TLDeref (ptlst));
		}
	return 0;
	}

static void GenGlobalElement (PTLST ptlst, PNODE pInit, int iAddr)
	{
	if (TLIsStruct (ptlst))
		GenGlobalStructElement (ptlst, pInit, iAddr);
	else if (TLIsArray (ptlst))
		GenGlobalArrayElement (ptlst, pInit, iAddr);
	else
		GenGlobalSimpleElement (ptlst, pInit, iAddr);
	}


static void GenGlobalElement (PGLOB pgl)
	{
   PSYM  ps    = pgl->pnode->pn1->ps;
	PTLST ptlst = ps->ptlst;
   PNODE pInit = pgl->pnode->pn2;

	GenGlobalElement (ptlst, pInit, pgl->iAddr);
	}



static void GenStaticElement (PGLOB pgl)
   {
	_AlignFill (pgl->iAddr);
   _DumpData (pgl->psz, pgl->iLen);
   }


/*
 * this writes out the globals record
 *
 */
void GenGlobals (void)
   {
   PGLOB pgl, pglNext;

	iDumpIdx = 0;
   GenDumpGlobStart (iGP);
   for (pgl = pglHEAD; pgl; pgl = pglNext)
      {
      pglNext = pgl->next;

      if (pgl->pnode) // a global variable
         GenGlobalElement (pgl);
      else
         GenStaticElement (pgl);

      FreeGLOB (pgl);
      }
   GenDumpGlobEnd ();

   pglHEAD = pglTAIL = NULL;
   iGP = 0;
   }

