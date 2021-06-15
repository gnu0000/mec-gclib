
/*
 *
 * vm.c
 * Tuesday, 3/4/1997.
 * Craig Fitzgerald
 *
 *
 * This file contains several globals and is in general quite bulky
 * for speed
 *
 */

#include "stdafx.h"
#include <math.h>
#include <GnuFile.h>
#include <GnuArg.h>
#include "opcodes.h"
#include "binfile.h"
#include "extern.h"

#include "codmaint.h"
#include "vm.h"

typedef void (_cdecl *OPPROC) (UCHAR cOp);

static void _cdecl OpSet0 (UCHAR cOp);
static void _cdecl OpSet1 (UCHAR cOp);
static void _cdecl OpSet2 (UCHAR cOp);
static void _cdecl OpSet3 (UCHAR cOp);
static void _cdecl OpSet4 (UCHAR cOp);
static void _cdecl OpSet5 (UCHAR cOp);
static void _cdecl OpSet6 (UCHAR cOp);
static void _cdecl OpSet7 (UCHAR cOp);
static void _cdecl OpSet8 (UCHAR cOp);
static void _cdecl OpSet9 (UCHAR cOp);
static void _cdecl OpSetA (UCHAR cOp);
static void _cdecl OpSetB (UCHAR cOp);
static void _cdecl OpSetC (UCHAR cOp);
static void _cdecl OpSetD (UCHAR cOp);
static void _cdecl OpSetE (UCHAR cOp);
static void _cdecl OpSetF (UCHAR cOp);

static OPPROC OpProcs [16] =  {OpSet0, OpSet1, OpSet2, OpSet3,
                               OpSet4, OpSet5, OpSet6, OpSet7,
                               OpSet8, OpSet9, OpSetA, OpSetB,
                               OpSetC, OpSetD, OpSetE, OpSetF};


PFUNC pfnCURR = NULL;  // currently executing fn
PLONG plSTACK = NULL;  // malloced at startup

#if defined (_DEBUG)
int  iDEBUG  = 0; 	 // writes traces
#endif

INT  iSF = 0;          // stack frame register
INT  iSP = 0;          // stack pointer
INT  iIP = 0;          // instruction ptr


BOOL VMExecFunction (PSZ pszFn, BOOL bDieIfNotFound);

#if defined (_DEBUG)
void DumpTheStack (PSZ psz);
#endif

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

static void BadOp (UCHAR cOp)
   {
   Error ("Bad Operand %2.2x IP:%4.4x(%5d) SP:%4.4x(%5d) SF:%4.4x(%5d)\n",
           cOp, iIP, iIP, iSP, iSP, iSF, iSF);
   }


PSZ VMFixPtr (ULONG ul)
   {
   if (!ul || (ul & 0xFFFF0000))
      return (PSZ)ul;

   return ul + pfnCURR->pszGlobals;
   }


ULONG VMTruncate (ULONG ul, UCHAR cOp)
   {
   cOp &= 0x03;
   if (cOp == M_BYTE)
      return ul &= 0xFF;
   if (cOp == M_SHORT)
      return ul &= 0xFFFF;
   return ul;
   }

/***************************************************************************/
/*                                                                         */
/* code stream manipulation                                                */
/*                                                                         */
/***************************************************************************/

INT VMCNextByte (void)
   {
   UCHAR c;

   c = *(PUCHAR)(pfnCURR->pszCode + iIP);
   iIP++;
   return c;
   }

INT VMCNextShort (void)
   {
   INT i;

   i = *(PSHORT)(pfnCURR->pszCode + iIP);
   iIP += MEMSIZE_SHORT;
   return i;
   }

LONG VMCNextWord (void)
   {
   LONG l;

   l = *(PLONG)(pfnCURR->pszCode + iIP);
   iIP += MEMSIZE_WORD;
   return l;
   }

/***************************************************************************/
/*                                                                         */
/* Stack manipulation                                                      */
/*                                                                         */
/***************************************************************************/

void VMSPush (LONG l)
   {
   plSTACK [iSP++] = l;
   }

void VMSPushF (BIG f)
   {
   MONOF m;

   m.bg = f;
   plSTACK [iSP++] = m.l1;
   plSTACK [iSP++] = m.l2;
   }

LONG VMSPeek (void)
   {
   return plSTACK [iSP-1];
   }

LONG VMSPeekIn (LONG l)
   {
   return plSTACK [iSP-1-l];
   }

LONG VMSPop (void)
   {
   return plSTACK [--iSP];
   }

BIG VMSPopF (void)
   {
   MONOF m;

   m.l2 = plSTACK [--iSP];
   m.l1 = plSTACK [--iSP];
   return m.bg;
   }

BIG VMSPeekF (void)
   {
   MONOF m;

   m.l2 = plSTACK [iSP-1];
   m.l1 = plSTACK [iSP-2];
   return m.bg;
   }

/***************************************************************************/
/*                                                                         */
/* Global memory access                                                    */
/*                                                                         */
/***************************************************************************/

/*--- memory manipulation ---*/
void VMGStoreByte (UCHAR c, LONG lAddr)
   {
   MONO m;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   *m.psz = c;
   }

void VMGStoreShort (SHORT s, LONG lAddr)
   {
   MONO m;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   *m.ps = s;
   }

void VMGStoreWord  (LONG l, LONG lAddr)
   {
   MONO m;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   *m.pl = l;
   }

void VMGStoreFloat (BIG f, LONG lAddr)
   {
   MONO m;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   *m.pbg = f;
   }

LONG VMGGetByte  (LONG lAddr)
   {
   MONO m;
   LONG lVal;


   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;

   lVal = *(m.psz);
   return lVal;
   }

LONG VMGGetShort (LONG lAddr)
   {
   MONO m;
   LONG lVal;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   lVal = *(m.ps);
   return lVal;
   }

LONG VMGGetWord  (LONG lAddr)
   {
   MONO m;
   LONG lVal;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   lVal  = *(m.pl);
   return lVal;
   }

BIG VMGGetFloat (LONG lAddr)
   {
   MONO m;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;
   return *(m.pbg);
   }

PVOID VMGGetPtr (LONG lAddr)
   {
   MONO m;

   if (lAddr & 0xFFFF0000)
      m.l = lAddr;
   else
      m.psz = pfnCURR->pszGlobals + lAddr;

   return m.psz;
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

#if defined (HASH_SUPPORT)


// a hash
// or actually - a hash someday
typedef struct _mhe
	{
	DWORD  dwKey;
	DWORD  dwVal;
	struct _mhe *next;
	} MHE, *PMHE;

// hash list
typedef struct _mh
	{
	DWORD  dwSize; // size of hash element vars
	DWORD  dwType; // do we compare strings or values of the keys
	DWORD  dwAddr; // addr of mem containing hash variable
	struct _mhe *list;
	struct _mh  *next;
	} MH, *PMH;

// a hash scope
// a hack indeed
typedef struct _hst
	{
	PMH 	 list;
	struct _hst* next;
	} HST, *PHST;

static PHST phstHEAD = NULL;	//	
static PHST phstTAIL = NULL;	//	globals list is at the tail



static PMHE NewElement (PMH pmh, DWORD dwKey)
	{
	PMHE pmhe = (PMHE)calloc (1, sizeof (MHE));
	pmhe->dwKey = dwKey;
	pmhe->next = pmh->list;
	pmh->list  = pmhe;

	if (pmh->dwSize <= 4)
		pmhe->dwVal = 0;
	else
		pmhe->dwVal = (DWORD) calloc (1, pmh->dwSize);
	return pmhe;
	}


static PMHE DeleteElement (PMH pmh, PMHE pmhe)
	{
	if (pmh->dwSize > 4)
		delete ((void *)pmhe->dwVal);
	delete (pmhe);
	return NULL;
	}

static PMHE FindElement (PMH pmh, DWORD dwKey)
	{
	PMHE pmhe;

	for (pmhe = pmh->list; pmhe; pmhe = pmhe->next)
		{
		if (pmh->dwType & 0x01)	// a string comparison vs a numeric comparison ?
			{
			if (!strcmp((char*)pmhe->dwKey, (char*)dwKey))
				return pmhe;
			}
		else
			{
			if (pmhe->dwKey == dwKey)
				return pmhe;
			}
		}
	return NewElement (pmh, dwKey);
	}


//	create a new hash header struct
//	put addr of this new struct in mem space for hash var
//	add this struct to the hash mem tracking chain
//	if mem space for hash var was initialized to 1, we know
//	 that this is a global hash var 
//
static PMH NewHash (DWORD dwAddr, DWORD dwSize, DWORD dwType)
	{
	PMH* ppmh 	 = (PMH*)dwAddr;
	BOOL bGlobal = (*ppmh == (PMH)1);

	*ppmh 		 	  = (PMH)calloc (1, sizeof (MH));
	(*ppmh)->dwSize  = dwSize;
	(*ppmh)->dwType  = dwType;
	(*ppmh)->dwAddr  = dwAddr;
	
	if (bGlobal)
		{
		(*ppmh)->next  = phstTAIL->list;
		phstTAIL->list = *ppmh;
		}
	else
		{
		(*ppmh)->next  = phstHEAD->list;
		phstHEAD->list = *ppmh;
		}
	return *ppmh;
	}

static PMH DeleteHash (PMH pmh)
	{
	PMHE pmhe, pmheNext; 
	for (pmhe = pmh->list; pmhe; pmhe = pmheNext)
		{
		pmheNext = pmhe->next;
		DeleteElement (pmh, pmhe);
		}
	PMH* ppmh = (PMH*)pmh->dwAddr;
	*ppmh = NULL;
	free (pmh);
	return NULL;
	}


static PMH FindHash (DWORD dwAddr, DWORD dwSize, DWORD dwType)
	{
	PMH *ppmh;

	ppmh = (PMH*)dwAddr;
	if (!*ppmh || *ppmh == (PMH)1)
		return NewHash (dwAddr, dwSize, dwType);
	return *ppmh;
	}

//	push a virtual stack scope
//	This is a sort of hack
//
static void HashScopeTrack ()
	{
	PHST phst  = (PHST)calloc (1, sizeof (HST));
	phst->next = phstHEAD;
	phst->list = NULL;
	phstHEAD = phst;
	if (!phstTAIL)
		phstTAIL = phst;
	}


// Pop a virtual stack scope
//	This is a sort of hack
//
static void HashScopeCleanup ()
	{
	PMH pmh, pmhNext;
	for (pmh = phstHEAD->list; pmh; pmh = pmhNext)
		{
		pmhNext = pmh->next;
		DeleteHash (pmh);
		}
	PHST phstNext = phstHEAD->next;
	free (phstHEAD);
	phstHEAD = phstNext;
	}


static DWORD HashGet (DWORD dwHash, DWORD dwKey, DWORD dwFlags)
	{
	DWORD dwSize = (dwFlags & 0xFFFF);
	DWORD dwType = (dwFlags >> 16);

#if defined (_DEBUG)
   if (iDEBUG & 0x02)
		{
		DWORD dwVal = (dwHash > 1 ? *((DWORD*)dwHash) : 0);
		printf ("## HashAddr:%8.8x  HashVal:%8.8x  flags:%8.8x  key:%8.8x\n", dwHash, dwVal, dwFlags, dwKey);
		}
#endif
	
	PMH  pmh  = FindHash (dwHash, dwSize, dwType);
	PMHE pmhe = FindElement (pmh, dwKey);

	return (dwSize <= 4 ? (DWORD)&pmhe->dwVal : pmhe->dwVal);
	}

#endif // defined (HASH_SUPPORT)

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

static void HandleCall (UCHAR cOp)
   {
   PSZ pszFn;
   UINT uCode;

   if (cOp == OP_CALL)
      {
      pszFn = pfnCURR->pszCode + iIP;  // load function name
      iIP += strlen (pszFn) + 1;       // inc IP past fn name string
      VMExecFunction (pszFn, TRUE);          // call (on REAL stack!)
      }
   else
      {
      uCode = VMCNextShort ();
      HandleInternalCall (uCode);
      }
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

/*
 *  OP_NOP    0x00                              no op
 *  OP_CALL   0x01      str                     call user fn
 *  OP_CALLI  0x02      byte                    call internal fn #w1
 *  OP_JMP    0x03      offset                  jump. w1 is +- offset
 *  OP_JZ     0x04      offset  w->        w    jump if zero
 *  OP_JNZ    0x05      offset  w->        w    jump if not zero
 *  OP_JZK    0x06      offset  w->w       w    jump if zero (keep stack)
 *  OP_JNZK   0x07      offset  w->w       w    jump if not zero (keep stack)
 *
 *  OP_PUSHSF 0x08              ->sp            push stack frame 
 *  OP_PUSHSP 0x09              ->sp            push stack ptr
 *  OP_POPSF  0x0A              sf->            pop stack frame
 *  OP_PUSH0  0x0B              ->0        w    push a 0 onto stack
 *  OP_PUSH1  0x0C              ->1        w    push a 1 onto stack
 *  OP_DUPS   0x0D              n1->n1,n1  w    duplicate top word
 *  OP_SWAPS  0x0E           n1,n2->n2,n1  w    swap top 2 stack words
 *
 *
 *
 */
static void _cdecl OpSet0 (UCHAR cOp)
   {
   LONG l1, l2;
   INT iJump;

   switch (cOp)
      {
      case OP_NOP:
         break;

      case OP_CALL :
      case OP_CALLI:
         HandleCall (cOp);
         break;

      case OP_JMP  :
         iIP += VMCNextShort ();
         break;

      case OP_JZ   :
         iJump = VMCNextShort ();
         if (!VMSPop ())
            iIP += iJump;
         break;

      case OP_JNZ  :
         iJump = VMCNextShort ();
         if (VMSPop ())
            iIP += iJump;
         break;

      case OP_JZK  :
         iJump = VMCNextShort ();
         if (!VMSPeek ())
            iIP += iJump;
         break;

      case OP_JNZK :
         iJump = VMCNextShort ();
         if (VMSPeek ())
            iIP += iJump;
         break;

      case OP_PUSHSF:
         VMSPush (iSF);
#if defined (_DEBUG)
		   if (iDEBUG & 0x08)
				DumpTheStack ("Just Pushed Stack Frame");
#endif
         break;

      case OP_PUSHSP:
         VMSPush (iSP);
         break;

      case OP_POPSF :
#if defined (_DEBUG)
		   if (iDEBUG & 0x08)
				if (iIP  > 3) // we dont want the POPSF at the beginning of the fn
					DumpTheStack ("Just about to pop Stack Frame");
#endif
         iSF = (INT)VMSPop ();
         break;
         
      case OP_PUSH0 :
         VMSPush (0);
         break;

      case OP_PUSH1 :
         VMSPush (1);
         break;

      case OP_DUPS  :
         l1 = VMSPop();
         VMSPush (l1);
         VMSPush (l1);
         break;
         
      case OP_SWAPS :
         l1 = VMSPop();
         l2 = VMSPop();
         VMSPush (l1);
         VMSPush (l2);
         break;

      default:
         BadOp (cOp);
      }
   }


                     
/*
 *  OP_POPL   0x10-0x13 offset  n1->       cwf  pop to local var
 *  OP_POPLS  0x14-0x17         n1,w->     cwf  pop to local var
 *  OP_POPG   0x18-0x1B addr    n1->       cwf  pop to global var
 *  OP_POPGS  0x1C-0x1F         n1,w->     cwf  pop to global var
 */
static void _cdecl OpSet1 (UCHAR cOp)
   {
   LONG l1, l2, lDest;
   UINT uDest;

   switch (cOp)
      {
      case OP_POPL + M_BYTE  :
      case OP_POPL + M_SHORT :
      case OP_POPL + M_WORD  :
         plSTACK[iSF+VMCNextShort()] = VMTruncate (VMSPop(), cOp);
         break;
         
      case OP_POPL + M_FLOAT :
         uDest = iSF+VMCNextShort();
         plSTACK[uDest+1] = VMSPop ();
         plSTACK[uDest]   = VMSPop ();
         break;

      case OP_POPLS + M_BYTE  :
      case OP_POPLS + M_SHORT :
      case OP_POPLS + M_WORD  :
         uDest = (UINT)(iSF+VMSPop ());
         plSTACK[uDest] = VMTruncate (VMSPop (), cOp);
         break;

      case OP_POPLS + M_FLOAT :
         uDest = (UINT)(iSF+VMSPop ());
         plSTACK[uDest+1] = VMSPop ();
         plSTACK[uDest]   = VMSPop ();
         break;

      case OP_POPG + M_BYTE  :
         VMGStoreByte ((UCHAR)VMSPop (), VMCNextWord ());
         break;

      case OP_POPG + M_SHORT  :
         VMGStoreShort ((SHORT)VMSPop (), VMCNextWord ());
         break;

      case OP_POPG + M_WORD  :
         VMGStoreWord (VMSPop (), VMCNextWord ());
         break;

      case OP_POPG + M_FLOAT :
         VMGStoreFloat (VMSPopF (), VMCNextWord ());
			break;

      case OP_POPGS + M_BYTE  :
         lDest = VMSPop () ;
         VMGStoreByte ((UCHAR)VMSPop (), lDest);
         break;

      case OP_POPGS + M_SHORT  :
         lDest = VMSPop () ;
         VMGStoreShort ((SHORT)VMSPop (), lDest);
         break;

      case OP_POPGS + M_WORD  :
         lDest = VMSPop () ;
         VMGStoreWord (VMSPop (), lDest);
         break;

      case OP_POPGS + M_FLOAT :
         lDest = VMSPop () ;
         l1    = VMSPop ();
         l2    = VMSPop ();
         VMGStoreWord (l1, lDest+MEMSIZE_WORD);
         VMGStoreWord (l2, lDest);
         break;

      default:
         BadOp (cOp);
      }
   }



/*
 *  OP_PUSHL  0x20-0x23 offset  ->n        cwf  push from local var
 *  OP_PUSHLS 0x24-0x27         w1->n      w    push from local var
 *  OP_PUSHG  0x28-0x2B addr    ->n        cwf  push from global var
 *  OP_PUSHGS 0x2C-0x2F         w1->n      cwf  push from global var
 */
static void _cdecl OpSet2 (UCHAR cOp)
   {
   UINT uDest;
   LONG lDest;

   switch (cOp)
      {
      case OP_PUSHL + M_BYTE:
      case OP_PUSHL + M_SHORT:
      case OP_PUSHL + M_WORD:
         VMSPush (plSTACK[iSF+VMCNextShort()]);
         break;

      case OP_PUSHL + M_FLOAT:
         uDest = iSF+VMCNextShort();
         VMSPush (plSTACK[uDest]);
         VMSPush (plSTACK[uDest+1]);
         break;

      case OP_PUSHLS + M_BYTE:
      case OP_PUSHLS + M_SHORT:
      case OP_PUSHLS + M_WORD:
         VMSPush (plSTACK[iSF+VMSPop()]);
         break;

      case OP_PUSHLS + M_FLOAT:
         uDest = (UINT)(iSF+VMSPop());
         VMSPush (plSTACK[uDest]);
         VMSPush (plSTACK[uDest+1]);
         break;

      case OP_PUSHG + M_BYTE:
         VMSPush (VMGGetByte(VMCNextWord()));
         break;

      case OP_PUSHG + M_SHORT:
         VMSPush (VMGGetShort(VMCNextWord()));
         break;

      case OP_PUSHG + M_WORD:
         VMSPush (VMGGetWord(VMCNextWord()));
         break;

      case OP_PUSHG + M_FLOAT:
         lDest = VMCNextWord();
         VMSPush (VMGGetWord(lDest));
         VMSPush (VMGGetWord(lDest + MEMSIZE_WORD));
         break;

      case OP_PUSHGS + M_BYTE:
         VMSPush (VMGGetByte(VMSPop()));
         break;

      case OP_PUSHGS + M_SHORT:
         VMSPush (VMGGetShort(VMSPop()));
         break;

      case OP_PUSHGS + M_WORD:
         VMSPush (VMGGetWord(VMSPop()));
         break;

      case OP_PUSHGS + M_FLOAT:
         lDest = VMSPop();
         VMSPush (VMGGetWord(lDest));
         VMSPush (VMGGetWord(lDest + MEMSIZE_WORD));
         break;

      default:
         BadOp (cOp);
      }
   }

/*
 *  OP_PUSHI  0x30-0x30 n       ->n        cswf push from literal
 */
static void _cdecl OpSet3 (UCHAR cOp)
   {
   switch (cOp)
      {
      case OP_PUSHI + M_BYTE:
         VMSPush (VMCNextByte ());
         break;

      case OP_PUSHI + M_SHORT:
         VMSPush (VMCNextShort ());
         break;

      case OP_PUSHI + M_WORD:
         VMSPush (VMCNextWord ());
         break;

      case OP_PUSHI + M_FLOAT:
         VMSPush (VMCNextWord ());
         VMSPush (VMCNextWord ());
         break;

      default:
         BadOp (cOp);
      }
   }


/*
 *  OP_INCL   0x40-0x43 offset             cwf  increment locl variable directly
 *  OP_INCG   0x44-0x47 addr               cwf  increment glbl variable directly
 *  OP_DECL   0x48-0x4B offset             cwf  decrement locl variable directly
 *  OP_DECG   0x4C-0x4F addr               cwf  decrement glbl variable directly
 */
static void _cdecl OpSet4 (UCHAR cOp)
   {
//   INT    i;
//   PUCHAR pc;
//   PSHORT ps;
//   PLONG  pl;

   switch (cOp)
      {
//      case OP_INCL + M_BYTE :
//      case OP_INCL + M_SHORT:
//      case OP_INCL + M_WORD :
//         i = VMCNextShort();
//         plSTACK[iSF+i] = VMTruncate (plSTACK[iSF+i] + 1, cOp);
//         break;
//
//      case OP_INCL + M_FLOAT:
//         *(PBIG)(plSTACK + iSF + VMCNextShort()) += 1.0;
//         break;
//         
//      case OP_INCG + M_BYTE :
//         pc = (PUCHAR)VMGGetPtr (VMCNextWord ());
//         (*pc)++;
//         break;
//
//      case OP_INCG + M_SHORT :
//         ps = (PSHORT)VMGGetPtr (VMCNextWord ());
//         (*ps)++;
//         break;
//
//      case OP_INCG + M_WORD :
//         pl = (PLONG)VMGGetPtr (VMCNextWord ());
//         (*pl)++;
//         break;
//
//      case OP_INCG + M_FLOAT:
//         *((PBIG)VMGGetPtr (VMCNextWord ())) += 1.0;
//         break;
//
//      case OP_DECL + M_BYTE :
//      case OP_DECL + M_SHORT:
//      case OP_DECL + M_WORD :
//         i = VMCNextShort();
//         plSTACK[iSF+i] = VMTruncate (plSTACK[iSF+i] - 1, cOp);
//         break;
//
//      case OP_DECL + M_FLOAT:
//         *(PBIG)(plSTACK + iSF + VMCNextShort()) -= 1.0;
//         break;
//         
//      case OP_DECG + M_BYTE :
//         pc = (PUCHAR)VMGGetPtr (VMCNextWord ());
//         (*pc)--;
//         break;
//
//      case OP_DECG + M_SHORT :
//         ps = (PSHORT)VMGGetPtr (VMCNextWord ());
//         (*ps)--;
//         break;
//
//      case OP_DECG + M_WORD :
//         pl = (PLONG)VMGGetPtr (VMCNextWord ());
//         (*pl)--;
//         break;
//
//      case OP_DECG + M_FLOAT:
//         *((PBIG)VMGGetPtr (VMCNextWord ())) += 1.0;
//         break;
//
		case OP_INC	+ M_BYTE :
			(*((BYTE*)VMSPeek ()))++;
         break;
		
		case OP_INC	+ M_SHORT :
			(*((USHORT*)VMSPeek ()))++;
         break;

		case OP_INC	+ M_WORD :
			(*((DWORD*)VMSPeek ()))++;
         break;

		case OP_INC	+ M_FLOAT :
			(*((PBIG)VMSPeek ())) += 1.0;
         break;

		case OP_DEC	+ M_BYTE :
			(*((BYTE*)VMSPeek ()))--;
         break;
		
		case OP_DEC	+ M_SHORT :
			(*((USHORT*)VMSPeek ()))--;
         break;

		case OP_DEC	+ M_WORD :
			(*((DWORD*)VMSPeek ()))--;
         break;

		case OP_DEC	+ M_FLOAT :
			(*((PBIG)VMSPeek ())) -= 1.0;
         break;

      default:
         BadOp (cOp);
      }
   }



/*
 *  OP_ISP    0x50      s       ???             inc SP by s words
 *  OP_DSP    0x51      s       ???             dec SP by s words
 *  OP_FTOW   0x52              f->w       f    cvt f -> w
 *  OP_WTOF   0x53              w->f       w    cvt w -> f
 *  OP_ADDRL  0x54              w1->w2     w    cvt local addr to global ptr
 *  OP_ADDRG  0x55              w1->w2     w    cvt global addr to global ptr
 *  OP_BYTE   0x56              w->b       w    top of stk truncate to byte
 *  OP_SHORT  0x57              w->s       w    top of stk truncate to word
 */
static void _cdecl OpSet5 (UCHAR cOp)
   {

   switch (cOp)
      {
      case OP_ISP  : iSP += VMCNextShort ();     							break;
      case OP_DSP  : iSP -= VMCNextShort ();     							break;
      case OP_FTOW : VMSPush ((LONG)VMSPopF ()); 							break;
      case OP_WTOF : VMSPushF ((BIG)VMSPop());   							break;
      case OP_ADDRL: VMSPush ((ULONG)(plSTACK + (iSF + VMSPop ()))); break;
      case OP_ADDRG: VMSPush ((ULONG)VMFixPtr (VMSPop ()));          break;
      case OP_BYTE : VMSPush (VMTruncate (VMSPop (), M_BYTE));       break;
      case OP_SHORT: VMSPush (VMTruncate (VMSPop (), M_SHORT));      break;

      default:
         BadOp (cOp);
      }
   }


/*
 *  OP_SAVL   0x10-0x13 offset  n1->n1     cwf  pop to local var
 *  OP_SAVLS  0x14-0x17         n1,w->n1   cwf  pop to local var
 *  OP_SAVG   0x18-0x1B addr    n1->n1     cwf  pop to global var
 *  OP_SAVGS  0x1C-0x1F         n1,w->n1   cwf  pop to global var
 */
static void _cdecl OpSet6 (UCHAR cOp)
   {
   LONG l1, l2, lDest;
   UINT uDest;

   switch (cOp)
      {
      case OP_SAVL + M_BYTE  :
         plSTACK[iSF+VMCNextShort()] = VMSPeek() & 0xFF;
         break;

      case OP_SAVL + M_SHORT  :
         plSTACK[iSF+VMCNextShort()] = VMSPeek() & 0xFFFF;
         break;

      case OP_SAVL + M_WORD  :
         plSTACK[iSF+VMCNextShort()] = VMSPeek();
         break;
         
      case OP_SAVL + M_FLOAT :
         uDest = iSF+VMCNextShort();
         plSTACK[uDest+1] = VMSPeek ();
         plSTACK[uDest]   = VMSPeekIn (1);
         break;

      case OP_SAVLS + M_BYTE  :
      case OP_SAVLS + M_SHORT :
      case OP_SAVLS + M_WORD  :
         uDest = (UINT)(iSF+VMSPop ());
         plSTACK[uDest] = VMTruncate (VMSPeek (), cOp);
         break;

      case OP_SAVLS + M_FLOAT :
         uDest = (UINT)(iSF+VMSPop ());
         plSTACK[uDest+1] = VMSPeek ();
         plSTACK[uDest]   = VMSPeekIn (1);
         break;

      case OP_SAVG + M_BYTE  :
         VMGStoreByte ((UCHAR)VMSPeek (), VMCNextWord ());
         break;

      case OP_SAVG + M_SHORT  :
         VMGStoreShort ((SHORT)VMSPeek (), VMCNextWord ());
         break;

      case OP_SAVG + M_WORD  :
         VMGStoreWord (VMSPeek (), VMCNextWord ());
         break;

      case OP_SAVG + M_FLOAT :
         VMGStoreFloat (VMSPeekF (), VMCNextWord ());
			break;

      case OP_SAVGS + M_BYTE  :
         lDest = VMSPop ();
         VMGStoreByte ((UCHAR)VMSPeek (), lDest);
         break;

      case OP_SAVGS + M_SHORT  :
         lDest = VMSPop ();
         VMGStoreShort ((SHORT)VMSPeek (), lDest);
         break;

      case OP_SAVGS + M_WORD  :
         lDest = VMSPop ();
         VMGStoreWord (VMSPeek (), lDest);
         break;

      case OP_SAVGS + M_FLOAT :
         lDest = VMSPop ();
         l1    = VMSPeek ();
         l2    = VMSPeekIn (1);
         VMGStoreWord (l1, lDest+MEMSIZE_WORD);
         VMGStoreWord (l2, lDest);
         break;

      default:
         BadOp (cOp);
      }
   }


/*
 */
static void _cdecl OpSet7 (UCHAR cOp)
   {
   LONG lLen, lDest, lVal, lTest;
   INT  i, iSize, iTestLoc, iJump, iTmp;
   BOOL bDef;

   switch (cOp)
      {
      case OP_SWITCH:
         lVal  = VMSPop ();
         iSize = VMCNextShort();
         bDef  = VMCNextShort();
         for (iJump=i=0; i<iSize; i++)
            {
            lTest    = VMCNextWord();
            iTestLoc = VMCNextShort();
            if (lVal == lTest)
               {
               iJump = iTestLoc;
               break;
               }
            }
         if (!iJump && bDef)
            iJump = VMCNextShort();
         iIP += iJump;
         break;

      case OP_PUSHNL:
         lLen = VMSPop ();                     // pop count
         lLen = lLen / 4 + (lLen % 4 ? 1 : 0); // cvt count to # words
         for (i = VMCNextShort(); lLen; lLen--, i++)
            VMSPush (plSTACK[iSF+i]);
         break;

      case OP_PUSHNG:
         lLen = VMSPop ();                     // pop count
         for (lDest = VMCNextWord(); lLen>3; lLen-=4, lDest+=4)
            VMSPush (VMGGetWord(lDest));
         if (lLen)
            {
            lVal = VMGGetWord(lDest) & 0x00FFFFFF;
            if (lLen < 3) lVal &= 0x0000FFFF;
            if (lLen < 2) lVal &= 0x000000FF;
            VMSPush (lVal);
            }
         break;

      case OP_POPNL :
         lLen = VMSPop ();                     // pop count
         lLen = lLen / 4 + (lLen % 4 ? 1 : 0); // cvt count to # words
         for (i = VMCNextShort(); lLen; lLen--, i++)
            plSTACK[iSF+i] = VMSPop();
         break;

      case OP_POPNG :
         lLen = VMSPop ();                     // pop count
         for (lDest = VMCNextWord(); lLen>3; lLen-=4, lDest+=4)
            VMGStoreWord (VMSPop (), lDest);
         if (lLen > 0)
            {
            lVal = VMSPop ();
            for (; lLen>0; lLen-=1, lDest+=1, lVal >>= 8)
               VMGStoreByte ((UCHAR)(lVal & 0xFF), lDest);
            }
         break;

      case OP_POPNLS :
         lLen = VMSPop ();                     // pop count
         lLen = lLen / 4 + (lLen % 4 ? 1 : 0); // cvt count to # words
         for (i = (INT)VMSPop(); lLen; lLen--, i++)
            plSTACK[iSF+i] = VMSPop();
         break;

      case OP_SAVNL :
         lLen = VMSPop ();                     // pop count
         lLen = lLen / 4 + (lLen % 4 ? 1 : 0); // cvt count to # words
         iTmp=0;
         for (i = VMCNextShort(); lLen; lLen--, i++)
            plSTACK[iSF+i] = VMSPeekIn(iTmp++);
         break;

      case OP_SAVNG :
         lLen = VMSPop ();                     // pop count
         iTmp=0;
         for (lDest = VMCNextWord(); lLen>3; lLen-=4, lDest+=4)
            VMGStoreWord (VMSPeekIn (iTmp++), lDest);
         if (lLen > 0)
            {
            lVal = VMSPeekIn(iTmp);
            for (; lLen>0; lLen-=1, lDest+=1, lVal >>= 8)
               VMGStoreByte ((UCHAR)(lVal & 0xFF), lDest);
            }
         break;

      default:
         BadOp (cOp);
      }

   }


//
// due to msc compiler error
//
#pragma optimize ("egl", off)

/*
 *  OP_GT     0x80-0x81         n1,n2->w   wf   comparison: >
 *  OP_LT     0x82-0x83         n1,n2->w   wf   comparison: <
 *  OP_GE     0x84-0x85         n1,n2->w   wf   comparison: >=
 *  OP_LE     0x86-0x87         n1,n2->w   wf   comparison: <=
 *  OP_EQ     0x88-0x89         n1,n2->w   wf   comparison: equal
 *  OP_NE     0x8A-0x8B         n1,n2->w   wf   comparison: not equal
 *  OP_ZE     0x8C-0x8D         n->w       wf   comparison: is zero
 *  OP_NZ     0x8E-0x8F         n->w       wf   comparison: is non zero
 */
static void _cdecl OpSet8 (UCHAR cOp)
   {
   LONG l;
   BIG  f;

   switch (cOp)
      {
      case OP_GT + M_WORD: l = VMSPop(); VMSPush (VMSPop() >  l); break;
      case OP_LT + M_WORD: l = VMSPop(); VMSPush (VMSPop() <  l); break;
      case OP_GE + M_WORD: l = VMSPop(); VMSPush (VMSPop() >= l); break;
      case OP_LE + M_WORD: l = VMSPop(); VMSPush (VMSPop() <= l); break;
      case OP_EQ + M_WORD: l = VMSPop(); VMSPush (VMSPop() == l); break;
      case OP_NE + M_WORD: l = VMSPop(); VMSPush (VMSPop() != l); break;
      case OP_ZE + M_WORD: l = VMSPop(); VMSPush (l == 0);     	break;
      case OP_NZ + M_WORD: l = VMSPop(); VMSPush (l != 0);     	break;

      case OP_GT + M_FLOAT: f = VMSPopF(); VMSPush (VMSPopF() >  f); break;
      case OP_LT + M_FLOAT: f = VMSPopF(); VMSPush (VMSPopF() <  f); break;
      case OP_GE + M_FLOAT: f = VMSPopF(); VMSPush (VMSPopF() >= f); break;
      case OP_LE + M_FLOAT: f = VMSPopF(); VMSPush (VMSPopF() <= f); break;
      case OP_EQ + M_FLOAT: f = VMSPopF(); VMSPush (VMSPopF() == f); break;
      case OP_NE + M_FLOAT: f = VMSPopF(); VMSPush (VMSPopF() != f); break;
      case OP_ZE + M_FLOAT: f = VMSPopF(); VMSPush (f == 0.0);    	break;
      case OP_NZ + M_FLOAT: f = VMSPopF(); VMSPush (f != 0.0);    	break;

      default:
         BadOp (cOp);
      }
   }


#pragma optimize ("", on)


/*
 *  OP_ADD    0x90-0x91         n1,n2->n3  wf   arithmetic +
 *  OP_SUB    0x92-0x93         n1,n2->n3  wf   arithmetic -
 *  OP_MUL    0x94-0x95         n1,n2->n3  wf   arithmetic *
 *  OP_DIV    0x96-0x97         n1,n2->n3  wf   arithmetic /
 *  OP_MOD    0x98-0x99         n1,n2->n3  wf   arithmetic %
 *  OP_NEG    0x9A-0x9B         n1->n2     wf   arithmetic -
 */
static void _cdecl OpSet9 (UCHAR cOp)
   {
   LONG  l;
   BIG f;

   switch (cOp)
      {
      case OP_ADD + M_WORD: l = VMSPop(); VMSPush (VMSPop() + l); break;
      case OP_SUB + M_WORD: l = VMSPop(); VMSPush (VMSPop() - l); break;
      case OP_MUL + M_WORD: l = VMSPop(); VMSPush (VMSPop() * l); break;
      case OP_DIV + M_WORD: l = VMSPop(); VMSPush (VMSPop() / l); break;
      case OP_MOD + M_WORD: l = VMSPop(); VMSPush (VMSPop() % l); break;
      case OP_NEG + M_WORD: l = VMSPop(); VMSPush (-l);        	break;

      case OP_ADD + M_FLOAT: f = VMSPopF(); VMSPushF (VMSPopF() + f); break;
      case OP_SUB + M_FLOAT: f = VMSPopF(); VMSPushF (VMSPopF() - f); break;
      case OP_MUL + M_FLOAT: f = VMSPopF(); VMSPushF (VMSPopF() * f); break;
      case OP_DIV + M_FLOAT: f = VMSPopF(); VMSPushF (VMSPopF() / f); break;
      case OP_MOD + M_FLOAT: f = VMSPopF(); VMSPushF (fmod (VMSPopF(), f)); break;
      case OP_NEG + M_FLOAT: f = VMSPopF(); VMSPushF (-f); 				 break;
      default:
         BadOp (cOp);
      }
   }


/*
 *  OP_SHR    0xA0              w1,w2->w3  w    arithmetic >>
 *  OP_SHL    0xA2              w1,w2->w3  w    arithmetic <<
 *  OP_AND    0xA4              w1,w2->w3  w    arithmetic &
 *  OP_XOR    0xA6              w1,w2->w3  w    arithmetic ^
 *  OP_OR     0xA8              w1,w2->w3  w    arithmetic |
 *  OP_NOT    0xAA              w1->w2     w    arithmetic ~
 */
static void _cdecl OpSetA (UCHAR cOp)
   {
   LONG  l;

   switch (cOp)
      {
      case OP_SHR: l = VMSPop(); VMSPush (VMSPop() >> l); break;
      case OP_SHL: l = VMSPop(); VMSPush (VMSPop() << l); break;
      case OP_AND: l = VMSPop(); VMSPush (VMSPop() &  l); break;
      case OP_XOR: l = VMSPop(); VMSPush (VMSPop() ^  l); break;
      case OP_OR : l = VMSPop(); VMSPush (VMSPop() |  l); break;
      case OP_NOT: l = VMSPop(); VMSPush (~l);        	 break;

      default:
         BadOp (cOp);
      }
   }


/*
 *  OP_LAND   0xB0-0xB1         n1,n2->w   wf   boolean and
 *  OP_LOR    0xB2-0xB3         n1,n2->w   wf   boolean or
 *  OP_LNOT   0xB4-0xB5         n->w       wf   boolean not
 */
static void _cdecl OpSetB (UCHAR cOp)
   {
   LONG l;
   BIG  f;

   switch (cOp)
      {
      case OP_LAND + M_WORD:  l = VMSPop(); VMSPush (VMSPop() && l); 	break;
      case OP_LOR  + M_WORD:  l = VMSPop(); VMSPush (VMSPop() || l); 	break;
      case OP_LNOT + M_WORD:  l = VMSPop(); VMSPush (!l); 					break;
      case OP_LAND + M_FLOAT: f = VMSPopF(); VMSPushF (VMSPopF() && f); break;
      case OP_LOR  + M_FLOAT: f = VMSPopF(); VMSPushF (VMSPopF() || f); break;
      case OP_LNOT + M_FLOAT: f = VMSPopF(); VMSPushF (!f); 				break;

      case OP_SHL1: VMSPush (VMSPop () << 1); 									break;
      case OP_SHL2: VMSPush (VMSPop () << 2); 									break;
      case OP_SHL3: VMSPush (VMSPop () << 3); 									break;

      default:
         BadOp (cOp);
      }
   }

/*
 */
static void _cdecl OpSetC (UCHAR cOp)
   {
#if defined (HASH_SUPPORT)
   DWORD dwHash, dwKey, dwVal, dwFlags;
   switch (cOp)
      {
      case OP_HGET:
			dwHash  = VMSPop();
			dwKey   = VMSPop();
			dwFlags = VMCNextWord ();
			dwVal   = HashGet (dwHash, dwKey, dwFlags);
			VMSPush (dwVal);
			break;	
      default:
		   BadOp (cOp);
      }
#else
   BadOp (cOp);
#endif // defined (HASH_SUPPORT)
   }

/*
 */
static void _cdecl OpSetD (UCHAR cOp)
   {
   BadOp (cOp);
   }

/*
 */
static void _cdecl OpSetE (UCHAR cOp)
   {
   BadOp (cOp);
   }

/*
 */
static void _cdecl OpSetF (UCHAR cOp)
   {
   BadOp (cOp);
   }



/*
 * hardware goes here!
 * This is where we spend our time
 *
 * Notice that a fn returns not by a return op but when 
 * we reach the end of the code for the fn
 * This requires a bounds check before the execution of each
 * opcode - but it protects against runaway code
 *
 */
void VirtualMachine (void)
   {
   UCHAR cSet, cOp;
   INT   iSize;


   iSize = (INT)pfnCURR->uSize;
   while (iIP < iSize)
      {
#if defined (_DEBUG)
      if (iDEBUG & 0x04)
         printf ("IP:%4.4x  SP:%4.4x  StackTop:%p (%ld)\n", iIP, iSP, VMSPeek(), VMSPeek());
#endif
      cOp  = *(pfnCURR->pszCode + iIP++);
      cSet = cOp >> 4;
      OpProcs[cSet] (cOp);
      }
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/


/*
 * exec a function
 * functions are not linked (except for internal fn's),
 * rather they are referenced by name
 */
BOOL VMExecFunction (PSZ pszFn, BOOL bDieIfNotFound)
   {
   PFUNC pfnNew, pfnOld;
   INT   iIPOld;

   if (!(pfnNew = CodFindFunction (pszFn)))
		{
		if (!bDieIfNotFound)
			return FALSE;
      Error ("cannot find function: %s", pszFn);
		}

   pfnOld  = pfnCURR;  // store prev fn
   iIPOld  = iIP;      //

   iIP     = 0;        // set new fn
   pfnCURR = pfnNew;   //

#if defined (_DEBUG)
   if (iDEBUG & 0x01)
		{
		printf ("--------------------------------------------------------------------------------\n");
		printf ("FN: %s  Code: %p(size %x)  Stack: %p  Global: %p\n", pszFn, pfnNew->pszCode, pfnNew->uSize, plSTACK, pfnNew->pszGlobals);
		printf ("--------------------------------------------------------------------------------\n");
		}
#endif

#if defined (HASH_SUPPORT)
	HashScopeTrack ();
#endif // defined (HASH_SUPPORT)

   VirtualMachine ();

#if defined (HASH_SUPPORT)
	// note that this cleans up all hashes create in this fn
	// including malloced hashes !  - There's got to be a better way
	HashScopeCleanup (); 
#endif // defined (HASH_SUPPORT)

   iIP     = iIPOld;   // restore old fn
   pfnCURR = pfnOld;   //

	return TRUE;
   }


/*
 * manually build a stack preamble and call main()
 * the stack conatins argc and argv adjusted to 
 * remove exec itself
 */
void VMExecMain (int argc, char *argv[])
   {
   plSTACK[0] = 0;                // place for return val;
   plSTACK[1] = (ULONG)(argv + 1);// int parm 2
   plSTACK[2] = argc-1;           // int parm 1
   plSTACK[3] = 0x00000020;       // status #2
   plSTACK[4] = 0x00020202;       // status #1
   plSTACK[5] = -7;               // offset to return area

   iSF = 0;
   iSP = 6;
   VMExecFunction ("main", TRUE);
   }


long VMReturn (void)
   {
//   return *plSTACK; // return the return value
	return plSTACK[iSP+1];
   }

void VMInit (UINT uStackSize, int iDebug)
   {
   if (plSTACK)
      free (plSTACK);
   plSTACK = (long*)calloc (uStackSize, sizeof (LONG));
#if defined (_DEBUG)
   iDEBUG  = iDebug;
#endif
   }


void VMTerm (void)
   {
	FreeAllFn ();				//	frees the external fn tree (from AddFn calls)
	CodFreeAllFunctions ();	// frees all code segments
   free (plSTACK);	//	frees the stack
   }



#if defined (_DEBUG)
void DumpTheStack (PSZ psz)
	{
	int i;
	printf ("(SF:%4.4lx SP:%4.4lx IP:%4.4lx) %s\n", iSF, iSP, iIP, psz);
	for (i=iSP-1; i>=0; i--)
		printf ("[%4.4x] %4.4lx\n", i, (ULONG)plSTACK[i]);
	}
#endif
