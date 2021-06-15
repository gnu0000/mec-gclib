/*
 *
 * symbol.c
 * Tuesday, 2/25/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include "symbol.h"
#include "error.h"
#include "type.h"


#define HASH_SIZE 257

PSYM ppsHASH [HASH_SIZE];

PRES pprHASH [HASH_SIZE];

/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

static UINT HashValue (PSZ pszSymbol)
   {
   UINT i = 0;

   while (*pszSymbol)
      i = (i << 1) ^ *pszSymbol++;
   return (i % HASH_SIZE);
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

static PSYM NewSymbol (PSZ pszSym)
   {
   PSYM ps;

   ps = (PSYM)calloc (sizeof (SYM), 1);
   ps->pszLiteral = strdup (pszSym);

   return ps;
   }

static void FreeSymbol (PSYM ps)
   {
   if (ps->pszLiteral) 
      free (ps->pszLiteral);

   if (ps->uKind == KIND_FUNCTION && ps->ppFormals)
      FreeTLArray (ps->ppFormals);

   if (ps->uKind == KIND_STRUCTDEF && ps->ppElements)
		{
//		for (int i=0; ps->ppElements && ps->ppElements[i]; i++)
//	      free (ps->ppElements[i]);
// AAAAAAAHHHHHH!!!!!
      free (ps->ppElements);
		}
	TLFree (ps->ptlst);
   free (ps);
   }


PSYM SymAdd (PSZ pszSym, BOOL bAddAtEnd)
   {
   PSYM ps, psTmp;
   UINT uHashVal;

   uHashVal = HashValue (pszSym);
   ps = NewSymbol (pszSym);

   if (!bAddAtEnd)
      {
      ps->next = ppsHASH[uHashVal];
      ppsHASH[uHashVal] = ps;
      }
   else
      {
      for (psTmp = ppsHASH[uHashVal]; psTmp && psTmp->next; psTmp=psTmp->next)
         ;
      if (psTmp)
         psTmp->next = ps;
      else
         ppsHASH[uHashVal] = ps;
      }
   return ps;
   }


PSYM SymFind (PSZ pszSym)
   {
   PSYM ps;
   UINT uHashVal;

   uHashVal = HashValue (pszSym);
   for (ps = ppsHASH[uHashVal]; ps; ps = ps->next)
      if (!strcmp (pszSym, ps->pszLiteral))
         return ps;
   return NULL;
   }


//BOOL SymDelete (PSZ pszSym)
//   {
//   PSYM ps, psPrev = NULL;
//   UINT uHashVal;
//
//   uHashVal = HashValue (pszSym);
//   for (ps = ppsHASH[uHashVal]; ps; ps = ps->next)
//      {
//      if (s!trcmp (pszSym, ps->pszLiteral))
//         {
//         if (psPrev)
//            psPrev->next = ps->next;
//         else
//            ppsHASH[uHashVal] = ps->next;
//         FreeSymbol (ps);
//         return TRUE;
//         }
//      psPrev = ps;
//      }
//   return FALSE;
//   }


void SymDeleteLocals (void)
   {
   PSYM ps, psPrev, psNext;
   UINT i;

   for (i=0; i<HASH_SIZE; i++)
      {
      psPrev = NULL;
      for (ps = ppsHASH[i]; ps; ps = psNext)
         {
         psNext = ps->next;
         if (ps->uKind == KIND_VARIABLE && (ps->uScope == SCOPE_LOCAL || ps->uScope == SCOPE_PARAM))
            {
            Log (16, "removing symbol %s\n", ps->pszLiteral);

            if (psPrev)
               psPrev->next = ps->next;
            else
               ppsHASH[i] = ps->next;
            FreeSymbol (ps);
            }
         else
            psPrev = ps;
         ps = psNext;
         }
      }
   }



/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/


void SymAddReserved  (PRES pr)
   {
   UINT uHashVal;

   uHashVal = HashValue (pr->pszString);
   pr->next = pprHASH[uHashVal];
   pprHASH[uHashVal] = pr;
   }



PRES SymFindReserved (PSZ pszStr)
   {
   PRES pr;
   UINT uHashVal;

   uHashVal = HashValue (pszStr);
   for (pr = pprHASH[uHashVal]; pr; pr = pr->next)
      if (!strcmp (pszStr, pr->pszString))
         return pr;
   return NULL;
   }


void SymInit (void)
   {
   UINT i;

   for (i=0; i<HASH_SIZE; i++)
      {
      ppsHASH[i] = NULL;   
      pprHASH[i] = NULL;   
      }
   }


void SymTerm (void)
   {
   PSYM ps, psNext;
   UINT i, j;

   for (i=0; i<HASH_SIZE; i++)
      {
      for (j=0, ps = ppsHASH[i]; ps; ps = psNext, j++)
         {
         psNext = ps->next;
         FreeSymbol (ps);
         }
      ppsHASH[i] = NULL;
      }
   }



/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

static void DumpDataType (PTLST ptlst)
	{
	for (;ptlst; ptlst = ptlst->next)
		{
		switch (TLGetType (ptlst))
			{
			case DATATYPE_VOID   : printf ("void"); 											break;
			case DATATYPE_CHAR   : printf ("char"); 											break;
			case DATATYPE_SHORT  : printf ("short"); 											break;
			case DATATYPE_LONG   : printf ("long"); 											break;
			case DATATYPE_FLOAT  : printf ("float"); 											break;
			case DATATYPE_PTR    : printf ("ptr"); 											break;
			case DATATYPE_ARRAY  : printf ("array[%d]", ptlst->iCount); 				break;
			case DATATYPE_STRUCT : printf ("struct[%s]", ptlst->pSym->pszLiteral);	break;
			case DATATYPE_STRING : printf ("string"); 										break;
			default              : printf ("invalid!");
			}
		if (ptlst->next)
			printf ("->");
		}
	}

void DumpDataTypeSymbol (PSYM ps)
   {
   printf ("type %s\n", ps->pszLiteral);
 	DumpDataType (ps->ptlst);
	printf ("\n");
   }

void DumpFunctionSymbol (PSYM ps)
   {
   printf ("fn   %s\n", ps->pszLiteral);
 	DumpDataType (ps->ptlst);
   printf ( "[%s] ", ps->bDefined ? "DEFINED " : "DECLARED"),
   printf ( "addr: %4.4d\n", ps->iAddr);
   }

void DumpVariableSymbol (PSYM ps)
   {
   printf ("var  %s\n", ps->pszLiteral);
 	DumpDataType (ps->ptlst);
   printf ( "[%s] ",
           (ps->uScope==SCOPE_GLOBAL   ? "SCOPE_GLOBAL" :
            (ps->uScope==SCOPE_PARAM   ? "SCOPE_PARAM " :
             (ps->uScope==SCOPE_LOCAL  ? "SCOPE_LOCAL " :
              (ps->uScope==SCOPE_STRUCT ? "SCOPE_STRUCT" : "<none>      ")))));
   printf ( "addr: %4.4d\n", ps->iAddr);
   }

void SymDump (void)
   {
   PSYM ps;
   UINT i;

   printf ("*************************[Struct Def]*************************\n");
   for (i=0; i<HASH_SIZE; i++)
      for (ps = ppsHASH[i]; ps; ps = ps->next)
         if (ps->uKind == KIND_STRUCTDEF)
            DumpDataTypeSymbol (ps);

   printf ("*************************[Data Types]*************************\n");
   for (i=0; i<HASH_SIZE; i++)
      for (ps = ppsHASH[i]; ps; ps = ps->next)
         if (ps->uKind == KIND_TYPEDEF)
            DumpDataTypeSymbol (ps);

   printf ("*************************[Functions ]*************************\n");
   for (i=0; i<HASH_SIZE; i++)
      for (ps = ppsHASH[i]; ps; ps = ps->next)
         if (ps->uKind == KIND_FUNCTION)
            DumpFunctionSymbol (ps);

   printf ("*************************[Variables ]*************************\n");
   for (i=0; i<HASH_SIZE; i++)
      for (ps = ppsHASH[i]; ps; ps = ps->next)
         if (ps->uKind == KIND_VARIABLE)
            DumpVariableSymbol (ps);
   }
