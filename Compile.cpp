/*
 *
 * compile.c
 * Monday, 3/31/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include "define.h"
#include "symbol.h"
#include "tokenize.h"
#include "parse.h"
#include "gen.h"
#include "error.h"


/*
 *
 *
 */
void Compile (PSZ pszIn, PSZ pszOut)
   {
   SymInit ();
   TokInit (pszIn);
   GenInit (pszOut);

   CompileDriver ();

   GenTerm ();
   TokTerm ();
   SymTerm ();    // terminate symbols module
   DefineTerm (); // terminate defines module
   }


//	Like Compile but used when the input file is already open
//
//
void Compile2 (FILE* fp, PSZ pszInFile, int iLine, PSZ pszOut)
   {
   SymInit ();
   TokInit2 (fp, pszInFile, iLine);
   GenInit (pszOut);

   CompileDriver ();

   GenTerm ();
   TokTerm ();
   SymTerm ();    // terminate symbols module
   DefineTerm (); // terminate defines module
   }



void SetCompileOption (UINT uOption, UINT uVal)
   {
   switch (uOption)
      {
      case CO_OPTIMIZE  :  uOPT = uVal;              break;
      case CO_STRUCTPACK:  uSTRUCT_PACK_SIZE = uVal; break;
      case CO_LOGVAL    :  LOG_VAL = uVal;           break;
      default: Error ("unknown compiler option [%d]", uOption);
      }
   }


