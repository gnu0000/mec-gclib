/*
 *
 * loadgx.c
 * Friday, 3/28/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include <GnuFile.h>
#include "binfile.h"
#include "codmaint.h"


UINT _cdecl _defLoadTag      (FILE *fp);
void _cdecl _defLoadFunction (FILE *fp);
PSZ  _cdecl _defLoadGlobals  (FILE *fp);


UINT (_cdecl *LoadTag)      (FILE *fp) = _defLoadTag     ;
void (_cdecl *LoadFunction) (FILE *fp) = _defLoadFunction;
PSZ  (_cdecl *LoadGlobals)  (FILE *fp) = _defLoadGlobals ;



/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/

/*
 * allocs result
 */
static PSZ ReadName (FILE *fp)
   {
   UCHAR szTmp[128];
   PSZ  pszTmp = (PSZ)szTmp;
   INT  c;

   for (c = f_getc (fp); c && c != EOF ; c = f_getc (fp))
      *pszTmp++ = c;
   *pszTmp = '\0';

   return (PSZ)strdup ((LPCTSTR)szTmp);
   }


/*
 * allocs result
 */
static PSZ ReadBuffer (FILE *fp, UINT uLen)
   {
   PSZ pszBuff;

   if (!uLen)
      return NULL;

   pszBuff = (PSZ)calloc (uLen + 1, 1);
   if (fread (pszBuff, 1, uLen, fp) < uLen)
      Error ("Unexpected EOF");
   return pszBuff;
   }


UINT _cdecl _defLoadTag (FILE *fp)
   {
   return FilReadUShort (fp);
   }


/*
 * global memory is a contiguous memory block
 * strings are actually 4byte pointers to strings outside
 * this block
 */
PSZ _cdecl _defLoadGlobals (FILE *fp)
   {
   UINT uSize;
   PSZ  pszGlobals;

   uSize = FilReadUShort (fp);
   pszGlobals = ReadBuffer (fp, uSize);

   if (LoadTag (fp) != BIN_TERM)
      Error ("bin file in bad format! (term)");

   return pszGlobals;
   }


void _cdecl _defLoadFunction (FILE *fp)
   {
   PFUNC pFunc;

   pFunc = CodNewFunction ();
   pFunc->pszName = ReadName (fp);
   pFunc->uSize   = FilReadUShort (fp);
   pFunc->pszCode = ReadBuffer (fp, pFunc->uSize);
   
   CodAddFunction (pFunc);

   if (LoadTag (fp) != BIN_TERM)
      Error ("bin file in bad format! (term)");
   }

/*
 * returns: 0 - ok
 *          1 - Unable to open program file
 *          2 - File is not a GNU program file
 *          3 - bin file in bad format!
 *
 */
UINT LoadGXModule (PSZ pszFile)
   {
   FILE  *fp;
   UINT  u;
   BOOL  bLoop = TRUE;
   BOOL  bGlobals = FALSE;
   PSZ   pszGlobals;

   if (!(fp = fopen (pszFile, "rb")))
      return 1;

   if (LoadTag (fp) != BIN_FILEHEADER)
      {
      fclose (fp);
      return 2;
      }

   while (bLoop && (u = FilReadUShort (fp)) && !feof (fp))
      {
      switch (u)
         {
         case BIN_FUNCTIONHEADER:
            LoadFunction (fp);
            break;

         case BIN_GLOBALHEADER:
            pszGlobals = LoadGlobals (fp);
            CodSetGlobalPtr (pszGlobals);
            bGlobals = TRUE;
            break;

         case BIN_TERM:
            bLoop = FALSE;

         default:
            fclose (fp);
            return 3;
         }
      }
   if (!bGlobals)
      CodSetGlobalPtr (NULL); // look at the fn
   fclose (fp);
   return 0;
   }


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*                                                                         */
/***************************************************************************/


void LoadSetInputFn (PVOID pFn, UINT uFn)
   {
   switch (uFn)
      {
      case FTYP_LOAD_FILE_TAGS   : LoadTag      = (UINT (_cdecl *) (FILE *fp))pFn; break;
      case FTYP_LOAD_FN          : LoadFunction = (void (_cdecl *) (FILE *fp))pFn; break;
      case FTYP_LOAD_GLOBAL_START: LoadGlobals  = (PSZ  (_cdecl *) (FILE *fp))pFn; break;
      default:
         Error ("Unknown fn id in SetCompileFunction [%d]", uFn);
      }
   }
