/*
 *
 * extern.h
 * Sunday, 3/30/1997.
 * Craig Fitzgerald
 *
 * Not an external header - This header is intended for 
 * internal use by the gclib library
 *
 *
 *
 */

//	defined in gclib.h
//void AddFn (PVOID pFn, UINT uSize, UINT uTag, UINT uCConv=0);

void OrderFn (void);

BOOL HandleInternalCall (UINT uTag);


