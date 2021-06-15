/*
 *
 * mem.c
 * Monday, 3/17/1997.
 * Craig Fitzgerald
 *
 */

#include "stdafx.h"
#include "error.h"
#include "opcodes.h"
#include "type.h"
#include "mem.h"

int TLMemRefSize (PTLST ptlst);




//	Make consistent with C
//
//
static int AlignmentRequirement (PTLST ptlst)
	{
	while (TLIsArray (ptlst))
		ptlst = TLDeref (ptlst);

	if (TLGetType (ptlst) != DATATYPE_STRUCT)
		return min ((int)uSTRUCT_PACK_SIZE, TLMemRefSize (ptlst));

	int   iReq = 0;
	PSYM  ps = ptlst->pSym;
	PSYM* ppElements = ps->ppElements;

	if (ps->bUnion)
		return AlignmentRequirement (ppElements[0]->ptlst);
	
	for (int i=0; ppElements[i]; i++)
		iReq = __max (iReq, AlignmentRequirement (ppElements[i]->ptlst));
	return iReq;
	}

int MemAlign (int* piOffset, PTLST ptlst)
	{
	int iAlign = AlignmentRequirement (ptlst);
	int iMod = *piOffset % iAlign;
	*piOffset += (iMod ? iAlign - iMod : 0);
	return *piOffset;
	}

int MemStructElementAlign (int* piOffset, PTLST ptlst)
	{
	return MemAlign (piOffset, ptlst);
	}

static int MemArraySize (PTLST ptlst)
	{
	return ptlst->iCount * TLMemSize (TLDeref(ptlst));
	}


static int MemStructSize (PTLST ptlst)
	{
	int   iSize=0;
	PSYM  ps = ptlst->pSym;
	PSYM* ppElements = ps->ppElements;

	for (int i=0; ppElements[i]; i++)
		{
		PTLST ptlstElement = ppElements[i]->ptlst;

		if (ps->bUnion)
			{
			iSize = __max (iSize, TLMemSize (ptlstElement));
			}
		else
			{
			MemStructElementAlign (&iSize, ptlstElement);
			iSize += TLMemSize (ptlstElement);
			}
		}
	return MemAlign (&iSize, ptlst);
	}



//	calculate the size of a variable of this type
//
//
int TLMemSize (PTLST ptlst)
	{
	assert (ptlst);
	switch (TLGetType (ptlst))
		{
		case DATATYPE_VOID 	: assert(0); return 0;
		case DATATYPE_CHAR 	: return 1;
		case DATATYPE_SHORT	: return 2;
		case DATATYPE_LONG 	: return 4;
		case DATATYPE_FLOAT	: return 8;
		case DATATYPE_PTR  	: return 4;
		case DATATYPE_ARRAY	: return MemArraySize(ptlst);
		case DATATYPE_HASH	: return 4;
		case DATATYPE_STRUCT	: return MemStructSize(ptlst);
		case DATATYPE_STRING	: return 4;
		}
	assert (0);
	return 0;
	}


//	size on the stack;
//
//
int TLMemStkSize (PTLST ptlst)
	{
	return (TLMemSize (ptlst) + 3) / 4;
	}


int TLMemRefSize (PTLST ptlst)
	{
	assert (ptlst);

	switch (TLGetType (ptlst))
		{
		case DATATYPE_VOID 	: return 0;
		case DATATYPE_CHAR 	: return 1;
		case DATATYPE_SHORT	: return 2;
		case DATATYPE_LONG 	: return 4;
		case DATATYPE_FLOAT	: return 8;
		case DATATYPE_PTR  	: return 4;
		case DATATYPE_ARRAY	: return 4;	// arrays passed as references
		case DATATYPE_HASH	: return 4;
		case DATATYPE_STRUCT	: return MemStructSize(ptlst);
		case DATATYPE_STRING	: return 4;
		}
	assert (0);
	return 0;
	}


int TLMemRefStkSize (PTLST ptlst)
	{
	return (TLMemRefSize (ptlst) + 3) / 4;
	}


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
