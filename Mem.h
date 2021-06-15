/*
 *
 * mem.h
 * Monday, 3/17/1997.
 * Craig Fitzgerald
 *
 * Not an external header - This header is intended for 
 * internal use by the gclib library
 */


/*
 * a variable of a given type will take up how many bytes
 * of memory you ask?
 */
//int MemSize (UINT uTypeVal, PSYM psStruct);


/*
 *
 */
//int MemSymSize (PSYM ps);

/*
 * a variable of a given type will take up how much room
 * on the stack you ask?
 */
//int MemStkSize (UINT uTypeVal, PSYM psStruct);




//	calculate the size of a variable of this type
//
//
//
int TLMemSize (PTLST ptlst);


//	size on the stack;
//
//
int TLMemStkSize (PTLST ptlst);


//	size of var when passed as a parameter
//	(smaller than TLMemStkSize if an array)
//
int TLMemRefStkSize (PTLST ptlst);


int MemStructElementAlign (int* piOffset, PTLST ptlst);
