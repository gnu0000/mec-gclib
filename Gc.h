/*
 *
 * gc.h
 * Monday, 3/10/1997.
 * Craig Fitzgerald
 *
 * Not an external header - This header is intended for 
 * internal use by the gclib library
 */

// define HASH_SUPPORT to allow base support for hashes (ie: int a[char *])
//#define HASH_SUPPORT


#define TOK_EOF               0         
#define TOK_LESSTHAN          1         
#define TOK_GREATERTHAN       2         
#define TOK_LESSOREQUAL       3         
#define TOK_GREATEROREQUAL    4         
#define TOK_EQUIVALENT        5         
#define TOK_NOTEQUAL          6         
#define TOK_EQUALS            7         
#define TOK_PLUSEQUAL         8         
#define TOK_MINUSEQUAL        9         
#define TOK_STAREQUAL         10        
#define TOK_SLASHEQUAL        11        
#define TOK_PERCENTEQUAL      12        
#define TOK_SHIFTRIGHTEQUAL   13        
#define TOK_SHIFTLEFTEQUAL    14
#define TOK_ANDEQUAL          15
#define TOK_XOREQUAL          16
#define TOK_OREQUAL           17
#define TOK_LOGICALOR         18
#define TOK_LOGICALAND        19
#define TOK_EXCLAMATION       20
#define TOK_AMPERSAND         21
#define TOK_OR                22
#define TOK_HAT               23
#define TOK_SHIFTRIGHT        24
#define TOK_SHIFTLEFT         25
#define TOK_TILDA             26
#define TOK_INCREMENT         27
#define TOK_DECREMENT         28
#define TOK_ARROW             29
#define TOK_SLASH             30
#define TOK_PERCENT           31
#define TOK_STAR              32
#define TOK_MINUS             33
#define TOK_PLUS              34
#define TOK_OCOMMENT          35
#define TOK_CCOMMENT          36
#define TOK_CPPCOMMENT        37
#define TOK_AT                38
#define TOK_POUND             39
#define TOK_DOLLAR            40
#define TOK_BACKSLASH         41
#define TOK_OPAREN            42
#define TOK_CPAREN            43
#define TOK_OBRACKET          44
#define TOK_CBRACKET          45
#define TOK_OBRACE            46
#define TOK_CBRACE            47
#define TOK_SINGLEQUOTE       48
#define TOK_DOUBLEQUOTE       49
#define TOK_COMMA             50
#define TOK_PERIOD            51
#define TOK_QUESTION          52
#define TOK_COLON             53
#define TOK_SEMICOLON         54
#define TOK_IF                55
#define TOK_ELSE              56
#define TOK_FOR               57
#define TOK_WHILE             58
#define TOK_SWITCH            59
#define TOK_CASE              60
#define TOK_DEFAULT           61
#define TOK_BREAK             62
#define TOK_CONTINUE          63
#define TOK_RETURN            64
#define TOK_TYPEDEF           65
#define TOK_SIZEOF            66
#define TOK_VOID              67
#define TOK_CHAR              68
#define TOK_SHORT             69
#define TOK_INT               70
#define TOK_LONG              71
#define TOK_FLOAT             72
#define TOK_STRING            73
#define TOK_STRUCT            74
#define TOK_UNION             75
#define TOK_INCLUDE           76
#define TOK_DEFINE            77
#define TOK_DUMPSYM           78
#define TOK_IDENTIFIER        79
#define TOK_CHARLITERAL       80
#define TOK_SHORTLITERAL      81
#define TOK_LONGLITERAL       82
#define TOK_INTLITERAL        83
#define TOK_FLOATLITERAL      84
#define TOK_STRINGLITERAL     85
#define TOK_DECLARATIONLIST   86 
#define TOK_COMPOUNDSTATEMENT 87 
#define TOK_EXPR              88
#define TOK_FUNCTION          89
#define TOK_VARIABLE          90
#define TOK_LITERAL           91
#define TOK_FTOW              92
#define TOK_WTOF              93
#define TOK_DEREF             94
#define TOK_ADDROF            95
#define TOK_SHL1              96
#define TOK_SHL2              97
#define TOK_SHL3              98
#define TOK_INITLIST          99
#define TOK_CAST              100
#define TOK_HASHDEREF         101


#define KIND_VARIABLE         1
#define KIND_FUNCTION         2
#define KIND_TYPEDEF          3
#define KIND_STRUCTDEF        4


#define DATATYPE_VOID         0
#define DATATYPE_CHAR         1
#define DATATYPE_SHORT        2
#define DATATYPE_LONG         3
#define DATATYPE_FLOAT        5
#define DATATYPE_PTR          6
#define DATATYPE_ARRAY        7
#define DATATYPE_STRUCT       8
#define DATATYPE_STRING     	9
#define DATATYPE_HASH         10

#define SCOPE_GLOBAL          1
#define SCOPE_PARAM           2
#define SCOPE_LOCAL           3
#define SCOPE_STRUCT          4

/***************************************************************************/

//typedef struct
//   {
//   union
//      {
//      UINT    uTypeVal;  
//      struct             
//         {
//         CHAR uTypeBase; 
//         CHAR uTypeDeref;
//         };
//      };
//   struct _sym *psStruct;
//   } FORMAL;
//typedef FORMAL *PFORMAL;


//typedef struct _node;

// TypeChain
//	A linked list of types
//	Last element of chain is always a non deref type 
//	(not a DATATYPE_ptr or DATATYPE_array)
//	All elements before last element are always a deref type
//
typedef struct _typlst
	{
	char   cType;   		// one of the DATATYPE_  types
	USHORT iCount;			// # of elements if an array
	union
		{
		struct _node* pExpr;	// Expr that evals to iCount if it's an array
		struct _sym*  pSym;	// Symbol table for a Struct if it's a struct
		};
	struct _typlst *next;	// next in list
	} TYPLST, *PTLST;


typedef struct _sym
   {
   PSZ   pszLiteral;       // symbol literal
   UINT  uKind;            // var, fn, or typedef

	PTLST ptlst;				//	type information
   struct _sym *next;     	// 

//	This is now part of the ptlst (the last node in the chain actually)
//   struct _sym *psStruct;  // var & typedef : if basetype is struct, this pts to structdef symbol

   union 
      {
      struct // elements for uKind=KIND_VARIABLE
         {   //---------------------------------
         INT   iAddr;       // var/array address
         UCHAR uScope;      // local, param, global, 0 if not yet defined
         };
      struct // elements for uKind=KIND_FUNCTION
         {   //---------------------------------
         PTLST*   ppFormals;   // array of symbol ptrs of formal parms in order
         UINT     bDefined;  			// TRUE if fn with body
         UINT     uInternal; 			// 0 if user fn
         };
//      struct // elements for uKind=KIND_TYPEDEF
//         {   //---------------------------------
//         UINT  uTArraySize; // # elements in array
//         CHAR  bTArray;     // TRUE if an array
//         };
      struct // elements for uKind=KIND_STRUCTDEF
         {   //---------------------------------
         struct _sym **ppElements; 
         CHAR   bUnion;     // TRUE if really a union
//         UINT   uStructSize;
         };
      };
   } SYM, *PSYM, **PPSYM;
//typedef SYM *PSYM;
//typedef PSYM *PPSYM;


typedef union
   {
   UCHAR c;    //
   UINT  s;    //
   LONG  l;    //
   BIG   bg;   //

   PSZ   psz;  //
   PUINT ps;   //
   PLONG pl;   //
   PBIG  pbg;  //
   PVOID *ppv;
   } VAL;


typedef struct _token
   {
   UINT uID;          // TOK_HAT  TOK_IDENTIFIER  TOK_CHARLITERAL
   PSZ  pszString;    // "=="     "identifier"    "float literal"
   PSYM ps;           //          symbol entry
   VAL  val;          //                          literal value
                      //-----------------------------------------
   PSZ  pszFile;      // token attributes common to all types
   UINT uLine;        //
   BOOL bEOF;         //
   BOOL bUnget;       //
   } TOKEN, *PTOKEN;


typedef struct _node
   {
   UINT uID;               // token identifier
   PSYM ps;                // symbol ptr for identifier
   VAL  val;               // char, int, float, string,  value for literal
                           // val.u used for parm count for fns
   PSZ  pszFile;           // from token attributes
   UINT uLine;             //

	PTLST ptlst;				// 
	BOOL  bOwnsTL; 			// 

   struct _node *pn1;      // node dependent ptrs...
   struct _node *pn2;      // typically, expression nodes use 1 to 3
   struct _node *pn3;      // typically, statement nodes use 4 (if also uses 3)
   struct _node *pn4;      // 
   struct _node *next;     // 
   } NODE, *PNODE;

extern UINT uOPT;

extern UINT uSTRUCT_PACK_SIZE;


