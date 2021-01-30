/* xlftab.c - xlisp function table */
/*	Copyright (c) 1985, by David Michael Betz */

#include "xlisp.h"

/* external functions */
extern LVAL
    rmhash(),rmquote(),rmdquote(),rmbquote(),rmcomma(),
    clnew(),clisnew(),clanswer(),
    obisnew(),obclass(),obshow(),
    rmlpar(),rmrpar(),rmsemi(),
    xeval(),xapply(),xfuncall(),xquote(),xfunction(),xbquote(),
    xlambda(),xset(),xsetq(),xsetf(),xdefun(),xdefmacro(),
    xgensym(),xmakesymbol(),xintern(),
    xsymname(),xsymvalue(),xsymplist(),
    xget(),xputprop(),xremprop(),
    xhash(),xmkarray(),xaref(),
    xcar(),xcdr(),
    xcaar(),xcadr(),xcdar(),xcddr(),
    xcaaar(),xcaadr(),xcadar(),xcaddr(),
    xcdaar(),xcdadr(),xcddar(),xcdddr(),
    xcaaaar(),xcaaadr(),xcaadar(),xcaaddr(),
    xcadaar(),xcadadr(),xcaddar(),xcadddr(),
    xcdaaar(),xcdaadr(),xcdadar(),xcdaddr(),
    xcddaar(),xcddadr(),xcdddar(),xcddddr(),
    xcons(),xlist(),xappend(),xreverse(),xlast(),xnth(),xnthcdr(),
    xmember(),xassoc(),xsubst(),xsublis(),xlength(),xsort(),
    xremove(),xremif(),xremifnot(),
    xmapc(),xmapcar(),xmapl(),xmaplist(),
    xrplca(),xrplcd(),xnconc(),
    xdelete(),xdelif(),xdelifnot(),
    xatom(),xsymbolp(),xnumberp(),xboundp(),xnull(),xlistp(),xendp(),xconsp(),
    xeq(),xeql(),xequal(),
    xcond(),xcase(),xand(),xor(),xlet(),xletstar(),xif(),
    xprog(),xprogstar(),xprog1(),xprog2(),xprogn(),xgo(),xreturn(),
    xcatch(),xthrow(),
    xerror(),xcerror(),xbreak(),
    xcleanup(),xtoplevel(),xcontinue(),xerrset(),
    xbaktrace(),xevalhook(),
    xdo(),xdostar(),xdolist(),xdotimes(),
    xminusp(),xzerop(),xplusp(),xevenp(),xoddp(),
    xfix(),xfloat(),
    xgcd(),xadd(),xsub(),xmul(),xdiv(),xrem(),xmin(),xmax(),xabs(),
    xadd1(),xsub1(),xlogand(),xlogior(),xlogxor(),xlognot(),
    xsin(),xcos(),xtan(),xexpt(),xexp(),xsqrt(),xrand(),
    xlss(),xleq(),xequ(),xneq(),xgeq(),xgtr(),
    xstrcat(),xsubseq(),xstring(),xchar(),
    xread(),xprint(),xprin1(),xprinc(),xterpri(),
    xflatsize(),xflatc(),
    xopen(),xclose(),xrdchar(),xpkchar(),xwrchar(),xreadline(),
    xload(),xtranscript(),
    xtype(),xexit(),xpeek(),xpoke(),xaddrs(),
    xvector(),xblock(),xrtnfrom(),xtagbody(),
    xpsetq(),xflet(),xlabels(),xmacrolet(),xunwindprotect(),xpp(),
    xstrlss(),xstrleq(),xstreql(),xstrneq(),xstrgeq(),xstrgtr(),
    xstrilss(),xstrileq(),xstrieql(),xstrineq(),xstrigeq(),xstrigtr(),
    xupcase(),xdowncase(),xnupcase(),xndowncase(),
    xtrim(),xlefttrim(),xrighttrim(),
    xuppercasep(),xlowercasep(),xbothcasep(),xdigitp(),xalphanumericp(),
    xcharcode(),xcodechar(),xchupcase(),xchdowncase(),xdigitchar(),
    xchrlss(),xchrleq(),xchreql(),xchrneq(),xchrgeq(),xchrgtr(),
    xchrilss(),xchrileq(),xchrieql(),xchrineq(),xchrigeq(),xchrigtr(),
    xintegerp(),xfloatp(),xstringp(),xarrayp(),xstreamp(),xobjectp(),
    xwhen(),xunless(),xloop(),
    xsymfunction(),xfboundp(),xsend(),xsendsuper(),
    xprogv(),xrdbyte(),xwrbyte(),xformat(),
    xcharp(),xcharint(),xintchar(),
    xmkstrinput(),xmkstroutput(),xgetstroutput(),xgetlstoutput(),
    xgetlambda(),xmacroexpand(),x1macroexpand(),
    xtrace(),xuntrace(),
    xdefstruct(),xmkstruct(),xcpystruct(),xstrref(),xstrset(),xstrtypep(),
    xasin(),xacos(),xatan(),
    Prim_POPEN(), Prim_PCLOSE(), Prim_SYSTEM(),	/* NPM */
    Prim_FSCANF_FIXNUM(), Prim_FSCANF_STRING(), Prim_FSCANF_FLONUM(), /* NPM */
    Prim_COPY_ARRAY(), Prim_ARRAY_INSERT_POS(), Prim_ARRAY_DELETE_POS(); /* NPM */

extern LVAL xosenvget(); /* JSP */
extern void xlinclude_hybrid_prims(); /* Voodoo */

/* Include hybrid-class functions: *//* JSP */
#define MODULE_XLFTAB_C_GLOBALS
#include "../../xmodules.h"
#undef MODULE_XLFTAB_C_GLOBALS

/* functions specific to xldmem.c */
LVAL xgc(),xexpand(),xalloc(),xmem();
#ifdef SAVERESTORE
LVAL xsave(),xrestore();
#endif

/* include system dependent definitions */
#include "osdefs.h"

/* SUBR/FSUBR indicator */
#define S	SUBR
#define F	FSUBR

/* forward declarations */
LVAL xnotimp();

/* the function table */
FUNDEF *funtab;

/* and its associated parts */ /* Voodoo */
#define xlisp_prim_max 500
int	iPrimCount;


/* xlfinit - setup xlisp function table */ /* Voodoo */
void xlfinit()
{
    int		iIndex;

    if (funtab = (FUNDEF *) malloc(xlisp_prim_max * sizeof(FUNDEF))) {
    
	iPrimCount = 0;

	/* load xlisp native prims, updates iPrimCount global */
	xlinclude_native_prims();  	
    
	/* load user's hybrid prims, updates iPrimCount global */
	xlinclude_hybrid_prims();  	
    
	/* reserve a slot for sentinel */
	funtab[iPrimCount].fd_name = 0;
	funtab[iPrimCount].fd_type = 0;
	funtab[iPrimCount].fd_subr = 0;
	iPrimCount ++;

	/* allocate permanent global funtable of exact size */
	funtab = (FUNDEF *) realloc(funtab, iPrimCount * sizeof(FUNDEF));
	}

    } /* xlfinit */



/*  xldefine_prim - enter xlisp prim into xlisp function table */
void xldefine_prim(sName, iType, pFun)  /* Voodoo */
    char		*sName;
    int			iType;
    LVAL 		(*pFun)();
{
    funtab[iPrimCount].fd_name = sName;		
    funtab[iPrimCount].fd_type = iType;		
    funtab[iPrimCount].fd_subr = pFun;		
    iPrimCount ++;
    }   



/* xnotimp - function table entries that are currently not implemented */
LOCAL LVAL xnotimp()
{
    xlfail("function not implemented");
}

/* funtab_offset - find given fn in funtab. */		/* JSP */
/* (Obviates need for hacks like FT_CLNEW.) */          /* JSP */
LOCAL int funtab_index = 0; /* For O(1) lookup time on ordered requests. */
funtab_offset(fn)					/* JSP */
LVAL        (*fn)();					/* JSP */
{							/* JSP */
    int wrapCount = 0;					/* JSP */
    while (wrapCount < 2) {				/* JSP */
        LVAL (*e)() = funtab[ funtab_index ].fd_subr;	/* JSP */
        if (e == fn)   return funtab_index;		/* JSP */
        if (e)              ++funtab_index;		/* JSP */
        else {++wrapCount;    funtab_index = 0;}	/* JSP */
    }							/* JSP */
    xlfatal("funtab_offset: internal error");		/* JSP */
}							/* JSP */




xlinclude_native_prims()
{
    /* read macro functions */

    
xldefine_prim(NULL,		S, rmhash		); /*   0 */
xldefine_prim(NULL,		S, rmquote		); /*   1 */
xldefine_prim(NULL,		S, rmdquote		); /*   2 */
xldefine_prim(NULL,		S, rmbquote		); /*   3 */
xldefine_prim(NULL,		S, rmcomma		); /*   4 */
xldefine_prim(NULL,		S, rmlpar		); /*   5 */
xldefine_prim(NULL,		S, rmrpar		); /*   6 */
xldefine_prim(NULL,		S, rmsemi		); /*   7 */
xldefine_prim(NULL,		S, xnotimp		); /*   8 */
#ifdef ORIGINAL
xldefine_prim(NULL,		S, xnotimp		); /*   9 */
#else
    /* BUGGO,need to put envget somewhere else. */
xldefine_prim("GETENV",		S, xosenvget		); /*   9 */
#endif
    
    /* methods */
xldefine_prim(NULL,		S, clnew		); /*  10 */
xldefine_prim(NULL,		S, clisnew		); /*  11 */
xldefine_prim(NULL,		S, clanswer		); /*  12 */
xldefine_prim(NULL,		S, obisnew		); /*  13 */
xldefine_prim(NULL,		S, obclass		); /*  14 */
xldefine_prim(NULL,		S, obshow		); /*  15 */
xldefine_prim(NULL,		S, xnotimp		); /*  16 */
xldefine_prim(NULL,		S, xnotimp		); /*  17 */
xldefine_prim(NULL,		S, xnotimp		); /*  18 */
xldefine_prim(NULL,		S, xnotimp		); /*  19 */
    
    /* evaluator functions */
xldefine_prim("EVAL",		S, xeval		); /*  20 */
xldefine_prim("APPLY",		S, xapply		); /*  21 */
xldefine_prim("FUNCALL",	S, xfuncall		); /*  22 */
xldefine_prim("QUOTE",		F, xquote		); /*  23 */
xldefine_prim("FUNCTION",	F, xfunction		); /*  24 */
xldefine_prim("BACKQUOTE",	F, xbquote		); /*  25 */
xldefine_prim("LAMBDA",		F, xlambda		); /*  26 */
    
    /* symbol functions */
xldefine_prim("SET",		S, xset			); /*  27 */
xldefine_prim("SETQ",		F, xsetq		); /*  28 */
xldefine_prim("SETF",		F, xsetf		); /*  29 */
xldefine_prim("DEFUN",		F, xdefun		); /*  30 */
xldefine_prim("DEFMACRO",	F, xdefmacro		); /*  31 */
xldefine_prim("GENSYM",		S, xgensym		); /*  32 */
xldefine_prim("MAKE-SYMBOL",	S, xmakesymbol		); /*  33 */
xldefine_prim("INTERN", 	S, xintern		); /*  34 */
xldefine_prim("SYMBOL-NAME",	S, xsymname		); /*  35 */
xldefine_prim("SYMBOL-VALUE",	S, xsymvalue		); /*  36 */
xldefine_prim("SYMBOL-PLIST",	S, xsymplist		); /*  37 */
xldefine_prim("GET",		S, xget	); /*  38 */
xldefine_prim("PUTPROP",	S, xputprop); /*  39 */
xldefine_prim("REMPROP",	S, xremprop		); /*  40 */
xldefine_prim("HASH",		S, xhash		); /*  41 */
    
    /* array functions */
xldefine_prim("MAKE-ARRAY",	S, xmkarray		); /*  42 */
xldefine_prim("AREF",		S, xaref		); /*  43 */
    
    /* list functions */
xldefine_prim("CAR",		S, xcar			); /*  44 */
xldefine_prim("CDR",		S, xcdr			); /*  45 */
    
xldefine_prim("CAAR",		S, xcaar		); /*  46 */
xldefine_prim("CADR",		S, xcadr		); /*  47 */
xldefine_prim("CDAR",		S, xcdar		); /*  48 */
xldefine_prim("CDDR",		S, xcddr		); /*  49 */
    
xldefine_prim("CAAAR",		S, xcaaar		); /*  50 */
xldefine_prim("CAADR",		S, xcaadr		); /*  51 */
xldefine_prim("CADAR",		S, xcadar		); /*  52 */
xldefine_prim("CADDR",		S, xcaddr		); /*  53 */
xldefine_prim("CDAAR",		S, xcdaar		); /*  54 */
xldefine_prim("CDADR",		S, xcdadr		); /*  55 */
xldefine_prim("CDDAR",		S, xcddar		); /*  56 */
xldefine_prim("CDDDR",		S, xcdddr		); /*  57 */
    
xldefine_prim("CAAAAR", 	S, xcaaaar		); /*  58 */
xldefine_prim("CAAADR",		S, xcaaadr		); /*  59 */
xldefine_prim("CAADAR",		S, xcaadar		); /*  60 */
xldefine_prim("CAADDR",		S, xcaaddr		); /*  61 */
xldefine_prim("CADAAR",		S, xcadaar		); /*  62 */
xldefine_prim("CADADR",		S, xcadadr		); /*  63 */
xldefine_prim("CADDAR",		S, xcaddar		); /*  64 */
xldefine_prim("CADDDR",		S, xcadddr		); /*  65 */
xldefine_prim("CDAAAR",		S, xcdaaar		); /*  66 */
xldefine_prim("CDAADR",		S, xcdaadr		); /*  67 */
xldefine_prim("CDADAR",		S, xcdadar		); /*  68 */
xldefine_prim("CDADDR",		S, xcdaddr		); /*  69 */
xldefine_prim("CDDAAR",		S, xcddaar		); /*  70 */
xldefine_prim("CDDADR",		S, xcddadr		); /*  71 */
xldefine_prim("CDDDAR",		S, xcdddar		); /*  72 */
xldefine_prim("CDDDDR",		S, xcddddr		); /*  73 */
    
xldefine_prim("CONS",		S, xcons		); /*  74 */
xldefine_prim("LIST",		S, xlist		); /*  75 */
xldefine_prim("APPEND",		S, xappend		); /*  76 */
xldefine_prim("REVERSE",	S, xreverse		); /*  77 */
xldefine_prim("LAST",		S, xlast		); /*  78 */
xldefine_prim("NTH",		S, xnth			); /*  79 */
xldefine_prim("NTHCDR",		S, xnthcdr		); /*  80 */
xldefine_prim("MEMBER",		S, xmember		); /*  81 */
xldefine_prim("ASSOC",		S, xassoc		); /*  82 */
xldefine_prim("SUBST", 		S, xsubst		); /*  83 */
xldefine_prim("SUBLIS",		S, xsublis		); /*  84 */
xldefine_prim("REMOVE",		S, xremove		); /*  85 */
xldefine_prim("LENGTH",		S, xlength		); /*  86 */
xldefine_prim("MAPC",		S, xmapc		); /*  87 */
xldefine_prim("MAPCAR",		S, xmapcar		); /*  88 */
xldefine_prim("MAPL",		S, xmapl		); /*  89 */
xldefine_prim("MAPLIST",	S, xmaplist		); /*  90 */
    
    /* destructive list functions */
xldefine_prim("RPLACA",		S, xrplca		); /*  91 */
xldefine_prim("RPLACD",		S, xrplcd		); /*  92 */
xldefine_prim("NCONC",		S, xnconc		); /*  93 */
xldefine_prim("DELETE",		S, xdelete		); /*  94 */
    
    /* predicate functions */
xldefine_prim("ATOM",		S, xatom		); /*  95 */
xldefine_prim("SYMBOLP",	S, xsymbolp		); /*  96 */
xldefine_prim("NUMBERP",	S, xnumberp		); /*  97 */
xldefine_prim("BOUNDP",		S, xboundp 		); /*  98 */
xldefine_prim("NULL",		S, xnull		); /*  99 */
xldefine_prim("LISTP",		S, xlistp		); /* 100 */
xldefine_prim("CONSP",		S, xconsp		); /* 101 */
xldefine_prim("MINUSP",		S, xminusp 		); /* 102 */
xldefine_prim("ZEROP",		S, xzerop		); /* 103 */
xldefine_prim("PLUSP",		S, xplusp		); /* 104 */
xldefine_prim("EVENP",		S, xevenp		); /* 105 */
xldefine_prim("ODDP",		S, xoddp		); /* 106 */
xldefine_prim("EQ",		S, xeq			); /* 107 */
xldefine_prim("EQL",		S, xeql			); /* 108 */
xldefine_prim("EQUAL",		S, xequal		); /* 109 */
    
    /* special forms */
xldefine_prim("COND",		F, xcond		); /* 110 */
xldefine_prim("CASE",		F, xcase		); /* 111 */
xldefine_prim("AND",		F, xand			); /* 112 */
xldefine_prim("OR",		F, xor			); /* 113 */
xldefine_prim("LET",		F, xlet			); /* 114 */
xldefine_prim("LET*",		F, xletstar		); /* 115 */
xldefine_prim("IF",		F, xif			); /* 116 */
xldefine_prim("PROG",		F, xprog		); /* 117 */
xldefine_prim("PROG*",		F, xprogstar		); /* 118 */
xldefine_prim("PROG1",		F, xprog1		); /* 119 */
xldefine_prim("PROG2",		F, xprog2		); /* 120 */
xldefine_prim("PROGN",		F, xprogn		); /* 121 */
xldefine_prim("GO",		F, xgo			); /* 122 */
xldefine_prim("RETURN",		F, xreturn  		); /* 123 */
xldefine_prim("DO",		F, xdo			); /* 124 */
xldefine_prim("DO*",		F, xdostar  		); /* 125 */
xldefine_prim("DOLIST",		F, xdolist  		); /* 126 */
xldefine_prim("DOTIMES",	F, xdotimes		); /* 127 */
xldefine_prim("CATCH",		F, xcatch		); /* 128 */
xldefine_prim("THROW",		F, xthrow		); /* 129 */
    
    /* debugging and error handling functions */
xldefine_prim("ERROR",		S, xerror		); /* 130 */
xldefine_prim("CERROR",		S, xcerror  		); /* 131 */
xldefine_prim("BREAK",		S, xbreak		); /* 132 */
xldefine_prim("CLEAN-UP",	S, xcleanup		); /* 133 */
xldefine_prim("TOP-LEVEL",	S, xtoplevel		); /* 134 */
xldefine_prim("CONTINUE",	S, xcontinue		); /* 135 */
xldefine_prim("ERRSET", 	F, xerrset  		); /* 136 */
xldefine_prim("BAKTRACE",       S, xbaktrace		); /* 137 */
xldefine_prim("EVALHOOK",	S, xevalhook		); /* 138 */
    
    /* arithmetic functions */
xldefine_prim("TRUNCATE",	S, xfix			); /* 139 */
xldefine_prim("FLOAT",		S, xfloat		); /* 140 */
xldefine_prim("+",		S, xadd			); /* 141 */
xldefine_prim("-",		S, xsub			); /* 142 */
xldefine_prim("*",		S, xmul			); /* 143 */
xldefine_prim("/",		S, xdiv			); /* 144 */
xldefine_prim("1+",		S, xadd1		); /* 145 */
xldefine_prim("1-",		S, xsub1		); /* 146 */
xldefine_prim("REM",		S, xrem			); /* 147 */
xldefine_prim("MIN",		S, xmin			); /* 148 */
xldefine_prim("MAX",		S, xmax			); /* 149 */
xldefine_prim("ABS",		S, xabs			); /* 150 */
xldefine_prim("SIN",		S, xsin			); /* 151 */
xldefine_prim("COS",		S, xcos			); /* 152 */
xldefine_prim("TAN",		S, xtan			); /* 153 */
xldefine_prim("EXPT",		S, xexpt		); /* 154 */
xldefine_prim("EXP",		S, xexp			); /* 155 */
xldefine_prim("SQRT",		S, xsqrt		); /* 156 */
xldefine_prim("RANDOM",		S, xrand		); /* 157 */
    
    /* bitwise logical functions */
xldefine_prim("LOGAND",		S, xlogand  		); /* 158 */
xldefine_prim("LOGIOR",		S, xlogior  		); /* 159 */
xldefine_prim("LOGXOR",		S, xlogxor  		); /* 160 */
xldefine_prim("LOGNOT",		S, xlognot  		); /* 161 */
    
    /* numeric comparison functions */
xldefine_prim("<",		S, xlss			); /* 162 */
xldefine_prim("<=",		S, xleq			); /* 163 */
xldefine_prim("=",		S, xequ			); /* 164 */
xldefine_prim("/=",		S, xneq			); /* 165 */
xldefine_prim(">=",		S, xgeq			); /* 166 */
xldefine_prim(">",		S, xgtr			); /* 167 */
    
    /* string functions */
xldefine_prim("STRCAT",		S, xstrcat  		); /* 168 */
xldefine_prim("SUBSEQ",		S, xsubseq  		); /* 169 */
xldefine_prim("STRING",		S, xstring  		); /* 170 */
xldefine_prim("CHAR",		S, xchar		); /* 171 */
    
    /* I/O functions */
xldefine_prim("READ",		S, xread		); /* 172 */
xldefine_prim("PRINT",		S, xprint		); /* 173 */
xldefine_prim("PRIN1",		S, xprin1		); /* 174 */
xldefine_prim("PRINC",		S, xprinc		); /* 175 */
xldefine_prim("TERPRI",		S, xterpri  		); /* 176 */
xldefine_prim("FLATSIZE",	S, xflatsize		); /* 177 */
xldefine_prim("FLATC",		S, xflatc		); /* 178 */
    
    /* file I/O functions */
xldefine_prim("OPEN",		S, xopen		); /* 179 */
xldefine_prim("FORMAT",		S, xformat  		); /* 180 */
xldefine_prim("CLOSE",		S, xclose		); /* 181 */
xldefine_prim("READ-CHAR",	S, xrdchar  		); /* 182 */
xldefine_prim("PEEK-CHAR",	S, xpkchar  		); /* 183 */
xldefine_prim("WRITE-CHAR",	S, xwrchar  		); /* 184 */
xldefine_prim("READ-LINE",	S, xreadline		); /* 185 */
    
    /* system functions */
xldefine_prim("LOAD",		S, xload		); /* 186 */
xldefine_prim("DRIBBLE",	S, xtranscript		); /* 187 */
    
    /* functions specific to xldmem.c */
xldefine_prim("GC",		S, xgc			); /* 188 */
xldefine_prim("EXPAND",		S, xexpand  		); /* 189 */
xldefine_prim("ALLOC",		S, xalloc		); /* 190 */
xldefine_prim("ROOM",		S, xmem			); /* 191 */
#ifdef SAVERESTORE
xldefine_prim("SAVE",		S, xsave		); /* 192 */
xldefine_prim("RESTORE",	S, xrestore		); /* 193 */
#else
xldefine_prim(NULL,		S, xnotimp		); /* 192 */
xldefine_prim(NULL,		S, xnotimp		); /* 193 */
#endif
    /* end of functions specific to xldmem.c */
    
xldefine_prim("TYPE-OF",	S, xtype		); /* 194 */
xldefine_prim("EXIT",		S, xexit		); /* 195 */
xldefine_prim("PEEK",		S, xpeek		); /* 196 */
xldefine_prim("POKE",		S, xpoke		); /* 197 */
xldefine_prim("ADDRESS-OF",	S, xaddrs		); /* 198 */
    
    /* new functions and special forms */
xldefine_prim("VECTOR",		S, xvector  		); /* 199 */
xldefine_prim("BLOCK",		F, xblock		); /* 200 */
xldefine_prim("RETURN-FROM",	F, xrtnfrom		); /* 201 */
xldefine_prim("TAGBODY",	F, xtagbody		); /* 202 */
xldefine_prim("PSETQ",		F, xpsetq		); /* 203 */
xldefine_prim("FLET",		F, xflet		); /* 204 */
xldefine_prim("LABELS",		F, xlabels  		); /* 205 */
xldefine_prim("MACROLET",	F, xmacrolet		); /* 206 */
xldefine_prim("UNWIND-PROTECT",	F, xunwindprotect	); /* 207 */
xldefine_prim("PPRINT",		S, xpp			); /* 208 */
xldefine_prim("STRING<",	S, xstrlss  		); /* 209 */
xldefine_prim("STRING<=",	S, xstrleq  		); /* 210 */
xldefine_prim("STRING=",	S, xstreql  		); /* 211 */
xldefine_prim("STRING/=",	S, xstrneq  		); /* 212 */
xldefine_prim("STRING>=",	S, xstrgeq  		); /* 213 */
xldefine_prim("STRING>",	S, xstrgtr  		); /* 214 */
xldefine_prim("STRING-LESSP",		S, xstrilss		); /* 215 */
xldefine_prim("STRING-NOT-GREATERP",	S, xstrileq		); /* 216 */
xldefine_prim("STRING-EQUAL",		S, xstrieql		); /* 217 */
xldefine_prim("STRING-NOT-EQUAL",	S, xstrineq		); /* 218 */
xldefine_prim("STRING-NOT-LESSP",	S, xstrigeq		); /* 219 */
xldefine_prim("STRING-GREATERP",	S, xstrigtr		); /* 220 */
xldefine_prim("INTEGERP",		S, xintegerp		); /* 221 */
xldefine_prim("FLOATP",			S, xfloatp  		); /* 222 */
xldefine_prim("STRINGP",		S, xstringp		); /* 223 */
xldefine_prim("ARRAYP",			S, xarrayp  		); /* 224 */
xldefine_prim("STREAMP",		S, xstreamp		); /* 225 */
xldefine_prim("OBJECTP",		S, xobjectp		); /* 226 */
xldefine_prim("STRING-UPCASE",		S, xupcase  		); /* 227 */
xldefine_prim("STRING-DOWNCASE",	S, xdowncase		); /* 228 */
xldefine_prim("NSTRING-UPCASE",		S, xnupcase		); /* 229 */
xldefine_prim("NSTRING-DOWNCASE",	S, xndowncase		); /* 230 */
xldefine_prim("STRING-TRIM",		S, xtrim		); /* 231 */
xldefine_prim("STRING-LEFT-TRIM",	S, xlefttrim		); /* 232 */
xldefine_prim("STRING-RIGHT-TRIM",	S, xrighttrim		); /* 233 */
xldefine_prim("WHEN",			F, xwhen		); /* 234 */
xldefine_prim("UNLESS",			F, xunless  		); /* 235 */
xldefine_prim("LOOP",			F, xloop		); /* 236 */
xldefine_prim("SYMBOL-FUNCTION",	S, xsymfunction		); /* 237 */
xldefine_prim("FBOUNDP",		S, xfboundp		); /* 238 */
xldefine_prim("SEND",			S, xsend		); /* 239 */
xldefine_prim("SEND-SUPER",		S, xsendsuper		); /* 240 */
xldefine_prim("PROGV",			F, xprogv		); /* 241 */
xldefine_prim("CHARACTERP",		S, xcharp		); /* 242 */
xldefine_prim("CHAR-INT",		S, xcharint		); /* 243 */
xldefine_prim("INT-CHAR",		S, xintchar		); /* 244 */
xldefine_prim("READ-BYTE",		S, xrdbyte  		); /* 245 */
xldefine_prim("WRITE-BYTE",		S, xwrbyte  		); /* 246 */
xldefine_prim("MAKE-STRING-INPUT-STREAM", 	S, xmkstrinput		); /* 247 */
xldefine_prim("MAKE-STRING-OUTPUT-STREAM",	S, xmkstroutput		); /* 248 */
xldefine_prim("GET-OUTPUT-STREAM-STRING",	S, xgetstroutput	); /* 249 */
xldefine_prim("GET-OUTPUT-STREAM-LIST",	S, xgetlstoutput	); /* 250 */
xldefine_prim("GCD",			S, xgcd			); /* 251 */
xldefine_prim("GET-LAMBDA-EXPRESSION", 	S, xgetlambda		); /* 252 */
xldefine_prim("MACROEXPAND",		S, xmacroexpand		); /* 253 */
xldefine_prim("MACROEXPAND-1",		S, x1macroexpand	); /* 254 */
xldefine_prim("CHAR<",			S, xchrlss  		); /* 255 */
xldefine_prim("CHAR<=",			S, xchrleq  		); /* 256 */
xldefine_prim("CHAR=",			S, xchreql  		); /* 257 */
xldefine_prim("CHAR/=",			S, xchrneq  		); /* 258 */
xldefine_prim("CHAR>=",			S, xchrgeq  		); /* 259 */
xldefine_prim("CHAR>",			S, xchrgtr  		); /* 260 */
xldefine_prim("CHAR-LESSP",		S, xchrilss		); /* 261 */
xldefine_prim("CHAR-NOT-GREATERP",	S, xchrileq		); /* 262 */
xldefine_prim("CHAR-EQUAL",		S, xchrieql		); /* 263 */
xldefine_prim("CHAR-NOT-EQUAL",		S, xchrineq		); /* 264 */
xldefine_prim("CHAR-NOT-LESSP",		S, xchrigeq		); /* 265 */
xldefine_prim("CHAR-GREATERP",		S, xchrigtr		); /* 266 */
xldefine_prim("UPPER-CASE-P",		S, xuppercasep		); /* 267 */
xldefine_prim("LOWER-CASE-P",		S, xlowercasep		); /* 268 */
xldefine_prim("BOTH-CASE-P",		S, xbothcasep		); /* 269 */
xldefine_prim("DIGIT-CHAR-P",		S, xdigitp		); /* 270 */
xldefine_prim("ALPHANUMERICP",		S, xalphanumericp	); /* 271 */
xldefine_prim("CHAR-UPCASE",		S, xchupcase		); /* 272 */
xldefine_prim("CHAR-DOWNCASE",		S, xchdowncase		); /* 273 */
xldefine_prim("DIGIT-CHAR",		S, xdigitchar		); /* 274 */
xldefine_prim("CHAR-CODE",		S, xcharcode		); /* 275 */
xldefine_prim("CODE-CHAR",		S, xcodechar		); /* 276 */
xldefine_prim("ENDP",			S, xendp		); /* 277 */
xldefine_prim("REMOVE-IF",		S, xremif		); /* 278 */
xldefine_prim("REMOVE-IF-NOT",		S, xremifnot		); /* 279 */
xldefine_prim("DELETE-IF",		S, xdelif		); /* 280 */
xldefine_prim("DELETE-IF-NOT",		S, xdelifnot		); /* 281 */
xldefine_prim("TRACE",			F, xtrace		); /* 282 */
xldefine_prim("UNTRACE",		F, xuntrace		); /* 283 */
xldefine_prim("SORT",			S, xsort		); /* 284 */
xldefine_prim("DEFSTRUCT",		F, xdefstruct		); /* 285 */
xldefine_prim("%STRUCT-TYPE-P",		S, xstrtypep		); /* 286 */
xldefine_prim("%MAKE-STRUCT",		S, xmkstruct		); /* 287 */
xldefine_prim("%COPY-STRUCT",		S, xcpystruct		); /* 288 */
xldefine_prim("%STRUCT-REF",		S, xstrref		); /* 289 */
xldefine_prim("%STRUCT-SET",		S, xstrset		); /* 290 */
xldefine_prim("ASIN",			S, xasin		); /* 291 */
xldefine_prim("ACOS",			S, xacos		); /* 292 */
xldefine_prim("ATAN",			S, xatan		); /* 293 */
    
    /* extra table entries */
xldefine_prim("SYSTEM",			S, Prim_SYSTEM		); /* 294 NPM */
xldefine_prim("POPEN",			S, Prim_POPEN		); /* 295 NPM */
xldefine_prim("PCLOSE",			S, Prim_PCLOSE		); /* 296 NPM */
xldefine_prim("FSCANF-FIXNUM",		S, Prim_FSCANF_FIXNUM	); /* 297 NPM */
xldefine_prim("FSCANF-STRING",		S, Prim_FSCANF_STRING	); /* 298 NPM */
xldefine_prim("FSCANF-FLONUM",		S, Prim_FSCANF_FLONUM	); /* 299 NPM */
xldefine_prim("COPY-ARRAY",		S, Prim_COPY_ARRAY	); /* 300 NPM */
xldefine_prim("ARRAY-INSERT-POS",	S, Prim_ARRAY_INSERT_POS); /* 301 NPM */
xldefine_prim("ARRAY-DELETE-POS",	S, Prim_ARRAY_DELETE_POS); /* 302 NPM */
    
    /* include system dependant function pointers */
#include "osptrs.h"
    
/* Include hybrid-class funtab entries: */  /* JSP a la Voodoo */
#define MODULE_XLFTAB_C_FUNTAB_S
#include "../../xmodules.h"
#undef MODULE_XLFTAB_C_FUNTAB_S

/* Include hybrid-class funtab entries: */  /* JSP a la Voodoo */
#define MODULE_XLFTAB_C_FUNTAB_F
#include "../../xmodules.h"
#undef MODULE_XLFTAB_C_FUNTAB_F

}
