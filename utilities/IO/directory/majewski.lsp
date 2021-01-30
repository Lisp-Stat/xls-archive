


Now that I've managed to compile xlispstat-3.47 successfully,
I've figured out enough of the internals to provide one of my own
previous requests: A 'directory' function to return a list of
a directory's contents. ( Note: I've still been searching for
examples or documentation on how to write a dynamically loadable
code extension for xlisp on Mac. There's little explaination
in the xlsx.h header file except for a reference to "the New S
book". ) 
  This has only been tested on Mac ( using CW8 ), but the 
code ought to be portable to any system with dirent.h routines.
  See comments in the code for outstanding questions about Common
Lisp compliance.  I have paper copy of CLtL1 nearby, but I may
have to wait until Im back iin the office to check what CLtL2
says. Comments on my comments appreciated!


 You need to add "DIRECTORY" to the function table. 
 On my first attempt ( with the simple #'xyzzy ), I added some
extra entries to the table and got some strange behaviour.
( And no - I didn't drop the end-of-table marker. ) 
When I instead just modified some of the extra not-implemented
entries, it worked fine. Is the size of this table hardcoded 
somewhere ? Or is there anything else that would explain this. 

---|  Steven D. Majewski   (804-982-0831)  <sdm7g@Virginia.EDU>  |---
---|  Computer Systems Engineer          University of Virginia  |---
---|  Department of Molecular Physiology and Biological Physics  |---
---|  Box 449 Health Science Center    Charlottesville,VA 22908  |---
 [ "The grass is always greener, except at t=0" - Stan Kelly-Bootle ]

#include "xlisp.h"
#include "xlstat.h"
#include "dirent.h" 

/* additional xlisp functions
 *
 * Steven D. Majewski / University of Virginia - Physiology / <sdm7g@Virginia.edu>
 *
 */ 

/* just an initial attempt to add a trivial function */
LVAL xlxyzzy(void)
{
  char *buf="(You are in a ((maze) of (((twisty) (little)) parentheses)), all the same!)"  ;
  xllastarg();
  return cvstring(buf);
}


/*
 * minimal common-lisp directory function
 * CLtL1 says:
 *  ( directory pathname &key )
 * But keys are not defined or required, they are allowable extensions.
 * ( I need to check CLtL2 to see if this still holds. ) 
 *
 * Defined in CLtL1 to return a list of pathnames. I'm returning a list of filename
 * strings. xlisp 'pathname' is implemented as a .lsp closure that just coerces a
 * stream into it's truename, else returns a string. 
 *
 * Returns NIL if not passwed a valid directory name ( or, more precisely, if opendir 
 * fails for any reason ): Should it signal an error ? 
 * 
 * I also need to check CLtL2 for other file/directory management routines. 
 * For example: is there any portable equivalent of fstat, particularly   
 *  to check is-a-directory ?  Since '( directory regular-file )' returns NIL
 *  it can be used as a boolean, but it isn't really is-a-directory, since empty
 *  directories will also return NIL -- it's functionally a Not-an-end-node test.
 *
 * [ Note: the above use is the reason I'm not raising an error for that case. ]
 *
 */

LVAL directory( char *dirname )
{
  DIR *dirp;
  struct dirent *entryp;
  LVAL result;

  xlsave1(result); 	/* protect 'result' on stack */
  result = NIL;		/* initial list, or final return value if not a valid directory */
  dirp = opendir( dirname );  /* failure here will return NL */
  if ( dirp ) {		/* build a list of file names */
  	while ( entryp = readdir( dirp )) {
  		result = cons( cvstring(entryp->d_name), result  );
  	}
  	closedir( dirp );
  }
  xlpop();		/* restore stack */
  return result;
}

/*
 * This is split up into an internal and external function because that's what 
 * Jeff Prothero's xlisp internals guide suggested ( and my initial attempt 
 * left out the xlpop() and messed up the stack, so I retried it following
 * his model more exactly. ) I don't know that there's really any need for an
 * internal directory function, and it's simple to fold this back into one. 
 */

LVAL xldirectory(void)
{
  char *dirname;
  dirname = getstring(xlgetfname());
  xllastarg();
  return directory( dirname );

} 

