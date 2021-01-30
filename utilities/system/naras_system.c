    Jan> Well, here is a quickie to capture system output. Bit of a
    Jan> hack, but it works. <stuff deleted>

A word of caution.  Two people doing this almost simultaneously can
overwrite the tmp files. One needs to generate unique file
names. Simply appending a $$ to the file name to make it use the
process id to produce a unique file name doesn't work, since each
invocation of the system file spawns a new process. However, there are
kludges that one can use with reasonable success.

****************   System function hack    **********
Instructions.

Grab a copy of your Makefile from xlispstat build.

Add a line

new_system:  system.o
	ld -o libsystem.so -shared system.o

or equivalent.  Look up man page for ld if in doubt.  On Linux, just
the .o file should do.

Also tack on the following to your Makefile variable UCFLAGS.

-I/usr/local/src/xlispstat-3-44 

or equivalent reflecting where the source files reside.

Now type 

% make new_system

Then experiment. For example,

(dyn-load "./libsystem.so")  
(call-lfun "new_system" "ls")

On Linux, use (dynload "./system.o"). 

Here are examples from my machine.

EULER:/usr/people/naras/xlisp/myexamples% !x
xlispstat
XLISP-PLUS version 2.1g
Portions Copyright (c) 1988, by David Betz.
Modified by Thomas Almy and others.
XLISP-STAT 2.1 Release 3.44 (Beta).
Copyright (c) 1989-1994, by Luke Tierney.

> (dyn-load "./libsystem.so")
T
> (call-lfun "new_system" "ls")
((0) ("xlisp.h" "xldmem.h" "system.o" "system.c~" "system.c,v" "system.c" "so_locations" "myxlisp.h" "libsystem.so" "kde2.c" "kde1.c" "kde.c" "example9.c" "example8.c" "example7.c" "example6.c" "example5.c" "example4.c" "example3.c" "example2.c" "example10.c" "example1.c" "README" "Makefile"))

> (call-lfun "new_system" "ls" :columns 30)
((0) ("xlisp.h" "xldmem.h" "system.o" "system.c~" "system.c,v" "system.c" "so_locations" "myxlisp.h" "libsystem.so" "kde2.c" "kde1.c" "kde.c" "example9.c" "example8.c" "example7.c" "example6.c" "example5.c" "example4.c" "example3.c" "example2.c" "example10.c" "example1.c" "README" "Makefile"))

> (call-lfun "new_system" "ls" :columns 3)
((0) ("h" "p." "is" "xl" "" ".h" "em" "dm" "xl" "" ".o" "em" "st" "sy" "~" ".c" "em" "st" "sy" "" ",v" ".c" "em" "st" "sy" "" ".c" "em" "st" "sy" "" "ns" "io" "at" "oc" "_l" "so" "h" "p." "is" "xl" "my" "" "so" "m." "te" "ys" "bs" "li" "" ".c" "e2" "kd" "" ".c" "e1" "kd" "c" "e." "kd" "" ".c" "e9" "pl" "am" "ex" "" ".c" "e8" "pl" "am" "ex" "" ".c" "e7" "pl" "am" "ex" "" ".c" "e6" "pl" "am" "ex" "" ".c" "e5" "pl" "am" "ex" "" ".c" "e4" "pl" "am" "ex" "" ".c" "e3" "pl" "am" "ex" "" ".c" "e2" "pl" "am" "ex" "c" "0." "e1" "pl" "am" "ex" "" ".c" "e1" "pl" "am" "ex" "" "ME" "AD" "RE" "" "le" "fi" "ke" "Ma"))
> 

The last is, of course, junk.  Columns should be larger than the
largest line. Also note the reverse order of the output.

****************** system.c ****************
static char rcsid[] = "@(#)$Header: /usr/people/naras/xlisp/myexamples/system.c,v 1.2 1995/02/17 19:38:54 naras Exp $";

/*  
   An xlisp function that returns the output of a system
   command. Returns a list of two elements: a status, and a list of
   character strings of the output. Status should always be checked
   and the output is returned in reverse order.

   NO warranties, express or implied.  Use at your own risk.
   
   B. Narasimhan                   naras@euler.bd.psu.edu.

*/

#include <xlisp.h>
#include <xldmem.h>

#define MAXCOLS 80

extern LVAL sk_columns; 

LVAL new_system()
{
  LVAL result, str_ptr, col_arg; 
  char *command, buf[MAXCOLS];
  int i, n, status, cols;
  FILE *p;
    
  command = (char *) getstring(xlgastring()); /* The command string */

  if (! xlgetkeyarg(sk_columns, &col_arg)) 
    cols = MAXCOLS;
  else
    cols = getfixnum(col_arg);

  xllastkey();   /* Last key argument */
  
  xlstkcheck(2);
  xlsave(result);
  xlsave(str_ptr);

  if ((p = popen(command, "r")) == NULL)
    xlfail("could not execute command!");
  
  result = NIL;
  while (fgets(buf, cols, p) != NULL) {
    n = strlen(buf);

    if (buf[n-1] == '\n')
      --n;

    str_ptr = newstring(n);
    for (i = 0; i < n; i++)
      setstringch(str_ptr, i, buf[i]);
    result = cons(str_ptr, result);
  }
  
  status = pclose(p);
  
  xlpopn(2);
  
  return(cons(cons(cvfixnum((FIXTYPE) status), NIL), cons(result, NIL)));
}






