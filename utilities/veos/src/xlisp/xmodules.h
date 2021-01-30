/* -*-C-*-                                                                   CrT
********************************************************************************
*
* File:         xmodules.h
* Description:  Master #include file for xlisp extension modules.
* Author:       Jeff Prothero
* Created:      90Nov16
* Modified:     
* Language:     C
* Package:      N/A
* Status:       
*
* Copyright (c) 1991, University of Washington (by Jeff Prothero)
*
* Permission to use, copy, modify, distribute, and sell this software
* and its documentation for any purpose is hereby granted without fee,
* provided that the above copyright notice appear in all copies and that
* both that copyright notice and this permission notice appear in
* supporting documentation, and that the name of University of
* Washington and Jeff Prothero not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission.  University of Washington and Jeff Prothero make no
* representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
* 
* UNIVERITY OF WASHINGTON AND JEFF PROTHERO DISCLAIM ALL WARRANTIES WITH
* REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL UNIVERSITY OF WASHINGTON
* NOR JEFF PROTHERO BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
* TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
* Please send modifications, improvements and bugfixes to jsp@milton.u.washington.edu
* Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
*
********************************************************************************
*/

/**************************************************************************/
/* Some xlisp functions are written in C.  We call them "primitive        */
/* functions" because they look atomic to xlisp code.			  */
/* 									  */
/* Some xlisp classes need C language support.  We call them "hybrid	  */
/* classes", since they are written partly in xlisp and partly in C.	  */
/* Sometimes the C part accesses special host facilities like graphics	  */
/* hardware, and sometimes it simply speeds up critical operations.	  */
/* 									  */
/* This file provides a central, single point of connection between the   */
/* xlisp interpreter code and the code for xlisp extension modules --     */
/* hybrid classes and optional libraries of primitive functions.  Rather  */
/* than scattering "#ifdef"s all through the xlisp interpreter, you should*/
/* simply add a single '#include "myclass/c/xmyclass.h"' line to this     */
/* file.  See the file "xcore/doc/mymodule.h" to find out what you should */
/* put in "xmyclass.h".							  */
/**************************************************************************/

/* Order is important! */
/*#include "winterp/c/xwinterp.h"/* Just a skeleton at the moment. */

#ifdef banana
#include   "gobject/c/xgbj.h"    /* General objects.		   */
#include   "3d/c/x3d.h"          /* Some 3-D graphics stuff.       */
#include "gplotlib/c/xgplot.h" /* gplotlib + interface.          */
#endif
