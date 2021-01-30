Distribution fun v1.0.1

What it is:

  This simple interactive software developed using the X-Lisp development environment
  is intended to be a didactic tool to help students to get familiar with some common
  distribution functions they learn in basic statistics courses at the University.

How it works:

  The game consists on fitting the histogram representation of a random generated
  data set of a particular distribution function with a line plot which is to be
  fined tuned on the histogram by the user. When the user decides the curve he/she
  sees fits well the data set then he/she can choose to see the real solution,
  that is the actual value of the parameters caracterizing the distribution function 
  (E[x],Var[x] or v1,v2 for example) and the guessed value of the parameters.

Application menus and menu items:

  Menu                  Menu items                  Description
----------------------------------------------------------------------------------------
  Miscellaneous     |   About Distibution fun...  | Shows a dialog with some info
                    |                             | about the software.
                    |   Exit                      | Removes all the menus belonging
                    |                             | to this software from the X-Lisp
                    |                             | environment.
  New distribution  |   Normal                    | These menu items create
                    |   Log-normal                | a new ramdom data set of the
                    |   T                         | corresponding ditribution type
                    |   Chi-sq                    | deleting any windows previously
                    |   F                         | displayed. (no more than one
                    |   Gamma                     | document at any time).
                    |   Beta                      |
                    |                             |
  Show              |   Show                      | Show the solution of the current
                    |                             | simulation. It does nothing if
                    |                             | no simulation is active.

The software's files:

  Source files            Description
----------------------------------------------------------------------------------------                    
  main.lsp          |     main program, THIS IS THE SOURCE FILE TO BE LOADED
                    |     IN THE X-LISP ENVIRONMENT TO START UP THE SOFTWARE.
  dist.lsp          |     abstract distribution class.
  normal.lsp        |     normal distribution class.
  log-n.lsp         |     log normal distribution class.
  t-dist.lsp        |     t distribution class.
  chi-sq.lsp        |     chi-squared distribution class.
  f.lsp             |     abstract f distribution class.
  f-mu-si2.lsp      |     f distribution class whose histogram data is fitted
                    |     fine tuning the E[x] and Var[x] parameters.
  f-v1-v2.lsp       |     f distribution class whose histogram data is fitted
                    |     fine tuning the v1 and v2 parameters.
  g.lsp             |     abstract gamma distribution class.
  g-mu-si2.lsp      |     gamma distribution class whose histogram data is fitted
                    |     fine tuning the E[x] and Var[x] parameters.
  g-v1-v2.lsp       |     gamma distribution class whose histogram data is fitted
                    |     fine tuning the v1 and v2 parameters.
  b.lsp             |     abstract beta distribution class.
  b-mu-si2.lsp      |     beta distribution class whose histogram data is fitted
                    |     fine tuning the E[x] and Var[x] parameters.
  b-v1-v2.lsp       |     beta distribution class whose histogram data is fitted
                    |     fine tuning the v1 and v2 parameters.
  dist-win.lsp      |
  slid-win.lsp      |
  v12-par.lsp       |     miscellaneous stuff, see comments in the respective files.
  v12-sld.lsp       |
  mu-si2.lsp        |

  I'm sorry for the awkward file names but I had to make sure they were suitable
  to use in an environment with the 8:3 limitation on file name length.

Development environment:

  Mainly X-Lisp 3.44 beta on the Mac and sometimes X-lisp 3.45 beta on a PC.

Info:

  This software has been written for the course of computational statistics
  run by professor Andrea Pastore (e-mail pastore@vega.unive.it)
  of the University of Venice, Italy, by Mirko Ravagnan (e-mail mravagna@dsi.unive.it).

Disclaimer:

  This software is released in the public domain.

Warranty:

  Free of charge Software is provided on an "AS IS" basis, without warranty of any kind,
  including without limitation the warranties of merchantability, fitness for a particular
  purpose and non-infringement. The entire risk as to the quality and performance
  of the Software is borne by you. Should the Software prove defective, you and not
  the Developer assume the entire cost of any service and repair.
  
That's all, have fun.
