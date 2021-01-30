#############################################################################
#
# xlispstat.tcl, version 0.1
#
# This is a set of TCL proc's that allow the shareware Macintosh text editor 
# Alpha to act as a front end for XLISP-STAT for Macintosh version 3.37 or
# higher.
#
#############################################################################
#
# Features:
#
#   Syntax coloring of keywords
#   Shell like command window for typing in XLISP-STAT commands
#   "Save and Go" for executing .lsp files
#   Execute line or selection sends command(s) to XLISP-STAT
#   Edit in XLISP-STAT command
#  
#
#############################################################################
#  
# Installation instructions:
#
# Drop this file in the UserCode folder (Alpha D:Tcl:UserCode).
# Add the following line (uncommented) to your prefs.tcl file.
#
# source $HOME:Tcl:UserCode:xlispstat.tcl
#
# That's it!  Now all .lsp files will automatically go into XLISP-STAT mode
#
#############################################################################
# 
# Usage:
#
#  Pretty simple, the command window acts like a bad mating between the
#  XLISP-STAT command window and the TCL shell in Alpha.  Type something at a 
#  line with a prompt and hit return and the line will be executed in XlispStat.
#  In a .lsp file you can save and go, execute a line or a selection.  Multi-
#  line selections should work fine.
# 
#############################################################################
#
# Customization:
#
#  Here are some things you can change in this file to make the XLISP-STAT mode 
#  more enjoyable.  It does require a small amount of TCL knowledge, but not 
#  much.
#
#   1. add or remove keywords
#   2. change the name of menu commands
#   3. replace the  menu with the XLISP-STAT ics# (requires using resedit)
#   4. change any of the ModeVar's to your taste
#
# 
#############################################################################
#  
# Version History:
#
# 0.1 
#
#
#############################################################################
#
#  This mode was written by Jan de Leeuw.  Almost everything was stolen
#  from mathlab.tcl by Stephen Merkowitz (merkowitz@phgrav.phys.lsu.edu).
#
#############################################################################
#
# Define the X1St mode for .lsp files
#

lappend modes X1St
set dummyProc(X1St)  dummyX1St
lappend allModeMenus  X1StMenu
set modeMenus(X1St)  { X1StMenu }
lappend modeSuffixes	 {*.lsp}   { set winMode X1St }
lappend modeSuffixes	 {*.LSP}   { set winMode X1St }

newModeVar X1St elecRBrace        {0} 1
newModeVar X1St prefixString      {% } 0
newModeVar X1St electricSemi      {0} 1
newModeVar X1St wordBreak         {[a-zA-Z0-9_]+} 0
newModeVar X1St elecLBrace        {0} 1
newModeVar X1St wordWrap          {0} 1
newModeVar X1St wordBreakPreface  {[^a-zA-Z0-9_]} 0
newModeVar X1St tabSize           {3} 0
newModeVar X1St funcExpr          {^[ \t]*(function|FUNCTION).*\(.*$} 0

#
# Define keywords
#

#
# First ... functions (including Common Lisp ones) are blue
#

set xlsFunctionWords { 
abs acons acos acosh adjoin adjustable-array-p
adjust-array alpha-char-p alphanumericp and append
apply applyhook apropos aref array array-dimension
array-dimension-limit array-dimensions array-element-type
array-has-fill-pointer-p array-in-bounds-p array-p
array-rank array-rank-limit array-row-major-index
array-total-size ash asin asinh assoc assoc-if
assoc-if-not atan atanh atom boundp break delete delete-duplicates
delete-if eq eql equal equalp error eval every exp export expt
fboundp find find-all-symbols find-if find-if-not find-package
find-symbol first format funcall gcd gensym gentemp get lambda
length read-byte read-char read-from-string read-line
+ * - / /= 1+ 1- < <= = > >=
}

#
# Macros are magenta
#

set xlsMacroWords { 
assert decf defconstant define-modify-macro
define-setf-method defmacro defparameter defsetf defstruct deftype defun
defvar do-symbols etypecase 
}

#
# Special Forms are red
#

set xlsFormWords { block catch declare flet function let }

#
# Type specifiers are cyan
#

set xlsTypeWords { fixnum }

#
# And Statistics are green
#

set xlsStatWords { kernel-dens mean median normal-cdf normal-dens normal-quant
normal-rand quantile standard-deviation}

regModeKeywords -e {;} -c red -k blue -m {:} -m {&} X1St $xlsFunctionWords
regModeKeywords -a -k magenta X1St $xlsMacroWords
regModeKeywords -a -k red X1St $xlsFormWords
regModeKeywords -a -k cyan X1St $xlsTypeWords
regModeKeywords -a -k green X1St $xlsStatWords

proc dummyX1St  {} {}

#############################################################################
#
# Define the X1Sc mode for the command line
#

lappend modes X1Sc
set dummyProc(X1Sc)  			dummyX1Sc
set modeMenus(X1Sc)				{ X1StMenu }

lappend modeSuffixes			{*Xlispstat \*} {set winMode X1Sc}

newModeVar X1Sc wordBreak 				{[a-zA-Z0-9_]+} 0
newModeVar X1Sc wordWrap					{0} 1
newModeVar X1Sc wordBreakPreface {[^a-zA-Z0-9_]} 0

#
# Use same keywords as X1St
#

regModeKeywords -e {;} -c red -k blue -m {:} -m {&} X1Sc $xlsFunctionWords
regModeKeywords -a -k magenta X1Sc $xlsMacroWords
regModeKeywords -a -k red X1Sc $xlsFormWords
regModeKeywords -a -k cyan X1Sc $xlsTypeWords
regModeKeywords -a -k green X1Sc $xlsStatWords

unset xlsFunctionWords
unset xlsMacroWords
unset xlsFormWords
unset xlsTypeWords
unset xlsStatWords

proc dummyX1Sc  {} {}	

#############################################################################
#
#  Bind some keys
#

bind '2'  <z>  xlispstatDoSelection     "X1St"
bind 'm'  <c>  xlispstatSaveAndExecute  "X1St"
bind '4'  <z>  editInXlispstat          "X1St"
bind '\r' <z>  xlispstatDoLine          "X1St"

bind '\r'  xlispstatCarriageReturn  "X1Sc"


#############################################################################
#
#  XLispstat Menu stuff
#


set X1StMenu  "%135"

menu -n $X1StMenu -p xlsMenuItem {
	commandWindow
	"(-)"  
	editInXlispstat
	saveAndExecute
	"(-)"  
	doSelection
	doLine
	doSelectionOrLine
}


proc xlsMenuItem {menu item} {
	global X1StMenu
	switch $menu $X1StMenu {
		switch $item {
			commandWindow      xlispstatWindow
			editInXlispstat    editInXlispstat
			saveAndExecute     xlispstatSaveAndExecute
			doSelection        xlispstatDoSelection
			doLine             xlispstatDoLine
			doSelectionOrLine  xlispstatDoSelectionOrLine
		}
	}
}


#############################################################################
#
#  Create and/or go to command window
#

proc xlispstatWindow {} {
	global winModes

	set wins [winNames]
	if {[set winThere [lsearch $wins "*Xlispstat Commands*"]] >= 0} {
		set name [lindex $wins $winThere]
		bringToFront $name
		goto [maxPos]
	} else {
		new -n {*Xlispstat Commands*}
		changeMode [set winModes([lindex [winNames] 0]) X1Sc]
		insertText "XLISP-PLUS version 2.1g\r"
		insertText "Portions Copyright (c) 1988, by David Betz.\r"
		insertText "Modified by Thomas Almy and others.\r"
		insertText "XLISP-STAT 2.1 Release 3.47 (Beta).\r"
		insertText "Copyright (c) 1989-1994, by Luke Tierney.\r"
		insertText "Alpha Shell by Jan de Leeuw, 1995.\r\r"
		insertText "Initialization may take a moment.\r"

	}

	insertText -w [lindex [winNames] 0] [xlispstatPrompt]
}


#############################################################################
#
#  Carriage return for command window
#

proc xlispstatCarriageReturn {} {

	set pos [getPos]

	set ind [string first ">" [getText [lineStart $pos] $pos]]
	if {$ind < 0} {
		carriageReturn
		return
	}
	set lStart [expr [lineStart $pos]+$ind+2]
	endOfLine
	set scriptName [getText $lStart [getPos]]
	set fileName [lindex [winNames] 0]
	if {[getPos] != [maxPos]} {
		goto [maxPos]
		insertText -w $fileName $scriptName
	}
	set res [dosc -c {'X1St'} -s $scriptName]
	
	insertText \n $res [xlispstatPrompt]
	
}


#############################################################################
#
#  Prompt, for command window
#

proc xlispstatPrompt {} {
	return "\r> "
}


#############################################################################
#
#  Edit current window in XLISP-STAT
#

proc editInXlispstat {} {
	global xlispstatPath

	catch {checkRunning XLISP-STAT X1St xlispstatPath} xlsName

	if {[winDirty]} {
		if {[askyesno "Save '[lindex [winNames] 0]'?"] == "yes"} {
			save
		} else {
			return
		}
	}
	
	if {[catch {sendOpenEvent -n $xlsName [lindex [winNames -f] 0]}] } {
		beep 
	} else {
		switchTo $xlsName
		fileMenuProc File close
	}
}


#############################################################################
#
#  Send selection to XLISP-STAT
#

proc xlispstatDoSelection {} {
	global xlispstatPath

	catch {checkRunning XLISP-STAT X1St xlispstatPath}

	set scriptName [getSelect]
	
	set res [dosc -c {'X1St'} -s $scriptName]
	
	xlispstatResults $scriptName $res
}


#############################################################################
#
#  Save current window and execute it in XLISP-STAT, just like XLISP-STAT command ;)
#

proc xlispstatSaveAndExecute {} {
	global xlispstatPath

	catch {checkRunning XLISP-STAT X1St xlispstatPath}

	if {[winDirty]} {save}

# Get the name of the current window

	set lspFile [lindex [winNames -f] 0]

	set res [dosc -c {'X1St'} -s "(load \"$lspFile\")"]

	xlispstatResults $scriptName $res
}




#############################################################################
#
#  Write results to command window
#

proc xlispstatResults {scriptName res} {

	xlispstatWindow
	insertText  $scriptName
	insertText \n $res [xlispstatPrompt]

}


#############################################################################
#
#  Send line to XLISP-STAT
#

proc xlispstatDoLine {} {
	global xlispstatPath

	catch {checkRunning XLISP-STAT X1St xlispstatPath}

	beginningOfLine
	set bol [getPos]
	endOfLine
	set eol [getPos]

	set scriptName [getText $bol $eol]
	carriageReturn
	
	set res [dosc -c {'X1St'} -s $scriptName]
	
	xlispstatResults $scriptName $res
}


#############################################################################
#
#  Send line or selection to XLISP-STAT
#

proc xlispstatDoSelectionOrLine {} {
	global xlispstatPath

	catch {checkRunning XLISP-STAT X1St xlispstatPath}

	if {[getPos] == [selEnd]} {
		beginningOfLine
		set bol [getPos]
		endOfLine
		set eol [getPos]
	
		set scriptName [getText $bol $eol]
		carriageReturn
	} else {
		set scriptName [getSelect]
	}
	
	set res [dosc -c {'X1St'} -s $scriptName]
	
	xlispstatResults $scriptName $res
}
