# S to Common Lisp: R.W. Oldford, January 1989.
#		    Martin B. Maechler, June 1990
#                   Steve McKinney, February 1992  XLisp, LispStat 
#                                                  modifications
#
# A collection of functions that allow New-S data structures
# to be written to a file (default "/tmp/S.to.CL") as Common Lisp
# code which, when loaded, will construct an analogous CL data structure
# (typically lists or arrays).
# Note: 
#	Because of the modular nature of these functions, the user
#	is responsible for ensuring that newline characters are sent as
#	appropriate (at least one should be sent at the end of a file).
#	This is accomplished by using
#	cat('\n', file="As appropriate", append=TRUE)
# 
#########################################################################
#
# Steve McKinney October 1991:  Xlisp Lisp-Stat - S routines
# February 1992 Version 1.1 - Faster output from S
#
# Set up a directory called  xlisp.S as a subdirectory of your home 
# directory, i.e.
#
# % cd ~
# % mkdir xlisp.S
#
# Now copy this file into ~/xlisp.S and source it in S, i.e.
#
# % cd xlisp.S
# % mkdir .Data
# % S
# > source("S2cl.S")
#
# This leaves all the S routines in the directory ~/xlisp.S/.Data
# which is attached by these routines so that
# they can be accessed by S running in any subdirectory.
#
# Also make the subdirectory ~/xlisp.S/.Data/.CL.data
# where S can store intermediate results.
#


CL <- function() source("S2cl.S")

CL.clobber.file <- function(filename) {

	# Just gives opportunity to clobber an existing file.
	# Creates a new file containing a single newline character.
	# This can be used to provide a "clean slate"
	# to write on for the other functions in this file
	# (using their argument append=TRUE).

	cat('\n', file=filename, append=FALSE)
}


CL.write.val <- function(S.val, file="/tmp/S.to.CL", append=TRUE,
			 ready.file, ack.ready = TRUE)
{	on.exit(cat("Error in 'CL.write.val' \n")) #--
	if(is.list(S.val)) CL.write.list(S.val, file, append)

	# NB Order of if-elses is VERY IMPORTANT!

	else if (is.ts(S.val)) CL.write.ts(S.val, file, append)
# Modified CL.write.matrix to speed up output for use with XLispStat
# Use CL.write.matrix with common lisp
# Use XL.write.matrix with XLispStat    - SM Feb 1992
#	else if (is.matrix(S.val)) CL.write.matrix(S.val, file, append)
	else if (is.matrix(S.val)) XL.write.matrix(S.val, file, append)
	else if (is.category(S.val)) CL.write.category(S.val, file, append)
	else if (is.array(S.val)) CL.write.array(S.val, file, append)
	else if (is.vector(S.val)) CL.write.vec(S.val, file, append)
	
	if (ack.ready)  unix(paste("touch", ready.file))
	on.exit()
}

CL.write.lisp.cmd <- function(fname, arglist, file="/tmp/S.to.CL", append=TRUE,
			      ready.file = '/tmp/S.ready', ack.ready = TRUE)
{

	cat('(', fname, sep='', file=file, append=append)
	for(x in arglist) {
		cat(' ', file=file, append=append)
		CL.write.val(x, file, append, ack.ready = FALSE)
		cat(' ', file=file, append=append)
	}
	cat(')', file=file, append=append)

	if (ack.ready)  unix(paste("touch", ready.file))
}
	

CL.write.assign <- function(sym.name, value, file="/tmp/S.to.CL", append=TRUE, 
			      ready.file = "/tmp/S.ready", ack.ready = TRUE)
{
#-- use to assign values into COMMON LISP --
	cat("(setf", sym.name, " ", file=file, append=append)
	CL.write.val(x, file, append, ack.ready = FALSE)

	# This function also sends a newline.
	cat(" )\n", file=file, append=append)

	if (ack.ready)  unix(paste("touch", ready.file))
}
	
CL.type <- function(S.object)
{
  switch(storage.mode(S.object),
	integer = "'fixnum",
		#-- Changed the next  2  for Allegro - CL, because of "Error:
		#--   Attempt to store the wrong type of a value in an array."
		# single = "'single-float",
		# double = "'double-float",
	single = "'float",
	double = "'float",
	complex = "'complex",
	character = "'string",
	"'T")
}

CL.write.list <- function(struct, file="/tmp/S.to.CL", append=TRUE)
{
#-- IS recursive, BUT always called by CL.write.val -- ATTENTION with 'ready'
	
	if(is.atomic(struct)) # everything but lists are atomic.
		CL.write.val(struct, file, append, ack.ready = FALSE) 

	else if(mode(struct) == "list") {

		cat("(list ", file=file, append=append)
		n <- names(struct)

		if (is.null(n))  {
			for(i in seq(1:length(struct)))
			CL.write.list(struct[[i]], file, append)
		} else {
			for(i in seq(1:length(struct))) {
				cat("(cons ", file=file, append=append)

				if(n[i] == "")
					cat("nil ", file=file, append=append)
				else
					cat("(quote ", n[i], ") ",
						file=file, append=append,
						sep="")

				CL.write.list(struct[[i]], file, append)
				cat(")", file=file, append=append)
			}
		}

		cat(")", file=file, append=append)

	} else cat("()", file=file, append=append)
}

CL.write.matrix <- function(mat, file="/tmp/S.to.CL", append=TRUE) {

	cat("(make-array ", file=file, append=append)
	
	# Write out dimensions.

	cat("(list ", file=file, append=append)
	for(i in dim(mat)) {
		cat(i, " ",  file=file, append= append)}
	cat(") ", file=file, append=append)

	# get element type.

	cat(":element-type", CL.type(mat), " ",
		file=file, append=append)

	# Now the contents.
# Modified to deal with NA's and to write out quickly when no NA's present.
# Too slow to write each element of each row to the file.  Dump the
# whole row at once if there are no NA's. Steve McKinney Feb 11, 1992

        if(sum(is.na(mat))==0) {

	  cat(":initial-contents ", file=file, append=append)

	  cat("(list ", file=file, append=append)
	  for (i in seq(length(mat[,1]))) {
		cat("(list ", file=file, append=append)
                cat(mat[i,], file=file, append=append)
		cat(") ", file=file, append=append)
          }
        }

        else {

	  cat(":initial-contents ", file=file, append=append)

	  cat("(list ", file=file, append=append)
	  for (i in seq(length(mat[,1]))) {
		cat("(list ", file=file, append=append)
                if(sum(is.na(mat[i,]))==0){
                  cat(mat[i,], file=file, append=append)
                } else {
		  for(j in seq(length(mat[1,]))) {
                    num<-mat[i,j]
                    if (is.na(num)) {
			cat("nil ",  file=file, append=append)
                    } else {
			cat(num, " ",  file=file, append=append)
		    }
                  }
                }
		cat(") ", file=file, append=append)
	}}
	cat(") ", file=file, append=append)

	cat(") ", file=file, append=append)
} 

# Modification to speed up output for use with Xlispstat.
# Always output the matrix so that it has more columns than rows
# since minimizing the number of rows means less output file
# thrashing.  Use the LispStat (transpose  ) function if needed.
# A similar fix can be made in CL.write.matrix using the array
# functions and equivalencing in Common Lisp.


XL.write.matrix <- function(mat, file="/tmp/S.to.CL", append=TRUE) {

# Transpose code:  S has to open and close the output file every time
# cat() is called.  For matrices with more rows than columns, this
# results in very slow output.  In this case, transpose the matrix,
# write it out then have LispStat transpose the matrix again.

        transpose.flag<-FALSE
        if (nrow(mat)>ncol(mat)) {
          transpose.flag<-TRUE
          mat<-t(mat)
          cat("(transpose ", file=file, append=append)
        }

	cat("(make-array ", file=file, append=append)
	
	# Write out dimensions.

	cat("(list ", file=file, append=append)
	for(i in dim(mat)) {
		cat(i, " ",  file=file, append= append)}
	cat(") ", file=file, append=append)

	# get element type.

	cat(":element-type", CL.type(mat), " ",
		file=file, append=append)

	# Now the contents.
# Modified to deal with NA's and to write out quickly when no NA's present.
# Too slow to write each element of each row to the file.  Dump the
# whole row at once if there are no NA's. Steve McKinney Feb 11, 1992

        if(sum(is.na(mat))==0) {

	  cat(":initial-contents ", file=file, append=append)

	  cat("(list ", file=file, append=append)
	  for (i in seq(length(mat[,1]))) {
		cat("(list ", file=file, append=append)
                cat(mat[i,], file=file, append=append)
		cat(") ", file=file, append=append)
          }
        }

        else {

	  cat(":initial-contents ", file=file, append=append)

	  cat("(list ", file=file, append=append)
	  for (i in seq(length(mat[,1]))) {
		cat("(list ", file=file, append=append)
                if(sum(is.na(mat[i,]))==0){
                  cat(mat[i,], file=file, append=append)
                } else {
		  for(j in seq(length(mat[1,]))) {
                    num<-mat[i,j]
                    if (is.na(num)) {
			cat("nil ",  file=file, append=append)
                    } else {
			cat(num, " ",  file=file, append=append)
		    }
                  }
                }
		cat(") ", file=file, append=append)
	}}
	cat(") ", file=file, append=append)

	cat(") ", file=file, append=append)
        if(transpose.flag) cat(") ", file=file, append=append)
} 


CL.write.ts <- function(t.series, file="/tmp/S.to.CL", append=TRUE) {
	
	if(is.matrix(t.series)){
		The.Data <- t.series
		attr(The.Data,"tsp") <- NULL
	} else {
		The.Data <- as.vector(t.series)
	}

	CL.write.list(list(	start=start(t.series),
				end=end(t.series),
				frequency=frequency(t.series),
				data=The.Data),
			file=file, append=append)

}


CL.write.category <- function(categ, file="/tmp/S.to.CL", append=TRUE) {
	
	The.Data <- as.vector(categ)
	CL.write.list(list(	levels=levels(categ),
				data=The.Data),
			file=file, append=append)

}


CL.write.array <- function(S.array, file="/tmp/S.to.CL", append=TRUE) {
	
	# Note that the "array" function in S and the
	# "make-array" function in Common-Lisp do not fill their
	# initial-contents in the same order!
	# S: First index changes most rapidly, then second, ...
	# CL: Last index changes most rapidly, then second-last, ...
	# Hence the need for shuffling the indices of the arrays first.

	# Produce two make-arrays.  The first displaced to the second.
	# Where the second is the result of coercing the array to a vector.

	# Since all elements of an S-vector have THE SAME MODE,
	# we can specialize functions that operate on whole vectors, hence
	# the switch at this level.


	cat("(make-array ", file=file, append=append)

	# Write out dimensions.

	cat("(list ", file=file, append=append)
	for(i in dim(S.array)) {
		cat(i, " ",  file=file, append= append)}
	cat(") ", file=file, append=append)

	# get element type.

	cat(":element-type", CL.type(S.array), " ",
		file=file, append=append)

	# Shuffle indices and then coerce to vector.

	vec <- as.vector(aperm(S.array, length(dim(S.array)):1))

	# Displace and hand-off as a vector.

	cat(":displaced-to ", file=file, append=append)
	CL.write.vec(vec, file, append)

	# Tidy up.

	cat(") ", file=file, append=append)

}




CL.write.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {
	
	# Even singletons will be stored as arrays, following S's philosophy.
	# Also, since all elements of an S-vector have THE SAME MODE,
	# we can specialize functions that operate on whole vectors, hence
	# the switch at this level.
	# The called functions are very nearly identical.
	# The stripped down version is CL.write.plain.vec().
	# The notable exception is the function for lists, which is recursive.
	# The list function will rarely be called.
	# It's included in case someone decides to call CL.write.vec
	# directly.  rwo.

	switch(mode(vec),
		character = CL.write.char.vec(vec, file, append),
		numeric = CL.write.num.vec(vec, file, append),
		complex = CL.write.complex.vec(vec, file, append),
		logical = CL.write.logical.vec(vec, file, append),
		null = CL.write.null.vec(vec, file, append),
		list = CL.write.list(vec, file, append),
		CL.write.plain.vec(vec, file, append))

}


CL.write.plain.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {

	# :element-type will be the default value.

	cat("(make-array", length(vec), ":initial-contents ",
		file=file, append=append)

	cat("(list", file=file, append=append)

#	for(i in seq(1:length(vec))) {
#		cat(vec[[i]], file=file, append=append)
#	}
# Speed up output by dumping whole vector at once - SM
        cat(vec, file=file, append=append)

	cat(")) ", file=file, append=append)
}


CL.write.char.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {

	cat("(make-array",length(vec),
		":element-type \'string",
		":initial-contents", " ", file=file, append=append)

	cat("(list ", file=file, append=append)

# next line: corrected by mbm: to make it work for vectors of length 0 --
	for(i in seq(vec)) {
		cat('"', vec[[i]], '"',' ',
			sep='', file=file, append=append)
	}

	cat(")) ", file=file, append=append)
}


CL.write.num.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {

	cat("(make-array ",length(vec),
		":element-type", CL.type(vec),
		":initial-contents ","(list ",
		file=file, append=append)

# Check if any NA's present.  If not, dump whole vector - SM

        if(sum(is.na(vec))==0) {
          cat(vec,  file=file, append=append)
        } else {
	  for(i in seq(1:length(vec))) {
		num <- vec[[i]]
		if (is.na(num)) {
			cat("nil ",  file=file, append=append)
		} else {
			cat(num, " ",  file=file, append=append)
		}
	}}

	cat(")) ", file=file, append=append)
}


CL.write.complex.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {

	cat("(make-array",length(vec),
		":element-type \"complex",
		":initial-contents ", file=file, append=append)
	cat("(list ", file=file, append=append)

	for(i in seq(1:length(vec))) {
		num <- vec[[i]]
		cat("(complex", Re(num),Im(num), ") ",
			file=file, append=append)
	}

	cat(")) ", file=file, append=append)
}


CL.write.logical.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {

	cat("(make-array",length(vec),
		":initial-contents ", file=file, append=append)
	cat("(list ", file=file, append=append)

	for(i in seq(1:length(vec))) {
		if(vec[[i]]) cat("T"," ", file=file, append=append)
		else cat("nil", " ", file=file, append=append)
			
	}

	cat(")) ", file=file, append=append)
}


CL.write.null.vec <- function(vec, file="/tmp/S.to.CL", append=TRUE) {

	# It's not clear to me what should be done here. rwo

	cat("(make-array", 0, ":adjustable T",") ",
		file=file, append=append)

}


#---------- The following are changes / improvements  by  Martin Maechler ---

# source(paste(unix("echo $HOME"), "/xlisp.S/bg.S", sep=""))
# SM Oct 1991 - bg.S included at bottom of this file

CL <- function()  
	source(paste(unix("echo 'echo ~kilroy' | csh -f"), #- unix("echo $HOME"), 
			"/xlisp.S/S2cl.S", sep = ""))

CL.write.Assign <- function(symbol.name, file = "/tmp/S.to.CL", append = TRUE,
			      ready.file = "/tmp/S.ready", ack.ready = TRUE)
{#--- example: xmat <- outer(1:5, 2:10);  CL.write.Assign(xmat) #---
	symbol.string <- paste(substitute(symbol.name))
	cat("(setf", symbol.string, " ", file = file, append = append)
	CL.write.val ( eval(expression(symbol.name)), file, append,
		       ack.ready = F)
	# This function also sends a newline.
	cat(" )\n", file = file, append = append)

	if (ack.ready)  unix(paste("touch", ready.file))
}

cat("S to Common Lisp 'loaded', i.e., sourced !\n\n")

##############################################################
# bg.S file with start.bg - Martin Maechler function
##############################################################

#--- Have a "background 'S'" running which gets input
#--- from other processes via unix files

#	>> IS CPU-inefficient : <<<<

if (! exists(as.name("is.file"))) {
	is.file <<- function(file)
		unix(paste("test -f", file), output = F) == 0
}

start.bg <- function(sleep.seconds = 1, stop.file = "/tmp/S.bg-stop", 
		new.file   = "/tmp/S.bg-new",
		input.file = "/tmp/S.bg-input",
		debug.output = FALSE )
{
#-- new.file & stop.file  are only 'logical switches':
#-- they are 'activated' by simply (e.g.) "touch /tmp/S.bg-stop"
   
   sleep.cmd <- paste("sleep", sleep.seconds <- as.integer(abs(sleep.seconds)))
   if (0 == sleep.seconds) 
	cat("\n#### ATTENTION: 'S' does 'sleep 0' which is VERY time-consuming !\n")
   while(! is.file(stop.file) ) {
	on.exit(cat(
	  "\n>> S-parser found error and wanted to exit infinite loop !!\n"))
	unix(sleep.cmd)
	if ( is.file(new.file) ) {
	   expr <- parse(file = input.file)	  
	   unix(paste("rm", new.file))
	   if (debug.output) {
		cat(date(),": Have parsed",length(expr),"expressions\n:")
                print(expr)
           }
	   res <- eval(expr, local = sys.parent(1))
	}
  }
  if (debug.output) cat("Stop 'background S': stop-file exists: \n",
			 unix(paste("ls -l",stop.file)),"\n")
  unix(paste("rm",stop.file))
  on.exit()
  res
}

#--
#-- this should be the last statement in the file such that CL() "looks nice" --
#--
README <- c(
  "The top-level functions in 'CL-to-S'  are:"                                
, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
, "CL.write.assign           have an assignment 'A <- .....'"                 
, "CL.write.lisp.cmd         call a LISP function with arguments from S "
, "CL.write.val              is called by the above AND the S<->CL interface"
, " "
, "CL                        start the whole thing, shouldn't be necessary"
, "CL.clobber.file           clear / open a new file to write on "
, " - - all other CL.write.. are called by the above !! - - -"               
)

