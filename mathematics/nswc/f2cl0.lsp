; f2cl0.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Confidential Trade Secret and Copyright (c) University of Waikato;;;;;
;;;;;;;;;;Hamilton, New Zealand 92-94 - all rights reserved;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or sparc clisp)(defvar $f2cl_dir "/u/galton/m3/lisp/common/f2cl/clisp/")
#+vms (defvar $f2cl_dir "d:[sen.f2cl.vms]")
#+aclpc (defvar *ext* ".fsl")
#+clisp (defvar *ext* ".fas")
#+(or sparc vms) (defvar *ext* ".l")

(defun load-f2cl (x) 
 (load (concatenate 'string $f2cl_dir x *ext*) :print nil :verbose nil))

(load-f2cl "f2cl1")
(load-f2cl "f2cl2")
(load-f2cl "f2cl3")
(load-f2cl "f2cl4")
(load-f2cl "f2cl5")
(load-f2cl "f2cl6")
(load-f2cl "f2cl7")

(format t "~&The f2cl software has been loaded.~%")
;-------------------------------------------------------------------------
; end of f2cl0.l

