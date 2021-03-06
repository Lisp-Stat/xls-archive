;;-----------------------------------------------------------
;; file: interactive.lsp
;;
;; Start up an autonomous entity
;;
;; creation: March 11, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;-----------------------------------------------------------
;;		    Module Initialization
;;-----------------------------------------------------------


;; FERN

(load "/home/veos/lisp/fern")



;;-----------------------------------------------------------
;;		       Define Behavior
;;-----------------------------------------------------------

(defun get-command ()
  (progn
    (printf1 "\n\nPersist =>> ")
    (pprint (eval (read)))
    (printf "\n\n")
    ))


;;-----------------------------------------------------------
;;		       Instill Behavior
;;-----------------------------------------------------------

(fcon-add-persist-proc "get-command" '(get-command))


;;-----------------------------------------------------------
;;			    Begin
;;-----------------------------------------------------------

(fcon-go)

