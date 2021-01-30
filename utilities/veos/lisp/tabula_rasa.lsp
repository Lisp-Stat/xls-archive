;;-----------------------------------------------------------
;; file: tabula_rasa.lsp
;;
;; Start up an autonomous entity with UM-Engine
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

;; The Universal Motivator Engine
(setq load-path "/home/colin/")
(load "um-engine")



;;-----------------------------------------------------------
;;		       Define Behavior
;;-----------------------------------------------------------

(setq sane t)

(defun do-sane ()
  (cond (sane (system "sleep 2"))))



;;-----------------------------------------------------------
;;		       Instill Behavior
;;-----------------------------------------------------------

(fcon-add-persist-proc "dot" '(printf "persist.."))
(fcon-add-persist-proc "sanity" '(do-sane))
(fcon-add-react-proc "dump" '(pprint (fe-copy.ext.sibs)))



;;-----------------------------------------------------------
;;			    Begin
;;-----------------------------------------------------------

(fcon-go)
