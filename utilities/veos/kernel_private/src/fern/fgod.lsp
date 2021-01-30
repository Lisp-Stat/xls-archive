;;-----------------------------------------------------------
;; file: fgod.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the FGOD compenent of the Fern System.
;;
;; creation: February 28, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Geoffrey P. Coco,
;; Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;-----------------------------------------------------------
#|

These functions provide users of the Fern System with clean
and well-defined mechanisms for directly affecting other
entities.  They represent the god component of the the Fern
system (or FGOD).  The FGOD primarily administrates a common
protocol for passing proactive instructions between entities.

|#
;;-----------------------------------------------------------
;;		    FGOD PUBLIC FUNCTIONS
;;-----------------------------------------------------------


;; fgod-make-node

;; dynamically create an entity ... somewhere.
;; pass which host where entity will run,
;; the binary executable of the entity,
;; and the lisp program for the entity to execute.
;; all these args are strings; defaults are below.

(defun fgod-make-node (&key (run-host (aref self 0))
			    (binary "entity")
			    (program "/home/veos/lisp/tabula_rasa")
			    (display-host (aref self 0)))
  (progn

    ;; make sure that entity can display locally
    (cond ((equal display-host (aref self 0))
	   (cond ((not (equal run-host (aref self 0)))
		  (system (sprintf "xhost + " run-host))))))

    ;; make unix call to launch remote entity
    (system (fgod-rsh-command run-host binary program display-host))

    ;; now, wait for reply of success
    ;; this is handled remotely by fgod-be-node
    (printf1 "waiting for offspring to respond...")

    ;; this var gets set by new entity via remote proc call to us - 
    ;; as part of it's startup protocol (see fgod-be-node)
    (setq fern-descendent nil)

    (read-time)
    (do ((reply nil) (timer 0))
	((cond 

	  ;; the entity lives !!!
	  (fern-descendent
	   (printf1 "\noffspring was: " (uid2str fern-descendent))
	   (setq reply fern-descendent)
	   (setq fern-descendent nil)
	   t)

	  ;; new entity didn't respond in reasonable amount of time
	  ((> timer fgod-timeout)
	   (printf "\noffspring didn't respond.")
	   t))

	 reply)

	;; give time to persist procs and hope for reply message.
	;; reply is in the form: (setq fern-descendent new-entity-uid)
	(fcon-time)

	(setq timer (+ timer (read-time)))
	)
    ))

;;-----------------------------------------------------------

;; fgod-impart

#|
arguments:
     uid of desired entity and
     *quoted* function call.

execute remote lisp functions.

consider this example of the proper way to use fgod-impart:

    (fgod-implant #("iris2" 5503) `(fe-enter ,self))

this call will cause the remote entity to 'enter' your entity as a
space.  this can be used for smart portals.

we quote the remote function call so that the function is finally
evaluated by the catcher of this message - not by the thrower.

notice that the code we want to send contains a variable (i.e. self)
which we want to evaluate *before* the message is thrown.  we can use
the 'backquote-comma' syntax as shown to do this.

here is another, more complex example:

   (fgod-impart #("iris2" 5503)
		 `(setq remote-var
                        (list ,(+ local-x local-y) (+ remote-x remote-y))))

again, we quote the entire message with backquote.  but we want to
evaluate the expression (+ local-x local-y) *before* throwing, thus
the comma before this expression.

notice that the second argument to setq is a call to (list ...).  this
is also passed on unevaluated to the catching entity.  when this
message is eventually evaluated, it will then create a list of the
already computed (+ local-x local-y) value and the result of the
expression (+ remote-x remote-y).

to restate, the (+ remote-x remote-y) is evaluated by the catcher of
the message.  the (list ...) is so that the remote lisp will not try
to evaluate (<computed-val> (+ local-x local-y)) as a function call.

NOTE: please use this function for remote entity editing, rather than
calling vthrow yourself - in the future, this function will also throw
an ancestral password.  
|#


(defun fgod-impart (uid remote-func-call)
  (vthrow (list uid) remote-func-call))

;;-----------------------------------------------------------

;; same as fgod-impart except that it holds and waits the
;; the result of the remote function call.
;; timeout is in seconds

(defun fgod-seq-impart (uid remote-func-call)
  (progn
    (vthrow (list uid) `(fgod-seq-remote ,self ,remote-func-call))
    
    (setq fgod-seq-reply nil)
    (read-time)
    (do ((reply nil) (timer 0))

	((cond
	  
	  ;; the entity responded !!!
	  (fgod-seq-reply
	   ;; the remote entity will pass back the result inside an extra list.
	   ;; this is so we can distinguish between no reponse and a response of nil.
	   (setq reply (car fgod-seq-reply))
	   (setq fgod-seq-reply nil)
	   t)
	  
	  ;; entity didn't respond in adequate time
	  ((> timer fgod-timeout)
	   t))

	 reply)

	;; give time to persist procs and hope for reply message.
	;; reply is in the form: (setq fgod-seq-reply data)
	(fcon-time)

	(setq timer (+ timer (read-time)))
	)
    ))


;;-----------------------------------------------------------


;;-----------------------------------------------------------
;;		    FGOD PRIVATE FUNCTIONS
;;-----------------------------------------------------------

(defun fgod-init ()
  ;; try to alert creator that we made it
  (fgod-be-node)

  (setq fgod-timeout 15)
  )

;;-----------------------------------------------------------

;; generate command string to pass to unix which does:
;; rsh to host,
;; xterm with display to local screen,
;; and run a chosen entity with a chosen startup program.

(defun fgod-rsh-command (run-host binary program display-host)
  (progn

    (cond (fern-debug
	   (printf "run-host: " run-host)
	   (printf "binary: " binary)
	   (printf "program: " program)
	   (printf "display-host: " display-host)))
    
    (let (xterm-command
	  window-title
	  entity-command)
      (setq
       entity-command (sprintf
		       ;; xlisp binary to execute
		       binary
		       " "
		       ;; the ancestor bits
		       (fgod-ancestor-code)
		       " "
		       ;; the xlisp startup program
		       program
		       )
       window-title (sprintf binary "@" run-host)
       xterm-command (sprintf
		      ;; call xterm remotely
		      "xterm "
		      ;; xterm window coords
		      "-geometry "
		      (fgod-new-wind)
		      " "
		      ;; xwindow tricks
		      "-iconic "
		      ;; xterm window name
		      "-T "
		      window-title
		      " "
		      ;; display on chosen screen
		      (cond ((not (equal run-host display-host))
			     (sprintf
			      "-display "
			      (fgod-host-xwindow display-host)
			      " ")))
		      ;; the entity program
		      "-e "
		      entity-command))
      
      (cond 
       ;; local case is simple, no rsh needed
       ((equal run-host (aref self 0))
	(sprintf
	 ;; the remote command
	 xterm-command
	 " "
	 ;; make this a local background process
	 "&"))
       
       ;; remote case, rsh the entire command
       (t
	(sprintf "rsh "
		 ;; where to rsh
		 run-host 
		 ;; don't pass this terminal's input to it.
		 " -n "
		 ;; the remote command
		 "\"" xterm-command "\" "
		 ;; make this a local background process
		 "&")))
      )))

;;-----------------------------------------------------------

;; generate command string for X-window placement on screen.
;; with repeated calls, this produces geometry for tiled windows.

(defun fgod-new-wind ()
  (progn
    (cond ((boundp 'xwindow-place)
	   (setf (nth 1 xwindow-place) (- (nth 1 xwindow-place) 25))
	   (setf (nth 3 xwindow-place) (- (nth 3 xwindow-place) 25)))
	  (t
	   (setq xwindow-place '("76x15+" 430 "+" 640))))
    (eval `(sprintf ,@xwindow-place))))


;;-----------------------------------------------------------

(defmacro fgod-ancestor-code ()
  (sprintf "/home/veos/lisp/ancestors/" 
	   (aref self 0) "_" (aref self 1) ".lsp "))

(defun fgod-host-xwindow (display-host)
  (sprintf display-host ":0.0"))

;;-----------------------------------------------------------

;; the remote reply handler for fgod-seq-impart
(defun fgod-seq-remote (sender-uid local-func-call)
  ;; note the particular protocol, here.
  ;; we send the reply inside an extra list.
  ;; this is so that the remote caller (fgod-seq-impart) can
  ;; distinguish between no response and a response of nil
  (vthrow (list sender-uid) `(setq fgod-seq-reply '(,local-func-call))))

;;-----------------------------------------------------------

;; remote counterpart to fgod-make-node
(defun fgod-be-node ()
  (cond ((boundp 'fern-ancestor)
	 (printf "throwing to ancestor...")
	 (print (vthrow `(,fern-ancestor) `(setq fern-descendent ,self)))
	 t)))

;;-----------------------------------------------------------


