;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is file runkde.lsp
;;; the main entry point to KDE, a Kernel Density Estimation lisp package
;;; read the FILES and README files in this directory
;;; F. Udina, may 95



(require "kde")
(require "distr")

#+macintosh (defvar bigmuch 150)
#-macintosh (defvar bigmuch 500)
#+macintosh (defvar bigless 100)
#-macintosh (defvar bigless 300)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;; main functions of the file
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make-kde builds an instance of kde-proto
; 

; from where it will decide the adequate kde object for the data
; depending on dimensionality...

(defun make-kde (&key title data x-values kernel-type bandwidth 
		      (want-window t)
		      (use-canonical t)
		      (show-previous-estimate nil)
; we made it default because is the fastest in the most likely case
                      (calc-method 'fft)
		      (variable-bandwidth nil)
		      (print nil) (show t) (sorted nil) (debug nil))
"args:(&key title data x-values kernel-type bandwidth (want-window t)
		      (print nil) (show nil) (sorted nil))
 builds a new kde object for kernel density estimation,
 display, and analysis.
 EXAMPLE CALL:
 (setq akde (make-kde :data my-data))"
(let* ((instance (send kde-proto :new 2
		       :show t
		       :sorted sorted
		       :data data :title title 
		       :x-values x-values
		       :kernel-type kernel-type :bandwidth bandwidth
                       :use-canonical use-canonical
		       :variable-bandwidth variable-bandwidth
		       :show-previous-estimate show-previous-estimate
                       :calc-method calc-method
		       :print print
		       :debug debug))
       (win (send instance :slot-value 'window)))
  (when print (print (list "instance: " instance)))
  (when print (print (list "window: " win)))
  instance))

(defun make-kde-from-file (&optional (filename (open-file-dialog)) &rest args)
  (when filename
    (let ((data (progn
		  (when *kde*computing*inspect*
			(princ (format nil "reading data, file ~a ~a"
				       filename newline)))
		  (read-data-columns filename 1))))
      (if data
	  (apply #'make-kde :data (first data) args)
;never reached, when file not found, read-data-columns gives an error
	(error "File ~a not found" filename)))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;some auxiliary functions

(defun active-kdes ()
"Returns a list of the kde object that have a window currently open"
(let ((winlist (copy-list (active-windows))))
  (setq winlist 
	(delete-if-not #'(lambda (w) (member wkde-proto (send w :parents)))
		       winlist))
  (mapcar #'(lambda (w) (send w :to-core))
	  winlist)))

(defun send-to-all-kdes (&rest args)
  (mapcar #'(lambda (k) (apply #'send k args))
	  (active-kdes)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions to build some instances
;;; of kde-objects specialized to some distributions

;;; normal distribution

(defun make-kde-normal (num &rest args)
  (let ((inst nil)
        (allargs (concatenate 'list
                              (list :data (normal-rand num)
                                    :title (concatenate 'string  "Normal: " 
                                                        (num-to-string num)))
                              args)))
    (setf inst (apply #'make-kde allargs))
    (send inst :distr-dens #'normal-dens)
    (send inst :distr-rand #'normal-rand)
    inst))

(defun mkn (&optional (num 50)) 
  (setf nkde (make-kde-normal num :x-values (rseq -4 4 bigless)))
  (print (send nkde :title (strcat "nkde: normal "
					   (num-to-string num))))
  nkde)

(defun mk50 () 
  (setf nkde (make-kde-normal 50 :x-values (rseq -4 4 bigless)))
  (print (send nkde :title "nkde: Normal 50"))
  nkde)

(defun mk500 () 
  (setf nkde (make-kde-normal 500 :x-values (rseq -4 4 bigless)))
  (print (send nkde :title "nkde: Normal 500"))
  nkde)

(defun mk5000 () 
  (setf nkde (make-kde-normal 5000))
  (print (send nkde :title "nkde: Normal 5000"))
  nkde)

;;; fischer f distribution

(defun make-kde-f (num)
  (let ((dd (f-rand num 2 2))
	(inst nil))
    (setf inst (make-kde :data dd :sorted t
			 :title (concatenate 'string  "f-(2 2): " 
					     (num-to-string num))))
    (send inst :distr-dens #'(lambda (x) (f-dens x 2 2)))
    (send inst :distr-rand #'(lambda (x) (f-rand x 2 2)))
    inst))

(defun mkf50 (&optional (num 50))
  (setq fkde (make-kde-f num)))

;;; a normal mixture

(defun make-kde-dinormal (num &rest args)
  (let ((inst nil)
        (allargs (concatenate 'list
                              (list :data (dinormal-rand num)
                                    :title (concatenate 'string  "Dinormal: " 
                                                        (num-to-string num)))
                              args)))
    (setf inst (apply #'make-kde allargs))
    (send inst :distr-dens #'dinormal-dens)
    (send inst :distr-rand #'dinormal-rand)
    inst))

(defun mkdn (&optional (num 50)) 
  (setf dnkde (make-kde-dinormal num :x-values (rseq -5 10 200)))
  (print (send dnkde :title (strcat "dnkde: dinormal "
					   (num-to-string num))))
  dnkde)

(defun mkdn50 () (mkdn 50))
(defun mkdn500 () (mkdn 500))



(defun make-kde-lognormal (num &rest args)
  (let ((inst nil)
        (allargs (concatenate 'list
                              (list :data (lognormal_0_1-rand num)
                                    :title (concatenate 'string  "lognormal: " 
                                                        (num-to-string num)))
                              args)))
    (setf inst (apply #'make-kde allargs))
    (send inst :distr-dens #'lognormal_0_1-dens)
    (send inst :distr-rand #'lognormal_0_1-rand)
    inst))

(defun mkln (&optional (num 50)) 
  (setf lnkde (make-kde-lognormal num :x-values (rseq -2 10 200)))
  (print (send lnkde :title (strcat "lnkde: lognormal " 
					   (num-to-string num))))
  lnkde)

(defun mkln50 () 
  (setf lnkde (make-kde-lognormal 50 :x-values (rseq -2 10 200)))
  (print (send lnkde :title "lnkde: lognormal 50"))
  lnkde)

(defun mkln500 () 
  (setf lnkde (make-kde-lognormal 500 :x-values (rseq -2 10 200)))
  (print (send lnkde :title "lnkde: lognormal 500"))
  lnkde)

(defun mkln5000 ()
  (print "this is too many lognormal random values, ein?")
  (setf lnkde (make-kde-lognormal 5000 :x-values (rseq -2 10 200)))
  (print (send lnkde :title "lnkde: lognormal 5000"))
  lnkde)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cauchy distribution
(defun make-kde-cauchy (num &rest args)
  (let ((inst nil)
        (allargs (concatenate 'list
                              (list :data (cauchy-rand num)
                                    :title (concatenate 'string  "Cauchy: " 
                                                        (num-to-string num)))
                              args)))
    (setf inst (apply #'make-kde allargs))
    (send inst :distr-dens #'cauchy-dens)
    (send inst :distr-rand #'cauchy-rand)
    (send inst :bw-ends nil)

    inst))

(defun mkch (&optional (num 50)) 
  (setf chkde (make-kde-cauchy num :x-values (rseq -50 50 200)
			       :show t))
  (print (send chkde :title (strcat "chkde: Cauchy "
					   (num-to-string num))))
  chkde)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this will allow to install a distribution object
;;; into a kde object. Some day we have to decide if
;;; kde object must use distributions objects as a whole entity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun install-distribution-in-kde (distr kde)
"given a distribution object distr and a kde object ..."
(require "distrobj")
  (send kde :distr-dens (send distr :slot-value 'density-function))
  (send kde :distr-rand (send distr :slot-value 'random-generator)))

#||| example of use:
  (load "distrobj")
  (setq mw8 (make-marron-wand 8))
  (setq mwkde (make-kde :data (send mw8 :generate-random 500)))
  (install-distribution-in-kde mw8 mwkde)
|#

(defun make-kde-from-distribution (adistr &optional (samplesize 100) 
					  &rest args)
  (let ((kdeobj (apply #'make-kde :data 
		       (send adistr :generate-random samplesize)
		       :title (format nil "Kde for ~a, ~a"
				      (send adistr :slot-value 'title)
				      samplesize)
		       args)))
    (install-distribution-in-kde adistr kdeobj)
    kdeobj))

;;;installing normal mixtures as in Marron-Wand 92 paper

(defun make-marron-wand-kde (&rest args)
  (apply #'make-mw-kde args))

(defun make-mw-kde (num &optional (samplesize 100) &rest args)
  (require "distrobj")
  (let ((kde (apply #'make-kde-from-distribution
	      (make-marron-wand num)
	      samplesize
	      :x-values (if (> samplesize 200)
			    (rseq -5 5 bigmuch)
			  (rseq -3 3 bigless))
	     :calc-method (if (> samplesize 200) 'fft 'direct) args)))
    (send kde :to-window :adjust-to-data)
    kde))


