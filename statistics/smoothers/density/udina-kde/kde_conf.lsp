;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File kde_conf.lsp
;;; This file is part of KDE, a set of xlisp-stat functions
;;; for Interactive Kernel Density Estimation.
;;; F. Udina, May 1995. udina@upf.es

;;; this file contains global variables used to configure KDE
;;; and some utility functions.


;;;some testing config functions
(defun featurep (sym)
  (member sym *features*))
(defun system-has-color ()
  (or (featurep 'color)
      (featurep :color)))




;;;some global vars, mostly for configuring and debugging
;;these var appear here with defvar, so they will keep his previous value
;;if they are already created.

(defvar *kde*computing*inspect* nil
  "if it's t, printed reports are generated for time consuming computations")

(defvar *kde-debugging* nil
  "When t, some extra written messages are generated")

(defvar *kde*significant*digits* 3
  "number of digits used when displaying bandwidth and quartiles")

(defvar *kde*binning*histograms*
  '(direct shift)
  "the default type of binning used to build histograms ('direct or 'linear)
and the policy for outsiders ('delete 'shift or 'none, error)")

;;;here macintosh or dos means slow machine.
(defvar *kde*default*xvalues*
  (if (or (featurep :macintosh) (featurep :msdos))  100 300)
"the number of x-values to be taken by default when creating kde objects")

(defvar *kde-wsize*
  (if (featurep :macintosh) '(400 280)  '(650 450))
"the default size of windows to be created")

(defvar *kde*poly* t
"a quick hack to be replaced, t means draw freq poly instead of histograms")

;;;newline is different in different operating systems
(when (featurep :macintosh) (setq newline "\r"))
(when (featurep :unix) (setq newline "\n"))
(when (featurep :msdos) (setq newline "\n"))

;;;some autoload definitions
(defmacro o-autoload (name module)
"this is the old autoload macro, release 2."
  `(if (not (member ,module *modules*))
       (defun ,name (&rest args)
         (fmakunbound ',name)
         (require ,module)
         (apply ',name args))))

(unless (fboundp 'system-has-windows)
	(defun system-has-windows ()
	  (or (member 'windows *features*)
	      (member :windows *features*))))
(unless (fboundp 'make-kde)
	(o-autoload make-kde "runkde")
	(o-autoload make-kde-from-file "runkde"))
(o-autoload integrate "funnorms")
(o-autoload second-derivative "funnorms")
(unless (fboundp 'make-transformation)
	(o-autoload make-transformation "transfor"))

(unless (fboundp 'golden-search)
	(o-autoload golden-search "goldsear"))
