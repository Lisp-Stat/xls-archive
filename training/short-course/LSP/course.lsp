(expand 5)
(load-help)
(load-example "addhandrotate")
(load "plotcontrols")

;*** :verbose nil on load
;*** don't load if data set is already right
;;;;
;;;; Data Set Objects
;;;;

(defvar *current-course-data* nil)

(defproto course-data-proto '(filename plots))

(defmeth course-data-proto :activate ()
  (if *current-course-data* (send *current-course-data* :deactivate))
  (load (slot-value 'filename))
  (setf *current-course-data* self))

(defmeth course-data-proto :deactivate ()
  (when (eq self *current-course-data*)
        (undef (variables))
        (dolist (p (slot-value 'plots)) (send p :close))))

(defmeth course-data-proto :isnew (fn)
  (setf (slot-value 'filename) fn))

(defun new-course-data (fn)
  (let ((d (send course-data-proto :new fn)))
    (send d :activate)))

;;;;
;;;; Script Management Functions
;;;;

(defvar *current-script* nil)

(defun paste-string (str)
  (send *listener* :paste-string (format nil "~a~%" str)))

(defun next-talk-line ()
  (let ((line (read-line *current-script*)))
    (if (stringp line)
        (paste-string line))))

(defun next-talk-expr ()
  (let ((expr (read *current-script* nil nil)))
    (if expr
        (let ((s (make-string-output-stream)))
          (progv '(*print-case*) '(:downcase) (pprint expr s))
          (send *listener* :paste-stream s)))))

(defun reset-script () (file-position *current-script* 0))

(defproto script-menu-item-proto () () menu-item-proto)

(defmeth script-menu-item-proto :update ()
  (send self :enabled (if *current-script* t nil)))

;;;;
;;;; Section Functions
;;;;

(defvar *course-section-menu* nil)

(if *course-section-menu* (send *course-section-menu* :remove))

(defun new-section (title &optional script)
  (undef (variables))
  (dolist (w (active-graph-windows)) (send w :close))
  (if *course-section-menu* (send *course-section-menu* :remove))
  (if *current-script* (close *current-script*))
  (setf *course-section-menu* (send menu-proto :new title))
  (send *course-section-menu* :install)
  (when script
        (setf *current-script* (open script))
        (send *course-section-menu* :append-items
              (send script-menu-item-proto :new "Next Line"
                    :key #\G
                    :action 'next-talk-expr)
              (send script-menu-item-proto :new "Reset Script"
                    :action 'reset-script)
              (send dash-item-proto :new))))

(defmacro define-section-item (title &rest body)
  `(let ((menu-item (send menu-item-proto :new ,title
                          :action #'(lambda () ,@body))))
     (send *course-section-menu* :append-items menu-item)))

(defproto section-menu-item-proto '(file-name) () menu-item-proto)

(defmeth section-menu-item-proto :isnew (title fname)
  (call-next-method title)
  (setf (slot-value 'file-name) fname))

(defmeth section-menu-item-proto :do-action ()
  (load (slot-value 'file-name)))

;;;;
;;;; Course Menu
;;;;

(defvar *course-menu* nil)

(if *course-menu* (send *course-menu* :remove))

(setf *course-menu* (send menu-proto :new "ENAR"))

(send *course-menu* :append-items
      (send section-menu-item-proto :new "Overview" "overview.lsp")
      (send section-menu-item-proto :new "Tutorial" "tutorial.lsp")
      (send section-menu-item-proto :new "Lisp" "lisp.lsp")
      (send section-menu-item-proto :new "Objects" "objects.lsp")
      (send section-menu-item-proto :new "Graphics" "graphbasics.lsp")
      (send section-menu-item-proto :new "Dynamic" "dyngraphs.lsp"))

(send *course-menu* :install)
