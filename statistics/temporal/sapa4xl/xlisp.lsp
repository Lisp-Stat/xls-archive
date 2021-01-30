(in-package :xlisp)

(shadow 'sequencep)
(export 'sequencep)

(defun sequencep (p)
  (or (listp p) (vectorp p)))

