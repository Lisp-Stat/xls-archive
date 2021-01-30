;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This adds useful functions to the *fake-menu-bar* Menu


(defvar *fake-menu-bar-menu*
  (send menu-proto :new "Menu-bar-menu"))

(send *fake-menu-bar-menu* :append-items
      (send menu-item-proto :new "Exit"
	    :action #'exit))
(send *fake-menu-bar* :menu *fake-menu-bar-menu*)

(new-provide :el-menubar)
