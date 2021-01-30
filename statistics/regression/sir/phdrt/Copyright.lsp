(def about (format nil "    Copyright (c) 1992, by Ker-Chau Li &
                            Heng-hui Lue~%        (Version 1.0, Jan. 1994)~%For information send e-mail to hlue@math.ucla.edu~%"))
(def wait (format nil "      Please wait while files are loading."))
;(send *listener* :hide-window)
(defun copyright ()
;  (def copyright (send display-window-proto :new
;                       :title "pHdrt: Interactive Regression Tree"
;                       :show nil))
   (def copyright(string 'pHdrt:Interactive-Regression-Tree))
;           (format t "~%====================================~%")
;           (format t "pHdrt: Interactive Regression Tree")
;           (format t "~%====================================~2%")

 ; (send copyright :size 330 70)
 ; (def center (/ (screen-size) 2))
 ; (def extent (/ (send copyright :size) 2))
 ; (def location (- center extent))
 ; (send copyright :location (select location 0) (select location 1))
 ; (send copyright :show-window)
 ; (send copyright :paste-string about)
 ; (send copyright :paste-string wait))
(copyright)

;(send *listener* :title "pHdrt Listener")
;(send *listener* :size 490 138)
;(send *listener* :location 10 321)
;(send copyright :close)
