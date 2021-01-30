;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picked up from one of the newsgroups. Don't know who did it. It works
;; in Xlisp without modifications.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-from-string (string
                         &key 
                         (start 0) 
                         (char-bag '(#\Space))
                         (test #'(lambda (ch)
                                   (not (member ch char-bag 
                                                :test 'char=))))
                         (post-process 'identity))
  (let ((pos (position-if test string :start start)))
    (if pos 
      (list-from-string* string :start  pos :char-bag char-bag
                         :test test :post-process post-process)
      nil)))

(defun list-from-string* (string
                          &key 
                          (start 0) 
                          (char-bag '(#\Space))
                          (test #'(lambda (ch)
                                    (not (member ch char-bag :test 'char=))))
                          (post-process 'identity))
  (let* ((pos (position-if-not test string :start start))
         (new-pos (if pos (position-if test string :start pos) nil)))
    (cond
     ((and pos new-pos)
      (cons (funcall post-process (subseq string start pos))
            (list-from-string* string :start new-pos :char-bag char-bag
                               :test test :post-process post-process)))
     (pos (list (funcall post-process (subseq string start pos))))
     
     (t (list (funcall post-process (subseq string start)))))))

#|

(defmethod string->symbol-list ((s string) &optional (package *package*))
  (list-from-string s :post-process
                    #'(lambda (str) 
                        (intern (nstring-upcase str) package))
                    :test 'alphanumericp))

|#

#|


(list-from-string  "chris dan ski elaine nick") --> 
  ("chris" "dan" "ski" "elaine" "nick")

(list-from-string  "chris dan ski elaine nick" 
                   :post-process 'nstring-capitalize) -->
  ("Chris" "Dan" "Ski" "Elaine" "Nick")

(list-from-string "chris! dan! ski! elaine! nick!"
                  :char-bag '(#\Space #\!)) -->
  ("chris" "dan" "ski" "elaine" "nick")

(string->symbol-list "chris dan ski elaine nick")-->
 (CHRIS DAN SKI ELAINE NICK)


|#

