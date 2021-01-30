(load "/u/laplace/h2/sgrad/jhilden/terrace/ter2")
(load "nels")

(defun sample-schools (n) 
  (let* ((r (array-dimension schl-data 0))
         (row-ind (sample (iseq r) n))
         (col-ind (cons 0 
                   (1+ (car (choose-subset-dialog 
                             "School Variables" 
                              (cdr schl-labels))))))
         (stud-ind (cons 0 
                   (1+ (car (choose-subset-dialog 
                             "Student Variables, last var is dependent" 
                              (cdr stud-labels)))))))
    (make-terrace
      (select schl-data row-ind col-ind)
      (select stud-data (iseq (array-dimension stud-data 0)) stud-ind)
      :x-labels (select stud-labels 
                  (select stud-ind (iseq (- (length stud-ind) 1))))
      :z-labels (select schl-labels col-ind))))
           

    

;;(setf ter1 (make-terrace school-data student-data
             ;:x-labels x-labels
             ;:z-labels z-labels))





