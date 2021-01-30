;;; -*- Mode:LISP; Base:10; Syntax:Common-Lisp; -*-

;************************************************************
;                                                           *
;  William M. Spears					    *
;  Navy Center for Applied Research in AI                   *
;  Naval Research Laboratory                                *
;                                                           *
;  This software is the property of the Department of the   *
;  Navy. Permission is hereby granted to copy all or any    *
;  part of this program for free distribution, however      *
;  this header is required on all copies.		    *
;                                                           *
;  File: myeval.lisp					    *
;************************************************************

(in-package user)
(proclaim '(special *c*))
(defun myeval
       (ind)
       (let* ((ea (aref *c* ind 10))
              (de (aref *c* ind 9))
              (ce (aref *c* ind 8))
              (cd (aref *c* ind 7))
              (be (aref *c* ind 6))
              (bd (aref *c* ind 5))
              (bc (aref *c* ind 4))
              (ad (aref *c* ind 3))
              (ac (aref *c* ind 2))
              (ab (aref *c* ind 1))
              (t00004
               (expt (/ (+ ea
                             de
                             (max (expt (/ (+ ce (- 1.0 cd)) 2.0) 2)
                                  (expt (/ (+ (- 1.0 ce) cd) 2.0) 2))
                             (max (expt (/ (+ be (- 1.0 bd) (- 1.0 bc)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 be) bd (- 1.0 bc)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 be) (- 1.0 bd) bc) 3.0)
                                        2))
                             (max (expt (/ (+ ad (- 1.0 ac) (- 1.0 ab)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 ad) ac (- 1.0 ab)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 ad) (- 1.0 ac) ab) 3.0)
                                        2))
                             (max (expt (/ (+ de (- 1.0 ce) (- 1.0 be)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 de) ce (- 1.0 be)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 de) (- 1.0 ce) be) 3.0)
                                        2))
                             (max (expt (/ (+ cd (- 1.0 bd) (- 1.0 ad)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 cd) bd (- 1.0 ad)) 3.0)
                                        2)
                                  (expt (/ (+ (- 1.0 cd) (- 1.0 bd) ad) 3.0)
                                        2))
                             (max (expt (/ (+ bc (- 1.0 ac)) 2.0) 2)
                                  (expt (/ (+ (- 1.0 bc) ac) 2.0) 2))
                             ab)
                         9.0)
                     2)))
             t00004))
