;;; -*- Syntax: Common-Lisp; Mode: Lisp; Package :Definitions -*-
;;;  File Name          :  exports.lisp
;;;  Version Number     :  1.3
;;;  Last Changed Date  :  95/10/17 At 12:32:48
;;;  Created Date       :  10/17/95
;;;
;;; Copyright 1991. John Alan McDonald
;;; All Rights Reserved. 
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and no warranty---about the
;;; software, its performance, or its conformity to any
;;; specification---is given or implied.
;;; 
;;;============================================================

(in-package :Definitions)

;;;============================================================

(eval-when (compile load eval)
  (export '(type-check
	    declare-check
	    fix-latex-string
	    Definition
	    definition-definee
	    definition-form
	    definition-definer
	    definition-method-qualifier
	    definition-name
	    definition-name->string
	    definition-symbol
	    definition-symbol-name
	    definition-lambda-list
	    definition-documentation
	    definition-declarations
	    definition-returns
	    definition-type-declarations
	    definition-slots
	    definition-parents
	    definition-children
	    definition-arg-types
	    definition-initial-value
	    definition-usage
	    definition-body
	    definition-specializers
	    lambda-list-specializers
	    lambda-list-keyword-args
	    lambda-list-rest-arg-name
	    lambda-list-optional-args
	    lambda-list-arg-names
	    lambda-list-required-arg-names
	    lambda-list-optional-arg-names
	    lambda-list-required-args
	    lambda-list-keyword-arg-names
	    lambda-list-whole-arg
	    Package-Definition
	    Global-Variable-Definition
	    Constant-Definition
	    Parameter-Definition
	    Variable-Definition
	    Lambda-List-Definition
	    Function-Definition
	    Macro-Definition
	    Setf-Definition
	    Generic-Function-Definition
	    Method-Definition
	    Message-Definition
	    User-Type-Definition
	    Type-Definition
	    Class-Definition
	    Prototype-Definition
	    Structure-Definition
	    Condition-Definition
	    Announcement-Definition
	    Schema-Definition
	    definer-class
	    definition-class-nice-name
	    make-slot-accessor-definitions
	    make-subdefinitions
	    make-definitions
	    read-definitions-from-file
	    read-definitions-from-files
	    exported-definition?
	    definition-alpha<
	    definition<
	    print-tex-definition
	    print-tex-definitions
	    print-html-definition
	    print-html-definitions
	    version-doc-proto
	    version>
	    author-doc-proto
	    project-doc-proto
	    build-project-definitions
	    print-tex-documentation
	    print-html-documentation
	    *doc*
	    *definition-hfuzz*)))





