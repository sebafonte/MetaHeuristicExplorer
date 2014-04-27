;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/excel:demo.lisp,v 1.2.6.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "W")

;;; function to start demo

(defun cl-user::demo ()
  (start-dde-server 'lisp-server)
  (setq cl-user::*ico* (capi:display 
			(make-instance 'cl-user::icosahedron-viewer))))


;;; Define a DDE server that excepts a Display() command via an 
;;; execute transaction

(define-dde-server lisp-server "LISP")

(define-dde-server-function (reset :server lisp-server)
    :execute
    ()
  (user::reset-object))

(defmacro excel-integer-range (topic range)
  `(dde-item* :excel ,topic ,range :type '(:excel-range integer)))

(defmacro excel-float-range (topic range)
  `(dde-item* :excel ,topic ,range :type '(:excel-range float)))


(define-dde-server-function (display :server lisp-server)
    :execute
    ((topic string)
     (vertex-list-range string)
     (face-list-range string))
  (let ((vertex-list (mapcar #'(lambda (x) (nconc x '(1.0)))
                             (excel-float-range topic vertex-list-range)))
        (face-list (excel-integer-range topic face-list-range)))
    (user::new-object vertex-list face-list)))


;;; --------------------------------------------------------------------
;;; Define a DDE service which allows us to fetch cell ranges from Excel

;;; We subclass the conversation so we can define unmarshalling for a new
;;; type, :excel-range

(defclass excel-conversation (dde-client-conversation)
  ())

(define-dde-client :excel :class excel-conversation)



;;; Implementation of :excel-range, which parses the string returned by
;;; Excel into a list of lists

(defmethod dde-request-unmarshall ((conversation excel-conversation) hdata data len format type)
  (if (eq (car type) :excel-range)
      (when (= format CF_TEXT)
        (let ((string (call-next-method conversation hdata data len format '(string))))
          (values (parse-excel-range string (or (second type) 'string))
                  t)))
    (call-next-method)))

(defun parse-excel-range (string type)
  (loop with rows = (split-string string #\Newline :ranges t :terminatorp t)
        for (row-start . row-end) in rows 
        collect
        (loop with items = (split-string string #\Tab :ranges t :start row-start :end row-end)
              for (item-start . item-end) in items
              collect (unmarshall-one-arg string item-start item-end type))))
  




