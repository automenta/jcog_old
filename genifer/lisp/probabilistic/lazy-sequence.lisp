;;; ********************************* functions for lazy sequences (zeqs) ***********************

;; These are pretty standard, taken from Luger & Stubblefield (2009), "AI algorithms, data
;;   structures, and idioms in Prolog, Lisp and Java".
;; Beware:  due to delayed evaluation, (setf x (cons-zeq 'a x)) will cause circular recursion.

;; Abbreviation:
;;     zeq           = lazy sequence

(defmacro delay (exp)
  `(function (lambda () ,exp)))

(defun force (function-closure)
  (funcall function-closure))

(defmacro cons-zeq (exp zeq)
  `(cons ,exp (delay ,zeq)))

(defun car-zeq (zeq)
  (car zeq))

(defun cdr-zeq (zeq)
  (force (cdr zeq)))

(defun null-zeq (zeq)
  (null zeq))

(defun make-new-zeq ()
  nil)
