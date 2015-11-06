;;; sloth.el --- A lazy list library                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author:  Hiroki YAMAKAWA <s06139@gmail.com>
;; URL: https://github.com/HKey/sloth
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)


;;;; Delay

(defconst sloth--delayed-marker (cl-gensym "delayed-marker:"))

(cl-defstruct (sloth--delay
               (:constructor sloth--make-delay))
  body
  (value sloth--delayed-marker))


(defmacro sloth--delay (&rest body)
  "Delay BODY.
Use `sloth--force' to evaluate it."
  (declare (indent 0))
  `(sloth--make-delay :body (lambda () ,@body)))

(defun sloth--force (delay)
  "Evaluate the delayed expression DELAY.
See also `sloth--delay'."
  (if (sloth--delay-p delay)
      (if (eq (sloth--delay-value delay) sloth--delayed-marker)
          (setf (sloth--delay-value delay)
                (funcall (sloth--delay-body delay)))
        (sloth--delay-value delay))
    delay))



;;;; Lazy list

(cl-defstruct (sloth--lazy-cdr
               (:constructor sloth--make-lazy-cdr))
  delay)


(defun sloth-lazy-list-p (object)
  "Return t if OBJECT is a `sloth-lazy-list'.
This cannot detect that OBJECT was a `sloth-lazy-list'."
  (and (listp object)
       (sloth--lazy-cdr-p (cdr (last object)))))

(cl-deftype sloth-lazy-list ()
  '(satisfies sloth-lazy-list-p))

(defun sloth-car (lazy-list)
  "Return the car of LAZY-LIST.
LAZY-LIST can be a normal list or a lazy list.
See also `sloth-cdr'."
  (car lazy-list))

(defun sloth-cdr (lazy-list)
  "Return the cdr of LAZY-LIST.
LAZY-LIST can be a normal list or a lazy list.
See also `sloth-car'."
  (let ((rest (cdr lazy-list)))
    (if (sloth--lazy-cdr-p rest)
        (setf (cdr lazy-list) (sloth--force (sloth--lazy-cdr-delay rest)))
      rest)))

(defmacro sloth-cons (car cdr)
  "Make a lazy cons cell with CAR and CDR.
This is a lazy version of `cons'.  CDR is delayed."
  `(cons ,car (sloth--make-lazy-cdr :delay (sloth--delay ,cdr))))

(defmacro sloth-list (&rest expressions)
  "Make a lazy list.
This is a lazy version of `list'.  The cdr of EXPRESSIONS are delayed."
  (when expressions
    `(sloth-cons ,(car expressions)
                 (sloth-list ,@(cdr expressions)))))

(defun sloth-doall (lazy-list)
  "Evaluate LAZY-LIST.
This returns a normal list which is evaluated LAZY-LIST.
This cannot return control if LAZY-LIST is an infinite list."
  (prog1 lazy-list
    (let ((list lazy-list))
      (while (setq list (sloth-cdr list))))))



;;;; Dash compatible functions

;; These functions are based on dash.el (https://github.com/magnars/dash.el).

;;; Maps

(defun sloth-map (fn list)
  "A lazy version of `-map'."
  (when list
    (sloth-cons (funcall fn (sloth-car list))
                (sloth-map fn (sloth-cdr list)))))

(defun sloth-map-when (pred rep list)
  "A lazy version of `-map-when'."
  (sloth-map (lambda (x)
               (if (funcall pred x)
                   (funcall rep x)
                 x))
             list))

(defun sloth-map-first (pred rep list)
  "A lazy version of `-map-first'."
  (when list
    (let* ((first (sloth-car list))
           (rest (sloth-cdr list))
           (matchedp (funcall pred first)))
      (sloth-cons (if matchedp
                      (funcall rep first)
                    first)
                  (if matchedp
                      rest
                    (sloth-map-first pred rep rest))))))

(defun sloth--map-indexed (fn list index)
  "An internal function for `sloth-map-indexed'."
  (when list
    (sloth-cons (funcall fn index (sloth-car list))
                (sloth--map-indexed fn (sloth-cdr list) (1+ index)))))

(defun sloth-map-indexed (fn list)
  "A lazy version of `-map-indexed.'"
  (sloth--map-indexed fn list 0))

(defun sloth-annotate (fn list)
  "A lazy version of `-annotate'."
  (sloth-map (lambda (x) (cons (funcall fn x) x)) list))

;;; Sublist selection

(defun sloth-take (n list)
  "A lazy version of `-take'."
  (cl-loop for i from 0 below n
           for e in list by #'sloth-cdr
           collect e))


(provide 'sloth)
;;; sloth.el ends here
