;;; sloth-test.el --- Tests for sloth.el             -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Keywords:

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

(require 'ert)
(require 'cl-lib)
(require 'sloth)


;;;; Delay

(ert-deftest sloth-test--force ()
  (should (= (sloth--force 1) 1))

  (let* ((result nil)
         (delay (sloth--delay (setq result t))))
    (should (null result))
    (should (eq (sloth--force delay) t))
    (should (eq result t))))



;;;; Lazy list

(ert-deftest sloth-test-lazy-list-p ()
  (should-not (sloth-lazy-list-p (list 1 2 3)))
  (should (sloth-lazy-list-p (sloth-cons 1 (list 2 3))))
  (should (eq (sloth-lazy-list-p (sloth-list 1 2 3)) t)))

(ert-deftest sloth-test-lazy-list ()
  (should-not (cl-typep (list 1 2 3) 'sloth-lazy-list))
  (should (cl-typep (sloth-list 1 2 3) 'sloth-lazy-list)))

(ert-deftest sloth-test-car ()
  (should (eql (sloth-car (list 1 2 3))
               (car (list 1 2 3))))
  (should (eql (sloth-car (sloth-list 1 2 3))
               (car (list 1 2 3)))))

(ert-deftest sloth-test-cdr ()
  (should (equal (sloth-cdr (list 1 2 3))
                 (cdr (list 1 2 3))))

  (let* ((a nil)
         (b nil)
         (c nil)
         (l (sloth-list (setq a 1) (setq b 2) (setq c 3)))
         (cdr l))
    ;; l = (1 . delayed...)
    (should (eql a 1))
    (should-not (eql b 2))
    (should-not (eql c 3))

    ;; l = (1 2 . delayed...)
    (setq cdr (sloth-cdr cdr))
    (should (eql (sloth-car cdr) 2))
    (should (eql b 2))
    (should-not (eql c 3))

    ;; l = (1 2 3 . delayed...)
    (setq cdr (sloth-cdr cdr))
    (should (eql (sloth-car cdr) 3))
    (should (eql c 3))

    ;; l = (1 2 3)
    (should (null (sloth-cdr cdr)))))

(ert-deftest sloth-test-cons ()
  (let* ((car nil)
         (cdr nil))
    (sloth-cons (setq car "car") (setq cdr '("cdr")))
    ;; Only the car is evaluated.
    (should (equal car "car"))
    (should-not (equal cdr '("cdr")))))

(ert-deftest sloth-test-list ()
  (should (null (sloth-list)))

  (let ((a nil)
        (b nil)
        (c nil))
    (sloth-list (setq a t) (setq b t) (setq c t))
    ;; Only the first element is evaluated.
    (should (eq a t))
    (should-not (eq b t))
    (should-not (eq c t))))

(ert-deftest sloth-test-doall ()
  (should (equal (sloth-doall (sloth-list 1 2 3))
                 (list 1 2 3))))

(provide 'sloth-test)
;;; sloth-test.el ends here
