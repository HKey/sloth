;;; sloth-test-dash.el --- Test for dash compatible functions of sloth.el  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'f)
(require 'sloth)


(eval-and-compile
  (defvar sloth-test-dash-dir
    (f-expand "../dash" (f-dirname (f-this-file)))))

(require 'dash (f-join sloth-test-dash-dir "dash"))
(require 'dash-functional (f-join sloth-test-dash-dir "dash-functional"))


(defun sloth-test-dash-replace (symbols)
  "Replace dash functions with SYMBOLS of sloth."
  (cl-dolist (symbol symbols)
    (let ((dash-symbol
           (intern (replace-regexp-in-string
                    "^sloth" "" (symbol-name symbol)))))
      (defalias dash-symbol
        (lambda (&rest args)
          (sloth-doall (apply symbol args)))))))

(sloth-test-dash-replace
 '(
   ;; Maps
   sloth-map
   sloth-map-when
   sloth-map-first
   sloth-map-indexed
   sloth-annotate

   ;; Sublist selection
   sloth-take))

(load (f-join sloth-test-dash-dir "dev" "examples-to-tests.el"))
(load (f-join sloth-test-dash-dir "dev" "examples.el"))

(provide 'sloth-test-dash)
;;; sloth-test-dash.el ends here
