;; emacs-lisp-format.lisp
;; Copyright (C) 2023  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the MIT License along with this program.
;; If not, see <https://opensource.org/license/mit/>.

;;; Commentary:

;; Emacs Lisp format specifiers used for their format function are closer to
;; printf than to Common Lisp format. This is neither complete nor exact, but
;; works as a band aid for some basic usage

;;; Code:

(defpackage #:printfcl/elisp
  (:use #:cl #:printfcl)
  (:export #:elisp-converter
           #:convert
           #:el-format

           #:*format-converter*))

(in-package :printfcl/elisp)

(defclass elisp-converter (printfcl:standard-converter)
  ())

(defmethod convert ((converter elisp-converter) (cs (eql :|s|))
                    argument flags field-width precision)
  (printfcl::convert-string argument flags field-width precision #'princ-to-string))

(defmethod convert ((converter elisp-converter) (cs (eql :|S|))
                    argument flags field-width precision)
  (printfcl::convert-string argument flags field-width precision #'prin1-to-string))

(cl:defvar *format-converter* (make-instance 'printfcl/elisp:elisp-converter))

;; just a very first-aid implementation
;; based on printfcl: https://github.com/splittist/printfcl
;; elisp format is closer to printf than cl-format, so this does as a band aid
(defun el-format (format-string &rest objects)
  (let ((printfcl:*converter* *format-converter*))
    (apply #'printfcl:sprintf format-string objects)))

;;; emacs-lisp-format.lisp ends here
