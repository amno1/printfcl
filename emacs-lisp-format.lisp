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
           #:convert))

(in-package :printfcl/elisp)

;; Emacs lisp format support - here because convert-string is not exported
(defclass elisp-converter (printfcl:standard-converter)
  ())

(defmethod convert ((converter elisp-converter) (cs (eql :|s|))
                    argument flags field-width precision)
  (printfcl::convert-string argument flags field-width precision #'princ-to-string))

(defmethod convert ((converter elisp-converter) (cs (eql :|S|))
                    argument flags field-width precision)
  (printfcl::convert-string argument flags field-width precision #'prin1-to-string))

;;; emacs-lisp-format.lisp ends here
