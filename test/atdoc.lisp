;;; ----------------------------------------------------------------------------
;;; atdoc.lisp
;;;
;;; Functions for generating documentation for the library test.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(asdf:load-system :atdoc)

(load "atdoc-test.lisp")

(defpackage :atdoc-test
  (:use :atdoc :common-lisp)
  (:export #:generate-html
           #:generate-html-single-page
           #:generate-latex
           #:generate-info))

(in-package :atdoc-test)

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :test)))
         (output-directory (merge-pathnames "atdoc/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:test :test-2 :test-3 :test-4)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "The Index Title"
      :heading "The Heading"
      :css "crategus.css"
      :logo nil
      :single-page-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :test)))
         (output-directory (merge-pathnames "atdoc/single-page/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:test :test-2 :test-3 :test-4)
      output-directory
      :index-title "atdoc API reference"
      :heading "atdoc"
      :css "crategus.css"
      :logo nil
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-latex ()
  (let* ((base (asdf:component-pathname (asdf:find-system :test)))
         (output-directory (merge-pathnames "atdoc/latex/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-latex-documentation
      '(:test)
      output-directory
      :title "atdoc API reference"
      :include-slot-definitions-p t
      :run-tex-p t)))

(defun generate-info ()
  (let* ((base (asdf:component-pathname (asdf:find-system :atdoc)))
         (output-directory (merge-pathnames "atdoc/info/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-info-documentation
      '(:test)
      output-directory
      :name "atdoc"
      :title "atdoc API reference"
      :include-slot-definitions-p t)))

(generate-html)
(generate-html-single-page)

;;; --- End of file atdoc.lisp -------------------------------------------------
