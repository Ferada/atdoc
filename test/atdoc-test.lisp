
(defpackage :test-asdf
  (:use :asdf :common-lisp))

(in-package :test-asdf)

(defsystem :test
  :name "test"
  :version "1.0.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on ()
  :components ((:file "test")))

(defpackage :test
  (:use :common-lisp)
  (:export #:my-class
           #:derived-class
           #:*global-variable*
           #:+global-constant+
           #:global-parameter
           #:global-no-value
           #:simple-function
           #:person
           #:new-type
           #:invalid-number)
  (:documentation
   "This is the documentation string for the package.
    @begin[First section]{section}
      We have started a first section with the command
      @code{@@begin[First section]{section@}}.

      The command @code{@@end{section@}} ends the First section.
    @end{section}
    @begin[Formating of text]{section}
      @b{This text is set bold with the command @code{@@b{<text>@}}.}

      @em{This text is emphasized with the command @code{@@em(<text>)@}}.}

      You get a new paragraph with an empty line or the command
      @code{@@break{@}}@break{}
      A newline without extra space is possible with the command @code{@@br{@}}.@br{}
      This is the next line after the command newline @code{@@br{@}}.

      The special chars @code{@@} and @code{@}} have to be escaped with the
      char @code{@@} for output in the documenation. E. g. the command
      @code{@@@@} prints the char @code{@@} in the documentation.

    @end{section}
"))

(defpackage :test-2
  (:use :common-lisp)
  (:documentation 
   "This is a second package."))

(defpackage :test-3
  (:use :common-lisp)
  (:documentation
   "This is a third package. "))

(defpackage :test-4
  (:use :common-lisp)
  (:documentation 
   "This is the fourth package."))

(in-package :test)

;;; Global variables

(defvar *global-variable* nil
 "This is the documentation string of a global variable.")

(defconstant +global-constant+ nil
 "This is the documentation string of a global constant.")

(defparameter global-parameter nil)

;; This symbol has no value and does not appear in the documentation.
(defvar global-no-value)

;;, TYPE

(deftype new-type 'integer)

(setf (documentation 'new-type 'type)
  "A new user definied type.")

;;; CLASS

(defclass my-class ()
  ((first-slot :initarg :first-slot
              :documentation
              "This is the documentation for the first slot.")
   (second-slot :initarg :second-slot
                :documentation
                "This is the documentation for the second slot."))
   (:documentation
    "This is the documentation for the class myClass"))

(defclass derived-class (my-class)
  ((derived :initarg :derived
              :documentation
              "This is the documentation for the slot @code{derived}."))
  (:documentation
   "This is the documentation for the derived class derived-class"))

;;; Structure

(defstruct person
  name
  age
  sex)

;;; Condition

(define-condition invalid-number (parse-error)
  ((value  :reader invalid-number-value
           :initarg :value
           :initform nil
           :documentation "The value which causes the error condition.")
   (reason :reader invalid-number-reason
           :initarg :reason
           :initform "Not specified"))
  (:report (lambda (c s)
             (format s "Invalid number: ~S [Reason: ~A]"
                     (invalid-number-value c)
                     (invalid-number-reason c)))))

;;; FUNCION

(defun simple-function (x y)
  "@arg[x]{documentation of argument x, but this line is very very long,
     more text, more text, more}
   @arg[y]{documentation of argument y}
   @return{documentation of the return value}
   This is the documentation string of simple-function.

   @fun{simple-function} - reference a function@br{}
   @variable{*global-variable*} - reference a variable@br{}
   @type{new-type} - reference a type@br{}
   @class{my-class} - reference a class@br{}
   @struct{person} - reference a struct@br{}
   @condition{invalid-number} - reference a condition type@b{}
"
  (+ x y))


