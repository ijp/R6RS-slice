(import (rnrs) (srfi :64) (slice))

(define pp write)
(define-syntax handle-exceptions
  (syntax-rules ()
    [(handle-exceptions exn handle-expr expr1 expr2 ...)
     (guard (exn (else handle-expr)) expr1 expr2 ...)]))
;;;
;;; Strings
;;;
(test-begin "Slice")

(define s "1234567")

(display "s = ")
(pp s)
(test-equal "" (slice s 0 0))
(test-equal "" (slice s 1 0))
(test-equal "1" (slice s 0 1))
(test-equal "" (slice s 10 10))
(test-equal "1234567" (slice s 0 10))
(test-equal "" (slice s 10 0))

(test-equal "1234567" (slice s 0))
(test-equal "7" (slice s -1))
(test-equal "" (slice s 10))
(test-equal "1234567" (slice s -10))
(test-equal "4567" (slice s -4))

(test-equal "" (slice s -4 -4))
(test-equal "45" (slice s -4 -2))
(test-equal "" (slice s -4 -10))
(test-equal "123" (slice s -10 -4))


;;;
;;; Lists
;;;
(define l '(1 2 3 4 5 6 7))

(newline)
(display "l = ")
(pp l)

(test-equal '() (slice l 0 0))
(test-equal '() (slice l 1 0))
(test-equal '(1) (slice l 0 1))
(test-equal '(2 3) (slice l 1 3))
(test-equal '() (slice l 10 10))
(test-equal '(1 2 3 4 5 6 7) (slice l 0 10))
(test-equal '() (slice l 10 0))

(test-equal '(1 2 3 4 5 6 7) (slice l 0))
(test-equal '(7) (slice l -1))
(test-equal '() (slice l 10))
(test-equal '(1 2 3 4 5 6 7) (slice l -10))
(test-equal '(4 5 6 7) (slice l -4))

(test-equal '() (slice l -4 -4))
(test-equal '(4 5) (slice l -4 -2))
(test-equal '() (slice l -4 -10))
(test-equal '(1 2 3) (slice l -10 -4))

;;;
;;; Vectors
;;;
(define v '#(1 2 3 4 5 6 7))

(newline)
(display "v = ")
(pp v)

(test-equal '#() (slice v 0 0))
(test-equal '#() (slice v 1 0))
(test-equal '#(1) (slice v 0 1))
(test-equal '#(2 3) (slice v 1 3))
(test-equal '#() (slice v 10 10))
(test-equal '#(1 2 3 4 5 6 7) (slice v 0 10))
(test-equal '#() (slice v 10 0))

(test-equal '#(1 2 3 4 5 6 7) (slice v 0))
(test-equal '#(7) (slice v -1))
(test-equal '#() (slice v 10))
(test-equal '#(1 2 3 4 5 6 7) (slice v -10))
(test-equal '#(4 5 6 7) (slice v -4))

(test-equal '#() (slice v -4 -4))
(test-equal '#(4 5) (slice v -4 -2))
(test-equal '#() (slice v -4 -10))
(test-equal '#(1 2 3) (slice v -10 -4))

;;;
;;; Custom object
;;;
(define-record-type custom-string (fields text))

(set! s (make-custom-string "custom string"))
(slice (lambda (obj)
         (and (custom-string? obj)
              (lambda (obj from to)
                (handle-exceptions
                 exn
                 ""
                 (substring (custom-string-text obj) from to))))))

(newline)
(display "s = ")
(pp s)

(test-equal "" (slice s 0 0))
(test-equal "" (slice s 1 0))
(test-equal "c" (slice s 0 1))

(test-end "Slice")
