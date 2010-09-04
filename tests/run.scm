#!/usr/bin/csi -script

(use test)
(load "../slice.scm")

;;;
;;; Strings
;;;
(test-begin "Slice")

(define s "1234567")

(display "s = ")
(pp s)
(test "" (slice s 0 0))
(test "" (slice s 1 0))
(test "1" (slice s 0 1))
(test "" (slice s 10 10))
(test "1234567" (slice s 0 10))
(test "" (slice s 10 0))

(test "1234567" (slice s 0))
(test "7" (slice s -1))
(test "" (slice s 10))
(test "1234567" (slice s -10))
(test "4567" (slice s -4))

(test "" (slice s -4 -4))
(test "45" (slice s -4 -2))
(test "" (slice s -4 -10))
(test "123" (slice s -10 -4))


;;;
;;; Lists
;;;
(define l '(1 2 3 4 5 6 7))

(newline)
(display "l = ")
(pp l)

(test '() (slice l 0 0))
(test '() (slice l 1 0))
(test '(1) (slice l 0 1))
(test '(2 3) (slice l 1 3))
(test '() (slice l 10 10))
(test '(1 2 3 4 5 6 7) (slice l 0 10))
(test '() (slice l 10 0))

(test '(1 2 3 4 5 6 7) (slice l 0))
(test '(7) (slice l -1))
(test '() (slice l 10))
(test '(1 2 3 4 5 6 7) (slice l -10))
(test '(4 5 6 7) (slice l -4))

(test '() (slice l -4 -4))
(test '(4 5) (slice l -4 -2))
(test '() (slice l -4 -10))
(test '(1 2 3) (slice l -10 -4))

;;;
;;; Vectors
;;;
(define v '#(1 2 3 4 5 6 7))

(newline)
(display "v = ")
(pp v)

(test '#() (slice v 0 0))
(test '#() (slice v 1 0))
(test '#(1) (slice v 0 1))
(test '#(2 3) (slice v 1 3))
(test '#() (slice v 10 10))
(test '#(1 2 3 4 5 6 7) (slice v 0 10))
(test '#() (slice v 10 0))

(test '#(1 2 3 4 5 6 7) (slice v 0))
(test '#(7) (slice v -1))
(test '#() (slice v 10))
(test '#(1 2 3 4 5 6 7) (slice v -10))
(test '#(4 5 6 7) (slice v -4))

(test '#() (slice v -4 -4))
(test '#(4 5) (slice v -4 -2))
(test '#() (slice v -4 -10))
(test '#(1 2 3) (slice v -10 -4))

;;;
;;; Custom object
;;;
(define-record custom-string text)

(define s (make-custom-string "custom string"))
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

(test "" (slice s 0 0))
(test "" (slice s 1 0))
(test "c" (slice s 0 1))

(test-end "Slice")