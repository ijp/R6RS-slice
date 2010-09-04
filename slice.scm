(module slice (slice)

(import scheme chicken)
(use (srfi 1 13))

(define slice
  (let ()
    (define (positive? n) (> n 0))
    (define (negative? n) (< n 0))
    (define (both-positive? n m) (and n m (>= n 0) (>= m 0)))
    
    (define (from/to from to len)
      (cond
       ((and from (> from len))         ; [XXX:2]
        #f)
       ((and (both-positive? from to)   ; [1:2]
             from to
             (> to from)
             (< to len))
        (cons from to))
       ((and (both-positive? from to)   ; [1:XXX]
             from to
             (> to from)
             (>= to len))
        (cons from len))
       ((and (both-positive? from to)   ; [2:1]
             from to
             (> from to))
        #f)
       ((and from                       ; [-1:]
             (< from 0)
             (not to))
        (cons (if (>= (abs from) len)
                  0
                  (+ len from))
              len))
       ((and (not from)                 ; [:-1]
             to
             (< to 0))
        (cons 0
              (if (>= (abs to) len)
                  len
                  (+ len to))))
       ((and from                       ; [1:]
             (>= from 0)
             (not to))
        (cons from len))
       ((and from to                    ; [-2:-1]
             (< from 0)
             (< to 0)
             (< to from))
        #f)
       ((and from to                    ; [-1:-2]
             (< from 0)
             (< to 0))
        (cons (if (>= (abs from) len)
                  0
                  (+ len from))
              (if (>= (abs to) len)
                  len
                  (+ len to))))        
       (else #f)))

    (define (generic-slicer obj from to ruler empty obj-slicer)
      (let* ((len (ruler obj))
             (from&to (from/to from to len))
             (from (and from&to (car from&to)))
             (to (and from&to (cdr from&to))))
        (if (and from to)
            (obj-slicer obj from to)
            empty)))

    (define (string-slice s from to)
      (generic-slicer s from to string-length "" substring))

    (define (list-slice l from to)
      (generic-slicer l from to length '()
                      (lambda (l from to)
                        (take (drop l from) (- to from)))))

    (define (vector-slice v from to)
      (define (subvector vector start end)
        (let ((sub (make-vector (- end start))))
          (let loop ((i 0))
            (if (< i (- end start))
                (begin (vector-set! sub i (vector-ref vector (+ i start)))
                       (loop (+ i 1)))))
          sub))
      (generic-slicer v from to vector-length '#() subvector))


    (let ((slicers
           (list (lambda (obj)
                   (and (string? obj)
                        string-slice))
                 (lambda (obj)
                   (and (vector? obj)
                        vector-slice))
                 (lambda (obj)
                   (and (list? obj)
                        list-slice)))))
      (lambda (obj #!optional from to)
        (if (procedure? obj)
            (set! slicers (cons obj slicers))
            (let loop ((slicers slicers))
              (if (null? slicers)
                  (error "No slicer for the given object.")
                  (let* ((slicer (car slicers))
                         (slice (slicer obj)))
                    (if slice
                        (slice obj from to)
                        (loop (cdr slicers)))))))))))
) ; end module
