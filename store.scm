;; store.scm

;; =================== Store ===================

(define the-store! 'uninitialized)

(define empty-store
(lambda ()
  (let ((new-store (make-vector 2)))
    (vector-fill! new-store #f) ; Fill the vector with false to indicate all slots are free.
    new-store)))

;; (initialize-store!) sets the-store! to an empty store.
(define initialize-store!
  (lambda ()
    (set! the-store! (empty-store))))

(define (vector-copy! target target-start source source-start length)
(let loop ((i 0))
  (when (< i length)
    (vector-set! target (+ target-start i) (vector-ref source (+ source-start i)))
    (loop (+ i 1)))))

(define (double-store-size store)
(let ((new-size (* 2 (vector-length store))))
  (let ((new-store (make-vector new-size)))
    (vector-copy! new-store 0 store 0 (vector-length store))
    new-store)))

(define (find-free-index store)
(let loop ((i 0))
  (if (= i (vector-length store))
      #f  ; No free index found, the store is full.
      (if (eq? (vector-ref store i) #f)  ; Found a free slot.
          i
          (loop (+ i 1))))))

(define (make-ref-val index)
      (ref-val index))

(define (newref! val)
      (let ((index (find-free-index the-store!)))
        (if (not index)  ; if #f, no free index was found.
            (begin
              (set! the-store! (double-store-size the-store!))
              (let ((new-index (quotient (vector-length the-store!) 2)))  ; Calculate the old store size, which is the new free index.
                (vector-set! the-store! new-index val)
                (make-ref-val new-index)))  ; Wrap the index in a ref-val expval
          (begin  ; Else, a free index was found
            (vector-set! the-store! index val)
            (make-ref-val index)))))  ; Wrap the index in a ref-val expval
    
    

;; (deref val) takes an expressed value which is a ref-val and returns
;; the element in the store at that location.
(define deref
(lambda (ref)
  (vector-ref the-store! (expval->ref ref))))

;; (setref! ref val) takes an expressed value ref which a ref-val and
;; an expressed value val.  Changes the cell pointed to by the ref-val
;; to val.
(define setref!
(lambda (ref val)
  (vector-set! the-store! (expval->ref ref) val)))

;; Display the contents of the store
(define (display-store)
(if (eq? the-store! 'uninitialized)
    (display "The store is uninitialized.\n")
    (let ((size (vector-length the-store!)))
      (display "The store contains:\n")
      (let loop ((i 0))
        (when (< i size)
          (display (format "Index ~a: ~a\n" i (vector-ref the-store! i)))
          (loop (+ i 1)))))))

