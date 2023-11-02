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
(let ((old-size (vector-length store))
      (new-size (* 2 (vector-length store))))
  (let ((new-store (make-vector new-size #f)))  ; Initialize all slots to #f
    (vector-copy! new-store 0 store 0 old-size)
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
    (if index  ; if index is not #f, a free index was found
        (begin  ; A free index was found
          (vector-set! the-store! index val)
          (make-ref-val index))  ; Wrap the index in a ref-val expval
        (begin  ; No free index was found, so double the store size
          (set! the-store! (double-store-size the-store!))
          (let ((new-index (find-free-index the-store!)))  ; Now find the first free index in the new store
            (vector-set! the-store! new-index val)
            (make-ref-val new-index))))))  ; Wrap the index in a ref-val expval


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

;; (marked) -- Checks whether an object is marked
;;(define (marked? object))

;; (mark object) -- Marks an object and all objects it references as in use.
;;(define (mark object))

;; (mark-all-roots) -- Marks all root references.
;;(define (mark-all-roots))

;; (sweep) -- Sweeps through the store and collects unmarked objects.
(define (sweep)
;; Implementation for sweeping through the store and collecting garbage.
  (let loop ((i 0))
    (when (< i (vector-length the-store!))
    (let ((object (vector-ref the-store! i)))
      (if (not (marked? object))  ; Assuming there's a way to check if the object is marked
          (vector-set! the-store! i #f))  ; Reclaim the space if not marked
      (loop (+ i 1))))))

;; (collect-garbage) -- Runs the garbage collection process.
(define (collect-garbage)
  ;; Reset all marks.
  (unmark-all)
  ;; Mark phase.
  (mark-all-roots)
  ;; Sweep phase.
  (sweep))

;;(define (unmark-all))

