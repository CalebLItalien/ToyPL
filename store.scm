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

(define (make-store-entry val)
  ;; Create a store entry with the value and an initial mark of #f (unmarked).
  (list val #f))

(define (newref! val)
  (let ((index (find-free-index the-store!)))
    (if index
        (begin
          (vector-set! the-store! index (make-store-entry val))
          (make-ref-val index))
        (begin
          (set! the-store! (double-store-size the-store!))
          (let ((new-index (find-free-index the-store!)))
            (vector-set! the-store! new-index (make-store-entry val))
            (make-ref-val new-index))))))


;; (deref val) takes an expressed value which is a ref-val and returns
;; the element in the store at that location.
(define deref
(lambda (ref)
  (let ((store-entry (vector-ref the-store! (expval->ref ref))))
    ;; Assuming the store entry is a list with the value as the first element.
    (if (pair? store-entry)  ; Check if the store entry is a non-empty list.
        (car store-entry)  ; Return the value, which is the first element of the list.
        #f))))  ; If the store entry is not a proper list, something is wrong.


;; (setref! ref val) takes an expressed value ref which a ref-val and
;; an expressed value val.  Changes the cell pointed to by the ref-val
;; to val.
(define setref!
(lambda (ref val)
  (let ((index (expval->ref ref)))
    ;; Get the current entry at the index to preserve the mark bit.
    (let ((current-entry (vector-ref the-store! index)))
      ;; Update the value at the index with new value, keeping the mark bit unchanged.
      (vector-set! the-store! index (cons val (cdr current-entry)))))))

;; Display the contents of the store
(define (display-store)
(if (eq? the-store! 'uninitialized)
    (display "The store is uninitialized.\n")
    (let ((size (vector-length the-store!)))
      (display "The store contains:\n")
      (let loop ((i 0))
        (when (< i size)
          (let ((store-entry (vector-ref the-store! i)))
            (if (pair? store-entry)
                (display (format "Index ~a: Value: ~a, Marked: ~a\n" i (car store-entry) (cadr store-entry)))
                (display (format "Index ~a: Free\n" i))))  ; Display free for unallocated or improperly structured entries.
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

