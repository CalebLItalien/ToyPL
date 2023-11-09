;; store.scm

;; =================== Store ===================

(define the-store! 'uninitialized)

(define empty-store
(lambda ()
  (let ((new-store (make-vector 2)))
    (vector-fill! new-store #f)
    new-store)))

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
  (let ((new-store (make-vector new-size #f)))
    (vector-copy! new-store 0 store 0 old-size)
    new-store)))

(define (find-free-index store)
  (let loop ((i 0))
  (if (= i (vector-length store))
      #f 
      (if (eq? (vector-ref store i) #f)  
          i
          (loop (+ i 1))))))

(define (make-ref-val index)
      (ref-val index))

(define (make-store-entry val)
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

(define deref
(lambda (ref)
  (let ((store-entry (vector-ref the-store! (expval->ref ref))))
    (if (pair? store-entry)  
        (car store-entry)  
        #f))))  

(define setref!
(lambda (ref val)
  (let ((index (expval->ref ref)))
    (let ((current-entry (vector-ref the-store! index)))
      (vector-set! the-store! index (cons val (cdr current-entry)))))))

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
                (display (format "Index ~a: Free\n" i)))) 
          (loop (+ i 1)))))))


(define (marked? object)
  (if (pair? object)               
    (cdr object)                 
    #f))                         

(define (sweep)
  (let loop ((i 0))
    (when (< i (vector-length the-store!))
      (let ((object (vector-ref the-store! i)))
        (if (not (marked? object))  
            (vector-set! the-store! i #f))  
        (loop (+ i 1)))))) 

(define (mark ref-val)
(let ((index (expval->ref ref-val)))
  (let ((entry (vector-ref the-store! index)))
    (if (and (pair? entry) (not (marked? entry))) 
        (begin
          (set-cdr! entry (cons #t '())) 
          (let ((value (car entry)))
            (when (ref-val? value)
              (mark value))))))))

(define (mark-all-env-entries env)
  (cases environ env
    [empty-env () '()] 
    [extend-env (var val env)
      (if (ref-val? val)
          (mark val))  
      (mark-all-env-entries env)]  
    [extend-env-rec (f-name f-vars f-body env)
      (let ((closure (newref! (proc-val f-vars f-body env))))
        (if (ref-val? closure)
            (mark closure)))
      (mark-all-env-entries env)] 
    ))

(define (collect-garbage env)
  (let ((unmark (lambda (index)
      (let ((store-entry (vector-ref the-store! index)))
        (if (and (pair? store-entry) (marked? store-entry))
            (set-cdr! store-entry #f))))))
  (do ((i 0 (+ i 1)))
      ((>= i (vector-length the-store!)))
    (unmark i)))
  
  (mark-all-env-entries env)

  (sweep)
)

