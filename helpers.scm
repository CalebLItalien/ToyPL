;; helpers.scm
;; Contains miscellaneous helper functions.

;; ================== Error Handling & Display ============================

;; Datatype for encapsulating exception information for later
;; display. Has one variant: (ex-val who format data)
;;   who = a symbol specifying the name of the function the exception
;;         occured in.
;;   format = an eopl:printf appropriate format string indicating the
;;            type of error.
;;   data = a list of values to put into the format string when displayed.
(define-datatype except except?
(runtime-except-val
 (who symbol?)
 (format string?)
 (data list?))
(parse-except-val
 (who symbol?)
 (format string?)
 (data list?))
)

;; Displays the exception message.  The input to this function should
;; be data constructed using (except-val who format data). 
(define display-exception
(lambda (e)
  (cond
   [(except? e)  
    (cases except e
       [runtime-except-val (who format data) 
               (display "Runtime Error:\n")
               (apply eopl:printf (cons format data))
               (newline)]
       [parse-except-val (who format data) 
                 (display "Parse Error:\n")
                 (apply eopl:printf (cons format data))
                 (newline)])]
   [else          ;; Raised by Scheme.
    (display "Exception raised by Scheme:\n")
    (set! doh e)
    (if (who-condition? e)
    (apply printf (cons "  Exception occured in ~:s:" (list (condition-who e))))
    #t
    )
    (if (and (message-condition? e) (irritants-condition? e))
    (apply printf (cons (string-append "  " (condition-message e)) (condition-irritants e)))
    (if (message-condition? e)
        (apply printf (list (condition-message e)))
        #t
        ))
    (newline)]
   )))

(define raise-exception-maker
(lambda (evar)
  (lambda (who format . data)
    (raise (evar who format data))
    )))

;; Overrides sllgen:error to prevent stopping.
(define sllgen:error (raise-exception-maker parse-except-val))
(define raise-exception (raise-exception-maker runtime-except-val))



;; ================== Input Prompt & Editor ============================
;; Known Bugs:
;; - Displays scheme error message when inputting a program with
;;   mismatched parens.
;; - Doesn't work with multi-line inputs, but could be modified to do so.

(module
(get-input-ee)

(define last-entry)
(define rtd-entry)
(define rtd-ln)
(define last-input)

(define eestate)
(define eentry)
(define eec)

(import expression-editor)

(define bind-key-inner-ee
 (lambda ()
   (ee-bind-key
    "^d"
    (ee-compose
     (lambda (a b c)
       (set! last-input "!quit")
       b)
     ee-eof/delete-char))
   (ee-bind-key
    #\return
    (ee-compose
     (lambda (a b c)
       (set! last-entry b)
       (set! rtd-entry (record-rtd last-entry))
       (set! rtd-ln (record-rtd (car ((record-accessor rtd-entry 0) last-entry))))
       (set! last-input ((record-accessor rtd-ln 0) (car ((record-accessor rtd-entry 0) last-entry))))
       b)
     (lambda (a b c)
       (set! eestate a)
       (set! eeentry b)
       (set! eec c)
       b)
     ee-accept))))

(define unbind-key-inner-ee
 (lambda ()
   (ee-bind-key "^d" ee-eof/delete-char)
   (ee-bind-key #\return ee-newline/accept)
   ))

(define display-ee-context
 (lambda (a b c)
   (let*
       [[last-entry b]
        [rtd-entry (record-rtd last-entry)]
        [rtd-ln (record-rtd (car ((record-accessor rtd-entry 0) last-entry)))]]
     (newline)
     (display a)
     (newline)
     (display b)
     (newline)
     (display c)
     (newline)
     (display r)
     (newline)
     b)))

(define my-current-eval
 (lambda (concrete-code)
   (ee-reset-entry eestate eeentry eec)
   (exit last-input)))

(define get-input-ee
 (lambda ()
   (begin
     (ee-auto-paren-balance #t)
     (bind-key-inner-ee)
     (new-cafe my-current-eval)
     (unbind-key-inner-ee)
     last-input
     )))
)


;; (get-input-string) -- Reads a line from the interactive input port.
;; Ignores zero length strings.
(define get-input-string
(lambda ()
  ;; New version with expeditor.
  (get-input-ee)))