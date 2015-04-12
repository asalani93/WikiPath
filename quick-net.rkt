#lang racket

(require net/url)
(require net/uri-codec)

(provide get-webpage)
(provide fix-url-arg)
(provide index)
(provide list-range)
(provide list-partition)
(provide timed-map)
(provide timed-filter)
(provide apply-batch)
(provide json-burrow)
(provide conv-string)

; return a web page
; has basic handling of exceptions, if a page times out, it will try again until eventually it works
;   url - the url of the resource on the web
(define (get-webpage url)
  (define (request-failed exn)
    (display "failed loading page, trying again\n\n")
    (get-webpage url))
  (with-handlers ((exn:fail? request-failed))
    (let ((port (get-pure-port (string->url url))))
      port)))

; fix-url-arg
; adds correct percent encoding to arguments to urls
(define (fix-url-arg url)
  (uri-unreserved-encode url))

; index
; create a list of lists where the first element in each sublist is a number starting from 1, and the
; second is the corresponding element in lst.
;   lst - the list to add indices to
(define (index lst)
  (define (helper lst num)
    (if (null? lst)
        '()
        (cons (list num (car lst)) (helper (cdr lst) (+ num 1)))))
  (helper lst 1))

; list-range
; takes the specified slice of the list
;   lst   - the list to operate on
;   start - the position to start from (zero-indexed)
;   end   - the position to end at (also zero-indexed)
(define (list-range lst start end)
  (take (drop lst start) (- end start)))

; list-partition
; helper function - divide a list into some number of almost equally sized parts
;   lst   - the list to partition
;   parts - the number of parts to divide the list into
(define (list-partition lst parts)
  (let ((frac (/ (length lst) parts)))
    (define (helper pos result)
      (if (= pos parts)
          result
          (let* ((prev (round (* frac pos)))
                 (next (round (* frac (+ 1 pos))))
                 (lstr (list-range lst prev next)))
            (helper (+ pos 1) (cons lstr result)))))
    (helper 0 '())))

; timed-map
; helper function - returns a version of map that runs for a duration before returning the current results
;   time - the time in milliseconds
(define (timed-map time)
  (define (future-time delta)
    (+ delta (current-milliseconds)))
  (define (inner-map proc lst)
    (let ((rt (future-time time)))
      (define (inner proc lst)
        (cond ((null? lst) '())
              ((< rt (current-milliseconds)) '())
              (else (cons (proc (car lst)) (inner proc (cdr lst))))))
      (inner proc lst)))
  inner-map)

; timed-filter
; helper function - returns a version of filter that runs for a duration before returning the current results
;   time - the time in milliseconds
(define (timed-filter time)
  (define (future-time delta)
    (+ delta (current-milliseconds)))
  (define (inner-filter pred lst)
    (let ((rt (future-time time)))
      (define (inner proc lst)
        (cond ((null? lst) '())
              ((< rt (current-milliseconds)) '())
              ((pred (car lst)) (cons (car lst) (inner pred (cdr lst))))
              (else (inner pred (cdr lst)))))
      (inner pred lst)))
  inner-filter)

; apply-batch
; applies either map or filter (or any function that takes a procedure and a list and returns a list)
; with a specified procedure to a provided list among a certain number of threads
;   op       - either map or filter
;   proc     - a function that takes one argument to use with either map or filter
;   threads  - the number of threads to distribute the workload amongst
;   lst-args - the list that you want to map or filter with proc
(define (apply-batch op proc threads lst-args)
  (let ((this (current-thread)))
    
    ; creates a certain number of threads and gives them a list to operate on
    (define (create-threads x lst-t lst-a)
      ; the thread driver
      (define (dispatch-threads)
        (thread-send this (op proc (car lst-a))))
      (if (= threads x)
          lst-t
          (create-threads (+ x 1) (cons (thread dispatch-threads) lst-t) (cdr lst-a))))
    
    ; receives the data from the threads and combines it into one list
    (define (empty-mailbox lst)
      (let ((x (thread-try-receive)))
        (if (not x)
            lst
            (empty-mailbox (append x lst)))))
    
    ; create threads, wait for them to finish, return the results
    (for-each 
     (lambda (t) (thread-wait t))
     (create-threads 0 '() (list-partition lst-args threads)))
    (empty-mailbox '())))

; json-burrow
; takes a json expression and "burrows" given a list of names and positions
(define (json-burrow jsexpr lst)
  (define (nth lst n)
    (cond ((= n 1) (car lst))
          ((< n 1) #f)
          ((null? lst) #f)
          (else (nth (cdr lst) (- n 1)))))
  (define (iter jsexpr lst)
    (cond ((null? lst) #f)
          ((and (eq? (car lst) #t) (hash? jsexpr))
           (let ((query (hash-ref jsexpr (car (hash-keys jsexpr)))))
             (if (null? (cdr lst))
                 query
                 (json-burrow query (cdr lst)))))
          ((and (symbol? (car lst)) (hash? jsexpr))
           (let ((query (hash-ref jsexpr (car lst) #f)))
             (if (null? (cdr lst))
                 query
                 (json-burrow query (cdr lst)))))
          ((and (integer? (car lst)) (list? jsexpr))
           (let ((query (nth jsexpr (car lst))))
             (if (null? (cdr lst))
                 query
                 (json-burrow query (cdr lst)))))
          (else #f)))
  (iter jsexpr lst))

; because the json from wikipedia sometimes has some articles (like 1987) returned as ints
; here's a helper function to convert anything to a string
(define (conv-string x)
  (cond ((string? x) x)
        ((number? x) (number->string x))
        (else (begin
                (display "can't convert type")
                x))))