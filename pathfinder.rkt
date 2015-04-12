#lang racket

(require "quick-net.rkt")

(provide pathfind)
(provide pathfinder->string)

(define (get-user-page src)  
  (display "Enter the name of the starting point: ")
  (let* ((name    (read-line (current-input-port)))
         (suggest ((src 'suggest) name)))
    (cond ((equal? name suggest)
           (begin
             (display (string-append "Using " name ".\n"))
             name))
          ((string? suggest)
           (begin
             (display (string-append "Couldn't find " name ", using " suggest " instead.\n"))
             suggest))
          ((list? suggest)
           (let ((indx (index suggest)))
             (display (string-append "Couldn't find " name ".  Did you mean any of these?\n"))
             (for-each (lambda (x)
                         (display (string-append "  " (number->string (car x)) ". " (cadr x) "\n")))
                       indx)
             (let ((choice (string->number (read-line (current-input-port)))))
               (if (false? choice)
                   #f
                   (cadar (list-range indx (- choice 1) choice))))))
          (else #f))))

(define (pathfind src start end)
  (let ((lh (make-hash))
        (rh  (make-hash)))
    
    (hash-set! lh start #t)
    (hash-set! rh end   #t)
    
    (define (get-left x)
      (let ((parent (hash-ref lh x)))
        (if (equal? parent #t)
            (list x)
            (append (get-left parent) (list x)))))
    
    (define (get-right x)
      (let ((parent (hash-ref rh x)))
        (if (equal? parent #t)
            (list x)
            (cons x (get-right parent)))))
    
    (define (collision-test)
      (map (lambda (x) (append (get-left x) (cdr (get-right x))))
           (filter (lambda (x) (hash-has-key? lh x)) (hash-keys rh))))
    
    (define (breadth l-lst r-lst)
      ;(display "Next iteration\n")
      (let ((l-res (apply append (apply-batch (timed-map 10000)
                                              (lambda (x)
                                                ((src 'f-links) x))
                                              8
                                              l-lst)))
            (r-res (apply append (apply-batch (timed-map 10000)
                                              (lambda (x)
                                                ((src 'b-links) x 1000))
                                              8
                                              r-lst))))
        (list
         (map (lambda (x) (car x))
              (filter (lambda (x)
                        (if (false? (hash-has-key? lh (car x)))
                            (begin
                              (hash-set! lh (car x) (cadr x))
                              #t)
                            #f))
                      l-res))
         (map (lambda (x) (car x))
              (filter (lambda (x)
                        (if (false? (hash-has-key? rh (car x)))
                            (begin
                              (hash-set! rh (car x) (cadr x))
                              #t)
                            #f))
                      r-res)))))
    
    (define (loop l r)
      ;(display ".")
      (if (or (null? l) (null? r))
          #f
          (let* ((results (breadth (shuffle l) r))
                 (collide (collision-test)))
            (cond ((pair? collide) collide)
                  ((null? results) '())
                  (else (loop (car results) (cadr results)))))))
    
    ;(breadth (list start) (list end))))
    ;(breadth start end)))
    (sort (loop (list start) (list end))
          (lambda (x y)
            (< (length x) (length y))))))

;(define (pathfinder->string-2 path)
;  (foldr (lambda (first rest)
;           (if (equal? "" rest)
;               (conv-string first)
;               (string-append (conv-string first) " → " rest)))
;         ""
;         path))

(define (pathfinder->string path)
  (if (null? path)
      "No paths found."
      (let ((out (foldr (lambda (first rest)
                          (if (equal? "" rest)
                              (conv-string first)
                              (string-append (conv-string first) " → " rest)))
                        ""
                        path)))
        (if (> (string-length out) 195)
            (string-append (substring out 0 195) "...")
            out))))