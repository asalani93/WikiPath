#lang racket

(require json)
(require "quick-net.rkt")

(provide wikipedia)

(define (wikipedia msg)

;  (define (suggest name)
;    (let ((req-exists (request-exists name))
;          (disambig?  (is-disambiguation name)))
;      (if (false? req-exists)
;          (let ((req-search (request-search name)))
;            (if (null? req-search)
;                #f
;                req-search))
;          req-exists)))
  
    (define (suggest name)
      (let ((req-exists (request-exists name))
            (disambig?  (is-disambiguation? name)))
        (cond ((list? disambig?) disambig?)
              ((not (false? req-exists)) req-exists)
              (else (let ((req-search (request-search name)))
                      (if (null? req-search)
                          #f
                          req-search))))))
  
  (define (f-links name)
    (let ((req-link (request-f-links name))
          (req-page (request-page name)))
      ;(display (length req-link))
      (map (lambda (x)
             (list x name))
           (filter (lambda (x)
                     (if (false? (regexp-match (regexp-quote (string-append "[[" x) #f) req-page)) #f #t))
                   req-link))))
  
  (define (b-links name time)
    (let ((req-link (request-b-links name)))
      (map (lambda (x)
             (list x name))
           (apply-batch (timed-filter time)
                        (lambda (x)
                          (let ((req-page (request-page x)))
                            ;(display x)
                            (if (false? (regexp-match (regexp-quote (string-append "[[" name) #f) req-page)) #f #t)))
                        8
                        req-link))))
  
  (define (picture name)
    (let ((url (request-picture name)))
      (cond
        ((false? url) (list 'png/alpha (open-input-file "q-mark.png" #:mode 'binary)))
        ((regexp-match ".png" url) (list 'png/alpha (get-webpage (request-picture name))))
        (else (list 'unknown (get-webpage (request-picture name)))))))
  
;  (define (picture name)
;    (let ((url (request-picture name)))
;      (if (false? url)
;          (list 'png/alpha (open-input-file "q-mark.png" #:mode 'binary))
;          (list 'unknown (get-webpage (request-picture name))))))
  
  (define (is-disambiguation? name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&format=json&prop=categories&clcategories=Category:Disambiguation%20pages&titles=" (fix-url-arg name)))
           (json (json-burrow (read-json (get-webpage url)) '(query pages #t categories 1 title))))
      (if (false? json)
          #f
          (map (lambda (x)
                 (car x))
               (f-links name)))))
  
  (define (request-exists name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&format=json&prop=links&plnamespace=0&pllimit=2&titles=" (fix-url-arg name)))
           (json (json-burrow (read-json (get-webpage url)) '(query pages #t links))))
      (cond ((eq? json #f) #f)
            ((= (length json) 1) (conv-string (json-burrow json '(1 title))))
            (else (conv-string name)))))
      
  (define (request-search name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&list=search&srprop=timestamp&format=json&srsearch=" (fix-url-arg name)))
           (json (json-burrow (read-json (get-webpage url)) '(query search))))
      (map (lambda (x)
             (conv-string (json-burrow x '(title))))
           json)))
  
  (define (request-f-links name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&format=json&prop=links&plnamespace=0&pllimit=500&titles=" (fix-url-arg name)))
           (rslt (read-json (get-webpage url)))
           (json (json-burrow rslt '(query pages #t links)))
           (cont (json-burrow rslt '(query-continue links plcontinue))))
      (cond ((false? json) '())
            ((false? cont) (map (lambda (x)
                                  (conv-string (json-burrow x '(title))))
                                json))
            (else (append (map (lambda (x)
                                 (conv-string (json-burrow x '(title))))
                               json)
                          (request-f-links-cont name cont))))))
  
  (define (request-f-links-cont name cont-str)
    (let* ((url (string-append "http://en.wikipedia.org/w/api.php?action=query&format=json&prop=links&plnamespace=0&pllimit=500&titles="
                               (fix-url-arg name)
                               "&plcontinue="
                               (fix-url-arg cont-str)))
           (rslt (read-json (get-webpage url)))
           (json (json-burrow rslt '(query pages #t links)))
           (cont (json-burrow rslt '(query-continue links plcontinue))))
      (cond ((false? json) '())
            ((false? cont) (map (lambda (x)
                                  (conv-string (json-burrow x '(title))))
                                json))
            (else (append (map (lambda (x)
                                 (conv-string (json-burrow x '(title))))
                               json)
                          (request-f-links-cont name cont))))))
           
    
  (define (request-b-links name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&format=json&list=backlinks&blnamespace=0&bllimit=500&bltitle=" (fix-url-arg name)))
           (json (json-burrow (read-json (get-webpage url)) '(query backlinks))))
      (if (false? json)
          '()
          (map (lambda (x)
                 (conv-string (json-burrow x '(title))))
               json))))
  
  (define (request-page name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&format=json&prop=revisions&rvprop=content&titles=" (fix-url-arg name)))
           (json (json-burrow (read-json (get-webpage url)) '(query pages #t revisions 1 *))))
      json))
  
  (define (request-picture name)
    (let* ((url  (string-append "http://en.wikipedia.org/w/api.php?action=query&prop=pageimages&format=json&pithumbsize=150&titles=" name))
           (json (json-burrow (read-json (get-webpage url)) '(query pages #t thumbnail source))))
      json))
  
  (define ops (hash
               'suggest suggest ; given a string, provide possible suggestions
               'f-links f-links ; given a list of names, provide a list of verified name/parent pairs out of each
               'b-links b-links ; given a list of names, provide a list of verified name/parent pairs in to each
               'picture picture ; given a string, load the article's first picture and return a bitmap of it
               ))
  
  (hash-ref ops msg #f))
  ;(hash-ref ops msg #f))
