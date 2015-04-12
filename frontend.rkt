#lang racket

(require racket/gui)
(require "pathfinder.rkt")
(require "wikipedia.rkt")

(define (make-search-callback input-field choice-field)
  (define (handler button event)
    (if (equal? "" (send input-field get-value))
        (void)
        (let ((result ((wikipedia 'suggest) (send input-field get-value))))
          (cond ((string? result) (begin
                                    (send choice-field set (list result))
                                    (send choice-field select 0)))
                ((list? result) (begin
                                  (send choice-field set result)
                                  (send choice-field select 0)
                                  (send choice-field select 0 #f)))
                (else (void))))))
  handler)

(define (make-pathfind-callback type)
  (let ((worker-thread (void))
        (start #f)
        (end #f)
        (this (current-thread)))
    (define (button-callback button event)
      (if (and (void? worker-thread)
               (not (null? (send start-search get-selections)))
               (not (null? (send end-search   get-selections))))
          (begin
            (send button enable #f)
            (send result-timer start 500)
            (set! start (send start-search get-string (car (send start-search get-selections))))
            (set! end   (send end-search   get-string (car (send end-search   get-selections))))
            (set! worker-thread (thread worker-proc)))
          (void)))
    (define (timer-callback)
      (let ((recv (thread-try-receive)))
        (if (false? recv)
            (send result-timer start 500)
            (begin
              (send result-timer stop)
              (send go-button enable #t)
              (send output-list set '())
              (for-each (lambda (x)
                          (send output-list append (pathfinder->string x) x))
                        recv)))))
    (define (worker-proc)
      (thread-send this (pathfind wikipedia start end))
      (set! worker-thread (void)))
    (case type
      ((button) button-callback)
      ((timer)  timer-callback)
      (else (void)))))

(define (display-callback button event)
  (if (eq? (send event get-event-type) 'list-box-dclick)
      (begin
        (let* ((images (map (lambda (x)
                              (let ((img ((wikipedia 'picture) x)))
                                (make-object bitmap% (cadr img) (car img))))
                            (send output-list get-data (car (send output-list get-selections)))))
               (q-mark (make-object bitmap% (open-input-file "right-black.png" #:mode 'binary)'png/alpha))
               (width (max 794 (+ 100 (ceiling (inexact->exact (foldl (lambda (first rest)
                                                                        (if (= rest 0)
                                                                            (+ (send first get-width) rest)
                                                                            (+ (send first get-width) rest (* 1.5 (send q-mark get-width)))))
                                                                      0
                                                                      images))))))
               ;(height (+ 30 (foldl (lambda (first rest)
               ;                       (if (> (send first get-height) rest)
               ;                           (send first get-height)
               ;                           rest))
               ;                     0
               ;                     images)))
               (height 200)
               (bitmap-mk (make-object bitmap% width 200))
               (bitmap-dc (new bitmap-dc%
                               (bitmap bitmap-mk))))
          ;(send output-canvas flush)
          ;(send output-canvas min-client-width (max (ceiling (inexact->exact width)) 794))
          (send output-canvas min-width (ceiling (inexact->exact width)))
          (sleep 0.2)
          (foldl (lambda (first rest)
                   (if (not (= 50 rest))
                       (send bitmap-dc draw-bitmap q-mark (- rest (* 1.25 (send q-mark get-width))) (/ (- height (send q-mark get-height)) 2))
                       (void))
                   (send bitmap-dc draw-bitmap first rest (/ (- height (send first get-height)) 2))
                   ;(send bitmap-dc draw-bitmap text-bmp rest (/ (+ height (send first get-height)) 2))
                   (+ rest (send first get-width) (* 1.5 (send q-mark get-width))))
                 50
                 images)
          ;(send bitmap-dc set-background (make-object color% 0 0 0 1.0))
          ;(send bitmap-dc clear)
          ;(let ((scale-value (/ (send output-canvas get-width) (send bitmap-mk get-width))))
          ;  (display scale-value)
          ;  (display (real? scale-value))
          ;  (if (< scale-value 1)
          ;      (begin (send bitmap-dc set-scale scale-value scale-value) (display "hi"))
          ;      (void)))
          ;(send (send output-canvas get-dc)
          ;(send (send output-canvas get-dc)
          ;      draw-bitmap bitmap-mk
          ;      (/ (- (send output-canvas get-width)  width)  2)
          ;      (/ (- (send output-canvas get-height) height) 2))
          (send output-canvas set-label bitmap-mk)))
          ;(display "Done!")
      (void)))
  

(define window (new frame%
                    (label "WikiPath")
                    (stretchable-width #t)
                    (stretchable-height #t)
                    ))

(define main-panel (new vertical-panel%
                        (parent window)
                        ;(style '(border))
                        ;(min-width 800)
                        ;(min-height 600)
                        ;(stretchable-width #f)
                        ;(stretchable-height #f)
                        ))

(define output-panel (new vertical-panel%
                          (parent main-panel)
                          ;(style '(hscroll))
                          (min-width 800)
                          (min-height 300)
                          ;(stretchable-width #f)
                          ;(stretchable-height #f)
                          ))

(define output-panel-2 (new panel%
                            (parent output-panel)
                            (style '(hscroll border))
                            (horiz-margin 2)
                            (vert-margin 2)))

;(define output-canvas (new canvas%
;                   (parent output-panel-2)
;                   ;(style '(border hscroll))
;                   (horiz-margin 0)
;                   (vert-margin 0)
;                   (min-width 794)
;                   (stretchable-width #t)
;                   (min-height 200)))

(define output-background (make-object bitmap% 794 200))

(define output-background-dc (make-object bitmap-dc% output-background))

(send output-background-dc set-background "White")
(send output-background-dc clear)

(define output-canvas (new message%
                           (label output-background)
                           (parent output-panel-2)
                           (horiz-margin 0)
                           (vert-margin 0)
                           (min-width 794)
                           (min-height 200)
                           (stretchable-width #t)))


(define output-list-box (new vertical-panel%
                            (parent output-panel)
                            (style '(border))
                            (horiz-margin 2)
                            (vert-margin 0)
                            (min-height 100)))
                             

(define output-list (new list-box%
                         (label #f)
                         (choices '())
                         (parent output-list-box)
                         (callback display-callback)
                         (horiz-margin 0)
                         (vert-margin 0)))
                         

(define user-panel (new horizontal-panel%
                       (parent main-panel)
                       ;(style '(border))
                       ;(alignment '(center center))
                       (min-width 800)
                       (min-height 300)
                       (stretchable-width #f)
                       (stretchable-height #f)))

(define user-l-panel (new vertical-panel%
                          (parent user-panel)
                          ;(style '(border))
                          (stretchable-width #f)))

(define start-panel (new vertical-panel%
                         (parent user-l-panel)
                         ;(style '(border))
                         (alignment '(center center))
                         (min-width 500)
                         (min-height 60)
                         (stretchable-width #f)
                         (stretchable-height #t)))

(define start-timer (new timer%
                         (notify-callback (void))
                         (interval #f)
                         (just-once? #t)))

(define start-label (new message%
                       (label "Start:")
                       (parent start-panel)
                       (stretchable-width #t)))

(define start-field-box (new horizontal-panel%
                             (parent start-panel)
                             ;(style '(border))
                             (min-height 0)
                             (stretchable-height #f)))
  
(define start-field-txt (new text-field%
                             (label #f)
                             (parent start-field-box)
                             (stretchable-height #t)))
                             

(define start-search-box (new vertical-panel%
                              (parent start-panel)
                              (style '(border))
                              (horiz-margin 2)
                              (vert-margin 0)))

(define start-search (new list-box%
                          (label #f)
                          (choices '())
                          (parent start-search-box)
                          (callback (lambda (button event)
                                      (if (eq? (send event get-event-type) 'list-box-dclick)
                                          (send start-field-txt set-value (send start-search get-string (car (send start-search get-selections))))
                                          (void))))
                          (horiz-margin 0)
                          (vert-margin 0)
                          (stretchable-width #t)
                          (stretchable-height #t)))

(define start-field-btn (new button%
                             (label "Search")
                             (parent start-field-box)
                             (callback (make-search-callback start-field-txt start-search))))

(define spacer-panel-1 (new vertical-panel%
                            (parent user-l-panel)
                            (min-height 4)
                            (stretchable-height #f)))

(define end-panel (new vertical-panel%
                         (parent user-l-panel)
                         ;(style '(border))
                         ;(alignment '(center center))
                         (min-width 500)
                         (min-height 60)
                         (stretchable-width #f)
                         (stretchable-height #t)))

(define end-label (new message%
                       (label "End:")
                       (parent end-panel)
                       (stretchable-width #t)))

(define end-field-box (new horizontal-panel%
                           (parent end-panel)
                           ;(style '(border))
                           (min-height 0)
                           (stretchable-height #f)))
  
(define end-field-txt (new text-field%
                           (label #f)
                           (parent end-field-box)
                           (stretchable-height #t)))

(define end-search-box (new vertical-panel%
                            (parent end-panel)
                            (style '(border))
                            (horiz-margin 2)
                            (vert-margin 0)))

(define end-search (new list-box%
                          (label #f)
                          (choices '())
                          (parent end-search-box)
                          (horiz-margin 0)
                          (vert-margin 0)
                          (stretchable-width #t)
                          (stretchable-height #t)))

(define end-field-btn (new button%
                           (label "Search")
                           (parent end-field-box)
                           (callback (make-search-callback end-field-txt end-search))))

(define spacer-panel-2 (new vertical-panel%
                            (parent user-l-panel)
                            (min-height 2)
                            (stretchable-height #f)))

(define go-panel (new vertical-panel%
                         (parent user-panel)
                         ;(style '(border))
                         (alignment '(center center))
                         ;(min-width 300)
                         (min-height 60)
                         ;(stretchable-width #f)
                         (stretchable-height #f)))

(define go-button (new button%
                       (label "Search")
                       (parent go-panel)
                       (callback (make-pathfind-callback 'button))
                       (min-width 150)
                       (min-height 80)))

(define result-timer (new timer%
                          (notify-callback (make-pathfind-callback 'timer))
                          (interval #f)
                          (just-once? #f)))

;(send output-list append "A" '())

(send window show #t)

;(send window show #t)
