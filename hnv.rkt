#lang racket/gui

(require canvas-list net/sendurl net/url racket/draw)
(require "hn.rkt")

(define face "Fira Sans")

(define bg-color (make-color #xed #xec #xeb))
(define story-color (make-color #xff #xff #xff))
(define sel-color (make-color #x48 #x47 #x46))
(define url-color (make-color #x00 #x7a #xff))
(define score-color (make-color #xff #x95 #x02))
(define info-color (make-color #x8e #x8e #x93))
(define by-color (make-color #xff #xcc #x00))
(define title-color (make-color #x27 #x27 #x27))
(define category-color (make-color #x00 #x00 #x00))
(define transparent-color (make-color #xff #xff #xff 0.0))
(define title-font (make-font #:face face #:size 13 #:weight 'heavy))
(define minor-font (make-font #:face face #:size 10 #:weight 'bold))
(define category-font (make-font #:face face #:size 14 #:weight 'bold))

(define (open-item canvas item event)
  (send-url (story-url item)))

(define (open-comments story-list)
  (let ([story (send story-list get-selected-item)])
    (when story
      (send-url (story-comments-url story)))))

(define (draw-colored-text dc x y text-list)
  (for ([text text-list])
    (match text
      [(list color s)
       (send dc set-text-foreground color)
       (send dc draw-text s x y)
       (let-values ([(w h b _) (send dc get-text-extent s)])
         (set! x (+ x w)))])))

(define (paint-story canvas item state dc w h)
  (let ([score-text (~a (hash-ref item 'score 0))]
        [domain (url-host (string->url (story-url item)))]
        [selected? (member state '(hover selected))]
        [info (format " \u2022 ~a comments \u2022 ~a"
                      (hash-ref item 'descendants 0)
                      (story-age item))])
    (send dc set-brush story-color 'solid)
    (send dc set-pen (if selected? url-color story-color) 3 'solid)
    (send dc draw-rounded-rectangle 10 5 (- w 20) (- h 10))
    (send dc set-clipping-rect 10 5 (- w 40) (- h 10))
    (send dc set-text-foreground title-color)
    (send dc set-font title-font)
    (send dc draw-text (hash-ref item 'title) 30 10)
    (send dc set-font minor-font)
    (send dc set-text-foreground url-color)
    (send dc draw-text domain 30 32)
    (draw-colored-text dc 30 52 `((,score-color ,score-text)
                                  (,info-color " \u2022 Posted by ")
                                  (,by-color ,(hash-ref item 'by))
                                  (,info-color ,info)))))

(define (paint-category canvas item state dc w h)
  (send dc set-font category-font)
  (send dc set-text-foreground (if (eq? state 'selected) story-color category-color))
  (send dc draw-text (~a item) 30 8)
  (when (eq? state 'selected)
    (send dc set-pen story-color 1 'solid)
    (send dc draw-line 0 (sub1 h) w (sub1 h))
    (send dc set-pen title-color 1 'solid)
    (send dc draw-line 0 0 w 0)))

(define (select-category story-list item)
  (let-values ([(stories ids) (match item
                                ['Top (top-stories)]
                                ['New (new-stories)]
                                ['Show (show-stories)]
                                ['Ask (ask-stories)]
                                ['Best (best-stories)]
                                ['Jobs (job-stories)])])
    (send story-list set-items (vector-filter identity stories))))

(define (launch)
  (letrec ([frame (new frame%
                       [label "Hacker News"]
                       [min-width 320]
                       [min-height 200]
                       [width 680]
                       [height 680])]
           [pane (new horizontal-pane% [parent frame])]
           [categories (new canvas-list%
                            [parent pane]
                            [min-width 160]
                            [stretchable-width #f]
                            [item-height 40]
                            [force-selection #t]
                            [item-color (make-color #xe1 #xe0 #xdf)]
                            [hover-color (make-color #xd2 #xd0 #xcf)]
                            [selection-color (make-color #x3c #x91 #xfc)]
                            [alt-color #f]
                            [items '(Top New Show Ask Best Jobs)]
                            [paint-item-callback paint-category]
                            [selection-callback (λ (canvas item event)
                                                  (select-category story-list item))])]
           [story-list (new canvas-list%
                            [parent pane]
                            [item-height 80]
                            [item-color bg-color]
                            [alt-color #f]
                            [hover-color #f]
                            [selection-color #f]
                            [paint-item-callback paint-story]
                            [action-callback open-item]
                            [context-action-callback (λ (canvas story event)
                                                       (let ([x (send event get-x)]
                                                             [y (send event get-y)])
                                                         (send canvas popup-menu context-menu x y)))])]
           [context-menu (new popup-menu% [title "Story"])]
           [open-mi (new menu-item%
                         [parent context-menu]
                         [label "Open Story..."]
                         [callback (λ (item event)
                                     (let ([story (send story-list get-selected-item)])
                                       (open-item story-list story #f)))])]
           [comment-mi (new menu-item%
                            [parent context-menu]
                            [label "Open Comments..."]
                            [callback (λ (item event)
                                        (open-comments story-list))])]
           [sep (new separator-menu-item% [parent context-menu])]
           [refresh-mi (new menu-item%
                            [parent context-menu]
                            [label "Refresh Stories"]
                            [callback (λ (item event)
                                        (let ([cat (send categories get-selected-item)])
                                          (select-category story-list cat)))])]
           [more-mi (new menu-item%
                         [parent context-menu]
                         [label "Load More"]
                         [callback (λ (item event)
                                     (void))])])
    (send frame show #t)
    (send categories select-first)))

(launch)
