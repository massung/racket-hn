#lang racket

(require json net/http-client)

(define home "https://news.ycombinator.com")
(define firebase "hacker-news.firebaseio.com")

(define story-cache (make-hash))

(define (hn-get path #:cache [cache #f] #:retry? [retry #t])
  (with-handlers ([exn? (λ (ex)
                          (if retry
                              (hn-get path #:cache cache #:retry? #f)
                              (raise ex)))])
    (let* ([uri (format "/v0/~a.json" path)]
           [fetch (λ ()
                    (let-values ([(status headers port)
                                  (http-sendrecv firebase uri #:ssl? #t)])
                      (read-json port)))])
      (if cache
          (hash-ref! cache uri fetch)
          (fetch)))))

(define (hn-story id)
  (hn-get (format "item/~a" id) #:cache story-cache))

(define (hn-stories ids)
  (let ([stories (make-vector (sequence-length ids) #f)]
        [mailbox (current-thread)])
    (for ([id ids] [index (in-naturals)])
      (thread (λ ()
                (let ([story (hn-story id)])
                  (thread-send mailbox (list index story))))))
    (for ([_ ids])
      (match (thread-receive)
        [(list index story)
         (unless (eq? story 'null)
           (vector-set! stories index story))]))
    stories))

(define (get-story-list ids [n 30])
  (let-values ([(head tail) (split-at ids (min n (sequence-length ids)))])
    (values (hn-stories head) tail)))

(define (top-stories) (get-story-list (hn-get "topstories")))
(define (new-stories) (get-story-list (hn-get "newstories")))
(define (best-stories) (get-story-list (hn-get "beststories")))
(define (show-stories) (get-story-list (hn-get "showstories")))
(define (ask-stories) (get-story-list (hn-get "askstories")))
(define (job-stories) (get-story-list (hn-get "jobstories")))

(define (story-comments-url story)
  (format "~a/item?id=~a" home (hash-ref story 'id)))

(define (story-url story)
  (let ([url (hash-ref story 'url #f)])
    (or url (story-comments-url story))))

(define (story-age story)
  (let ([age (quotient (- (current-seconds) (hash-ref story 'time)) 60)])
    (cond
      [(< age 2)     "just now"]
      [(< age 60)    (format "~am" age)]
      [(< age 1440)  (format "~ah" (quotient age 60))]
      [(< age 10080) (format "~ad" (quotient age 1440))]
      [else          (format "~aw" (quotient age 10080))])))

(provide (all-defined-out))
