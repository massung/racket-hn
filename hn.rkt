#lang racket

(require json net/http-client)

(define home "https://news.ycombinator.com")
(define firebase "hacker-news.firebaseio.com")

(define story-cache (make-hash))

(define (hn-get path #:cache [cache #f] #:retry? [retry #t] #:force [force #f])
  (with-handlers ([exn? (λ (ex)
                          (if retry
                              (hn-get path #:cache cache #:retry? #f)
                              (raise ex)))])
    (let* ([uri (format "/v0/~a.json" path)]
           [fetch (λ ()
                    (let-values ([(status headers port)
                                  (http-sendrecv firebase uri #:ssl? #t)])
                      (read-json port)))])

      ; force reload the uri
      (when force
        (hash-remove! cache uri))

      ; lookup in cache or just return it
      (if cache
          (hash-ref! cache uri fetch)
          (fetch)))))

(define (hn-story id #:force [force #f])
  (hn-get (format "item/~a" id) #:cache story-cache #:force force))

(define (hn-stories ids #:force [force #f])
  (let ([stories (make-vector (sequence-length ids) #f)]
        [mailbox (current-thread)])
    (for ([id ids] [index (in-naturals)])
      (thread (λ ()
                (let ([story (hn-story id #:force force)])
                  (thread-send mailbox (list index story))))))
    (for ([_ ids])
      (match (thread-receive)
        [(list index story)
         (unless (eq? story 'null)
           (vector-set! stories index story))]))
    stories))

(define (get-story-list ids #:n [n 30] #:force [force #f])
  (let-values ([(head tail) (split-at ids (min n (sequence-length ids)))])
    (values (hn-stories head #:force force) tail)))

(define (top-stories #:force [force #f])
  (get-story-list (hn-get "topstories") #:force force))
(define (new-stories #:force [force #f])
  (get-story-list (hn-get "newstories") #:force force))
(define (best-stories #:force [force #f])
  (get-story-list (hn-get "beststories") #:force force))
(define (show-stories #:force [force #f])
  (get-story-list (hn-get "showstories") #:force force))
(define (ask-stories #:force [force #f])
  (get-story-list (hn-get "askstories") #:force force))
(define (job-stories #:force [force #f])
  (get-story-list (hn-get "jobstories") #:force force))

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
