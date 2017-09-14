#lang racket

(require racket/system)
(require racket/trace)

(define (reassoc alist k v)
  (cons (cons k v)
        (remf (lambda (x) (eq? k (car x))) alist)))

;; (equal? '((:foo . 3) (:bar . 2)) (reassoc '((:foo . 1) (:bar . 2)) ':foo 3))

(define (assoc-cdr key alist)
  (let [(v (assoc key alist))]
    (when v
      (cdr v))))

;; (= 3 (assoc-cdr ':foo '((:foo . 3) (:bar . 2))))

(define (safely-take lst n)
  (if (<= n (length lst))
      (take lst n)
      lst))

;; (equal? '(:a :b) (safely-take '(:a :b :c) 2))
;; (equal? '(:a) (safely-take '(:a) 2))

(define (safely-first lst)
  (and lst (list? lst)
       (<= 1 (length lst))
       (first lst)))

(define (call-with-current-directory dir thunk)
  (if (false? dir)
      (thunk)
      (let ([wd (current-directory)])
        (dynamic-wind
          void
          (lambda ()
            (current-directory dir)
            (thunk))
          (lambda ()
            (current-directory wd))))))

(define (git-cmd cmd . args)
  (let* ((exit #t)
         (out (with-output-to-string
                (lambda ()
                  (let ([git (find-executable-path "git")])
                    (set! exit
                      (apply system* git cmd args)))))))
    `((:exit . ,exit)
      (:output . ,out))))

(define (process-output proc-result)
  (let ([exit (assoc-cdr ':exit proc-result)])
    (when (eq? #t exit)
      (let ([out (assoc-cdr ':output proc-result)])
        (when (string? out)
          (regexp-split #px"\n" out))))))

(define (git-tags #:max-tags [max-tags 10])
  (let ([tags (process-output
               (git-cmd "tag" "--sort=-creatordate"))])
    (when tags
      (safely-take tags max-tags))))

(define (git-empty-tree-hash)
  (let ([out (process-output
              (git-cmd "hash-object" "-t" "tree" "/dev/null"))])
    (when out
      (first out))))

;; (equal? "4b825dc642cb6eb9a060e54bf8d69288fbee4904" (git-empty-tree-hash))

(define (git-distance earlier . args)
  (let* ((later (or (safely-first args) "HEAD"))
         (out (process-output
               (git-cmd "rev-list"
                        (string-append earlier ".." later)
                        "--count"))))
    (string->number (safely-first out))))

(define (latest-semver-tag)
  (let ((tags (git-tags #:max-tags 1)))
    (when (and tags (< 0 (length tags)))
      (first tags))))

(define semver-regex (pregexp "(\\d+)\\.(\\d+)\\.(\\d+)"))

(define (index-as-num lst i)
  (string->number (list-ref lst i)))

(define (parse-semver s i)
  (let* ((parsed (regexp-match semver-regex s)))
    (and parsed
         `((:major . ,(index-as-num parsed 1))
           (:minor . ,(index-as-num parsed 2))
           (:patch . ,(+ i (index-as-num parsed 3)))))))

(define (initial-new-tag latest-tag)
  (let* ((earlier-commit (or latest-tag (git-empty-tree-hash)))
         (distance (git-distance earlier-commit))
         (increment (if (positive? distance) 1 0))
         (parsed-ver (parse-semver earlier-commit increment)))
    (or parsed-ver
        '((:major 0) (:minor 0) (:patch 0)))))

(define (last-release)
  (let* ((result (process-output
		  (git-cmd "describe" "--tags" "--match" "RELEASE-\\*")))
	 (parsed (and (not (void? result))
	  	      (regexp-match "RELEASE-(\\d+)\\.(\\d+).*" result))))
    (if parsed
	`((:release-major . ,(index-as-num parsed 1))
	  (:release-minor . ,(index-as-num parsed 2)))
	`((:release-major . 0) (:release-minor . 0)))))

(define (sync-local-tags-to-remote)
  (git-cmd "fetch" "--prune" "--tags"))

#|
(latest-semver-tag)
(trace call-with-current-directory)
(call-with-current-directory
 "/home/kasim/work/omnyway/pantheon"
 (lambda ()
   (initial-new-tag (latest-semver-tag))
   ;;(last-release)
))
|#
