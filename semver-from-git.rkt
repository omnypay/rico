#lang racket

(require racket/system)

(define (debug tag v msg)
  (displayln (~a tag "=>" v " --- " msg)))

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
		    (when (not (path-string? git))
		      (raise 'git-command-not-found))
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

(define (nth-as-num lst i)
  (string->number (list-ref lst i)))

(define (semver-as-list major minor patch)
  `((:major . ,major)
    (:minor . ,minor)
    (:patch . ,patch)))

(define (initial-new-tag latest-tag)
  (let* ((earlier-commit (or latest-tag (git-empty-tree-hash)))
         (distance (git-distance earlier-commit))
         (increment (if (positive? distance) 1 0))
         (parsed (regexp-match semver-regex earlier-commit)))
    (if parsed
	(semver-as-list (nth-as-num parsed 1)
			(nth-as-num parsed 2)
			(+ increment (nth-as-num parsed 3)))
	(semver-as-list 0 0 0))))

(define release-regex (pregexp "RELEASE-(\\d+)\\.(\\d+).*"))

(define (last-release)
  (let* ((result (safely-first
		  (process-output
		   (git-cmd "describe" "--tags" "--match" "RELEASE-*"))))
	 (parsed (and (not (void? result))
	  	      (regexp-match release-regex result))))
    (if parsed
	`((:release-major . ,(nth-as-num parsed 1))
	  (:release-minor . ,(nth-as-num parsed 2)))
	`((:release-major . 0) (:release-minor . 0)))))

(define (sync-remote-tags)
  (displayln "Syncing local/remote tags by running 'git fetch --prune --tags' ...")
  (git-cmd "fetch" "--prune" "--tags")
  (displayln "Synced local/remote tags."))

(define (semver-as-string semver)
  (string-append (number->string (assoc-cdr ':major semver))
		 "."
		 (number->string (assoc-cdr ':minor semver))
		 "."
		 (number->string (assoc-cdr ':patch semver))))

(define (final-semver-value semver release)
  (let* ((major (assoc-cdr ':major semver))
	 (minor (assoc-cdr ':minor semver))
	 (patch (assoc-cdr ':patch semver))
	 (final-major (max major (assoc-cdr ':release-major release)))
	 (final-minor (max minor (assoc-cdr ':release-minor release))))
    (if (or (> final-major major) (> final-minor minor))
	(semver-as-list final-major final-minor 0)
	(semver-as-list major minor patch))))

(define (current-branch)
  (safely-first
   (process-output
    (git-cmd "rev-parse" "--abbrev-ref" "HEAD"))))

(define (branch-semver-string final-semver-value)
  (let* ((branch (current-branch)))
    (when (verbose?)
      (debug "branch" branch "from running git rev-parse --abbrev-ref HEAD"))
    (if (not (string=? "master" branch))
	(string-append branch "-" final-semver-value)
	final-semver-value)))

(define (new-semver)
  (let* ((initial-tag (initial-new-tag (latest-semver-tag)))
	 (final-semver-value (final-semver-value initial-tag (last-release)))
	 (final-semver-string (semver-as-string final-semver-value))
	 (final-result (branch-semver-string final-semver-string)))
    (when (verbose?)
      (debug "initial-tag" initial-tag "Result from initial-new-tag fn")
      (debug "final-semver-string"
	     final-semver-string
	     "Result from (semver-as-string final-semver-value)")
      (debug "final-result"
	     final-result
	     "Result from (branch-semver-string final-semver-string)"))
    final-result))

#|
(call-with-current-directory
 "/path/to/git/repo"
 (lambda ()
   ;;(initial-new-tag (latest-semver-tag))
   ;;(last-release)
   (new-semver)
))
|#

;; command line parsing

(define sync? (make-parameter #f))
(define stdout? (make-parameter #f))
(define verbose? (make-parameter #f))

(define main
  (command-line
   #:once-each
   [("-v") "Run in verbose mode for debugging " (verbose? #t)]
   [("-s") "Sync local/remove tags" (sync? #t)]
   [("-n") "Only write to stdout. No output file" (stdout? #t)]
   #:args args
   (and (sync?)
	(sync-remote-tags))
   (let* ((semver (new-semver))
	  (semver-file (safely-first args)))
     (if (stdout?)
	 (displayln semver)
	 (with-output-to-file (or semver-file "SEMVER")
	   (lambda () (printf semver))
	   #:exists 'replace)))))

