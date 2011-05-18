(use hyde hyde-atom environments posix srfi-1 regex)

;; This has been taken from the gazette's hyde config
;; Thanks to Moritz Heidkamp for that

(define root-path (make-parameter (string-append (current-directory) "/" (output-dir) "/")))
(root-path "/~ckeen/blog/")

(define $ (environment-ref (page-eval-env) '$))

(define page-path (environment-ref (page-eval-env) 'page-path))
(define current-page (environment-ref (page-eval-env) 'current-page))

(define (page-updated page)
  (or ($ 'updated page) ($ 'date page)))

(define (sort-by pages accessor)
  (sort pages (lambda (p1 p2) (> (accessor p1) (accessor p2)))))

(define (pages-matching regex)
  (map cdr (filter (lambda (p) (string-match regex (car p)))
		   ((environment-ref (page-eval-env) 'pages)))))

(define (page-url #!optional (page (current-page)))
  (string-append (root-path) (page-path page)))

(define (neat-date d)
  (time->string (seconds->utc-time d) "%Y-%m-%d %Z"))

(define (count-tags pages)
  (let ((tags '()))
    (map (lambda (page)
           (map (lambda(t) (let ((new-v (or (alist-ref t tags equal?) '())))
                             (set! tags (alist-update! t (cons (list ($ 'title page) (page-url page)) new-v) tags equal?))))
                (or ($ 'tags page) '())))
         pages)
    tags))

(define (tag-class t)
  ; ranges are up-to limits, so tags with a count up to 3 are considered 'low'
  (let* ((ranges '((3 "low")
                   (5 "medium")
                   (100 "high")))
        (class (filter (lambda (a) (< (length (cdr t)) (car a))) ranges)))
    (string-append "tag-" (if (pair? class)
                              (cadar class)
                              (cadar ranges)))))

(define (navigation-links)
  (let ((nav '(("Home" "index.html")
              ("Tags" "tags.html")
              ("Archive" "archive.html"))))
    (map (lambda (l) `((a (@ (href ,(root-path)"/",(cdr l))) ,(car l)) " | ")) nav)))

(for-each (lambda (binding)
	    (apply environment-extend! (cons (page-eval-env) binding)))
	  `((page-updated ,page-updated)
            (neat-date ,neat-date)
            (count-tags ,count-tags)
            (max-articles ,(lambda () 5))
            (navigation-links ,navigation-links)
            (page-url ,page-url)
            (root-path ,root-path)
            (tag-class ,tag-class)
	    (all-pages ,(lambda ()
			   (sort-by (pages-matching "posts/.+") page-updated)))))
