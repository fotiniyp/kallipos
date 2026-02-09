#lang racket

;; Convert YAML metadata files to Racket hash format

(require racket/file
         racket/string
         racket/list
         racket/path)

;; Simple YAML parser for our format
(define (parse-yaml-file path)
  (define lines (file->lines path))
  (define result (make-hash))
  (for ([line lines])
    (define match (regexp-match #rx"^([a-z_]+):\\s*['\"]?([^'\"]*)['\"]?$" line))
    (when match
      (hash-set! result (second match) (third match))))
  result)

;; Convert figures
(define (convert-figures)
  (define figures-dir (build-path ".." "figures"))
  (define files (directory-list figures-dir))
  (define entries '())

  (for ([f files])
    (when (regexp-match #rx"\\.md$" (path->string f))
      (define path (build-path figures-dir f))
      (define id (regexp-replace #rx"\\.md$" (path->string f) ""))
      (define data (parse-yaml-file path))
      (define image-url (hash-ref data "image_url" ""))
      (define caption (hash-ref data "caption" ""))
      (define title (hash-ref data "title" ""))
      (define license (hash-ref data "license_text" ""))

      (set! entries
            (cons (format "   \"~a\"\n   (hash 'image_url \"~a\"\n         'caption \"~a\"\n         'title \"~a\"\n         'license_text \"~a\")\n"
                          id
                          (string-replace image-url "\"" "\\\"")
                          (string-replace caption "\"" "\\\"")
                          (string-replace title "\"" "\\\"")
                          (string-replace license "\"" "\\\""))
                  entries))))

  (define output
    (string-append
     "#lang racket/base\n\n"
     "(provide figures)\n\n"
     "(define figures\n  (hash\n"
     (string-join (reverse entries) "\n")
     "   ))\n"))

  (display-to-file output (build-path "figures" "fig-data.rkt") #:exists 'replace)
  (printf "Converted ~a figures\n" (length entries)))

;; Convert quotes
(define (convert-quotes)
  (define quotes-dir (build-path ".." "quotes"))
  (define files (directory-list quotes-dir))
  (define entries '())

  (for ([f files])
    (when (regexp-match #rx"\\.md$" (path->string f))
      (define path (build-path quotes-dir f))
      (define id (regexp-replace #rx"\\.md$" (path->string f) ""))
      (define data (parse-yaml-file path))
      (define caption (hash-ref data "caption" ""))
      (define person (hash-ref data "person" ""))

      (when (and (not (string=? caption ""))
                 (not (string=? person "")))
        (set! entries
              (cons (format "   \"~a\"\n   (hash 'caption \"~a\"\n         'person \"~a\")\n"
                            id
                            (string-replace caption "\"" "\\\"")
                            (string-replace person "\"" "\\\""))
                    entries)))))

  (define output
    (string-append
     "#lang racket/base\n\n"
     "(provide quotes)\n\n"
     "(define quotes\n  (hash\n"
     (string-join (reverse entries) "\n")
     "   ))\n"))

  (display-to-file output (build-path "quotes" "quotes-data.rkt") #:exists 'replace)
  (printf "Converted ~a quotes\n" (length entries)))

(convert-figures)
(convert-quotes)
(displayln "Done!")
