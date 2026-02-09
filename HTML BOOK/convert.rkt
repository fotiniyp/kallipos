#lang racket

;; Convert kallipos markdown to Pollen markup

(require racket/file
         racket/string
         racket/list)

(define (convert-line line)
  ;; Convert headings
  (set! line (regexp-replace #rx"^# (.+)$" line "◊chapter{\\1}"))
  (set! line (regexp-replace #rx"^## (.+)$" line "◊section{\\1}"))
  (set! line (regexp-replace #rx"^### (.+)$" line "◊subsection{\\1}"))

  ;; Convert custom tags
  (set! line (regexp-replace* #rx"!\\[\\]\\(([^)]+)\\.md\\)\\{\\.epigraph\\}" line "◊epigraph[\"\\1\"]"))
  (set! line (regexp-replace* #rx"!\\[\\]\\(([^)]+)\\.md\\)\\{\\.include\\}" line "◊include[\"\\1\"]"))
  (set! line (regexp-replace* #rx"!\\[\\]\\(([^)]+)\\.md\\)\\{\\.figure\\}" line "◊figure[\"\\1\"]"))

  ;; Convert figure references: ^[@fig:id] -> ◊fn{◊figref["id"]}
  (set! line (regexp-replace* #rx"\\^\\[@fig:([^]]+)\\]" line "◊fn{◊figref[\"\\1\"]}"))

  ;; Convert multiple citations: ^[@key1, @key2] -> ◊fn{◊cite["key1"], ◊cite["key2"]}
  (set! line (regexp-replace* #rx"\\^\\[@([^],]+),\\s*@([^]]+)\\]" line "◊fn{◊cite[\"\\1\"], ◊cite[\"\\2\"]}"))

  ;; Convert single citations: ^[@key] -> ◊fn{◊cite["key"]}
  (set! line (regexp-replace* #rx"\\^\\[@([^]]+)\\]" line "◊fn{◊cite[\"\\1\"]}"))

  ;; Convert inline formatting
  (set! line (regexp-replace* #rx"\\*\\*([^*]+)\\*\\*" line "◊strong{\\1}"))
  (set! line (regexp-replace* #rx"\\*([^*]+)\\*" line "◊em{\\1}"))
  (set! line (regexp-replace* #rx"`([^`]+)`" line "◊code{\\1}"))

  line)

(define (convert-file input-path output-path)
  (define lines (file->lines input-path))
  (define converted (map convert-line lines))
  (define output (string-append "#lang pollen\n\n" (string-join converted "\n")))
  (display-to-file output output-path #:exists 'replace)
  (printf "Converted: ~a -> ~a\n" input-path output-path))

;; Convert all chapters
(define chapters '("pre" "intro" "ch01" "ch02" "ch03" "ch04" "ch05" "ch06" "ch07" "ch08" "web" "bio" "apx01"))

(for ([ch chapters])
  (define input (build-path ".." "text" (string-append ch ".txt")))
  (define output (build-path "chapters" (string-append ch ".poly.pm")))
  (when (file-exists? input)
    (convert-file input output)))

(displayln "Done!")
