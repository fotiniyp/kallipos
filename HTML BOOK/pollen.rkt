#lang racket/base

(require pollen/decode
         pollen/tag
         pollen/setup
         racket/list
         racket/string
         racket/file
         racket/path
         txexpr
         "figures/fig-data.rkt"
         "quotes/quotes-data.rkt")

(provide (all-defined-out))

;; ============================================
;; PROJECT METADATA
;; ============================================

(define project-title "Ο Προγραμματισμός της Διάδρασης")
(define project-author "Κωνσταντίνος Χωριανόπουλος")
(define project-lang "el")

;; Greek figure label
(define figure-prefix "Εικόνα")

;; Figure counter for numbering
(define figure-counter 0)

(define (reset-figure-counter!)
  (set! figure-counter 0))

(define (next-figure-number!)
  (set! figure-counter (add1 figure-counter))
  figure-counter)

;; ============================================
;; HELPER: CHECK OUTPUT TARGET
;; ============================================

;; Use Pollen's built-in poly target detection
(define (latex-target?)
  (eq? (current-poly-target) 'ltx))

(define (html-target?)
  (eq? (current-poly-target) 'html))

;; Helper for converting content to string
(define (~a x)
  (cond
    [(string? x) x]
    [(list? x) (apply string-append (map ~a x))]
    [(txexpr? x) (apply string-append (map ~a (get-elements x)))]
    [else (format "~a" x)]))

;; ============================================
;; CUSTOM TAG FUNCTIONS (MULTI-TARGET)
;; ============================================

;; FIGURE TAG
(define (figure fig-id)
  (define fig-info (hash-ref figures fig-id #f))
  (cond
    [fig-info
     (define fig-num (next-figure-number!))
     (define img-url (hash-ref fig-info 'image_url ""))
     (define caption (hash-ref fig-info 'caption ""))
     (define title (hash-ref fig-info 'title ""))
     (define license (hash-ref fig-info 'license_text ""))
     (if (latex-target?)
         ;; LaTeX output
         (string-append
          "\n\\begin{figure}[htbp]\n"
          "\\centering\n"
          "\\includegraphics[width=\\textwidth,height=0.35\\textheight,keepaspectratio]{"
          (string-replace img-url "/images/" "images/") "}\n"
          "\\caption{" caption
          (if (string=? license "") "" (string-append " \\textit{(" license ")}"))
          "}\n"
          "\\label{fig:" fig-id "}\n"
          "\\end{figure}\n\n")
         ;; HTML output - use absolute path from root
         `(figure ((id ,(string-append "fig-" fig-id))
                   (class "book-figure"))
                  (img ((src ,img-url)
                        (alt ,title)))
                  (figcaption
                   (span ((class "figure-label"))
                         ,(string-append figure-prefix " " (number->string fig-num) ": "))
                   (span ((class "figure-caption")) ,caption)
                   ,(if (string=? license "")
                        ""
                        `(span ((class "figure-license")) ,(string-append " (" license ")"))))))
     ]
    [else
     (if (latex-target?)
         (string-append "% Figure not found: " fig-id "\n")
         `(span ((class "error")) ,(format "[Figure not found: ~a]" fig-id)))]))

;; EPIGRAPH TAG
(define (epigraph quote-id)
  (define quote-info (hash-ref quotes quote-id #f))
  (cond
    [quote-info
     (define text (hash-ref quote-info 'caption ""))
     (define person (hash-ref quote-info 'person ""))
     (if (latex-target?)
         ;; LaTeX output
         (string-append
          "\n\\begin{quote}\n"
          "\\textit{" text "}\n"
          "\\hfill --- " person "\n"
          "\\end{quote}\n\n")
         ;; HTML output
         `(blockquote ((class "epigraph"))
                      (p ,text)
                      (footer (cite ,person))))]
    [else
     (if (latex-target?)
         (string-append "% Quote not found: " quote-id "\n")
         `(span ((class "error")) ,(format "[Quote not found: ~a]" quote-id)))]))

;; INCLUDE TAG
(define here-dir (path-only (path->complete-path (syntax-source #'here))))
(define (include filename)
  (define filepath (build-path here-dir ".." "extras" (string-append filename ".md")))
  (if (file-exists? filepath)
      (if (latex-target?)
          (file->string filepath)
          `(div ((class "included-content")) ,(file->string filepath)))
      (if (latex-target?)
          (string-append "% Include file not found: " filename "\n")
          `(span ((class "error")) ,(format "[Include file not found: ~a]" filename)))))

;; FIGURE REFERENCE TAG
(define (figref fig-id)
  (if (latex-target?)
      (string-append figure-prefix "~\\ref{fig:" fig-id "}")
      `(a ((href ,(string-append "#fig-" fig-id))
           (class "figure-ref"))
          ,(string-append figure-prefix " " fig-id))))

;; CITATION TAG
(define (cite key)
  (if (latex-target?)
      (string-append "\\cite{" key "}")
      `(a ((href ,(string-append "#ref-" key))
           (class "citation"))
          ,(string-append "[" key "]"))))

;; FOOTNOTE TAG
(define (fn . content)
  (if (latex-target?)
      (string-append "\\footnote{" (~a content) "}")
      `(span ((class "footnote"))
             (sup ((class "fn-marker")) "*")
             (span ((class "fn-content")) ,@content))))

;; ============================================
;; STANDARD TYPOGRAPHY TAGS (MULTI-TARGET)
;; ============================================

(define (em . elements)
  (if (latex-target?)
      (string-append "\\emph{" (~a elements) "}")
      `(em ,@elements)))

(define (strong . elements)
  (if (latex-target?)
      (string-append "\\textbf{" (~a elements) "}")
      `(strong ,@elements)))

(define (code . elements)
  (if (latex-target?)
      (string-append "\\texttt{" (~a elements) "}")
      `(code ,@elements)))

(define (link url . text)
  (if (latex-target?)
      (string-append "\\href{" url "}{" (~a text) "}")
      `(a ((href ,url)) ,@text)))

;; Section headings
(define (chapter . title)
  (if (latex-target?)
      (string-append "\\chapter{" (~a title) "}\n\n")
      `(h1 ((class "chapter-title")) ,@title)))

(define (section . title)
  (if (latex-target?)
      (string-append "\n\\section{" (~a title) "}\n\n")
      `(h2 ((class "section-title")) ,@title)))

(define (subsection . title)
  (if (latex-target?)
      (string-append "\n\\subsection{" (~a title) "}\n\n")
      `(h3 ((class "subsection-title")) ,@title)))

;; Lists
(define (ul . items)
  (if (latex-target?)
      (string-append "\\begin{itemize}\n" (~a items) "\\end{itemize}\n\n")
      `(ul ,@items)))

(define (ol . items)
  (if (latex-target?)
      (string-append "\\begin{enumerate}\n" (~a items) "\\end{enumerate}\n\n")
      `(ol ,@items)))

(define (li . content)
  (if (latex-target?)
      (string-append "\\item " (~a content) "\n")
      `(li ,@content)))

;; Code blocks
(define (codeblock . content)
  (if (latex-target?)
      (string-append "\\begin{verbatim}\n" (~a content) "\n\\end{verbatim}\n\n")
      `(pre ((class "code-block"))
            (code ,@content))))

;; ============================================
;; ROOT FUNCTION (DECODE PIPELINE)
;; ============================================

(define block-level-tags '(h1 h2 h3 h4 h5 h6 ul ol li figure blockquote pre div section nav header footer))

(define (root . elements)
  (reset-figure-counter!)
  (if (latex-target?)
      ;; LaTeX: just join all content
      (~a elements)
      ;; HTML: use decode pipeline
      (txexpr 'div '((class "content"))
              (decode-elements elements
                               #:txexpr-elements-proc (lambda (es) (decode-paragraphs es #:force? #f))
                               #:exclude-tags block-level-tags
                               #:string-proc (compose1 smart-quotes smart-dashes)))))

;; ============================================
;; POLLEN SETUP MODULE
;; ============================================

(module setup racket/base
  (provide (all-defined-out))
  ;; HTML output only (ltx removed to prevent nav conflicts)
  (define poly-targets '(html))
  (define publish-directory "docs")
  (define server-extras-dir ".")
  (define block-tags '(figure blockquote pre root)))
