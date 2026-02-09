<!DOCTYPE html>
<html lang="◊|project-lang|">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>◊|project-title|</title>
    <link rel="stylesheet" href="/styles.css">
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=GFS+Neohellenic:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
</head>
<body>
    <nav class="book-nav">
        <a href="/index.html" class="nav-home">◊|project-title|</a>
        <div class="nav-links">
            ◊(let ([prev (previous here)])
               (if prev
                   (string-append "<a href=\"" (symbol->string prev) "\" class=\"nav-prev\">← Προηγούμενο</a>")
                   ""))
            ◊(let ([nxt (next here)])
               (if nxt
                   (string-append "<a href=\"" (symbol->string nxt) "\" class=\"nav-next\">Επόμενο →</a>")
                   ""))
        </div>
    </nav>

    <main class="chapter-content">
        ◊(->html doc)
    </main>

    <footer class="book-footer">
        <p>◊|project-author|</p>
        <nav class="footer-nav">
            ◊(let ([prev (previous here)])
               (if prev
                   (string-append "<a href=\"" (symbol->string prev) "\" class=\"nav-prev\">← Προηγούμενο</a>")
                   ""))
            ◊(let ([nxt (next here)])
               (if nxt
                   (string-append "<a href=\"" (symbol->string nxt) "\" class=\"nav-next\">Επόμενο →</a>")
                   ""))
        </nav>
    </footer>
</body>
</html>
