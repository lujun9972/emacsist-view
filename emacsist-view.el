(defun emacsist-url (page)
  "return the url of emacsist.com"
  (format "http://emacsist.com/list/%d/com" page))

(defun emacsist-list (url)
  "fetch links from url"
  (let ((content (url-get-content-from-html url)))
    content))

(defun emacsist-link-p (tree)
  (ignore-errors
    (and (eq 'span (car tree))
         (equal '((class . "title")) (cadr tree)))))

(defun emacsist-extract-paper-link (line)
  (string-match "href=\"\\(http://emacsist.com/[0-9]+\\)\" data-wid=\"[0-9]+\">\\(.+\\)</a>" line)
  (let ((url (match-string-no-properties 1 line))
        (title (match-string-no-properties 2 line)))
    (propertize title 'help-echo url)))

(defun emacsist-extract-author-link (line)
  (string-match "href=\"\\(http://emacsist.com/[^\"]+\\)\" class=\"author\">\\(.+\\)</a>" line)
  (let ((url (match-string-no-properties 1 line))
        (author (match-string-no-properties 2 line)))
    (propertize author 'help-echo url)))

(defun emacsist-extract-links (&optional url)
  "extract links from HTML which is the source code of emacsist URL"
  (let* ((url (or url (emacsist-url 1)))
         (buf (url-retrieve-synchronously url))
         paper-link author-link entries)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (search-forward "<span class=\"title\">" nil t)
        (forward-line)
        (setq paper-link (emacsist-extract-paper-link (thing-at-point 'line)))
        (forward-line)
        (setq author-link (emacsist-extract-author-link (thing-at-point 'line)))
        (push (list nil (vector paper-link author-link)) entries)))
    (kill-buffer buf)
    (reverse entries)))

(emacsist-extract-links)


;; define emacsist-mode

(defun emacsist-view ()
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)));help-echo is also the url
    (eww-browse-url url)))

(define-derived-mode emacsist-mode tabulated-list-mode "emacsist-mode"
  "mode for viewing emacsist.com"
  (setq tabulated-list-format [("title" 60 nil)
                               ("author" 10 t)]
        tabulated-list-entries 'emacsist-extract-links)
  (tabulated-list-init-header)
  (define-key emacsist-mode-map "v" 'emacsist-view))


(defun list-emacsist ()
  "list paper in emacsist.com"
  (interactive)
  (switch-to-buffer (get-buffer-create "*emacsist*"))
  (emacsist-mode)
  (tabulated-list-print t))

