(defvar emacsist-current-page 1)

(defun emacsist-url (page)
  "return the url of emacsist.com"
  (format "http://emacsist.com/list/%d/com" page))

(defun emacsist-list (url)
  "fetch links from url"
  (let ((content (url-get-content-from-html url)))
    content))

(defun emacsist-extract-paper-link (line)
  (string-match "href=\"\\(http://emacsist.com/[0-9]+\\)\" data-wid=\"[0-9]+\">\\(.+\\)</a>" line)
  (let ((url (match-string-no-properties 1 line))
        (title (decode-coding-string (match-string-no-properties 2 line) 'utf-8)))
    (propertize title 'help-echo url)))

(defun emacsist-extract-author-link (line)
  (string-match "href=\"\\(http://emacsist.com/[^\"]+\\)\" class=\"author\">\\(.+\\)</a>" line)
  (let ((url (match-string-no-properties 1 line))
        (author (decode-coding-string (match-string-no-properties 2 line) 'utf-8)))
    (propertize author 'help-echo url)))

(defun emacsist-extract-links (&optional url)
  "extract links from HTML which is the source code of emacsist URL"
  (let* ((url (or url (emacsist-url emacsist-current-page)))
         (buf (url-retrieve-synchronously url))
         paper-link author-link entries)
    (unless buf
      (error "fetch % failed" url))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (search-forward "<span class=\"title\">" nil t)
        (forward-line)
        (setq paper-link (emacsist-extract-paper-link (thing-at-point 'line)))
        (forward-line)
        (setq author-link (emacsist-extract-author-link (thing-at-point 'line)))
        (push (list nil (vector paper-link author-link)) entries)))
    (kill-buffer buf)
    (unless entries
      (error "NO MORE DATA"))
    (reverse entries)))

(defun emacsist--select-or-create-buffer-window (buffer-or-name)
  "若frame中有显示`buffer-or-name'的window,则选中该window,否则创建新window显示该buffer"
  (let ((buf (get-buffer-create buffer-or-name)))
    (unless (get-buffer-window buf)
      (split-window)
      (switch-to-buffer buf))
    (select-window (get-buffer-window buf))))

;; define emacsist-mode

(defun emacsist-eww-view ()
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)));help-echo is also the url
    (emacsist--select-or-create-buffer-window "*eww*")
    (eww-browse-url url)))

(defun emacsist-browser-view ()
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)));help-echo is also the url
    (browse-url url)))

(defun emacsist-next-page (&optional N)
  (interactive)
  (let ((N (or N 1)))
    (setq emacsist-current-page (+ emacsist-current-page N))
    (condition-case err
	(list-emacsist)
      (error (setq emacsist-current-page (- emacsist-current-page 1))
	     (signal (car err) (cdr err))))))

(defun emacsist-previous-page (&optional N)
  (interactive)
  (let ((N (or N 1)))
    (setq emacsist-current-page (- emacsist-current-page N))
    (when (< emacsist-current-page 0)
      (setq emacsist-current-page 0))
    (list-emacsist)))

(define-derived-mode emacsist-mode tabulated-list-mode "emacsist-mode"
  "mode for viewing emacsist.com"
  (setq tabulated-list-format [("title" 60 nil)
			       ("author" 10 t)]
	tabulated-list-entries 'emacsist-extract-links)
  (tabulated-list-init-header)
  (define-key emacsist-mode-map (kbd "v") 'emacsist-eww-view)
  (define-key emacsist-mode-map (kbd "<RET>") 'emacsist-browser-view)
  (define-key emacsist-mode-map (kbd "<down-mouse-1>") 'emacsist-browser-view)
  (define-key emacsist-mode-map (kbd "<next>") 'emacsist-next-page)
  (define-key emacsist-mode-map (kbd "<prior>") 'emacsist-previous-page)
  )

;;;###autoload
(defun list-emacsist ()
  "list paper in emacsist.com"
  (interactive)
  (switch-to-buffer (get-buffer-create "*emacsist*"))
  (emacsist-mode)
  (tabulated-list-print t))

