;; -*- lexical-binding: t; -*-

(defun my-icomplete-exit-minibuffer-with-input ()
  "Exit the minibuffer with the current input, without forcing completion."
  (interactive)
  (exit-minibuffer))

(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there are only two windows, jump directly to the other window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (if (= (length window-list) 2)
        ;; If there are only two windows, switch to the other one directly.
        (select-window (other-window-for-scrolling))
      ;; Otherwise, show the key selection interface.
      (let* ((my/quick-window-overlays nil)
             (sorted-windows (sort window-list
                                   (lambda (w1 w2)
                                     (let ((edges1 (window-edges w1))
                                           (edges2 (window-edges w2)))
                                       (or (< (car edges1) (car edges2))
                                           (and (= (car edges1) (car edges2))
                                                (< (cadr edges1) (cadr edges2))))))))
             (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
                                    (length sorted-windows)))
             (window-map (cl-pairlis window-keys sorted-windows)))
        (setq my/quick-window-overlays
              (mapcar (lambda (entry)
                        (let* ((key (car entry))
                               (window (cdr entry))
                               (start (window-start window))
                               (overlay (make-overlay start start (window-buffer window))))
                          (overlay-put overlay 'after-string 
                                       (propertize (format "[%s]" key)
                                                   'face '(:foreground "white" :background "blue" :weight bold)))
                          (overlay-put overlay 'window window)
                          overlay))
                      window-map))
        (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
          (mapc #'delete-overlay my/quick-window-overlays)
          (setq my/quick-window-overlays nil)
          (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)))))))

(defun my/recentf-open (file)
  "Prompt for FILE in `recentf-list' and visit it.
Enable `recentf-mode' if it isn't already."
  (interactive
   (list
    (progn (unless recentf-mode (recentf-mode 1))
           (completing-read "Open recent file: " recentf-list nil t))))
  (when file
    (funcall recentf-menu-action file)))

(defun my/rainbow-mode ()
  "Overlay colors represented as hex values in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max))
  (let ((hex-color-regex "#[0-9a-fA-F]\\{3,6\\}"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hex-color-regex nil t)
        (let* ((color (match-string 0))
               (overlay (make-overlay (match-beginning 0) (match-end 0))))
          (if (string-greaterp color "#888888")
              (overlay-put overlay 'face `(:background ,color :foreground "black"))
            (overlay-put overlay 'face `(:background ,color :foreground "white"))))))))
;;
(defun my/rainbow-mode-clear ()
  "Remove all hex color overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max)))

(defun toggle-centered-buffer ()
  "Toggle center alignment of the buffer by adjusting window margins based on the fill-column."
  (interactive)
  (let* ((current-margins (window-margins))
         (margin (if (or (equal current-margins '(0 . 0))
                         (null (car (window-margins))))
                     (/ (- (window-total-width) fill-column) 2)
                   0)))
    (visual-line-mode 1)
    (set-window-margins nil margin margin)))

(defun my/find-file ()
  "Find file from current directory in many different ways."
  (interactive)
  (let* ((find-options (delq nil
                             (list (when (executable-find "find")
                                     '("find -type f -printf \"$PWD/%p\\0\"" . :string))
                                   (when (executable-find "fd")
                                     '("fd --absolute-path --type f -0" . :string))
                                   (when (executable-find "rg")
                                     '("rg --follow --files --null" . :string))
                                   (when (fboundp 'find-name-dired)
                                     '("find-name-dired" . :command)))))
         (selection (completing-read "Select: " find-options))
         file-list
         file)
    (pcase (alist-get selection find-options nil nil #'string=)
      (:command
       (call-interactively (intern selection)))
      (:string
       (setq file-list (split-string (shell-command-to-string selection) "\0" t))
       (setq file (completing-read
                   (format "Find file in %s: "
                           (abbreviate-file-name default-directory))
                   file-list))))
    (when file (find-file (expand-file-name file)))))

(defun my/sync-tab-bar-to-theme (&optional color)
  "Synchronize tab-bar faces with the current theme, and set
mode-line background color interactively using `read-color`
if COLOR is not provided as an argument."
  (interactive (list (when current-prefix-arg (read-color "Color: "))))
  ;; Determine the color to use
  (let ((selected-color (or color (read-color "Select mode-line background color: "))))
    (set-hl-line-darker-background)
    (set-face-attribute 'mode-line nil :height 120 :underline nil :overline nil :box nil
                        :background selected-color :foreground "#000000")
    (set-face-attribute 'mode-line-inactive nil :height 120 :underline nil :overline nil
                        :background "#000000" :foreground "#aaaaaa")
    (let ((default-bg (face-background 'default))
          (default-fg (face-foreground 'default))
          (default-hl (face-background 'highlight))
          (inactive-fg (face-foreground 'mode-line-inactive)))
      (custom-set-faces
       `(vertical-border ((t (:foreground ,(darken-color default-fg 60)))))
       `(window-divider ((t (:foreground ,(darken-color default-fg 60)))))
       `(fringe ((t (:foreground ,default-bg :background ,default-bg))))
       `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
       `(tab-bar-tab ((t (:inherit 'highlight :background ,selected-color :foreground "#000000"))))
       `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg
                                            :box (:line-width 2 :color ,default-bg :style released-button)))))))))

(defun my/grep (search-term &optional directory glob)
  "Run ripgrep (rg) with SEARCH-TERM and optionally DIRECTORY and GLOB.
  If ripgrep is unavailable, fall back to Emacs's rgrep command. Highlights SEARCH-TERM in results.
  By default, only the SEARCH-TERM needs to be provided. If called with a
  universal argument, DIRECTORY and GLOB are prompted for as well."
  (interactive
   (let ((univ-arg current-prefix-arg))
     (list
      (read-string "Search for: ")
      (when univ-arg (read-directory-name "Directory: "))
      (when univ-arg (read-string "File pattern (glob, default: ): " nil nil "")))))
  (let* ((directory (expand-file-name (or directory default-directory)))
         (glob (or glob ""))
         (buffer-name "*grep*"))
    (if (executable-find "rg")
        (let* ((rg-command (format "rg --color=never --max-columns=500 --column --line-number --no-heading --smart-case -e %s --glob %s %s"
                                   (shell-quote-argument search-term)
                                   (shell-quote-argument glob)
                                   directory))
               (debug-output (shell-command-to-string (format "rg --debug --files %s" directory)))
               (ignore-files (when (string-match "ignore file: \\(.*?\\.ignore\\)" debug-output)
                               (match-string 1 debug-output)))
               (raw-output (shell-command-to-string rg-command))
               (formatted-output
                (when (not (string-empty-p raw-output))
                  (concat
                   (format "[s] Search:    %s\n[d] Directory: %s\n" search-term directory)
                   (format "[o] Glob:      %s\n" glob)
                   (if ignore-files (format "%s\n" ignore-files) "")
                   "\n"
                   (replace-regexp-in-string (concat "\\(^" (regexp-quote directory) "\\)") "./" raw-output)))))
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name))
          (with-current-buffer (get-buffer-create buffer-name)
            (setq default-directory directory)
            (erase-buffer)
            (insert (or formatted-output "No results found."))
            (insert "\nripgrep finished.")
            (goto-char (point-min))
            (when formatted-output
              (let ((case-fold-search t))
                (while (search-forward search-term nil t)
                  (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                               'face '(:slant italic :weight bold :underline t)))))
            (grep-mode)
            (pop-to-buffer buffer-name)
            (goto-char (point-min))
            (message "ripgrep finished.")))
      (progn
        (setq default-directory directory)
        (message (format "%s : %s : %s" search-term glob directory))
        (rgrep search-term  (if (string= "" glob) "*" glob) directory)))
    (with-current-buffer "*grep*"
      (local-set-key (kbd "d") (lambda () 
                                 (interactive)
                                 (my/grep search-term 
                                          (read-directory-name "New search directory: ")
                                          glob)))
      (local-set-key (kbd "s") (lambda () 
                                 (interactive)
                                 (my/grep (read-string "New search term: ")
                                          directory
                                          glob)))
      (local-set-key (kbd "o") (lambda () 
                                 (interactive)
                                 (my/grep search-term
                                          directory
                                          (read-string "New glob: "))))
      (local-set-key (kbd "g") (lambda () 
                                 (interactive)
                                 (my/grep search-term directory glob))))))

(setq ispell-local-dictionary "en_GB")
(setq ispell-program-name "hunspell")
(setq dictionary-default-dictionary "*")
(setq dictionary-server "dict.org")
(setq dictionary-use-single-buffer t)
(define-prefix-command 'my-spell-prefix-map)
(global-set-key (kbd "C-c s") 'my-spell-prefix-map)
(global-set-key (kbd "C-c s s") #'(lambda()(interactive)
                                    (flyspell-buffer)
                                    (call-interactively 'flyspell-mode)))
(global-set-key (kbd "C-c s d") #'dictionary-lookup-definition)
(global-set-key (kbd "C-0") #'ispell-word)

(require 'cl-lib)
(require 'color)
;;
(defun my/color-hex-to-rgb (hex-color)
  "Convert a HEX-COLOR string to a list of RGB values."
  (unless (string-match "^#[0-9a-fA-F]\\{6\\}$" hex-color)
    (error "Invalid hex color: %s" hex-color))
  (mapcar (lambda (x) (/ (string-to-number x 16) 255.0))
          (list (substring hex-color 1 3)
                (substring hex-color 3 5)
                (substring hex-color 5 7))))
;;
(defun my/color-rgb-to-hex (rgb)
  "Convert a list of RGB values to a hex color string."
  (format "#%02x%02x%02x"
          (round (* 255 (nth 0 rgb)))
          (round (* 255 (nth 1 rgb)))
          (round (* 255 (nth 2 rgb)))))
;;
(defun my/color-adjust-brightness (hex-color delta)
  "Adjust the brightness of HEX-COLOR by DELTA (-1.0 to 1.0)."
  (let* ((rgb (my/color-hex-to-rgb hex-color))
         (adjusted-rgb (mapcar (lambda (c) (min 1.0 (max 0.0 (+ c delta)))) rgb)))
    (my/color-rgb-to-hex adjusted-rgb)))
;;
(defun my/color-adjust-saturation (hex-color delta)
  "Adjust the saturation of HEX-COLOR by DELTA (-1.0 to 1.0)."
  (let* ((rgb (my/color-hex-to-rgb hex-color))
         (max (apply 'max rgb))
         (adjusted-rgb (mapcar
                        (lambda (c)
                          (if (= max 0.0)
                              c
                            (+ (* c (- 1 delta)) (* max delta))))
                        rgb)))
    (my/color-rgb-to-hex adjusted-rgb)))
;;
(defun my/color-adjust-hue (hex-color delta)
  "Adjust the hue of HEX-COLOR by DELTA (in degrees)."
  (let* ((rgb (my/color-hex-to-rgb hex-color))
         (hsl (color-rgb-to-hsl (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
         (new-h (mod (+ (nth 0 hsl) (/ delta 360.0)) 1.0)) ;; Wrap hue around
         (new-rgb (apply 'color-hsl-to-rgb (list new-h (nth 1 hsl) (nth 2 hsl)))))
    (my/color-rgb-to-hex new-rgb)))
;;
(defun my/replace-color-at-point (transform-fn &rest args)
  "Replace the hex color code at point using TRANSFORM-FN with ARGS."
  (let ((bounds (bounds-of-thing-at-point 'sexp))
        (original (thing-at-point 'sexp t)))
    (if (and bounds (string-match "^#[0-9a-fA-F]\\{6\\}$" original))
        (let ((new-color (apply transform-fn original args)))
          (delete-region (car bounds) (cdr bounds))
          (insert new-color))
      (error "No valid hex color code at point"))))
;;
(global-set-key (kbd "M-<up>") 
                (lambda () 
                  (interactive) 
                  (my/replace-color-at-point 'my/color-adjust-brightness 0.02) 
                  (my/rainbow-mode)))
(global-set-key (kbd "M-<down>") 
                (lambda () 
                  (interactive) 
                  (my/replace-color-at-point 'my/color-adjust-brightness -0.02) 
                  (my/rainbow-mode)))
(global-set-key (kbd "M-<prior>") 
                (lambda () 
                  (interactive) 
                  (my/replace-color-at-point 'my/color-adjust-saturation 0.02) 
                  (my/rainbow-mode)))
(global-set-key (kbd "M-<next>") 
                (lambda () 
                  (interactive) 
                  (my/replace-color-at-point 'my/color-adjust-saturation -0.02) 
                  (my/rainbow-mode)))
(global-set-key (kbd "M-<left>") 
                (lambda () 
                  (interactive) 
                  (my/replace-color-at-point 'my/color-adjust-hue -5) 
                  (my/rainbow-mode)))
(global-set-key (kbd "M-<right>") 
                (lambda () 
                  (interactive) 
                  (my/replace-color-at-point 'my/color-adjust-hue 5) 
                  (my/rainbow-mode)))
(global-set-key (kbd "M-<home>") 'my/insert-random-color-at-point)

(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (abort-recursive-edit))))
;;
(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)

(defvar my/popper-current-popup nil
  "Stores the currently active popup buffer for quick toggle.")
;;
(defun my/popper-toggle-popup ()
  "Toggle visibility of pop-up buffers.
Pop-ups are identified by their names and certain buffer modes.
When toggled, the function displays the next available pop-up
buffer or hides currently displayed pop-ups. Stores the last
active popup in `my/popper-current-popup`."
  (interactive)
  (let* ((popup-patterns '("\\*Help\\*" "\\*eshell\.*\\*" "\\*eldoc\.*\\*"))
         (popup-buffers (seq-filter (lambda (buf)
                                      (let ((bufname (buffer-name buf)))
                                        (seq-some (lambda (pattern)
                                                    (string-match-p pattern bufname))
                                                  popup-patterns)))
                                    (buffer-list)))
         (current-popup (car (seq-filter (lambda (win)
                                           (member (window-buffer win) popup-buffers))
                                         (window-list)))))
    (if current-popup
        ;; If a pop-up buffer is currently visible, bury it.
        (let ((buf (window-buffer current-popup)))
          (delete-window current-popup)
          (bury-buffer buf)
          (setq my/popper-current-popup nil) ;; Clear the currently tracked popup.
          (message "Hid pop-up buffer: %s" (buffer-name buf)))
      ;; Otherwise, display the first available pop-up buffer.
      (if popup-buffers
          (let ((buf (car popup-buffers)))
            (pop-to-buffer buf
                           '(display-buffer-at-bottom
                             (inhibit-same-window . t)
                             (window-height . 0.3)))
            (setq my/popper-current-popup buf) ;; Store the displayed popup buffer.
            (message "Displayed pop-up buffer: %s" (buffer-name buf)))
        (message "No pop-up buffers to display!")))))
;;
(defun my/popper-toggle-current ()
  "Toggle visibility of the last active popup buffer (`my/popper-current-popup`).
If the popup is visible, hide it. If the popup is not visible, restore it."
  (interactive)
  (if (and my/popper-current-popup (buffer-live-p my/popper-current-popup))
      (if (get-buffer-window my/popper-current-popup)
          (progn
            (delete-window (get-buffer-window my/popper-current-popup))
            (message "Hid active popup buffer: %s" (buffer-name my/popper-current-popup)))
        (pop-to-buffer my/popper-current-popup
                       '(display-buffer-at-bottom
                         (inhibit-same-window . t)
                         (window-height . 0.3)))
        (message "Restored active popup buffer: %s" (buffer-name my/popper-current-popup)))
    ;; If no valid currently tracked popup:
    (message "No active popup buffer to toggle.")))
;;
;; Cycle through popups or show the next popup.
(global-set-key (kbd "C-c p") #'my/popper-toggle-popup)
;;
;; Toggle the currently selected popup.
(global-set-key (kbd "C-c l") #'my/popper-toggle-current)

(defun my/md-to-org-convert-buffer ()
  "Convert the current buffer from Markdown to Org-mode format"
  (interactive)
  (save-excursion
    ;; Lists: Translate `-`, `*`, or `+` lists to Org-mode lists
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
      (replace-match (concat (match-string 1) "- \\2")))
    ;; Bold: `**bold**` -> `*bold*` only if directly adjacent
    (goto-char (point-min))
    (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
      (replace-match "*\\1*"))
    ;; Italics: `_italic_` -> `/italic/`
    (goto-char (point-min))
    (while (re-search-forward "\\b_\\([^ ]\\(.*?\\)[^ ]\\)_\\b" nil t)
      (replace-match "/\\1/"))
    ;; Links: `[text](url)` -> `[[url][text]]`
    (goto-char (point-min))
    (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
      (replace-match "[[\\2][\\1]]"))
    ;; Code blocks: Markdown ```lang ... ``` to Org #+begin_src ... #+end_src
    (goto-char (point-min))
    (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
      (replace-match "#+begin_src \\1\n\\2#+end_src"))
    ;; Inline code: `code` -> =code=
    (goto-char (point-min))
    (while (re-search-forward "`\\(.*?\\)`" nil t)
      (replace-match "=\\1="))
    ;; Horizontal rules: `---` or `***` -> `-----`
    (goto-char (point-min))
    (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
      (replace-match "-----"))
    ;; Images: `![alt text](url)` -> `[[url]]`
    (goto-char (point-min))
    (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
      (replace-match "[[\\1]]"))
    (goto-char (point-min))
    ;; Headers: Adjust '#'
    (while (re-search-forward "^\\(#+\\) " nil t)
      (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1))))
;;
(defun my/md-to-org-convert-file (input-file output-file)
  "Convert a Markdown file INPUT-FILE to an Org-mode file OUTPUT-FILE."
  (with-temp-buffer
    (insert-file-contents input-file)
    (md-to-org-convert-buffer)
    (write-file output-file)))
;;
(defun my/convert-markdown-clipboard-to-org ()
  "Convert Markdown content from clipboard to Org format and insert it at point."
  (interactive)
  (let ((markdown-content (current-kill 0))
        (original-buffer (current-buffer)))
    (with-temp-buffer
      (insert markdown-content)
      (my/md-to-org-convert-buffer)
      (let ((org-content (buffer-string)))
        (with-current-buffer original-buffer
          (insert org-content))))))
;;
(defun my/org-promote-all-headings (&optional arg)
  "Promote all headings in the current Org buffer along with their subheadings."
  (interactive "p")
  (org-map-entries
   (lambda () 
     (dotimes (_ arg) (org-promote)))))

(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-v") #'icomplete-vertical-toggle)
(define-key icomplete-minibuffer-map (kbd "RET") #'icomplete-force-complete-and-exit)
(add-hook 'after-init-hook (lambda () (fido-mode 1)))
(setq tab-always-indent t)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-separator " | ")
(setq icomplete-with-completion-tables t)
(setq icomplete-in-buffer t)
(setq icomplete-max-delay-chars 0)
(setq icomplete-scroll t)
(setq max-mini-window-height 2)
(setq completion-auto-help nil)
(setq completion-styles '(flex basic substring))

(defun my/kanban-to-table (&optional match)
  "Format Org headings into a Kanban-style Org table.
Each TODO state becomes a column, and headings under each state are placed in rows.
Optionally filter headings by MATCH (e.g., a tag or property match)."
  (interactive)
  (let ((todo-states org-todo-keywords-1)  ;; Gather all TODO states defined in the current Org file.
        (kanban-table (list))
        (column-data (make-hash-table :test 'equal))) ;; Store TODO states and their associated headings.
    ;; Initialize data structure for each TODO state (columns).
    (dolist (state todo-states)
      (puthash state '() column-data))
    ;; Collect headlines into their respective TODO state buckets.
    (save-excursion
      (goto-char (point-min))
      ;; Optionally filter entries using `match`.
      (org-map-entries
       (lambda ()
         (let* ((todo (org-get-todo-state))       ;; Get the TODO state of the current heading.
                (heading (org-get-heading t t t t))) ;; Get the heading text.
           (when (and todo (not (string-empty-p todo))) ;; Check if the heading has a TODO state.
             (puthash todo
                      (append (gethash todo column-data) (list heading))
                      column-data))))
       match 'file)) ;; Search the entire file or based on optional `match`.
    ;; Filter out empty columns
    (setq todo-states (seq-filter (lambda (state)
                                    (not (null (gethash state column-data))))
                                  todo-states))
    ;; Build the rows for the Kanban Org table.
    (let ((max-rows 0))
      (dolist (state todo-states)
        (let ((headings (gethash state column-data)))
          (setq max-rows (max max-rows (length headings)))
          (push (list state headings) kanban-table)))
      ;; Construct the table rows.
      (let ((rows '()))
        ;; Fill rows by extracting each heading under TODO states.
        (dotimes (i max-rows)
          (let ((row '()))
            (dolist (state todo-states)
              (let ((headings (gethash state column-data)))
                (push (or (nth i headings) "") row))) ;; Add the heading or an empty string.
            (push (reverse row) rows)))
        (setq rows (nreverse rows))
        (push 'hline rows)
        ;; Insert TODO column headers at the top.
        (push todo-states rows)))))

(require 'ox-publish)
;;
(setq org-publish-project-alist
      '(("split-org"
         :base-directory "~/DCIM/content"
         :base-extension "org"
         :publishing-directory "~/DCIM/content/split"
         :exclude ".*"
         :include ("linux--all.org"
                   "emacs--all.org"
                   "blog--all.org")
         :publishing-function my-org-publish-split-headings
         :recursive nil)
        ("blog-posts"
         :base-directory "~/DCIM/content/split"
         :base-extension "org"
         :publishing-directory "~/publish/public_html"
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-preamble t
         :html-postamble t
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "the DyerDwelling"
         :html-head "<link rel=\"stylesheet\"
                    href=\"../other/mystyle.css\"
                    type=\"text/css\"/>"
         :sitemap-function my-sitemap-format
         :sitemap-sort-files alphabetically)
        ("images-emacs"
         :base-directory "~/DCIM/content/emacs"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/public_html/emacs"
         :publishing-function org-publish-attachment)
        ("images-blog"
         :base-directory "~/DCIM/content/blog"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/public_html/blog"
         :publishing-function org-publish-attachment)
        ("images-bingo"
         :base-directory "~/DCIM/content/bingo"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/public_html/bingo"
         :publishing-function org-publish-attachment)j
        ("images-music"
         :base-directory "~/DCIM/content/music"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/public_html/music"
         :publishing-function org-publish-attachment)
        ("images-dad--dictionary"
         :base-directory "~/DCIM/content/dad--dictionary"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/public_html/dad--dictionary"
         :publishing-function org-publish-attachment)
        ("blog" ;; Meta-project to combine phases
         :components ("split-org" "images-emacs" "images-blog" "images-bingo" "images-music" "images-dad--dictionary" "blog-posts"))))
;;
(defun my-org-publish-split-headings (plist filename pub-dir)
  "Split an Org file into separate files, each corresponding to a top-level heading.
Each file name is prefixed with the date in YYYYMMDD format extracted from the :EXPORT_HUGO_LASTMOD: property.
PLIST is the property list for the publishing process,
FILENAME is the input Org file, and PUB-DIR is the publishing directory."
  (with-temp-buffer
    (insert-file-contents filename) ;; Load the content of the current Org file
    (goto-char (point-min))
    (let ((heading-level 1) ;; Level of the top-level heading to split by
          prev-start heading-title sanitized-title output-file lastmod-date)
      ;; Iterate over all top-level headings
      (while (re-search-forward (format "^\\*\\{%d\\} \\(.*\\)" heading-level) nil t)
        (setq prev-start (match-beginning 0)) ;; Start of the current heading
        (setq heading-title (match-string 1)) ;; Extract heading text
        (setq sanitized-title (when heading-title
                                (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" heading-title))) ;; Sanitize
        ;; Extract the :EXPORT_HUGO_LASTMOD: property for the current section
        (save-excursion
          (when (re-search-forward ":EXPORT_HUGO_LASTMOD: +\\(<.+>\\)" (save-excursion (re-search-forward "^\\* " nil t) (point)) t)
            (let* ((raw-lastmod (match-string 1)) ;; Extract the timestamp string (e.g., "<2024-12-08 08:37>")
                   (date-elements (when (string-match "<\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" raw-lastmod)
                                    (list (match-string 1 raw-lastmod) ;; Year
                                          (match-string 2 raw-lastmod) ;; Month
                                          (match-string 3 raw-lastmod))))) ;; Day
              (setq lastmod-date (when date-elements
                                   (apply #'concat date-elements)))))) ;; Combine YYYYMMDD
        ;; Default to "00000000" if no valid lastmod-date is found
        (setq lastmod-date (or lastmod-date "00000000"))
        ;; Find the end of this section (right before the next top-level heading)
        (let ((section-end (save-excursion
                             (or (re-search-forward (format "^\\*\\{%d\\} " heading-level) nil t)
                                 (point-max))))) ;; End of current section or end of file
          ;; Only proceed if sanitized title exists and is valid
          (when (and sanitized-title (not (string-empty-p sanitized-title)))
            ;; Create the output file name (prepend the date)
            (setq output-file (expand-file-name (format "%s-%s.org" lastmod-date sanitized-title) pub-dir))
            ;; Write the section content (from prev-start to section-end)
            (write-region prev-start section-end output-file)
            (message "Wrote %s" output-file)))))
    ;; Return nil to indicate successful processing
    nil))
;;
(defun my-sitemap-format (title list)
  "Generate a sitemap with TITLE and reverse-sorted LIST of files."
  (setq list (nreverse (cdr list)))
  (concat "#+TITLE: " title "\n\n"
          "* Blog Posts\n"
          (mapconcat
           (lambda (entry)
             (format "- %s\n" (car entry)))
           list)
          "\n"))
