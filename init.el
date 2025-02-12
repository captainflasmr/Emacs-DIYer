;; -*- lexical-binding: t; -*-

(defun my-icomplete-exit-minibuffer-with-input ()
  "Exit the minibuffer with the current input, without forcing completion."
  (interactive)
  (exit-minibuffer))

(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there is only a single window, split it horizontally.
If there are only two windows, jump directly to the other window.
Side windows are ignored."
  (interactive)
  (let* ((window-list (seq-filter (lambda (w)
                                    (not (window-parameter w 'window-side)))
                                  (window-list nil 'no-mini))))
    (cond
     ((= (length window-list) 1)
      (split-window-horizontally)
      (other-window 1))
     ((= (length window-list) 2)
      (let ((other-window (if (eq (selected-window) (nth 0 window-list))
                              (nth 1 window-list)
                            (nth 0 window-list))))
        (select-window other-window)))
     (t
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
                                                   'face 'highlight))
                          (overlay-put overlay 'window window)
                          overlay))
                      window-map))
        (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
          (mapc #'delete-overlay my/quick-window-overlays)
          (message ".")
          (setq my/quick-window-overlays nil)
          (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window))))))))

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
  (let* ((selected-color (or color (read-color "Select mode-line background color: ")))
         (default-bg (face-background 'default))
         (default-fg (face-foreground 'default))
         (default-hl (face-background 'highlight))
         (inactive-fg (face-foreground 'mode-line-inactive))
         (is-dark (not (string-greaterp default-bg "#888888")))
         (adjusted-bg (if is-dark
                          (adjust-color default-bg 20)
                        (adjust-color default-bg -5))))
    (set-face-attribute 'mode-line nil :height 140 :underline nil :overline nil :box nil
                        :background selected-color :foreground "#000000")
    (set-face-attribute 'mode-line-inactive nil :height 140 :underline nil :overline nil
                        :background adjusted-bg :foreground "#aaaaaa")
    (custom-set-faces
     `(hl-line ((t (:background, adjusted-bg))))
     `(vertical-border ((t (:foreground ,(adjust-color default-fg -60)))))
     `(window-divider ((t (:foreground ,(adjust-color default-fg -60)))))
     `(fringe ((t (:foreground ,default-bg :background ,default-bg))))
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit 'highlight :background ,selected-color :foreground "#000000"))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg
                                          :box (:line-width 2 :color ,default-bg :style pressed-button))))))))

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

(defun my-org-reveal-on-next-error ()
  "Reveal the location of search results in an Org file."
  (when (derived-mode-p 'org-mode)
    (org-reveal)))

(add-hook 'next-error-hook 'my-org-reveal-on-next-error)

(setq ispell-local-dictionary "en_GB")
(setq ispell-program-name "hunspell")
(setq dictionary-default-dictionary "*")
(setq dictionary-server "dict.org")
(setq dictionary-use-single-buffer t)
(defun spelling-menu ()
  "Menu for spelling."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- Spelling [q] Quit: -------
Run        [s] Spelling
Dictionary [l] Check"
               'face 'minibuffer-prompt))))
    (pcase key
      ;; Spelling
      (?s (progn
            (flyspell-buffer)
            (call-interactively 'flyspell-mode)))
      (?l (call-interactively 'ispell-word))
      ;; Quit
      (?q (message "Quit Build menu."))
      (?\C-g (message "Quit Build menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c s") #'spelling-menu)
(global-set-key (kbd "C-0") #'ispell-word)

(require 'cl-lib)
(require 'color)

(defun my/color-hex-to-rgb (hex-color)
  "Convert a HEX-COLOR string to a list of RGB values."
  (unless (string-match "^#[0-9a-fA-F]\\{6\\}$" hex-color)
    (error "Invalid hex color: %s" hex-color))
  (mapcar (lambda (x) (/ (string-to-number x 16) 255.0))
          (list (substring hex-color 1 3)
                (substring hex-color 3 5)
                (substring hex-color 5 7))))

(defun my/color-rgb-to-hex (rgb)
  "Convert a list of RGB values to a hex color string."
  (format "#%02x%02x%02x"
          (round (* 255 (nth 0 rgb)))
          (round (* 255 (nth 1 rgb)))
          (round (* 255 (nth 2 rgb)))))

(defun my/color-adjust-brightness (hex-color delta)
  "Adjust the brightness of HEX-COLOR by DELTA (-1.0 to 1.0)."
  (let* ((rgb (my/color-hex-to-rgb hex-color))
         (adjusted-rgb (mapcar (lambda (c) (min 1.0 (max 0.0 (+ c delta)))) rgb)))
    (my/color-rgb-to-hex adjusted-rgb)))

(defun my/color-adjust-saturation (hex-color delta)
  "Adjust the saturation of HEX-COLOR by DELTA (-1.0 to 1.0)."
  (let* ((rgb (my/color-hex-to-rgb hex-color))
         (max (apply 'max rgb))
         (adjusted-rgb (mapcar
                        (lambda (c)
                          (if (= max 0.0)
                              c
                            (+ (* c (+ 1 delta)) (* max (- delta)))))
                        rgb)))
    (my/color-rgb-to-hex adjusted-rgb)))

(defun my/color-adjust-hue (hex-color delta)
  "Adjust the hue of HEX-COLOR by DELTA (in degrees)."
  (let* ((rgb (my/color-hex-to-rgb hex-color))
         (hsl (color-rgb-to-hsl (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
         (new-h (mod (+ (nth 0 hsl) (/ delta 360.0)) 1.0)) ;; Wrap hue around
         (new-rgb (apply 'color-hsl-to-rgb (list new-h (nth 1 hsl) (nth 2 hsl)))))
    (my/color-rgb-to-hex new-rgb)))

(defun my/replace-color-at-point (transform-fn &rest args)
  "Replace the hex color code at point using TRANSFORM-FN with ARGS."
  (let ((bounds (bounds-of-thing-at-point 'sexp))
        (original (thing-at-point 'sexp t)))
    (if (and bounds (string-match "^#[0-9a-fA-F]\\{6\\}$" original))
        (let ((new-color (apply transform-fn original args)))
          (delete-region (car bounds) (cdr bounds))
          (insert new-color))
      (error "No valid hex color code at point"))))

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

(global-set-key (kbd "M-g o") #'org-goto)
(setq org-goto-interface 'outline-path-completionp)
(setq org-outline-path-complete-in-steps nil)

(let ((bash-history-file "~/.bash_history")
      (eshell-history-file (expand-file-name "eshell/history" user-emacs-directory)))
  (when (file-exists-p bash-history-file)
    (with-temp-buffer
      (insert-file-contents bash-history-file)
      (append-to-file (buffer-string) nil eshell-history-file))))

(defun my/eshell-history-completing-read ()
  "Search eshell history using completing-read"
  (interactive)
  (insert
   (completing-read "Eshell history: "
                    (delete-dups
                     (ring-elements eshell-history-ring)))))

(setq eshell-history-size 10000)
(setq eshell-save-history-on-exit t)
(setq eshell-hist-ignoredups t)

(defun my/setup-eshell-keybindings ()
  "Setup eshell keybindings with version compatibility checks and fallbacks."
  ;; Try modern mode-specific maps first
  (with-eval-after-load 'em-hist
    (if (boundp 'eshell-hist-mode-map)
        (progn
          (define-key eshell-hist-mode-map (kbd "M-r") #'my/eshell-history-completing-read)
          (define-key eshell-hist-mode-map (kbd "M-s") nil))
      ;; Fallback to eshell-mode-map if specific mode maps don't exist
      (when (boundp 'eshell-mode-map)
        (define-key eshell-mode-map (kbd "M-r") #'my/eshell-history-completing-read)
        (define-key eshell-mode-map (kbd "M-s") nil))))
  (with-eval-after-load 'em-cmpl
    ;; Add completion category overrides
    (add-to-list 'completion-category-overrides
                 '(eshell-history (styles basic substring initials)))
    ;; Try modern completion map first, fallback to general map
    (if (boundp 'eshell-cmpl-mode-map)
        (define-key eshell-cmpl-mode-map (kbd "C-M-i") #'completion-at-point)
      (when (boundp 'eshell-mode-map)
        (define-key eshell-mode-map (kbd "C-M-i") #'completion-at-point)))))

(add-hook 'eshell-mode-hook #'my/setup-eshell-keybindings)

(defun my/load-bash-history ()
  "Load commands from .bash_history into shell history ring."
  (interactive)
  (let* ((bash-history-file (expand-file-name "~/.bash_history"))
         (existing-history (ring-elements comint-input-ring))
         (bash-history
          (when (file-exists-p bash-history-file)
            (with-temp-buffer
              (insert-file-contents bash-history-file)
              (split-string (buffer-string) "\n" t)))))
    ;; Add bash history entries to comint history ring
    (when bash-history
      (dolist (cmd (reverse bash-history))
        (unless (member cmd existing-history)
          (comint-add-to-input-history cmd))))))

(add-hook 'shell-mode-hook 'my/load-bash-history)

(defun my/shell-history-complete ()
  "Search shell history with completion."
  (interactive)
  (let* ((history (ring-elements comint-input-ring))
         (selection (completing-read "Shell history: " 
                                     (delete-dups history)
                                     nil 
                                     t)))
    (when selection
      (delete-region (comint-line-beginning-position)
                     (line-end-position))
      (insert selection))))

(define-key shell-mode-map (kbd "M-r") #'my/shell-history-complete)

(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (let ((copied-text candidate))
        (run-with-timer 0 nil (lambda () 
                                (message "Copied: %s" copied-text)))
        (abort-recursive-edit)))))

(global-set-key (kbd "C-c ,") 'find-file-at-point)
(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)

(defun my/popper-matching-buffers ()
  "Return a list of buffers matching pop-up patterns."
  (let ((popup-patterns '("\\*\.*shell\.*\\*"
                          "\\*\.*term\.*\\*"
                          "\\*eldoc\.*\\*"
                          "\\*Flymake\.*")))
    (seq-filter (lambda (buf)
                  (let ((bufname (buffer-name buf)))
                    (seq-some (lambda (pattern)
                                (string-match-p pattern bufname))
                              popup-patterns)))
                (buffer-list))))

(defun my/popper-handle-popup (buffer)
  "Display BUFFER as a popup, setting it as the current popup."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window display-buffer-at-bottom)
                   (inhibit-same-window . t)
                   (window-height . 0.3)))
  (message "Displayed pop-up buffer: %s" (buffer-name buffer)))

(defun my/popper-cycle-popup ()
  "Cycle visibility of pop-up buffers."
  (interactive)
  (let* ((popup-buffers (my/popper-matching-buffers))
         (current-popup-window (car (seq-filter (lambda (win)
                                                  (member (window-buffer win) popup-buffers))
                                                (window-list)))))
    (when current-popup-window
      (let ((buf (window-buffer current-popup-window)))
        (delete-window current-popup-window)
        (bury-buffer buf)
        (setq popup-buffers (my/popper-matching-buffers))
        (message "Hid pop-up buffer: %s" (buffer-name buf))))
    (if popup-buffers
        (my/popper-handle-popup (car popup-buffers))
      (message "No pop-up buffers to display!"))))

(defun my/popper-toggle-current ()
  "Toggle visibility of pop-up buffers."
  (interactive)
  (let* ((popup-buffers (my/popper-matching-buffers))
         (current-popup-window (car (seq-filter (lambda (win)
                                                  (member (window-buffer win) popup-buffers))
                                                (window-list)))))
    (if current-popup-window
        (let ((buf (window-buffer current-popup-window)))
          (delete-window current-popup-window)
          (message "Hid pop-up buffer: %s" (buffer-name buf)))
      (if popup-buffers
          (my/popper-handle-popup (car popup-buffers))
        (message "No pop-up buffers to display!")))))

;; Toggle the currently selected popup.
(global-set-key (kbd "M-'") #'my/popper-toggle-current)

;; Cycle through popups or show the next popup.
(global-set-key (kbd "M-#") #'my/popper-cycle-popup)

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
    (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
      (replace-match "\\1/\\2/\\3"))
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

(defun my/md-to-org-convert-file (input-file output-file)
  "Convert a Markdown file INPUT-FILE to an Org-mode file OUTPUT-FILE."
  (with-temp-buffer
    (insert-file-contents input-file)
    (md-to-org-convert-buffer)
    (write-file output-file)))

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

(defun my/org-promote-all-headings (&optional arg)
  "Promote all headings in the current Org buffer along with their subheadings."
  (interactive "p")
  (org-map-entries
   (lambda () 
     (dotimes (_ arg) (org-promote)))))

(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "RET") #'icomplete-force-complete-and-exit)
(add-hook 'after-init-hook (lambda () (fido-mode 1)))
(setq completion-styles '(flex basic substring))
(setq tab-always-indent t)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-separator " | ")
(add-hook 'buffer-list-update-hook
          (lambda ()
            (unless (minibufferp)
              (setq-local icomplete-separator "\n"))))
(setq icomplete-in-buffer t)
(setq completion-auto-help nil)
(define-key minibuffer-local-completion-map (kbd "TAB")
            (lambda ()
              (interactive)
              (let ((completion-auto-help t))
                (minibuffer-complete))))
(setq completion-show-help nil)
(setq icomplete-with-completion-tables t)
(setq icomplete-prospects-height 1)
(setq icomplete-scroll t)

(defun my/simple-completion-at-point ()
  "Use completing-read-in-buffer for completion at point."
  (interactive)
  (let* ((completion-data (run-hook-with-args-until-success 
                           'completion-at-point-functions))
         (beg (nth 0 completion-data))
         (end (nth 1 completion-data))
         (table (nth 2 completion-data))
         (pred (plist-get (nthcdr 3 completion-data) :predicate))
         (prefix (buffer-substring-no-properties beg end))
         (completion (completing-read-default
                      "Complete: "
                      table
                      pred
                      nil  ; no require-match
                      prefix)))
    (when completion
      (delete-region beg end)
      (insert completion))))

(global-set-key (kbd "C-c TAB") #'my/simple-completion-at-point)

(defun my/eshell-history-capf ()
  "Completion-at-point function for eshell history."
  (let* ((beg (save-excursion
                (eshell-bol)
                (point)))
         (end (point))
         (prefix (buffer-substring-no-properties beg end))
         (candidates (delete-dups
                      (ring-elements eshell-history-ring))))
    (list beg end candidates
          :exclusive 'no
          :annotation-function
          (lambda (_) " (history)"))))

(defun my/setup-eshell-history-completion ()
  "Setup eshell history completion."
  (add-hook 'completion-at-point-functions #'my/eshell-history-capf nil t))

(add-hook 'eshell-mode-hook #'my/setup-eshell-history-completion)

(defun my/shell-history-capf ()
  "Completion-at-point function for shell history completion."
  (let* ((beg (comint-line-beginning-position))
         (end (point))
         (prefix (buffer-substring-no-properties beg end))
         (history (ring-elements comint-input-ring))
         (matching-history
          (cl-remove-if-not
           (lambda (cmd)
             (string-prefix-p prefix cmd))
           history)))
    (list beg end matching-history
          :exclusive 'no
          :annotation-function
          (lambda (_) " (history)"))))

(defun my/setup-shell-history-completion ()
  "Setup shell history completion."
  (add-hook 'completion-at-point-functions #'my/shell-history-capf nil t))

(add-hook 'shell-mode-hook #'my/setup-shell-history-completion)

(with-eval-after-load 'shell
  (add-to-list 'completion-category-overrides
               '(shell-history (styles basic substring initials))))

(defun my/kanban-to-table (&optional match exclude-tag)
  "Format Org headings into a Kanban-style Org table, filtering by MATCH and excluding EXCLUDE-TAG."
  (interactive)
  (let ((todo-states org-todo-keywords-1)
        (kanban-table (list))
        (column-data (make-hash-table :test 'equal)))
    (dolist (state todo-states)
      (puthash state '() column-data))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (let* ((todo (org-get-todo-state))
                (heading (org-get-heading t t t t))
                (tags (org-get-tags))) ;; Get tags for current heading.
           (when (and todo (not (string-empty-p todo))
                      (not (member exclude-tag tags))) ;; Exclude headings with the `exclude-tag`.
             (puthash todo
                      (append (gethash todo column-data) (list heading))
                      column-data))))
       match 'file))
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

(defun my/org-html-src-block-filter (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\n\\s-*\n" "<br>\n" text)))

(defun my/org-setup-src-block-filter (backend)
  "Set `org-export-filter-src-block-functions` dynamically based on BACKEND."
  (message "Exporting with backend: %s" backend) ;; For debugging
  (cond
   ((eq backend 'hugo) ;; Clear the filter for ox-hugo
    (setq-local org-export-filter-src-block-functions nil))
   ((eq backend 'html) ;; Apply filter for ox-html/ox-publish
    (setq-local org-export-filter-src-block-functions
                '(my/org-html-src-block-filter)))))

(add-hook 'org-export-before-processing-functions #'my/org-setup-src-block-filter)

(setq org-publish-project-alist
      '(("split-emacs"
         :base-directory "~/DCIM/content"
         :base-extension "org"
         :publishing-directory "~/DCIM/content/split/emacs"
         :exclude ".*"
         :include ("emacs--all.org")
         :publishing-function my-org-publish-split-headings
         :recursive nil)
        ("blog-posts-emacs"
         :base-directory "~/DCIM/content/split/emacs"
         :base-extension "org"
         :publishing-directory "~/publish/hugo-emacs/site/static/public_html"
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-preamble t
         :html-postamble t
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "the DyerDwelling"
         :html-head "<link rel=\"stylesheet\"
                    href=\"../assets/css//bootstrap.css\"
                    type=\"text/css\"/>\n
                    <link rel=\"stylesheet\"
                    href=\"../assets/css//style-ignore.css\"
                    type=\"text/css\"/>"
         :sitemap-function my-sitemap-format
         :sitemap-sort-files alphabetically)
        ("images-emacs"
         :base-directory "~/DCIM/content/emacs"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/hugo-emacs/site/static/public_html/emacs"
         :publishing-function org-publish-attachment)
        ("blog" ;; Meta-project to combine phases
         :components ("split-emacs" "images-emacs" "blog-posts-emacs"))))

(defun my-org-publish-split-headings (plist filename pub-dir)
  "Split an Org file into separate files, each corresponding to a top-level heading
that is marked as DONE.
Each file name is prefixed with the date in YYYYMMDD format extracted from the
:EXPORT_HUGO_LASTMOD: property. PLIST is the property list for the publishing
process, FILENAME is the input Org file, and PUB-DIR is the publishing directory."
  (with-temp-buffer
    (insert-file-contents filename) ;; Load the content of the current Org file
    (goto-char (point-min))
    (let ((heading-level 1) ;; Level of the top-level heading to split by
          prev-start heading-title sanitized-title output-file lastmod-date)
      ;; Iterate over all top-level headings
      (while (re-search-forward (format "^\\*\\{%d\\} \\(?:\\([[:upper:]]+\\) \\)?\\(.*\\)" heading-level) nil t)
        (let ((todo-keyword (match-string 1)) ;; Extract the TODO keyword (if it exists)
              (heading-title (match-string 2))) ;; Extract the title of the heading
          ;; Process only headings marked as DONE
          (when (and todo-keyword (string-equal todo-keyword "DONE"))
            (setq prev-start (match-beginning 0)) ;; Start of the current heading
            (setq sanitized-title (when heading-title
                                    (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" heading-title))) ;; Sanitize title
            ;; Extract the :EXPORT_HUGO_LASTMOD: property for the current section
            (save-excursion
              (when (re-search-forward ":EXPORT_HUGO_LASTMOD: +\\(<.+>\\)" (save-excursion (re-search-forward "^\\* " nil t) (point)) t)
                (let* ((raw-lastmod (match-string 1)) ;; Extract the timestamp string (e.g., "<2024-12-08 08:37>")
                       (date-elements (when (string-match "<\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" raw-lastmod)
                                        (list (match-string 1 raw-lastmod) ;; Year
                                              (match-string 2 raw-lastmod) ;; Month
                                              (match-string 3 raw-lastmod))))) ;; Day
                  (setq lastmod-date (when date-elements
                                       (apply #'concat date-elements))))))
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
                (message "Wrote %s" output-file)))))))
    ;; Return nil to indicate successful processing
    nil))

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

(defun my-generate-rss-feed ()
  "Generate a detailed RSS feed for Org-published blog posts."
  (interactive)
  (let* ((rss-file (expand-file-name "index.xml" "/home/jdyer/publish/hugo-emacs/site/static/public_html"))
         (base-url "https://www.emacs.dyerdwelling.family/public_html/")
         (self-link "https://www.emacs.dyerdwelling.family/public_html/index.xml") ;; Self-referencing link for Atom feeds
         (last-build-date (format-time-string "%a, %d %b %Y %H:%M:%S %z")) ;; Current time as lastBuildDate
         (org-directory "/home/jdyer/source/test/elisp")
         (static-author "captainflasmr@gmail.com (James Dyer)") ;; Static author 
         ;; (org-directory "/home/jdyer/DCIM/content/split/emacs")
         (rss-items ""))
    ;; Iterate over all Org files in the directory
    (dolist (org-file (directory-files org-directory t "\\.org$"))
      (let* ((html-file (concat (file-name-sans-extension
                                 (file-name-nondirectory org-file)) ".html"))
             (url (concat base-url html-file))
             (heading-level 1)
             (guid url) ;; Default GUID as the post URL
             title
             content
             html-content
             raw-pubdate
             pubdate)
        ;; Read and process the org file
        (with-temp-buffer
          (insert-file-contents org-file)
          (goto-char (point-min))
          ;; Extract the title from the first heading
          (when  (re-search-forward (format "^\\*\\{%d\\} \\(?:\\([[:upper:]]+\\) \\)?\\(.*\\)" heading-level) nil t)
            (setq title (match-string 2)))
          ;; Extract the :EXPORT_HUGO_LASTMOD: property value
          (when (re-search-forward "^.*EXPORT_HUGO_LASTMOD: *<\\([^>]+\\)>" nil t)
            (setq raw-pubdate (match-string 1)))
          ;; Convert the raw-pubdate to the RFC 822 format for <pubDate>
          (when raw-pubdate
            (setq pubdate (format-time-string
                           "%a, %d %b %Y %H:%M:%S %z"
                           (org-time-string-to-time (concat "<" raw-pubdate ">")))))
          ;; Move to the end of :END: and extract the remaining contents
          (when (re-search-forward "^:END:\n" nil t)
            (setq content (buffer-substring-no-properties (point) (point-max)))
            ;; Convert the content to HTML
            (setq html-content (org-export-string-as content 'html t '(:with-toc nil)))
            ;; (setq html-content (xml-escape-string html-content))
            ))
        ;; Add an item to the RSS feed
        (setq rss-items
              (concat rss-items (format "
<item>
  <title>%s</title>
  <link>%s</link>
  <guid>%s</guid>
  <pubDate>%s</pubDate>
  <author>%s</author>
  <description><![CDATA[%s]]></description>
</item>"
                                        (or title "Untitled Post")
                                        url
                                        guid ;; Use the generated GUID
                                        (or pubdate last-build-date) ;; Fallback to lastBuildDate if missing
                                        static-author ;; Static author name
                                        (or html-content "No content available"))))))
    ;; Write the RSS feed to the file
    (with-temp-file rss-file
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
<channel>
  <title>Emacs@Dyerdwelling</title>
  <image>
      <url>/images/banner/favicon-james.png</url>
      <title>Emacs@Dyerdwelling</title>
      <link>https://emacs.dyerdwelling.family/public_html/</link>
      <width>32</width>
      <height>32</height>
  </image>
  <link>" base-url "</link>
  <description>Recent content on Emacs@Dyerdwelling</description>
  <language>en</language>
  <managingEditor>captainflasmr@gmail.com (James Dyer)</managingEditor>
  <webMaster>captainflasmr@gmail.com (James Dyer)</webMaster>
  <lastBuildDate>" last-build-date "</lastBuildDate>
  <atom:link href=\"" self-link "\" rel=\"self\" type=\"application/rss+xml\" />"
  rss-items "
</channel>
</rss>"))
    (message "RSS feed generated at %s" rss-file)))

(defun my/etags-load ()
  "Load TAGS file from the first it can find up the directory stack."
  (interactive)
  (let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))

(when (executable-find "my-generate-etags.sh")
  (defun my/etags-update ()
    "Call external bash script to generate new etags for all languages it can find."
    (interactive)
    (async-shell-command "my-generate-etags.sh" "*etags*")))

(defun predicate-exclusion-p (dir)
  "exclusion of directories"
  (not
   (or
    (string-match "/home/jdyer/examples/CPPrograms/nil" dir)
    )))

(defun my/generate-etags ()
  "Generate TAGS file for various source files in `default-directory` and its subdirectories."
  (interactive)
  (message "Getting file list...")
  (let ((all-files
         (append
          (directory-files-recursively default-directory "\\(?:\\.cpp$\\|\\.c$\\|\\.h$\\)" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.cs$\\|\\.cs$\\)" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.ads$\\|\\.adb$\\)" nil 'predicate-exclusion-p)))
        (tags-file-path (expand-file-name (concat default-directory "TAGS"))))
    (unless (file-directory-p default-directory)
      (error "Default directory does not exist: %s" default-directory))
    ;; Generate TAGS file
    (dolist (file all-files)
      (message file)
      (shell-command (format "etags --append \%s -o %s" file tags-file-path)))))
(global-set-key (kbd "C-x p l") 'my/etags-load)
(global-set-key (kbd "C-x p u") 'my/etags-update)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "W") 'dired-do-async-shell-command)
  (setq dired-guess-shell-alist-user
        '(("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\)$" "gthumb")
          ("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|mpg\\)$" "mpv")
          ("\\.\\(mp3\\|wav\\|ogg\\|\\)$" "mpv")
          ("\\.\\(kra\\)$" "org.kde.krita")
          ("\\.\\(xcf\\)$" "gimp")
          ("\\.\\(odt\\|ods\\|doc\\|docx\\)$" "libreoffice")
          ("\\.\\(html\\|htm\\)$" "firefox")
          ("\\.\\(pdf\\|epub\\)$" "xournalpp"))))

(defun my/rsync (dest)
  "Rsync copy."
  (interactive
   (list
    (expand-file-name (read-file-name "rsync to:"
                                      (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        (command "rsync -arvz --progress --no-g "))
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    (setq command (concat command (shell-quote-argument dest)))
    (async-shell-command command "*rsync*")
    (dired-unmark-all-marks)
    (other-window 1)
    (sleep-for 1)
    (dired-revert)
    (revert-buffer nil t nil)))

(require 'dabbrev)

(defun simple-autosuggest--get-completion (input &optional bounds)
  "Core function handling suggestion logic for INPUT with optional BOUNDS."
  (let* ((bounds (or bounds
                     (cond ((derived-mode-p 'comint-mode)
                            (when-let ((proc-mark (process-mark (get-buffer-process (current-buffer)))))
                              (and (>= (point) proc-mark) (cons proc-mark (line-end-position)))))
                           ((derived-mode-p 'eshell-mode)
                            (when (>= (point) eshell-last-output-end)
                              (cons (save-excursion (eshell-bol) (point)) (point-max))))
                           (t (bounds-of-thing-at-point 'symbol)))))
         (input (or input (and bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
         (min-length (cond ((derived-mode-p 'comint-mode) 0)
                           ((derived-mode-p 'eshell-mode) 0)
                           (t 3)))
         (suggestion (and input (>= (length input) min-length)
                          (memq last-command '(org-self-insert-command self-insert-command yank))
                          (cond ((derived-mode-p 'comint-mode)
                                 (when-let ((ring comint-input-ring))
                                   (seq-find (lambda (h) (string-prefix-p input h t))
                                             (ring-elements ring))))
                                ((derived-mode-p 'eshell-mode)
                                 (when-let ((ring eshell-history-ring))
                                   (seq-find (lambda (h) (string-prefix-p input h t))
                                             (ring-elements ring))))
                                (t (let ((dabbrev-case-fold-search t)
                                         (dabbrev-case-replace nil))
                                     (ignore-errors
                                       (dabbrev--reset-global-variables)
                                       (dabbrev--find-expansion input 0 t))))))))
    (when (and suggestion (not (string= input suggestion)))
      (let ((suffix (substring suggestion (length input))))
        (put-text-property 0 1 'cursor 0 suffix)
        (overlay-put simple-autosuggest--overlay 'after-string
                     (propertize suffix 'face '(:inherit shadow)))
        (move-overlay simple-autosuggest--overlay (point) (point))
        suggestion))))

(defun simple-autosuggest-move-end-of-line (arg)
  "Move to end of line, accepting suggestion first if available."
  (interactive "^p")
  (if-let ((overlay simple-autosuggest--overlay)
           (suggestion (overlay-get overlay 'after-string)))
      (progn
        (insert (substring-no-properties suggestion))
        (overlay-put overlay 'after-string nil))
    (move-end-of-line arg)))

(defun simple-autosuggest-update ()
  "Update the auto-suggestion overlay."
  (when simple-autosuggest--overlay
    (unless (simple-autosuggest--get-completion nil nil)
      (overlay-put simple-autosuggest--overlay 'after-string nil))))

(define-minor-mode simple-autosuggest-mode
  "Minor mode for showing auto-suggestions from history or dabbrev completion."
  :lighter " SAM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap move-end-of-line] #'simple-autosuggest-move-end-of-line)
            map)
  (if simple-autosuggest-mode
      (progn
        (setq-local simple-autosuggest--overlay (make-overlay (point) (point) nil t t))
        (add-hook 'post-command-hook #'simple-autosuggest-update nil t))
    (remove-hook 'post-command-hook #'simple-autosuggest-update t)
    (when simple-autosuggest--overlay
      (delete-overlay simple-autosuggest--overlay)
      (setq simple-autosuggest--overlay nil))))

(provide 'simple-autosuggest)

(define-globalized-minor-mode global-simple-autosuggest-mode
  simple-autosuggest-mode       ;; The mode to be globalized
  (lambda ()                    ;; A function to enable the mode
    (unless (minibufferp)       ;; Avoid enabling the mode in the minibuffer
      (simple-autosuggest-mode 1))))

(global-simple-autosuggest-mode 1)
