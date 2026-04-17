;; -*- lexical-binding: t; -*-

;; Set custom variables
(setq newsticker-retrieval-interval 3600)  ; Update every hour
(setq newsticker-treeview-treeview-face-fn 'ignore)
(setq newsticker-treeview-date-format "%Y-%m-%d %H:%M")
(setq newsticker-url-list
      '(("Emacs Dyer Dwelling"
         "https://www.emacs.dyerdwelling.family/index.xml" nil nil nil)))

;; Key bindings and configuration
(with-eval-after-load 'newsticker
  (define-key newsticker-treeview-mode-map (kbd "n") 'newsticker-treeview-next-item)
  (define-key newsticker-treeview-mode-map (kbd "p") 'newsticker-treeview-prev-item)
  (define-key newsticker-treeview-mode-map (kbd "m") 'newsticker-treeview-mark-item)
  (newsticker-start)
  (defun my-newsticker-treeview-custom-filter ()
    "Custom filter to show items from the last month."
    (let ((one-month-ago (time-subtract (current-time) (days-to-time 30))))
      (lambda (item)
        (time-less-p one-month-ago (newsticker--age item)))))
  (setq newsticker-treeview-filter-functions (list #'my-newsticker-treeview-custom-filter)))

(define-key my-jump-keymap (kbd "t") #'newsticker-show-news)

(defun csv-parse-buffer (first-line-contains-keys &optional buffer coding-system)
  "Parse a buffer containing CSV data, return data as a list of alists or lists.
The first line in the buffer is interpreted as a header line
if FIRST-LINE-CONTAINS-KEYS is non-nil, resulting in a list of alists.
Otherwise, return a list of lists.

If BUFFER is non-nil it gives the buffer to be parsed.  If it is
nil the current buffer is parsed.

CODING-SYSTEM gives the coding-system for reading the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((lines (csv-parse-lines))
            header result)
        (when lines
          (if first-line-contains-keys
              (progn
                (setq header (car lines)
                      lines (cdr lines))
                (dolist (line lines)
                  (when line
                    (push (csv-combine-with-header header line) result))))
            (setq result (reverse lines))))
        result))))

(defun csv-parse-lines ()
  "Parse CSV lines in current buffer, returning a list of parsed lines.
Each line is represented as a list of field values."
  (let ((lines nil)
        (in-quoted nil)
        (current-line nil)
        (field-chars nil)
        (previous-char nil))
    (while (not (eobp))
      (let ((char (char-after)))
        (cond
         ;; Handle quoted field
         ((and (eq char ?\") (not (and in-quoted (eq previous-char ?\"))))
          (if in-quoted
              (setq in-quoted nil)
            (setq in-quoted t)))

         ;; Handle escaped quote within quoted field
         ((and (eq char ?\") in-quoted (eq previous-char ?\"))
          (push ?\" field-chars)
          (setq previous-char nil))

         ;; Handle field separator (comma)
         ((and (eq char ?,) (not in-quoted))
          (push (apply #'string (nreverse field-chars)) current-line)
          (setq field-chars nil))

         ;; Handle end of line
         ((and (eq char ?\n) (not in-quoted))
          (push (apply #'string (nreverse field-chars)) current-line)
          (push (nreverse current-line) lines)
          (setq field-chars nil)
          (setq current-line nil))

         ;; Handle carriage return (part of CRLF)
         ((and (eq char ?\r) (not in-quoted))
          nil)

         ;; Accumulate character
         (t
          (push char field-chars)))

        (setq previous-char char)
        (forward-char)))

    ;; Handle any remaining content
    (when (or field-chars current-line)
      (push (apply #'string (nreverse field-chars)) current-line)
      (push (nreverse current-line) lines))

    (nreverse lines)))

(defun csv-combine-with-header (header line)
  "Combine HEADER and LINE into an alist."
  (let ((result nil))
    (dotimes (i (min (length header) (length line)))
      (push (cons (nth i header) (nth i line)) result))
    (reverse result)))

(defun my-icomplete-exit-minibuffer-with-input ()
  "Exit the minibuffer with the current input, without forcing completion."
  (interactive)
  (exit-minibuffer))

(define-key icomplete-minibuffer-map (kbd "M-RET") 'my-icomplete-exit-minibuffer-with-input)

(defcustom my/quick-window-split-width-threshold 160
  "Minimum frame width (in columns) for a side-by-side split.
When the sole window is narrower than this, `my/quick-window-jump'
splits top/bottom instead so each pane keeps a usable width."
  :type 'integer
  :group 'convenience)

(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there is only a single window, split it: side-by-side when the
frame is wide enough (see `my/quick-window-split-width-threshold'),
otherwise top/bottom.
If there are only two windows, jump directly to the other window.
Side windows are ignored."
  (interactive)
  (let* ((window-list (seq-filter (lambda (w)
                                    (not (window-parameter w 'window-side)))
                                  (window-list nil 'no-mini))))
    (cond
     ((= (length window-list) 1)
      (if (< (window-total-width) my/quick-window-split-width-threshold)
          (split-window-vertically)
        (split-window-horizontally))
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

(defun my/rainbow-mode ()
  "Overlay colors represented as hex values in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'my/rainbow t)
  (let ((hex-color-regex "#[0-9a-fA-F]\\{3,6\\}"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hex-color-regex nil t)
        (let* ((color (match-string 0))
               (overlay (make-overlay (match-beginning 0) (match-end 0)))
               (fg-color (if (string-greaterp color "#888888") "black" "white")))
          (overlay-put overlay 'my/rainbow t)
          (overlay-put overlay 'face `(:background ,color :foreground ,fg-color))))))
  (when (derived-mode-p 'org-mode)
    (org-set-startup-visibility)))

(defun my/rainbow-mode-clear ()
  "Remove all hex color overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'my/rainbow t))

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
                             (list (when (executable-find "rg")
                                     '("rg --follow --files --null" . :string))
                                   (when (executable-find "find")
                                     '("find -type f -printf \"$PWD/%p\\0\"" . :string))
                                   (when (executable-find "fd")
                                     '("fd --absolute-path --type f -0" . :string))
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

(defun my/vc-dir-show-branches-and-tags (&optional branch-separator tag-separator)
  "Show Git branches and tags in the header line of the *vc-dir* buffer.
The current branch is highlighted. If BRANCH-SEPARATOR or TAG-SEPARATOR 
are provided, they are used to separate the branches or tags in the display."
  (interactive)
  (when (and (boundp 'vc-dir-backend) (eq vc-dir-backend 'Git))
    (let* ((default-directory (if (boundp 'vc-dir-directory) 
                                  vc-dir-directory 
                                default-directory))
           ;; Get branches
           (branches (split-string (shell-command-to-string "git branch") "\n" t "\\s-*"))
           ;; Get tags
           (tags (split-string (shell-command-to-string "git tag") "\n" t))
           ;; Get current commit hash
           (current-commit (string-trim (shell-command-to-string "git rev-parse HEAD")))
           ;; Default separators
           (branch-sep (or branch-separator " | "))
           (tag-sep (or tag-separator ", "))
           ;; Format branches
           (styled-branches (mapconcat
                             (lambda (branch)
                               (if (string-prefix-p "* " branch)
                                   (propertize (concat "*" (string-trim-left branch "\\* "))
                                               'face '(:weight bold))
                                 branch))
                             branches branch-sep))
           ;; Check which tags point to the current commit
           (current-tags '())
           (tag-part ""))
      
      ;; Find tags pointing to current commit
      (dolist (tag tags)
        (when (string-prefix-p 
               current-commit
               (string-trim (shell-command-to-string (format "git rev-parse %s" tag))))
          (push tag current-tags)))
      
      ;; Format tag display if we have any
      (when current-tags
        (setq tag-part 
              (concat " [Tags: " 
                      (propertize 
                       (mapconcat 'identity current-tags tag-sep)
                       'face '(:slant italic :foreground "goldenrod"))
                      "]")))
      
      ;; Set the header line
      (setq-local header-line-format
                  (concat "  Branches: " styled-branches tag-part)))))

;; Add the function to vc-dir-mode-hook
(add-hook 'vc-dir-mode-hook #'my/vc-dir-show-branches-and-tags)

;; Define advice function for refreshing branches and tags after switching
(defun my/after-vc-switch-branch (&rest _args)
  "Update branch and tag display in all vc-dir buffers after switching branches."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'vc-dir-mode)
        (my/vc-dir-show-branches-and-tags)))))

;; Add the advice to vc-git-branch function (handles git checkout)
(advice-add 'vc-create-branch :after #'my/after-vc-switch-branch)
(advice-add 'vc-switch-branch :after #'my/after-vc-switch-branch)

;; Let's also add a command to show all tags
(defun my/vc-dir-show-all-tags ()
  "Display all Git tags in a separate buffer."
  (interactive)
  (when (and (derived-mode-p 'vc-dir-mode)
             (eq vc-dir-backend 'Git))
    (let* ((default-directory (if (boundp 'vc-dir-directory) 
                                  vc-dir-directory 
                                default-directory))
           (buffer (get-buffer-create "*git-tags*"))
           (tags (shell-command-to-string "git tag -n"))) ; -n shows annotations
      (with-current-buffer buffer
        (erase-buffer)
        (insert "Git Tags:\n\n")
        (insert tags)
        (goto-char (point-min))
        (special-mode))
      (switch-to-buffer buffer))))

;; Lets show tracked files in Git!!
(defun my/vc-dir-show-tracked-files ()
  "Show all tracked files in the current vc-dir buffer."
  (interactive)
  (when (and (derived-mode-p 'vc-dir-mode)
             (eq vc-dir-backend 'Git))
    (let* ((default-directory (if (boundp 'vc-dir-directory) 
                                  vc-dir-directory 
                                default-directory))
           (files (split-string 
                   (shell-command-to-string "git ls-files")
                   "\n" t)))
      (vc-dir-refresh)
      (let ((buf (get-buffer-create "*vc-tracked-files*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Tracked files in %s:\n\n" default-directory))
            (dolist (file files)
              (insert (format "  %s\n" file)))
            (goto-char (point-min))
            (special-mode)))
        (pop-to-buffer buf)))))

;; Bind keys in vc-dir-mode
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "B") 'my/vc-dir-show-branches-and-tags)
  (define-key vc-dir-mode-map (kbd "T") 'my/vc-dir-show-all-tags)  ; New key for showing all tags
  (define-key vc-dir-mode-map (kbd "F") 'my/vc-dir-show-tracked-files)) ; Changed from T to F

(setq emacs-solo-dired-gutter-enabled t)

(defvar emacs-solo/dired-git-status-overlays nil
  "List of active overlays in Dired for Git status.")

(defun emacs-solo/dired--git-status-face (code)
  "Return a cons cell (STATUS . FACE) for a given Git porcelain CODE."
  (let* ((git-status-untracked "??")
         (git-status-modified " M")
         (git-status-modified-alt "M ")
         (git-status-deleted "D ")
         (git-status-added "A ")
         (git-status-renamed "R ")
         (git-status-copied "C ")
         (git-status-ignored "!!")
         (status (cond
                  ((string-match-p "\\?\\?" code) git-status-untracked)
                  ((string-match-p "^ M" code) git-status-modified)
                  ((string-match-p "^M " code) git-status-modified-alt)
                  ((string-match-p "^D" code) git-status-deleted)
                  ((string-match-p "^A" code) git-status-added)
                  ((string-match-p "^R" code) git-status-renamed)
                  ((string-match-p "^C" code) git-status-copied)
                  ((string-match-p "\\!\\!" code) git-status-ignored)
                  (t "  ")))
         (face (cond
                ((string= status git-status-ignored) 'shadow)
                ((string= status git-status-untracked) 'warning)
                ((string= status git-status-modified) 'font-lock-function-name-face)
                ((string= status git-status-modified-alt) 'font-lock-function-name-face)
                ((string= status git-status-deleted) 'error)
                ((string= status git-status-added) 'success)
                (t 'font-lock-keyword-face))))
    (cons status face)))

(defun emacs-solo/dired-git-status-overlay ()
  "Overlay Git status indicators on the first column in Dired."
  (interactive)
  (when (and emacs-solo-dired-gutter-enabled
             (not (file-remote-p default-directory))
             (executable-find "git"))
    (condition-case nil
        (progn
          (require 'vc-git)
          (let ((git-root (ignore-errors (vc-git-root default-directory))))
            (when git-root
              (setq git-root (expand-file-name git-root))
              (let* ((git-status (vc-git--run-command-string
                                  nil "status" "--porcelain"
                                  "--ignored" "--untracked-files=normal"))
                     (status-map (make-hash-table :test 'equal)))
                (mapc #'delete-overlay emacs-solo/dired-git-status-overlays)
                (setq emacs-solo/dired-git-status-overlays nil)
                (when git-status
                  (dolist (line (split-string git-status "\n" t))
                    (when (string-match "^\\(..\\) \\(.+\\)$" line)
                      (let* ((code (match-string 1 line))
                             (file (match-string 2 line))
                             (fullpath (expand-file-name file git-root))
                             (status-face (emacs-solo/dired--git-status-face code)))
                        (puthash fullpath status-face status-map)))))
                (save-excursion
                  (goto-char (point-min))
                  (while (not (eobp))
                    (let* ((file (ignore-errors
                                   (expand-file-name (dired-get-filename nil t)))))
                      (when file
                        (setq file (if (file-directory-p file)
                                       (concat file "/") file))
                        (let* ((status-face (gethash file status-map
                                                     (cons "  " 'font-lock-keyword-face)))
                               (status (car status-face))
                               (face (cdr status-face))
                               (status-str (propertize (format " %s " status)
                                                       'face face))
                               (ov (make-overlay (line-beginning-position)
                                                 (1+ (line-beginning-position)))))
                          (overlay-put ov 'before-string status-str)
                          (push ov emacs-solo/dired-git-status-overlays))))
                    (forward-line 1)))))))
      (error nil))))

(add-hook 'dired-after-readin-hook #'emacs-solo/dired-git-status-overlay)

(defun my/dired-revert-and-refresh (&rest _)
  "Revert dired buffer to refresh git status overlays."
  (when (derived-mode-p 'dired-mode)
    (revert-buffer)))

(with-eval-after-load 'dired
  (advice-add 'dired-do-delete :after #'my/dired-revert-and-refresh)
  (advice-add 'dired-do-rename :after #'my/dired-revert-and-refresh)
  (advice-add 'dired-do-copy :after #'my/dired-revert-and-refresh)
  (advice-add 'dired-do-flagged-delete :after #'my/dired-revert-and-refresh))

(defun my-git-diff-stash (stash-ref)
  "Diff working directory against specified stash"
  (interactive "sStash reference: ")
  (let ((buffer (get-buffer-create "*git-stash-diff*")))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process "git" nil buffer t "diff" (format "stash@{%s}" stash-ref))
      (diff-mode)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;; Bind to C-x v S (capital S for stash diff)
(define-key vc-prefix-map (kbd "S") 'my-git-diff-stash)

(defvar my/sync-ui-accent-color--current "orange"
  "Current accent color used by `my/sync-ui-accent-color'.
Updated on each call.  Used as the fallback when the function is
called without a COLOR argument (e.g. from theme-change hooks or
startup), so non-interactive callers never prompt.")

(defun my/sync-ui-accent-color (&optional color)
  "Synchronize various Emacs UI elements with a chosen accent color.
Affects mode-line, cursor, tab-bar, and other UI elements for a coherent theme.
When called interactively, prompts for COLOR.  When called from Lisp
without COLOR, reuses `my/sync-ui-accent-color--current'.
The function adjusts:
- Mode-line (active and inactive states)
- Cursor
- Tab-bar (active and inactive tabs)
- Window borders and dividers
- Highlighting
- Fringes"
  (interactive (list (read-color "Select accent color: ")))
  (let* ((accent-color (or color my/sync-ui-accent-color--current))
         (bg-color (face-background 'default))
         (fg-color (face-foreground 'default))
         (hl-color (face-background 'highlight))
         (inactive-fg-color (face-foreground 'mode-line-inactive))
         (is-dark-theme (not (string-greaterp bg-color "#888888")))
         (adjusted-bg-color (if is-dark-theme
                                (color-lighten-name bg-color 20)
                              (color-darken-name bg-color 5))))
    ;; Mode-line configuration
    (set-face-attribute 'mode-line nil 
                        :height 140 
                        :underline nil 
                        :overline nil 
                        :box nil
                        :background accent-color 
                        :foreground "#000000")
    (set-face-attribute 'mode-line-inactive nil 
                        :height 140 
                        :underline nil 
                        :overline nil
                        :background adjusted-bg-color 
                        :foreground "#aaaaaa")
    ;; Other UI elements configuration
    (if is-dark-theme
        (custom-set-faces
         `(cursor ((t (:background ,accent-color))))
         `(hl-line ((t (:background ,adjusted-bg-color))))
         `(vertical-border ((t (:foreground ,(color-darken-name fg-color 60)))))
         `(window-divider ((t (:foreground ,(color-darken-name fg-color 60)))))
         `(fringe ((t (:foreground ,bg-color :background ,bg-color))))
         `(tab-bar ((t (:inherit default :background ,bg-color :foreground ,fg-color))))
         `(tab-bar-tab ((t (:inherit 'highlight :background ,accent-color :foreground "#000000"))))
         `(tab-bar-tab-inactive ((t (:inherit default :background ,bg-color :foreground ,fg-color
                                              :box (:line-width 1 :color ,bg-color :style flat-button))))))
      (custom-set-faces
       `(cursor ((t (:background ,accent-color))))
       `(hl-line ((t (:background ,adjusted-bg-color))))
       `(vertical-border ((t (:foreground ,(color-darken-name fg-color 60)))))
       `(window-divider ((t (:foreground ,(color-darken-name fg-color 60)))))
       `(fringe ((t (:foreground ,bg-color :background ,bg-color))))
       `(tab-bar ((t (:inherit default :background "#000000" :foreground ,bg-color))))
       ;; `(tab-bar-tab ((t (:inherit 'highlight :box (:line-width 6 :color ,accent-color :style flat-button)))))
       `(tab-bar-tab ((t (:inherit 'highlight :background ,accent-color))))
       `(tab-bar-tab-inactive ((t (:inherit default :background ,bg-color :foreground ,fg-color
                                            :box (:line-width 1 :color ,bg-color :style flat-button)))))))
    (setq my/sync-ui-accent-color--current accent-color)))

(if (version<= "29.1" emacs-version)
    ;; Emacs 29.1+ — use the official theme hook
    (add-hook 'enable-theme-functions
              (lambda (_theme)
                (my/sync-ui-accent-color)))
  ;; Older Emacs — fall back to advising load-theme
  (progn
    (defun selected-window-accent-sync-tab-bar-to-theme--after (&rest _)
      (my/sync-ui-accent-color))
    (advice-add 'load-theme :after
                #'selected-window-accent-sync-tab-bar-to-theme--after)))

(defun my/consult-theme (theme)
  "Load THEME with live preview during candidate navigation.
A DIY replacement for `consult-theme'.  When called interactively,
previews each candidate as you navigate the `completing-read'.
On abort (C-g), restores the themes enabled before the preview began."
  (interactive
   (let* ((themes (mapcar #'symbol-name (custom-available-themes)))
          (saved  (copy-sequence custom-enabled-themes))
          (current nil)
          (preview
           (lambda ()
             (let* ((cand
                     (cond
                      ((and (bound-and-true-p icomplete-mode)
                            completion-all-sorted-completions)
                       (car completion-all-sorted-completions))
                      (t (minibuffer-contents-no-properties))))
                    (match (and cand (car (member cand themes)))))
               (when (and match (not (equal current match)))
                 (setq current match)
                 (mapc #'disable-theme custom-enabled-themes)
                 (condition-case nil
                     (load-theme (intern match) t)
                   (error nil)))))))
     (unwind-protect
         (list (intern
                (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'post-command-hook preview nil t))
                  (completing-read "Load theme: " themes nil t))))
       ;; Always restore originals — the body below loads the final pick.
       (mapc #'disable-theme custom-enabled-themes)
       (dolist (th (reverse saved))
         (condition-case nil (load-theme th t) (error nil))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(global-set-key (kbd "M-m") #'my/consult-theme)

(defun my/grep (search-term &optional directory glob)
  "Run ripgrep (rg) with SEARCH-TERM and optionally DIRECTORY and GLOB.
If ripgrep is unavailable, fall back to Emacs's rgrep command. Highlights SEARCH-TERM in results.
By default, only the SEARCH-TERM needs to be provided. If called with a
universal argument, DIRECTORY and GLOB are prompted for as well."
  (interactive
   (let* ((univ-arg current-prefix-arg)
          (default-search-term
           (cond
            ((use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end)))
            ((thing-at-point 'symbol t))
            ((thing-at-point 'word t))
            (t ""))))
     (list
      (read-string (if (string-empty-p default-search-term)
                       "Search for: "
                     (format "Search for (default `%s`): " default-search-term))
                   nil nil default-search-term)
      (when univ-arg (read-directory-name "Directory: "))
      (when univ-arg (read-string "File pattern (glob, default: ): " nil nil "")))))
  (let* ((directory (expand-file-name (or directory default-directory)))
         (glob (or glob ""))
         (buffer-name "*grep*"))
    (if (executable-find "rg")
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (setq default-directory directory)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n" directory))
              (if (not (string= "" glob))
                  (insert (format "[o] Glob: %s\n\n" glob)))
              (insert "Searching...\n\n"))
            (grep-mode)
            (setq-local my/grep-search-term search-term)
            (setq-local my/grep-directory directory)
            (setq-local my/grep-glob glob))
          
          (pop-to-buffer buffer)
          (goto-char (point-min))
          
          (make-process
           :name "ripgrep"
           :buffer buffer
           :command `("rg" "--color=never" "--max-columns=500" 
                      "--column" "--line-number" "--no-heading" 
                      "--smart-case" "-e" ,search-term
                      "--glob" ,glob ,directory)
           :filter (lambda (proc string)
                     (when (buffer-live-p (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (let ((inhibit-read-only t)
                               (moving (= (point) (process-mark proc))))
                           (setq string (replace-regexp-in-string "[\r\0\x01-\x08\x0B-\x0C\x0E-\x1F]" "" string))
                           ;; Replace full directory path with ./ in the incoming output
                           (setq string (replace-regexp-in-string 
                                         (concat "^" (regexp-quote directory))
                                         "./"
                                         string))
                           (save-excursion
                             (goto-char (process-mark proc))
                             (insert string)
                             (set-marker (process-mark proc) (point)))
                           (if moving (goto-char (process-mark proc)))))))
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   ;; Remove "Searching..." line
                   (goto-char (point-min))
                   (while (re-search-forward "Searching\\.\\.\\.\n\n" nil t)
                     (replace-match "" nil t))

                   ;; Clean up the output - replace full paths with ./
                   (goto-char (point-min))
                   (forward-line 3)
                   (let ((start-pos (point)))
                     (while (re-search-forward (concat "^" (regexp-quote directory)) nil t)
                       (replace-match "./" t t))
                     
                     ;; Check if any results were found
                     (goto-char start-pos)
                     (when (= (point) (point-max))
                       (insert "No results found.\n")))
                   
                   (goto-char (point-max))
                   (insert "\nRipgrep finished\n")

                   ;; Highlight search terms using grep's match face
                   (goto-char (point-min))
                   (forward-line 3)
                   (save-excursion
                     (while (re-search-forward (regexp-quote search-term) nil t)
                       (put-text-property (match-beginning 0) (match-end 0)
                                          'face 'match)
                       (put-text-property (match-beginning 0) (match-end 0)
                                          'font-lock-face 'match))))
                 
                 ;; Set up keybindings
                 (local-set-key (kbd "D") 
                                (lambda () 
                                  (interactive)
                                  (my/grep my/grep-search-term 
                                           (read-directory-name "New search directory: ")
                                           my/grep-glob)))
                 (local-set-key (kbd "S") 
                                (lambda () 
                                  (interactive)
                                  (my/grep (read-string "New search term: "
                                                        nil nil my/grep-search-term)
                                           my/grep-directory
                                           my/grep-glob)))
                 (local-set-key (kbd "o") 
                                (lambda () 
                                  (interactive)
                                  (my/grep my/grep-search-term
                                           my/grep-directory
                                           (read-string "New glob: "))))
                 (local-set-key (kbd "g") 
                                (lambda () 
                                  (interactive)
                                  (my/grep my/grep-search-term my/grep-directory my/grep-glob)))
                 
                 (goto-char (point-min))
                 (message "ripgrep finished."))))
           )
          (message "ripgrep started..."))
      ;; Fallback to rgrep
      (progn
        (setq default-directory directory)
        (message (format "%s : %s : %s" search-term glob directory))
        (rgrep search-term (if (string= "" glob) "*" glob) directory)))))

(defun my-org-reveal-on-next-error ()
  "Reveal the location of search results in an Org file."
  (when (derived-mode-p 'org-mode)
    (org-reveal)))

(add-hook 'next-error-hook 'my-org-reveal-on-next-error)

(setq ispell-local-dictionary "en_GB")
(setq ispell-program-name "hunspell")
(setq ispell-alternate-dictionary
      (expand-file-name "en_GB-words.txt"
                        (file-name-directory (or load-file-name buffer-file-name))))
(setq dictionary-default-dictionary "*")
(setq dictionary-server "dict.org")
(setq dictionary-use-single-buffer t)

(defun my/flyspell-add-word-to-dict ()
  "Add the word under point to the personal dictionary and refresh the errors list."
  (interactive)
  (when-let ((button (button-at (point))))
    (let* ((word (button-label button))
           (target-buffer (button-get button 'buffer))
           (target-pos (button-get button 'position)))

      ;; Switch to the source buffer, go to the word, and add it to dictionary
      (with-current-buffer target-buffer
        (save-excursion
          (goto-char target-pos)
          ;; Use ispell to add the word to the personal dictionary
          (ispell-send-string (concat "*" word "\n"))
          ;; Tell ispell we're done and the buffer hasn't changed
          (ispell-send-string "#\n")
          (when-let ((proc (get-process "ispell")))
            (accept-process-output proc 0.5))
          (message "Added '%s' to the dictionary." word)))

      (with-current-buffer target-buffer
        (pop-to-buffer target-buffer)
        (my/collect-flyspell-errors)))))

(defun my/collect-flyspell-errors ()
  "Collect all flyspell errors in the current buffer and display them in a separate buffer with clickable links."
  (interactive)
  ;; Store the buffer name and buffer itself for later reference
  (let* ((source-buffer (current-buffer))
         (source-buffer-name (buffer-name))
         (error-list nil))
    
    ;; Ensure the buffer is fully spell-checked
    (flyspell-buffer)
    
    ;; Collect all misspelled words and their positions
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((overlays (overlays-at (point)))
              (moved nil))
          (dolist (overlay overlays)
            (when (overlay-get overlay 'flyspell-overlay)
              (let ((start (overlay-start overlay))
                    (end (overlay-end overlay))
                    (word (buffer-substring-no-properties 
                           (overlay-start overlay) 
                           (overlay-end overlay)))
                    (line-num (line-number-at-pos (overlay-start overlay))))
                ;; Store the buffer name string rather than buffer object
                (push (list word start end line-num) error-list)
                (goto-char end)
                (setq moved t))))
          (unless moved
            (forward-char 1)))))
    
    ;; Sort by position in buffer
    (setq error-list (nreverse error-list))
    
    ;; Create and populate the errors buffer
    (let ((errors-buffer (get-buffer-create "*Flyspell Errors*")))
      (with-current-buffer errors-buffer
        (let ((inhibit-read-only t))
          (pop-to-buffer errors-buffer)
          (visual-line-mode 1)
          (erase-buffer)
          (insert (format "Flyspell Errors in %s (%d found)\n\n" 
                          source-buffer-name
                          (length error-list)))
          
          ;; Add all the errors with buttons
          (dolist (error-info error-list)
            (let ((word (nth 0 error-info))
                  (start (nth 1 error-info)))
              
              ;; Store position as a text property for the button
              (insert-button word
                             'follow-link t
                             'help-echo "Click to jump to this misspelled word"
                             'buffer source-buffer
                             'position start
                             'action (lambda (button)
                                       (switch-to-buffer (button-get button 'buffer))
                                       (goto-char (button-get button 'position))
                                       (recenter)))
              (insert " ")))
          
          (special-mode)

          ;; keybindings
          (local-set-key (kbd "g") 
                         (lambda () 
                           (interactive)
                           (let ((button (button-at (point))))
                             (with-current-buffer (button-get button 'buffer)
                               (my/collect-flyspell-errors)))))
          (local-set-key (kbd "a") 'my/flyspell-add-word-to-dict)
          (local-set-key (kbd "q") 'quit-window))))))

(defun spelling-menu ()
  "Menu for spelling."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- Spelling [q] Quit: -------
[s] Spelling
[l] Summary"
               'face 'minibuffer-prompt))))
    (pcase key
      ;; Spelling
      (?s (progn
            (flyspell-buffer)
            (call-interactively 'flyspell-mode)))
      (?l (call-interactively 'my/collect-flyspell-errors))
      ;; Quit
      (?q (message "Quit menu."))
      (?\C-g (message "Quit menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c s") #'spelling-menu)
(global-set-key (kbd "M-4") #'ispell-word)

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
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)

(defun my/import-bash-history-to-eshell ()
  "Import bash history into eshell history file, once per session."
  (unless (bound-and-true-p my/bash-history-imported)
    (let ((bash-history-file "~/.bash_history")
          (eshell-history-file (expand-file-name "eshell/history" user-emacs-directory)))
      (when (file-exists-p bash-history-file)
        (with-temp-buffer
          (insert-file-contents bash-history-file)
          (append-to-file (buffer-string) nil eshell-history-file))))
    (setq my/bash-history-imported t)))

(add-hook 'eshell-mode-hook #'my/import-bash-history-to-eshell)

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
  "Return a list of buffers matching pop-up patterns but excluding specific buffers."
  (let ((popup-patterns '("\\*\.*shell\.*\\*"
                          "\\*\.*term\.*\\*"
                          "\\*eldoc\.*\\*"
                          "\\*Flymake\.*"))
        (exclusion-patterns '("\\*shell\\*-comint-indirect")))
    (seq-filter (lambda (buf)
                  (let ((bufname (buffer-name buf)))
                    (and (seq-some (lambda (pattern)
                                     (string-match-p pattern bufname))
                                   popup-patterns)
                         (not (seq-some (lambda (pattern)
                                          (string-match-p pattern bufname))
                                        exclusion-patterns)))))
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
(global-set-key (kbd "C-x j") #'my/popper-toggle-current)
(global-set-key (kbd "C-'") #'my/popper-toggle-current)
(global-set-key (kbd "C-`") #'my/popper-toggle-current)

;; Cycle through popups or show the next popup.
(global-set-key (kbd "C-M-<return>") #'my/popper-cycle-popup)

(defun my/md-to-org-convert-buffer ()
  "Convert the current buffer from Markdown to Org-mode format."
  (interactive)
  (save-excursion
    ;; First, protect code blocks by replacing them with placeholders
    (let ((code-blocks '())
          (counter 0))
      ;; Collect and replace code blocks with placeholders
      (goto-char (point-min))
      (while (re-search-forward "^\\([ \t]*\\)```\\([^\n]*\\)\n\\(\\(?:.*\n\\)*?\\)\\1```" nil t)
        (let ((indent (match-string 1))
              (lang (match-string 2))
              (code (match-string 3)))
          (push (list counter indent lang code) code-blocks)
          (replace-match (format "<<<CODE-BLOCK-%d>>>" counter))
          (setq counter (1+ counter))))

      ;; Also protect inline code
      (let ((inline-codes '())
            (inline-counter 0))
        (goto-char (point-min))
        (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
          (push (cons inline-counter (match-string 1)) inline-codes)
          (replace-match (format "<<<INLINE-CODE-%d>>>" inline-counter))
          (setq inline-counter (1+ inline-counter)))

        ;; Now do all the conversions safely

        ;; Headers: # -> * (must come before list conversion)
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\) " nil t)
          (replace-match (concat (make-string (length (match-string 1)) ?*) " ")))

        ;; Lists: -, *, + -> -
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*\\)[*+-] " nil t)
          (replace-match "\\1- "))

        ;; Bold: **text** -> *text*
        (goto-char (point-min))
        (while (re-search-forward "\\*\\*\\(.+?\\)\\*\\*" nil t)
          (replace-match "*\\1*"))

        ;; Italics: _text_ or *text* (single) -> /text/
        ;; Only match _text_ to avoid conflicting with bold
        (goto-char (point-min))
        (while (re-search-forward "\\_<_\\([^_]+\\)_\\_>" nil t)
          (replace-match "/\\1/"))

        ;; Images: ![alt](url) -> [[url]] (must come before links)
        (goto-char (point-min))
        (while (re-search-forward "!\\[\\(?:[^]]*\\)\\](\\([^)]+\\))" nil t)
          (replace-match "[[\\1]]"))

        ;; Links: [text](url) -> [[url][text]]
        (goto-char (point-min))
        (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
          (replace-match "[[\\2][\\1]]"))

        ;; Horizontal rules
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*\\(-\\{3,\\}\\|\\*\\{3,\\}\\|_\\{3,\\}\\)[ \t]*$" nil t)
          (replace-match "-----"))

        ;; Blockquotes: > text -> : text (simple version)
        (goto-char (point-min))
        (while (re-search-forward "^> \\(.*\\)$" nil t)
          (replace-match ": \\1"))

        ;; Fix encoding issues
        (goto-char (point-min))
        (while (re-search-forward "â€" nil t)
          (replace-match "—"))

        ;; Restore inline code as =code=
        (dolist (item inline-codes)
          (goto-char (point-min))
          (when (search-forward (format "<<<INLINE-CODE-%d>>>" (car item)) nil t)
            (replace-match (format "=%s=" (cdr item)) t t))))

      ;; Restore code blocks as #+begin_src ... #+end_src
      (dolist (item code-blocks)
        (let ((n (nth 0 item))
              (indent (nth 1 item))
              (lang (nth 2 item))
              (code (nth 3 item)))
          (goto-char (point-min))
          (when (search-forward (format "<<<CODE-BLOCK-%d>>>" n) nil t)
            (replace-match (format "%s#+begin_src %s\n%s%s#+end_src"
                                   indent lang code indent)
                           t t)))))))

(defun my/md-to-org-convert-file (input-file output-file)
  "Convert a Markdown file INPUT-FILE to an Org-mode file OUTPUT-FILE."
  (with-temp-buffer
    (insert-file-contents input-file)
    (my/md-to-org-convert-buffer)
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

(defvar my/popup-max 8
  "Maximum visible candidates in popup.")

(defvar my/popup-width 70
  "Maximum width of popup candidates in characters.")

(defface my/popup-face
  '((((background dark))
     :background "#1e1e2e" :foreground "#bac2de" :extend t)
    (t
     :background "#eff1f5" :foreground "#4c4f69" :extend t))
  "Face for popup completion candidates.")

(defface my/popup-current-face
  '((((background dark))
     :background "#45475a" :foreground "#cdd6f4" :extend t)
    (t
     :background "#ccd0da" :foreground "#4c4f69" :extend t))
  "Face for selected popup candidate.")

(defvar-local my/popup--ovs nil "List of popup overlays.")
(defvar-local my/popup--cands nil "Completion candidates.")
(defvar-local my/popup--idx 0 "Selected index.")
(defvar-local my/popup--beg nil "Start of completion prefix.")
(defvar-local my/popup--table nil "Completion table.")
(defvar-local my/popup--pred nil "Completion predicate.")
(defvar-local my/popup--plist nil "Capf extra properties.")
(defvar-local my/popup--scroll 0 "Scroll offset.")
(defvar-local my/popup--source "" "Name of the capf that provided candidates.")

(defvar my/popup-active-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'my/popup-next)
    (define-key map (kbd "C-p") #'my/popup-prev)
    (define-key map (kbd "RET") #'my/popup-insert)
    (define-key map (kbd "TAB") #'my/popup-insert)
    (define-key map (kbd "C-g") #'my/popup-abort)
    map)
  "Keymap for active popup.")

(define-minor-mode my/popup-active-mode
  "Transient minor mode active while popup is showing."
  :keymap my/popup-active-map)

(defun my/popup--get-candidates (prefix table pred)
  "Get sorted candidates matching PREFIX from TABLE with PRED."
  (let* ((md (completion-metadata prefix table pred))
         (all (completion-all-completions
               prefix table pred (length prefix) md))
         (sort-fn (or (completion-metadata-get
                       md 'display-sort-function)
                      #'identity)))
    (when (consp all)
      (when (numberp (cdr (last all)))
        (setcdr (last all) nil))
      (setq all (cl-remove-if
                 (lambda (s)
                   (or (not (stringp s))
                       (zerop (string-width
                               (string-trim (substring-no-properties s))))))
                 all))
      (funcall sort-fn all))))

(defun my/popup--ann-fn ()
  "Get annotation function from metadata or capf plist."
  (or (completion-metadata-get
       (completion-metadata
        (buffer-substring-no-properties my/popup--beg (point))
        my/popup--table my/popup--pred)
       'annotation-function)
      (plist-get my/popup--plist :annotation-function)))

(defun my/popup--render ()
  "Display popup by overlaying existing lines below point."
  (my/popup--hide)
  (when my/popup--cands
    (let* ((col (save-excursion
                  (goto-char my/popup--beg)
                  (current-column)))
           (n (length my/popup--cands))
           (_ (cond
               ((>= my/popup--idx
                    (+ my/popup--scroll my/popup-max))
                (setq my/popup--scroll
                      (1+ (- my/popup--idx my/popup-max))))
               ((< my/popup--idx my/popup--scroll)
                (setq my/popup--scroll my/popup--idx))))
           (end-idx (min n (+ my/popup--scroll my/popup-max)))
           (visible (seq-subseq my/popup--cands
                                my/popup--scroll end-idx))
           (ann-fn (my/popup--ann-fn))
           (widest (if visible
                       (apply #'max
                              (mapcar #'string-width visible))
                     10))
           (w my/popup-width)
           (popup-lines
            (cl-loop
             for c in visible
             for i from my/popup--scroll
             for sel = (= i my/popup--idx)
             for face = (if sel
                            'my/popup-current-face
                          'my/popup-face)
             for ann = (replace-regexp-in-string
                        "[\n\r]" ""
                        (if ann-fn (or (funcall ann-fn c) "") ""))
             for tag = (if (string= my/popup--source "") ""
                         (concat "(" my/popup--source ")"))
             for left = (concat " "
                         (substring-no-properties c)
                         " " ann)
             for left-trunc = (truncate-string-to-width
                               left (- w (1+ (length tag))) 0 ?\s)
             for raw = (concat left-trunc " " tag)
             for text = (truncate-string-to-width raw w 0 ?\s)
             collect (propertize text 'face face)))
           (counter-line
            (propertize
             (truncate-string-to-width
              (format " [%d/%d %s]" (1+ my/popup--idx) n my/popup--source)
              w 0 ?\s)
             'face 'my/popup-face))
           (all-lines (if counter-line
                          (append popup-lines
                                  (list counter-line))
                        popup-lines)))
      (save-excursion
        (let ((past-end nil)
              (extra-lines nil))
          (dolist (pline all-lines)
            (if (and (not past-end)
                     (zerop (forward-line 1)))
                (let* ((bol (line-beginning-position))
                       (eol (line-end-position))
                       (end (min (1+ eol) (point-max)))
                       (orig (buffer-substring
                              bol eol))
                       (prefix
                        (truncate-string-to-width
                         orig col 0 ?\s))
                       (ov (make-overlay bol end)))
                  (overlay-put ov 'display
                               (concat prefix pline
                                       (if (< eol (point-max)) "\n" "")))
                  (overlay-put ov 'priority 1000)
                  (push ov my/popup--ovs))
              (setq past-end t)
              (push pline extra-lines)))
          (when extra-lines
            (let* ((pad (make-string col ?\s))
                   (str (mapconcat
                         (lambda (l) (concat pad l))
                         (nreverse extra-lines) "\n"))
                   (ov (make-overlay (point-max)
                                     (point-max))))
              (overlay-put ov 'after-string
                           (concat "\n" str))
              (overlay-put ov 'priority 1000)
              (push ov my/popup--ovs)))))
      (my/popup-active-mode 1))))

(defun my/popup--hide ()
  "Remove all popup overlays."
  (dolist (ov my/popup--ovs)
    (when (overlayp ov) (delete-overlay ov)))
  (setq my/popup--ovs nil))

(defun my/popup-next ()
  "Select next candidate."
  (interactive)
  (when my/popup--cands
    (setq my/popup--idx
          (mod (1+ my/popup--idx)
               (length my/popup--cands)))
    (my/popup--render)))

(defun my/popup-prev ()
  "Select previous candidate."
  (interactive)
  (when my/popup--cands
    (setq my/popup--idx
          (mod (1- my/popup--idx)
               (length my/popup--cands)))
    (my/popup--render)))

(defun my/popup-insert ()
  "Insert selected candidate, run exit-function, and close."
  (interactive)
  (when (and my/popup--cands my/popup--beg)
    (let ((chosen (nth my/popup--idx my/popup--cands))
          (exit-fn (plist-get my/popup--plist :exit-function)))
      (when chosen
        (delete-region my/popup--beg (point))
        (insert chosen)
        (when exit-fn
          (funcall exit-fn chosen 'finished)))))
  (my/popup-abort))

(defun my/popup-abort ()
  "Dismiss the popup."
  (interactive)
  (my/popup--hide)
  (my/popup-active-mode -1)
  (setq my/popup--cands nil
        my/popup--idx 0
        my/popup--beg nil
        my/popup--table nil
        my/popup--pred nil
        my/popup--plist nil
        my/popup--ovs nil
        my/popup--scroll 0
        my/popup--source ""))

(defun my/popup--start (beg cands table pred plist)
  "Start popup with BEG, CANDS, TABLE, PRED, and PLIST."
  (setq my/popup--beg beg
        my/popup--cands cands
        my/popup--table table
        my/popup--pred pred
        my/popup--plist plist
        my/popup--idx 0
        my/popup--scroll 0)
  (if (= (length cands) 1)
      (progn
        (delete-region beg (point))
        (insert (car cands))
        (let ((exit-fn (plist-get plist :exit-function)))
          (when exit-fn
            (funcall exit-fn (car cands) 'finished))))
    (my/popup--render)))

(defun my/popup-completion-in-region (beg end table &optional pred)
  "Show popup for region BEG..END using TABLE and PRED.
Picks up extra capf properties via `completion-extra-properties'."
  (my/popup-abort)
  (let* ((prefix (buffer-substring-no-properties beg end))
         (plist (bound-and-true-p completion-extra-properties))
         (cands (my/popup--get-candidates prefix table pred)))
    (when cands
      (setq my/popup--source "completion")
      (my/popup--start beg cands table pred plist))))

(defun my/popup--refresh ()
  "Re-query candidates after prefix changed."
  (when (and my/popup--beg my/popup--table
             (>= (point) my/popup--beg))
    (let* ((prefix (buffer-substring-no-properties
                    my/popup--beg (point)))
           (cands (my/popup--get-candidates
                   prefix my/popup--table my/popup--pred)))
      (if cands
          (progn
            (setq my/popup--cands cands
                  my/popup--idx
                  (min my/popup--idx (1- (length cands)))
                  my/popup--scroll
                  (min my/popup--scroll
                       (max 0 (- (length cands)
                                 my/popup-max))))
            (my/popup--render))
        (my/popup-abort)))))

(defun my/popup--post-command ()
  "Handle live filtering and popup dismissal."
  (when my/popup-active-mode
    (cond
     ((memq this-command
            '(my/popup-next my/popup-prev
              my/popup-insert my/popup-abort
              my/popup-complete completion-at-point))
      nil)
     ((and my/popup--beg (>= (point) my/popup--beg)
           (or (eq this-command 'self-insert-command)
               (get this-command 'delete-selection)
               (memq this-command
                     '(delete-backward-char
                       backward-delete-char-untabify
                       c-electric-backspace
                       org-self-insert-command))))
      (my/popup--refresh))
     (t (my/popup-abort)))))

(add-hook 'post-command-hook #'my/popup--post-command)

(defun my/popup--capf-name (fn)
  "Return a short display name for capf function FN."
  (let ((name (symbol-name fn)))
    (cond
     ((string-match-p "dabbrev" name) "dabbrev")
     ((string-match-p "eglot" name) "eglot")
     ((string-match-p "elisp" name) "elisp")
     ((string-match-p "eshell" name) "eshell")
     ((string-match-p "shell" name) "shell")
     ((string-match-p "ispell" name) "ispell")
     ((string-match-p "tag" name) "tags")
     ((string-match-p "file" name) "file")
     (t (replace-regexp-in-string
         "\\(?:-capf\\|-completion-at-point\\)" "" name)))))

(defun my/popup-complete ()
  "Trigger popup completion at point by querying capf directly."
  (interactive)
  (my/popup-abort)
  (let ((data nil)
        (source ""))
    (cl-loop for fn in completion-at-point-functions
             when (and (functionp fn)
                       (setq data (funcall fn)))
             do (progn
                  (setq source (my/popup--capf-name fn))
                  (cl-return)))
    (if (and data (>= (length data) 3))
        (let* ((beg (nth 0 data))
               (end (nth 1 data))
               (table (nth 2 data))
               (plist (nthcdr 3 data))
               (pred (plist-get plist :predicate))
               (prefix (buffer-substring-no-properties beg end))
               (cands (my/popup--get-candidates
                       prefix table pred)))
          (if cands
              (progn
                (setq my/popup--source source)
                (my/popup--start beg cands table pred plist))
            (message "No completions for '%s'" prefix)))
      (message "No completion backend at point"))))

(defun my/dabbrev-capf ()
  "Completion-at-point function using dabbrev as a fallback."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties beg end))
             (inhibit-message t)
             (message-log-max nil))
        (ignore-errors
          (dabbrev--reset-global-variables)
          (let ((completions (dabbrev--find-all-expansions prefix nil)))
            (when completions
              (list beg end completions
                    :exclusive 'no
                    :annotation-function
                    (lambda (_) " (dabbrev)")))))))))

(add-hook 'completion-at-point-functions #'my/dabbrev-capf 100)

(setq tab-always-indent 'complete)
(setq completion-in-region-function #'my/popup-completion-in-region)
(global-set-key (kbd "C-c TAB") #'my/popup-complete)
(global-set-key (kbd "C-c <tab>") #'my/popup-complete)
(global-set-key (kbd "C-M-i") #'my/popup-complete)
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

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

(with-eval-after-load 'org
  (require 'ox-publish))

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
           list "")
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
          (directory-files-recursively default-directory "\\.cs$" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.ads$\\|\\.adb$\\)" nil 'predicate-exclusion-p)))
        (tags-file-path (expand-file-name (concat default-directory "TAGS"))))
    (unless (file-directory-p default-directory)
      (error "Default directory does not exist: %s" default-directory))
    ;; Generate TAGS file
    (dolist (file all-files)
      (message file)
      (shell-command (format "etags --append %s -o %s" file tags-file-path)))))
(global-set-key (kbd "C-x p l") 'my/etags-load)
(global-set-key (kbd "C-x p u") 'my/etags-update)

(defun simple-orderless-completion (string table pred point)
  "Enhanced orderless completion with better partial matching."
  (let* ((words (split-string string "[-, ]+"))
         (patterns (mapcar (lambda (word)
                             (concat "\\b.*" (regexp-quote word) ".*"))
                           words))
         (full-regexp (mapconcat 'identity patterns "")))
    (if (string-empty-p string)
        (all-completions "" table pred)
      (cl-remove-if-not
       (lambda (candidate)
         (let ((case-fold-search completion-ignore-case))
           (and (cl-every (lambda (word)
                            (string-match-p
                             (concat "\\b.*" (regexp-quote word))
                             candidate))
                          words)
                t)))
       (all-completions "" table pred)))))

;; Register the completion style
(add-to-list 'completion-styles-alist
             '(simple-orderless simple-orderless-completion
                                simple-orderless-completion))

;; Set different completion styles for minibuffer vs other contexts
(defun setup-minibuffer-completion-styles ()
  "Use orderless completion in minibuffer, regular completion elsewhere."
  ;; For minibuffer: use orderless first, then fallback to flex and basic
  (setq-local completion-styles '(basic simple-orderless flex substring)))

;; Hook into minibuffer setup
(add-hook 'minibuffer-setup-hook #'setup-minibuffer-completion-styles)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "W") 'dired-do-async-shell-command)
  (setq dired-guess-shell-alist-user
        '(("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\)$" "gthumb")
          ("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|mpg\\)$" "mpv")
          ("\\.\\(mp3\\|wav\\|ogg\\|\\)$" "mpv")
          ("\\.\\(kra\\)$" "krita")
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
                           (t 2)))
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
                                         (dabbrev-case-replace nil)
                                         (inhibit-message t)
                                         (message-log-max nil))
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

(defun simple-autosuggest-end-of-line (arg)
  "Move to end of line, accepting suggestion first if available.
Works with both standard `move-end-of-line` and `org-end-of-line`."
  (interactive "^p")
  (if-let ((overlay simple-autosuggest--overlay)
           (suggestion (overlay-get overlay 'after-string)))
      (progn
        (insert (substring-no-properties suggestion))
        (overlay-put overlay 'after-string nil))
    ;; Detect whether we're in org-mode and use the appropriate function
    (if (and (eq major-mode 'org-mode)
             (fboundp 'org-end-of-line))
        (org-end-of-line arg)
      (move-end-of-line arg))))

(defun simple-autosuggest-update ()
  "Update the auto-suggestion overlay."
  (when simple-autosuggest--overlay
    (unless (simple-autosuggest--get-completion nil nil)
      (overlay-put simple-autosuggest--overlay 'after-string nil))))

(define-minor-mode simple-autosuggest-mode
  "Minor mode for showing auto-suggestions from history or dabbrev completion."
  :lighter " SAM"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Use a unified function for both cases
            (define-key map [remap move-end-of-line] #'simple-autosuggest-end-of-line)
            (when (fboundp 'org-end-of-line)
              ;; If org-mode is loaded, also remap org-end-of-line
              (define-key map [remap org-end-of-line] #'simple-autosuggest-end-of-line))
            ;; Explicitly bind C-e which is commonly used
            (define-key map (kbd "C-e") #'simple-autosuggest-end-of-line)
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

(defvar my/dired-icons-map
  '(("el" . "λ") ("rb" . "◆") ("js" . "○") ("ts" . "●") ("json" . "◎") ("md" . "■")
    ("txt" . "□") ("html" . "▲") ("css" . "▼") ("png" . "◉") ("jpg" . "◉")
    ("pdf" . "▣") ("zip" . "▢") ("py" . "∆") ("c" . "◇") ("sql" . "▦")
    ("mp3" . "♪") ("mp4" . "▶") ("exe" . "▪")))

(defvar-local my/dired-icon-overlays nil
  "List of icon overlays in current dired buffer.")

(defun my/dired-add-icons ()
  "Add file type icon overlays in dired."
  (when (derived-mode-p 'dired-mode)
    (mapc #'delete-overlay my/dired-icon-overlays)
    (setq my/dired-icon-overlays nil)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case nil
            (when-let ((filename (dired-get-filename 'no-dir t)))
              (when (dired-move-to-filename t)
                (let* ((is-dir (file-directory-p (dired-get-filename nil t)))
                       (icon (cond (is-dir "▶")
                                   ((string-match "\\.\\([^.]+\\)$" filename)
                                    (or (cdr (assoc (downcase (match-string 1 filename))
                                                    my/dired-icons-map))
                                        "◦"))
                                   (t "◦")))
                       (ov (make-overlay (point) (point))))
                  (overlay-put ov 'before-string (concat icon " "))
                  (overlay-put ov 'my/dired-icon t)
                  (push ov my/dired-icon-overlays))))
          (error nil))
        (forward-line)))))

(add-hook 'dired-after-readin-hook #'my/dired-add-icons)

(defun my/dired-collapse--deepest (dir)
  "Return the deepest single-child descendant directory of DIR.
Walks the directory chain as long as each directory contains exactly
one entry which is itself an accessible directory.  Stops after 100
iterations to guard against symlink cycles."
  (let ((current dir)
        (depth 0))
    (catch 'done
      (while (< depth 100)
        (let ((entries (condition-case nil
                           (directory-files current t
                                            directory-files-no-dot-files-regexp
                                            t)
                         (error nil))))
          (if (and entries
                   (null (cdr entries))
                   (file-directory-p (car entries))
                   (file-accessible-directory-p (car entries)))
              (setq current (car entries)
                    depth (1+ depth))
            (throw 'done current)))))
    current))

(defun my/dired-collapse ()
  "Collapse single-child directory chains in the current dired buffer.
A DIY replacement for `dired-collapse-mode' from the dired-hacks
package.  Rewrites the filename portion of each line in place and
reapplies the `dired-filename' text property so that standard dired
navigation still resolves to the deepest directory."
  (when (derived-mode-p 'dired-mode)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case nil
              (let ((file (dired-get-filename nil t)))
                (when (and file
                           (file-directory-p file)
                           (not (member (file-name-nondirectory
                                         (directory-file-name file))
                                        '("." "..")))
                           (file-accessible-directory-p file))
                  (let ((deepest (my/dired-collapse--deepest file)))
                    (unless (string= deepest file)
                      (when (dired-move-to-filename)
                        (let* ((start (point))
                               (end (dired-move-to-end-of-filename t))
                               (displayed (buffer-substring-no-properties
                                           start end))
                               (suffix (substring deepest
                                                  (1+ (length file))))
                               (new (concat displayed "/" suffix)))
                          (delete-region start end)
                          (goto-char start)
                          (insert (propertize new
                                              'face 'dired-directory
                                              'mouse-face 'highlight
                                              'dired-filename t))))))))
            (error nil))
          (forward-line))))))

(add-hook 'dired-after-readin-hook #'my/dired-collapse)

(defun my/dired-collapse--find-hit (target-dir)
  "Return (BUFFER . POS) of a dired buffer with a collapsed line covering TARGET-DIR.
TARGET-DIR is an absolute directory name (trailing slash added if
missing).  A collapsed line \"covers\" TARGET-DIR when the line's
deepest path lies anywhere along the chain between the buffer's
`default-directory' (strict ancestor) and TARGET-DIR — in particular
when the collapsed chain's deepest path is equal to TARGET-DIR or a
descendant of it."
  (let ((target (file-name-as-directory (expand-file-name target-dir)))
        hit)
    (dolist (buf (buffer-list))
      (unless hit
        (with-current-buffer buf
          (when (and (derived-mode-p 'dired-mode)
                     (stringp default-directory))
            (let ((buf-dir (file-name-as-directory
                            (expand-file-name default-directory))))
              (when (and (string-prefix-p buf-dir target)
                         (not (string= buf-dir target)))
                (save-excursion
                  (goto-char (point-min))
                  (catch 'found
                    (while (not (eobp))
                      (let ((line-file (ignore-errors
                                         (dired-get-filename nil t))))
                        (when (and line-file
                                   (file-directory-p line-file))
                          (let ((line-dir (file-name-as-directory
                                           (expand-file-name line-file))))
                            (when (string-prefix-p target line-dir)
                              (setq hit (cons buf (point)))
                              (throw 'found nil)))))
                      (forward-line))))))))))
    hit))

(defun my/dired-collapse--up-advice (orig-fn &optional other-window)
  "Around-advice for `dired-up-directory' restoring collapsed round-trip.
When the literal parent of `default-directory' is not already open as a
dired buffer but an existing dired buffer shows the current directory
inside a collapsed chain, pop to that buffer and place point on the
collapsed line instead of opening a fresh dired for the literal parent."
  (let* ((dir (and (derived-mode-p 'dired-mode)
                   (stringp default-directory)
                   (expand-file-name default-directory)))
         (up (and dir (file-name-directory (directory-file-name dir))))
         (parent-buf (and up (dired-find-buffer-nocreate up)))
         (hit (and dir (null parent-buf)
                   (my/dired-collapse--find-hit dir))))
    (if hit
        (let ((buf (car hit))
              (pos (cdr hit)))
          (if other-window
              (switch-to-buffer-other-window buf)
            (pop-to-buffer-same-window buf))
          (goto-char pos)
          (dired-move-to-filename))
      (funcall orig-fn other-window))))

(advice-add 'dired-up-directory :around #'my/dired-collapse--up-advice)

(defun emacs-solo/ollama-run-model ()
  "Run `ollama list`, let the user choose a model, and open it in `ansi-term`.
Asks for a prompt when run. If none is passed (RET), starts it interactive.
If a region is selected, prompt for additional input and pass it as a query."
  (interactive)
  (let* ((output (shell-command-to-string "ollama list"))
         (models (let ((lines (split-string output "\n" t)))
                   (mapcar (lambda (line) (car (split-string line))) (cdr lines))))
         (selected (completing-read "Select Ollama model: " models nil t))
         (region-text (when (use-region-p)
                        (shell-quote-argument
                         (replace-regexp-in-string "\n" " "
                                                   (buffer-substring-no-properties
                                                    (region-beginning)
                                                    (region-end))))))
         (prompt (read-string "Ollama Prompt (leave it blank for interactive): " nil nil nil)))
    (when (and selected (not (string-empty-p selected)))
      (ansi-term "/bin/sh")
      (sit-for 1)
      (let ((args (list (format "ollama run %s"
                                selected))))
        (when (and prompt (not (string-empty-p prompt)))
          (setq args (append args (list (format "\"%s\"" prompt)))))
        (when region-text
          (setq args (append args (list (format "\"%s\"" region-text)))))

        (term-send-raw-string (string-join args " "))
        (term-send-raw-string "\n")))))

(defun tiny-diminish (mode &optional replacement)
  "Hide or replace modeline display of minor MODE with REPLACEMENT."
  (when-let ((entry (assq mode minor-mode-alist)))
    (setcdr entry (list (or replacement "")))))

(tiny-diminish 'abbrev-mode)
(tiny-diminish 'visual-line-mode)
(tiny-diminish 'org-indent-mode)

(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 350 \"%s\"" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(define-minor-mode thanos/type-mode
  "Minor mode for inserting text via wtype."
  :keymap `((,(kbd "C-c C-c") . ,(lambda () (interactive)
                                   (call-process-shell-command
                                    (thanos/wtype-text (buffer-string))
                                    nil 0)
                                   (delete-frame)))
            (,(kbd "C-c C-k") . ,(lambda () (interactive)
                                   (kill-buffer (current-buffer))))))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (flyspell-mode)
      (thanos/type-mode)
      (setq-local header-line-format
                  (format " %s to insert text or %s to cancel."
                          (propertize "C-c C-c" 'face 'help-key-binding)
			  (propertize "C-c C-k" 'face 'help-key-binding)))
      ;; Make the frame more temporary-like
      (set-frame-parameter frame 'delete-before-kill-buffer t)
      (set-window-dedicated-p (selected-window) t))))

(server-start)

(defun rsvp-minibuffer ()
  "Display words from point (or mark to point) in minibuffer using RSVP.
Use f/s for speed, [/] for size, b/n to skip, SPC to pause, q to quit."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning) (point)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties start end))
         (wpm 350) (font-size 200) (orp-column 20)
         (word-positions '()) (pos 0) (i 0)
         (message-log-max nil))  ; Disable message logging
    ;; Build word positions list
    (dolist (word (split-string text))
      (unless (string-blank-p word)
        (when-let ((word-start (string-match (regexp-quote word) text pos)))
          (push (cons word (+ start word-start)) word-positions)
          (setq pos (+ word-start (length word))))))
    (setq word-positions (nreverse word-positions))
    ;; Display loop
    (while (< i (length word-positions))
      (let* ((word (car (nth i word-positions)))
             (word-pos (cdr (nth i word-positions)))
             (word-len (length word))
             (delay (* (/ 60.0 wpm)
                      (cond ((< word-len 3) 0.8) ((> word-len 8) 1.3) (t 1.0))
                      (if (string-match-p "[.!?]$" word) 1.5 1.0)))
             (orp-pos (min (/ word-len 3) (1- word-len)))
             (face-mono `(:height ,font-size :family "monospace"))
             (face-orp `(:foreground "red" :weight normal ,@face-mono))
             (padded-word (concat
                          (propertize (make-string (max 0 (- orp-column orp-pos)) ?\s) 'face face-mono)
                          (propertize (substring word 0 orp-pos) 'face face-mono)
                          (propertize (substring word orp-pos (1+ orp-pos)) 'face face-orp)
                          (propertize (substring word (1+ orp-pos)) 'face face-mono))))
        (goto-char (+ word-pos word-len))
        (message "%s" padded-word)
        (pcase (read-event nil nil delay)
          (?f (setq wpm (min 1000 (+ wpm 50))))
          (?s (setq wpm (max 50 (- wpm 50))))
          (?\[ (setq font-size (max 100 (- font-size 20))))
          (?\] (setq font-size (min 400 (+ font-size 20))))
          (?b (setq i (max 0 (- i 10))))
          (?n (setq i (min (1- (length word-positions)) (+ i 10))))
          (?\s (read-event (format "%s [PAUSED - WPM: %d]" padded-word wpm)))
          (?q (setq i (length word-positions)))
          (_ (setq i (1+ i))))))))

;; Optional: bind to a key
;; (global-set-key (kbd "C-c r") 'rsvp-minibuffer)

;; Part 1: free annotations for commands, variables, faces (Emacs 28+)
(when (boundp 'completions-detailed)
  (setq completions-detailed t))

;; Part 2: annotations for buffer, file and bookmark categories

(defun tiny-marginalia--ann (str)
  "Format STR as a right-padded completion annotation."
  (propertize (concat "    " str) 'face 'completions-annotations))

(defun tiny-marginalia-annotate-buffer (candidate)
  "Annotate buffer CANDIDATE with its major mode and file path."
  (when-let ((buf (get-buffer candidate)))
    (tiny-marginalia--ann
     (format "%-25s %s"
             (with-current-buffer buf (symbol-name major-mode))
             (abbreviate-file-name (or (buffer-file-name buf) ""))))))

(defun tiny-marginalia-annotate-file (candidate)
  "Annotate file CANDIDATE with its size and modification date."
  (let* ((path (expand-file-name candidate default-directory))
         (attrs (ignore-errors (file-attributes path))))
    (when attrs
      (tiny-marginalia--ann
       (format "%6s  %s"
               (file-size-human-readable (file-attribute-size attrs))
               (format-time-string "%Y-%m-%d"
                                   (file-attribute-modification-time attrs)))))))

(defun tiny-marginalia-annotate-bookmark (candidate)
  "Annotate bookmark CANDIDATE with its target file."
  (require 'bookmark)
  (when-let* ((rec (assoc candidate bookmark-alist))
              (file (bookmark-prop-get rec 'filename)))
    (tiny-marginalia--ann (abbreviate-file-name file))))

(defun tiny-marginalia--setup ()
  "Inject an annotation function based on the current completion category."
  (unless (plist-get completion-extra-properties :annotation-function)
    (let* ((metadata (completion-metadata
                      (buffer-substring-no-properties
                       (minibuffer-prompt-end) (point-max))
                      minibuffer-completion-table
                      minibuffer-completion-predicate))
           (category (completion-metadata-get metadata 'category))
           (fn (pcase category
                 ('buffer   #'tiny-marginalia-annotate-buffer)
                 ('file     #'tiny-marginalia-annotate-file)
                 ('bookmark #'tiny-marginalia-annotate-bookmark))))
      (when fn
        (setq-local completion-extra-properties
                    (plist-put (copy-sequence completion-extra-properties)
                               :annotation-function fn))))))

(add-hook 'minibuffer-setup-hook #'tiny-marginalia--setup)
