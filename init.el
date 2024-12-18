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

(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (abort-recursive-edit))))
;;
(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)

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
  (let* ((popup-patterns '("\\*Help\\*" "\\*eshell\\*" "\\*eldoc\\*"))
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
(global-set-key (kbd "C-c l") #'my/popper-toggle-popup)
;;
;; Toggle the currently selected popup.
(global-set-key (kbd "C-c p") #'my/popper-toggle-current)
