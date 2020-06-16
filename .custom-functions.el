;; custom functions

;; (setf count -1)
;; (defun f-incf (&optional init step repeat)
;;   (let ((index (floor (/ (cl-incf count 1) (or repeat 1)))))
;;     (+ (or init 1) (* (or step 1) index))
;;     )
;;   )

;; reload init file
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)    ; Reload .emacs file

;; save words
(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; query-replace current word
(defun qrc (replace-str)
  (interactive "sDo query-replace current word with: ")
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (query-replace (current-kill 0) replace-str) ))

;; replace current word or selection using vim style for evil mode
(defun evil-replace-word-selection()
  (interactive)
  (if (use-region-p)
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "empty string")
          (evil-ex (concat "'<,'>s/" selection "/"))
          ))
    (evil-ex (concat "%s/" (thing-at-point 'word) "/"))))

(global-set-key (kbd "\C-co") 'evil-replace-word-selection)

(defun show-in-finder()
  (interactive)
  ;; (evil-ex (concat "!open ." )) ;; how to auto insert enter?
  (shell-command (concat "open -R " buffer-file-name))
  (message "Sucessfully opened in Finder")
  )

(defun iterm-here ()
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))


(defun evil-replace-sr()
  (interactive)
  (if (use-region-p)
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end)))
            ;; (base (progn (string-match " \\(.*\\)\\(sr\\)" selection) (match-string 2 selection)))
            )
        (if (= (length selection) 0)
            (message "empty string")
          (replace-regexp-in-string "sr" "^{2}" selection)
          ))
    ;; (evil-ex (concat "%s/" (thing-at-point 'word) "/" 'base "^{2}"))))
    (evil-ex (concat "%s/" (thing-at-point 'word) "/" ))))
    ;; (replace-regexp-in-string "sr" "^{2}" (thing-at-point 'word 'no-properties))))

;; define a function to use multi functions then I can bind it with a key
;; (defun my-run-org-babel-codeblock-format ()
;;   "Run org babel codeblock formatting in sequence."
;;   (interactive)
;;   (call-interactively 'org-edit-special)
;;   (call-interactively 'mark-whole-buffer)
;;   (call-interactively 'indent-region))

(defun copy-to-x-clipboard ()
  (interactive)
  (let ((thing (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol))))
    (simpleclip-set-contents thing)
    (message "thing => clipboard!")))

(defun paste-from-x-clipboard()
  "Paste string clipboard"
  (interactive)
  (insert (simpleclip-get-contents)))

(defun my/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard))

(add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)


(defun then_R_operator ()
  "%>% operator or 'then' pipe operator"
  (interactive)
  (insert " %>% ")               ; note the space before the first %
  ;; (reindent-then-newline-and-indent)
  )
   
(global-set-key (kbd "C-'") 'then_R_operator)  

;; (defun org-export-docx ()
;;   (interactive)
;;   (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
;;            (template-file "/path/template.docx"))
;;     (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
;;     (message "Convert finish: %s" docx-file)))

(defun my-git-extract-based (target)
  "Extract based version from TARGET."
  (replace-regexp-in-string "^tag: +"
                            ""
                            (car (nreverse (split-string target ", +")))))

(defun kill-this-buffer-volatile ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (save-some-buffers t)
  (kill-this-buffer)
  )

(defun current-path ()
  (interactive)
  (message "Directory: %s" (shell-command-to-string "pwd")) 
  )

;; (require 'ivy) ;;optional
(defun my-find-file-internal (directory)
  ;; (interactive)
  (let* ((keyword (read-string "Please input keyword: ")))
    (when (and keyword (not (string= keyword "")))
        (let* ((default-directory directory)
                ;; (cmd "find . -path \"*/.git\" -prune -o -print -type f -name \"*.*\"") (output (shell-command-to-string cmd)) 
                ;; ignore all dotfiles & name insensibility
                (cmd (format "find . -path \"*/.*\" -prune -o  -type f -iname \"*%s*\" -print" keyword))
                (default-directory directory)
                (output (shell-command-to-string cmd)) 
                ;; (lines (cdr (split-string output "[\n\r]+")))
                (lines (split-string output "[\n\r]+"))
                selectd-file)
            ;; (message "cmd=%s" cmd)
            ;; (message "output=%s" output)
            ;; (message "lines=%s" lines)
            (setq selected-file
                (ivy-read (format "Find file in %s " default-directory ) lines)
                )
            ;; (message "Selected-file: %s" selected-file)
            (when (and selected-file (file-exists-p selected-file)) 
            (find-file selected-file)
            )
            )
      )
    )
  )

(defun my-find-file-in-project ()
  (interactive)
  (my-find-file-internal (locate-dominating-file default-directory ".git"))
  )


;; (defun my-find-file ()
;;     (interactive)
;;     (my-find-file-internal default-directory)
;;   )
(defun my-find-file (&optional level)
  (interactive "P")
  (unless level (setq level 0))
  (let* ((parent-directory default-directory)
         (i 0))
    (while (< i level)
      (setq parent-directory
            (file-name-directory (directory-file-name parent-directory)))
      (setq i (+ i 1)))
    ;; (message "Current Directory:%s" parent-directory)
    (my-find-file-internal parent-directory)))


(defun my-find-file-internal-fd (directory)
  ;; (interactive)
  (let* ((keyword (read-string "Please input keyword: ")))
    (when (and keyword (not (string= keyword "")))
        (let* ((default-directory directory)
               ;; (cmd (format "fd %s . -a" keyword))
               (cmd (format "fd %s . " keyword))
               (output (shell-command-to-string cmd)) 
               (lines (split-string output "[\n\r]+"))
               )
            (setq selected-file
                (ivy-read (format "Find file in %s " default-directory ) lines)
                )
            (when (and selected-file (file-exists-p selected-file)) 
            (find-file selected-file)
            )
            )

      )

    )
  )


(defun my-find-file-fd (&optional level)
  (interactive "P")
  (unless level (setq level 0))
  (let* ((parent-directory default-directory)
         (i 0))
    (while (< i level)
      (setq parent-directory
            (file-name-directory (directory-file-name parent-directory)))
      (setq i (+ i 1)))
    (message "Current Directory:%s" parent-directory)
  (my-find-file-internal-fd parent-directory)))

;; (general-imap "f"
;;   (general-key-dispatch 'self-insert-command
;;     :timeout 0.3
;;     "g" 'my-counsel-company))


;; (defun company-mytags (command &optional arg &rest ignored)
;;   "`company-mode' completion backend for GNU Global."
;;   (interactive (list 'interactive))
;;   (cl-case command
;;     (interactive (company-begin-backend 'company-gtags))
;;     (prefix (and company-gtags-executable
;;                  buffer-file-name
;;                  (apply #'derived-mode-p company-gtags-modes)
;;                  (not (company-in-string-or-comment))
;;                  (company-gtags--tags-available-p)
;;                  (or (company-grab-symbol) 'stop)))
;;     (candidates (company-gtags--fetch-tags arg))
;;     ))

(defun my-latexmk-compiler ()
  (interactive)
  (let* (
         (cmd (format "latexmk %s" buffer-file-name))
         )
    (message "run: %s" cmd)
    ;; (shell-command-to-string cmd)
    (shell-command cmd)
    )
  )


(evil-define-text-object my-evil-a-text-object (count &optional beg end type)
  "Select a word."
  (evil-select-an-object 'evil-word beg end type count))

(evil-define-text-object my-evil-inner-text-object (count &optional beg end type)
  "Select inner word."
  (evil-select-inner-object 'evil-word beg end type count))

(define-key evil-outer-text-objects-map "t" 'my-evil-a-text-object)
(define-key evil-inner-text-objects-map "t" 'my-evil-inner-text-object)


(evil-define-text-object evil-a-WORD (count &optional beg end type)
  "Select a WORD."
  (evil-select-an-object 'evil-WORD beg end type count))

(evil-define-text-object evil-inner-WORD (count &optional beg end type)
  "Select inner WORD."
  (evil-select-inner-object 'evil-WORD beg end type count))

