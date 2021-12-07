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

(defun my-finder-path ()
  "Return path of the frontmost Finder window, or the empty string.

Asks Finder for the path using AppleScript via `osascript', so
this can take a second or two to execute."
  (let ($applescript)
    (setq $applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")
    (with-temp-buffer
      ;; Produce a list of process exit code and process output (from the temp buffer)
      (call-process "/usr/bin/osascript" nil (current-buffer) nil "-e" $applescript)
      (string-trim (buffer-string)))))

(defun my-dired-finder-path ()
  (interactive)
  (let (($path (my-finder-path)))
    (if (string-equal "" $path)
        (message "No Finder window found.")
      (dired $path))))


;; (defun iterm-here ()
;;   (interactive)
;;   (dired-smart-shell-command "open -a iTerm $PWD" nil nil))

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))


(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
	" tell application \"iTerm2\"\n"
	"   tell the current session of current window\n"
	(format "     write text \"cd %s\" \n"
			;; string escaping madness for applescript
			(replace-regexp-in-string "\\\\" "\\\\\\\\"
									  (shell-quote-argument (or default-directory "~"))))
	"   end tell\n"
	" end tell\n"
	" do shell script \"open -a iTerm\"\n"
	))
  )

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
  ;; (insert " %>% ")               ; note the space before the first %
  (insert " |> ")               ; note the space before the first %
  ;; (reindent-then-newline-and-indent)
  )
;; (global-set-key (kbd "C-'") 'then_R_operator)
(add-hook 'ess-r-mode-hook (lambda ()
                             ;; (define-key ess-r-mode-map (kbd "C-'") 'then_R_operator)))
                             (set-local-key (kbd "C-'") 'then_R_operator)))

(defun equal_latex_operator ()
  (interactive)
  (insert " \\!=\\! ")               ; note the space before the first %
  ;; (reindent-then-newline-and-indent)
  )
(add-hook 'latex-mode-hook (lambda ()
                             (set-local-key (kbd "C-=") 'equal_latex_operator)))

;; (global-set-key (kbd "C-=") 'equal_latex_operator)

(defun then_Julia_operator ()
  "%>% operator or 'then' pipe operator"
  (interactive)
  (insert " |> ")                       ; note the space before the first %
  ;; (reindent-then-newline-and-indent)
  )

(add-hook 'julia-mode-hook (lambda ()
                             (local-set-key  (kbd "C-'") 'then_Julia_operator)))

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
  (interactive)
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
  (set-file-times (buffer-file-name)) ;; sets mod time to current time
  (let* (
         (cmd (format "latexmk -xelatex -outdir=out %s" buffer-file-name))
         ;; (cmd2 (format "latexmk -xelatex -outdir=out %s" buffer-file-name))
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

;; (defun my/paste-in-minibuffer ()
;;   (local-set-key (kbd "M-y") 'paste-from-x-clipboard))

;; (add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)

(defun lazy-M-x ()
  (interactive)
  (counsel-M-x (current-word))
  )


(defun hl-then-flush-line ()
  (interactive)
  ;; (message "input=%s" (current-word))
  (hi-lock-line-face-buffer (current-word) "match")
  ;; y to delete
  ;; n stop can cancel hl
  (if (y-or-n-p "Delete all matched lines?")
      (progn
        ;; (message "yes")
        (flush-lines (current-word) (point-min) (point-max))
        )
    (progn
      (unhighlight-regexp t)
      )
    )
  )

;; (defun my/capitalize-first-char (&optional string)
;;   "Capitalize only the first character of the input STRING."
;;   (interactive)
;;   (when (and string (> (length string) 0))
;;     (let ((first-char (substring string nil 1))
;;           (rest-str   (substring string 1)))
;;       (concat (capitalize first-char) rest-str))))


(defun my-open-message-in-new-window ()
  (interactive)
  ;; (split-window-horizontally)
  ;; (sleep-for 2)
  ;; (split-window-horizontally)
  (evil-window-vsplit)
  ;; move cursor to window 2: right buffer
  ;; switch to buffer "message"
  (switch-to-buffer "*Messages*")
  (end-of-buffer)
  (previous-line)
  ;;to move top
  ;; (evil-scroll-line-to-top line-number-at-pos)
  (evil-scroll-line-to-top (string-to-number (format-mode-line "%l")))
  (ace-swap-window)
  ;; (find-file-other-window "*Messages*")
  ;; (pop-to-buffer "*Messages*")
  ;; return focus to window 1: left buffer
  (winum-select-window-1)
  )

;; (defun org-map-entries ()
;;   (interactive)
;;           (when
;;               (string=
;;                (nth 2 (org-heading-components)) "TODO")
;;              (org-todo "DONE")))

(defun j-change-todo (start end state)
  "Change heading todo states in region defined by START and END to STATE.
Operate on whole buffer if no region is defined."
  (interactive (list
                (if (region-active-p) (region-beginning) (point-min))
                (if (region-active-p) (region-end) (point-max))
                (completing-read "State: " org-todo-keywords-1)))
  (save-excursion
    (goto-char start)
    (when (org-at-heading-p)
      (org-todo state))
    (while (re-search-forward org-heading-regexp end t)
      (org-todo state))))

(defun my-open-custom-el ()
  (interactive)
  (find-file "~/Dotfiles/.custom.el")
  )

(defun cppcm-cmake-in-root-build-dir ()
  "Compile in build directory"
  (interactive)
  (setq cmake-git-path (locate-dominating-file default-directory ".git"))
  (setq cmake-build-path (concat cmake-git-path "build"))
  ;; (setq cmake-src-path (concat cmake-git-path "src"))
  (setq cmake-src-path (concat cmake-git-path "src/script-cxx"))
  ;; accept parameter: problem bumber
  (setq filename (file-name-nondirectory buffer-file-name))
  (setq problemNum (substring filename 0 4))
  ;; if ./build not exist, create it
  (unless (file-directory-p cmake-build-path)
    (message "%s" "@alert@: build/ doesn't exist, create it")
    (make-directory cmake-build-path))
  (setq compile-command (concat "cd " cmake-build-path " && " "cmake " cmake-src-path " -D PROBLEM_NUMBER=" problemNum " && make && make run"))
  (call-interactively 'compile)
  )

(defun org-to-md-then-preview ()
  (interactive)
  (org-hugo-export-to-md)
  (easy-hugo-preview))
;; time check
;; (defmacro my/timer (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))
(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))
