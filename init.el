(message "Running ~/.emacs.d/init.el")
(message (getenv "LANG"))
(setenv "LANG")
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))                       ; turn off tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))                 ; turn off scroll bar

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (dolist (it packages)
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     auctex
     csv-mode
     deft
     elpy
     exec-path-from-shell
     expand-region 
     ghc
     java-snippets
     magit
     markdown-mode
     matlab-mode
     multi-term
     multiple-cursors
     peep-dired
     writeroom-mode
     yasnippet
     yasnippet-snippets
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq inhibit-splash-screen t)                                          ; no splash screen
(add-to-list 'load-path "~/.emacs.d/lisp")              ; where all additional packages go
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")             ; homebrew packages
(savehist-mode t)                                        ; remember stuff between sessions
(recentf-mode t)                                                 ; remember previous files
(setq recentf-max-saved-items 100) ;                               ; just 20 is too recent
(setq user-mail-address "harmeling@gmail.com")                        ; e.g. for ChangeLog
(set-cursor-color "#00FF00")                                       ; turn the cursor green
(show-paren-mode t)                                            ; show matching parenthesis
(setq show-paren-style 'expression)                                 ; highlight expression
(setq frame-title-format "%f")                                        ; show the file name
(column-number-mode)                                            ; show also column numbers
(setq-default indent-tabs-mode nil)                                       ; never use tabs
(cua-selection-mode t)                                           ; enable rectangular edit
(fset 'yes-or-no-p 'y-or-n-p)                                    ; replace yes/no with y/n
(setq ns-pop-up-frames nil)                      ; don't open new frames for Finder events
(setq visible-bell t)                                                ; don't ring the bell
(setq ring-bell-function 'ignore)                                     ; shut up completely
(setq-default truncate-lines t)                         ; don't break lines for me, please
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)      ; single esc instead of three
(delete-selection-mode 0)

;;;(desktop-save-mode 1)                                      ; open same files as last time
;;;(setq mouse-drag-copy-region nil)                    ; don't copy when marking with mouse ;;;(setq focus-follows-mouse t) ;;;(setq mouse-autoselect-window 0.03)                                 ; focus follows mouse
;(add-hook 'text-mode-hook 'turn-on-auto-fill)                ; switch on auto-fill in text
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)
;(global-set-key (kbd "C-x C-b") 'ace-jump-buffer)          ; switch to the list of buffers
(global-set-key (kbd "s-+") 'text-scale-increase)      ; same as C-x C-= or C-x C-+
(global-set-key (kbd "s-=") 'text-scale-increase)      ; same as C-x C-= or C-x C-+
(global-set-key (kbd "s--") 'text-scale-decrease)      ; same as C-x C--
(global-set-key (kbd "s-0") 'text-scale-adjust)      ; same as C-x C-0
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)         ; jump to beginning of buffer
(global-set-key (kbd "s-<down>") 'end-of-buffer)                   ; jump to end of buffer
(global-set-key (kbd "s-<left>") 'previous-buffer)             ; switch to previous buffer
(global-set-key (kbd "s-<right>") 'next-buffer)                    ; switch to next buffer
;(global-set-key (kbd "s-w") 'kill-this-buffer)                               ; close buffer
(global-set-key (kbd "s-w") (if (buffer-modified-p) (kbd "C-x C-s s-k") 'kill-this-buffer)); save and close buffer
(global-set-key (kbd "s-b") 'buffer-menu)                      ; switch to list of buffers
(global-set-key (kbd "s-1") (kbd "C-x 1"))                                      ; no split
(global-set-key (kbd "s-2") (kbd "C-x 2"))                              ; horizontal split
(global-set-key (kbd "s-3") (kbd "C-x 3"))                                ; vertical split
(global-set-key (kbd "s-q") nil)                                           ; don't do quit
(global-set-key (kbd "s-t") nil)                               ; don't show the font panel
(global-set-key (kbd "s-`") (kbd "C-x b RET"))                    ; switch to other buffer
(global-set-key (kbd "s-o")
                '(lambda () (interactive) (find-file default-directory)))      ; open file
(global-set-key [ns-drag-file] 'ns-find-file)       ; dragging files into emacs opens them
(global-set-key (kbd "s-S") 'write-file)                                  ; save as buffer


(global-eldoc-mode -1)      ; shut down eldoc

;;; from https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-Q") 'unfill-paragraph)

; from https://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
(global-set-key (kbd "s-j") 'unfill-region)   ; use in combination with visual-line-mode


(setq vc-make-backup-files t); make backups of files, even when they're in version control
(set-default 'indicate-empty-lines t)               ; show me empty lines after buffer end


 (defun timestamp ()
   (interactive)
   (insert (format-time-string "[%Y-%m-%d %H:%M:%S]")))
(global-set-key (kbd "M-n") `timestamp)

(global-set-key (kbd "<kp-delete>") 'delete-char)                     ; assigns delete key

(message "Still running ~/.emacs.d/init.el")




;; backup options
(setq
   backup-by-copying t      ; don't clobber symlinks
   ;; backup-directory-alist '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 5
   kept-old-versions 5
   version-control t)       ; use versioned backups

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t trash-directory "~/.Trash/emacs")

;; yas snippet
(add-hook 'java-mode-hook #'yas-minor-mode)              ; switch on yas snippet
(add-hook 'python-mode-hook #'yas-minor-mode)            ; switch on yas snippet
 
;; latex
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin" "~/.local/bin") exec-path))
(setq TeX-save-query nil)					 ; don't ask before TeXing
(setq TeX-view-program-list		
;      '(("MacOS-PDF-viewer" "open -a 'Preview' %o")		  ; most likely Preview or Skim
      '(("MacOS-PDF-viewer" "open -a 'Skim' %o")		  ; most likely Preview or Skim
	("MacOS-DVI-viewer" "open %o")))			    ; most likely Skim.app
(setq TeX-view-program-selection
      '((output-pdf "MacOS-PDF-viewer")
	(output-dvi "MacOS-DVI-viewer")))
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq font-latex-fontify-script nil)				    ; don't use subscripts
(setq font-latex-fontify-sectioning 'color); don't use different fonts for section heading
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)				   ; for math menu
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)  ;; TOO SLOW
(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "s-r") (kbd "C-c C-c C-j")))
;(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "s-r") 'TeX-command-master))
(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "s-e") (kbd "M-b C-SPC M-f C-c C-f C-e")))  ; emphasize
(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "M-<return>") 'latex-insert-item))
;(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))); http://tex.stackexchange.com/questions/124246/uninformative-error-message-when-using-auctex
(setq reftex-ref-macro-prompt nil)	  ; to avoid extra question about \ref vs \pageref
 
;; set path to info directory
(setq Info-default-directory-list
      (append '("/usr/local/share/info") Info-default-directory-list))


;; java
(defun my-java-mode-hook ()
  (set (make-local-variable 'compile-command)
       (concat "javac " (file-name-nondirectory buffer-file-name)
               " && java " (file-name-base) " "))
  (setq compilation-ask-about-save nil)
  (local-set-key (kbd "s-r") 'compile)
  )
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; python
(eval-after-load 'python '(define-key python-mode-map (kbd "<s-return>") 'elpy-shell-send-region-or-codecell))
(eval-after-load 'python '(define-key python-mode-map (kbd "<M-return>") 'elpy-shell-send-buffer))
(eval-after-load 'python '(define-key python-mode-map (kbd "<M-s-return>") 'elpy-shell-send-codecell-and-step))
(eval-after-load 'python '(define-key python-mode-map (kbd "<s-backspace>") 'elpy-shell-kill-yes-or-no))
(setq python-indent-guess-indent-offset t)            ; do guess
(setq python-indent-guess-indent-offset-verbose nil)  ; but don't complain if you can't
(defun elpy-shell-send-region-or-codecell ()
  "send region if region active, otherwise send codecell"
  (interactive)
  (setq pmi (point-min))      ; there's a weird bug w/o widen otherwise
  (setq pma (point-max))
  (if (> pmi 1) (widen))
  (if (use-region-p) (elpy-shell-send-region-or-buffer)
    (elpy-shell-send-codecell))
  (if (> pmi 1) (narrow-to-region pmi pma))
  (if (> pmi 1) (deactivate-mark))
  )
(defun elpy-shell-kill-yes-or-no ()
  "ask whether we should kill the shell and do it possibly"
  (interactive)
  (if (y-or-n-p "Do you really want to kill the associated python shell?")
      (elpy-shell-kill)))
(eval-after-load 'python '(define-key inferior-python-mode-map (kbd "<up>") (kbd "<C-up>")))
(eval-after-load 'python '(define-key inferior-python-mode-map (kbd "<down>") (kbd "<C-down>")))


;; matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
;; matlab (additional settings)
;(global-set-key (kbd "s-m") 'matlab-shell)                     ; short cut to start matlab
(setq mymatlab-path "/Applications/MATLAB_R2014b.app/")
;; (setenv "MATLAB_JAVA" "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home")
;; (setq matlab-shell-command (concat mymatlab-path "bin/matlab"))
(setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
(setq matlab-indent-level 2)
(setq matlab-verify-on-save-flag nil)
;; (setq matlab-case-level '(1 . 1))
(eval-after-load 'matlab
   '(define-key matlab-mode-map (kbd "s-r") 'matlab-shell-save-and-go))
(defun rename-matlab ()
   "Renames the current buffer to *MATLAB*. Useful when running several matlabs."
   (interactive)
   (rename-buffer "*MATLAB*")
   )
(add-hook 'matlab-mode-hook 'turn-off-auto-fill)          ; switch off auto-fill in matlab
(add-hook 'matlab-shell-mode-hook
       (lambda () (remove-hook 'comint-output-filter-functions
                  'matlab-shell-render-errors-as-anchor t))); to keep matlab responsive (see email from Odd Andersen from 2014-03-06)

(defun htmlize-and-browse-url-of-buffer ()
  "Run `htmlize-buffer' and `browse-url-of-buffer' in sequence."
  (interactive)
  (browse-url-of-buffer (htmlize-buffer)))
(defun htmlize-and-browse-url-of-region (start end)
  "Run `htmlize-region' and `browse-url-of-buffer' in sequence."
  (interactive "r")
  (browse-url-of-buffer (htmlize-region start end)))
(global-set-key (kbd "s-p") 'htmlize-and-browse-url-of-buffer)
(global-set-key (kbd "s-P") 'htmlize-and-browse-url-of-region)


;; emacs lisp
(define-key emacs-lisp-mode-map (kbd "C-c ;") 'comment-region)

;; org-mode
(require 'org)
(setq org-use-speed-commands nil)     ; for now no speed commands (see info)
(setq org-catch-invisible-edits 'show-and-error)  ; show and error if typing into the invisible region

;(setq org-log-done 'time)         ; mark closed todo with CLOSE [2019-xx-xx]
(setq org-icalendar-combined-agenda-file "~/sciebo/org/test.ics")
(setq org-todo-keywords '((sequence "TODO" "WAIT" "|" "DONE")))
(setq org-support-shift-select "always")
(setq org-CUA-compatible t)                         ; allow marking text with shift-cursor
(defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
  (if (and cua-mode
           org-support-shift-select
           (not (use-region-p)))
      (cua-set-mark)))

;; multiple-cursors (ala Sublime Text)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "s-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; markdown-mode



;; ;; multi-term (improves term.el)
;; (setq multi-term-program "/bin/zsh")                                    ; my default shell
;; ;;;(setq multi-term-program-switches "--login")        ; ensures that ~/.profile is sourced
(global-set-key (kbd "s-T") 'multi-term)                           ; create a new terminal
(global-set-key (kbd "s-t") 'multi-term-next)             ; create or switch to a terminal
(global-set-key (kbd "s-,") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))         ; open .emacs.d/init.el
(global-set-key (kbd "s-i") (lambda () (interactive) (find-file "~/work/notes/index.org")))     ; open main org list

;;;   ;; default directory
;;;   (setq default-directory "~/")                                 ; sets the default directory


;; Mac-like Umlaute and sharp s
(global-unset-key (kbd "M-u"))
(global-set-key (kbd "M-u a") (kbd "ä"))
(global-set-key (kbd "M-u o") (kbd "ö"))
(global-set-key (kbd "M-u u") (kbd "ü"))
(global-set-key (kbd "M-u A") (kbd "Ä"))
(global-set-key (kbd "M-u O") (kbd "Ö"))
(global-set-key (kbd "M-u U") (kbd "Ü"))
(global-set-key (kbd "M-s") (kbd "ß"))   ; sharp s
(global-set-key (kbd "M-@") (kbd "€"))   ; Euro
(global-set-key (kbd "M-,") (kbd "≤"))   ; less than
(global-set-key (kbd "M-.") (kbd "≥"))   ; greater than
(global-set-key (kbd "M-l") (kbd "¬"))   ; logical negation
;(global-set-key (kbd "M-o") (kbd "∨"))   ; logical or    USE "v"
;(global-set-key (kbd "M-a") (kbd "∧"))   ; logical and   USE "^"
(global-set-key (kbd "M-p") (kbd "→"))   ; logical implications
(global-set-key (kbd "M-<") (kbd "⊂"))   ; subset
(global-set-key (kbd "M->") (kbd "⊃"))   ; subset



;; expand-region (emacs-rocks 09)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;; deft (notational velocity)
(require 'deft)
(setq deft-default-extension "md")
(setq deft-directory "~/Dropbox/notes-2022")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval 100.0)
(setq deft-extensions '("txt" "md" "org"))
(setq deft-text-mode 'markdown-mode)
(global-set-key (kbd "s-n") 'deft)
;(setq deft-markdown-mode-title-level 1)
(defun enable-my-deft-settings ()
  (local-set-key [s-return] 'deft-new-file)
  (local-set-key [s-backspace] 'deft-delete-file)
  (local-set-key (kbd "s-r") 'deft-rename-file)
  (local-set-key (kbd "s-l") 'deft-filter-clear)
  (local-set-key (kbd "s-n") 'deft-filter-clear))    ;; s-n activates deft and inside deft deletes the search string
(add-hook 'deft-mode-hook 'enable-my-deft-settings)
 
;; magit - a git client
(global-set-key (kbd "C-x g") 'magit-status)

;; Haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; prefer prolog over perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))


(require 'dired-x)
(define-key dired-mode-map (kbd "s-o") (kbd "!open RET"))   ; open a file/directory externally
(define-key dired-mode-map (kbd "s-t") (kbd "!open SPC -a SPC Terminal SPC . RET")) ; Terminal at directory
(setq dired-listing-switches "-alno")
(setq dired-guess-shell-alist-user '(("" "open")))

;; Setup environment variables from the user's shell.
;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
(when is-mac
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (setq dired-use-ls-dired nil)
  (if (display-graphic-p)
      (progn
        (load-theme 'deeper-blue t)
        (set-face-attribute 'default nil :family "Inconsolata" :height 180)
        (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 180)
        ;(set-face-attribute 'default nil :family "SF Mono" :height 180)
        )))

;; treemacs



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(csv-separators '("," "	" ";"))
 '(cua-mode t nil (cua-base))
 '(debug-on-error t)
 '(doc-view-continuous t)
 '(doc-view-resolution 200)
 '(elpy-mode-hook '((lambda nil (highlight-indentation-mode -1))))
 '(ledger-reports
   '(("register" "ledger ")
     ("balance" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(line-spacing 0.2)
 '(org-agenda-files
   '("~/work/notes/index.org" "~/work/notes/syllabus-2019-deep-learning.org" "~/work/notes/syllabus-2019-masterseminar.org" "~/work/notes/syllabus-2019-causality.org" "~/work/notes/students.org"))
 '(package-selected-packages
   '(paredit slime multi-term speed-type julia-mode julia-repl processing-mode processing-snippets multiple-cursors csv-mode writeroom-mode peep-dired ghc exec-path-from-shell java-snippets yasnippet openwith auctex))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((TeX-command-extra-options . "--enable-write18")
     (TeX-file-line-error . t)))
 '(show-paren-mode t))

(put 'narrow-to-region 'disabled nil)



;; comint
;; match to the stuff already typed in
(eval-after-load 'comint
  '(progn
     ;; originally on C-c M-r and C-c M-s
     (define-key comint-mode-map (kbd "<up>") #'comint-previous-matching-input-from-input)
     (define-key comint-mode-map (kbd "<down>") #'comint-next-matching-input-from-input)
     ;; originally on M-p and M-n
     (define-key comint-mode-map (kbd "C-c M-r") #'comint-previous-input)
     (define-key comint-mode-map (kbd "C-c M-s") #'comint-next-input)))

; pinbar
(require 'pinbar)
(global-set-key "\M-0" 'pinbar-add)
(pinbar-mode t)
; Run pinbar-mode
; Execute `M-x pinbar-mode`.
; Press (kbd “M-0”) to put a tab associated with current buffer to the header line.
; Press (kbd “M-- M-0”) to remove all the tabs associated with current buffer.
; Press M - 1~9 to visit the buffer associated with the tab shown on the header line.
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(mode-line ((t (:background "gray25" :foreground "black" :box (:line-width (1 . 1) :style released-button)))))
;; '(mode-line-buffer-id ((t (:foreground "gray" :weight bold)))))
;; '(pinbar-unselected-face ((t (:inherit pinbar-default-face :foreground "black")))))

;; writeroom
(setq
 writeroom-bottom-divider-width 0
 writeroom-width 120)
(global-set-key (kbd "M-w") 'writeroom-mode)
(global-writeroom-mode 0)

;; regular expressions
(defun attribution ()
  (interactive)
  (move-beginning-of-line nil)
  (insert "% https://lizenzhinweisgenerator.de\n")
  (insert "% " (thing-at-point 'line))
  (insert "{\\tiny Source: ")
  (re-search-forward "<a href=\"\\([^\"]*\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
  (replace-match "\\\\href{\\1}{\\2}" nil nil)
  (re-search-forward "<a href=\"\\([^\"]*\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
  (replace-match "\\\\url{\\1}" nil nil)
  (re-search-forward "<a href=\"\\([^\"]*\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
  (replace-match "\\\\href{\\1}{\\2}" nil nil)
  (insert "}")
  (next-line)
  )


;; SLIME and common lisp
(setq inferior-lisp-program "sbcl")
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
