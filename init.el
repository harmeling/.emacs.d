(message "Running ~/.emacs.d/init.el")
(message (getenv "LANG"))
(setenv "LANG")
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))                       ; turn off tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))                 ; turn off scroll bar

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)                     ; necessary to allow customizing in the following

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
     multiple-cursors
     openwith
     peep-dired
     dash
     writeroom-mode
     yasnippet
     yasnippet-snippets
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


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
;;(global-set-key [down-mouse-3] #'flyspell-correct-word)     ; right click to flyspell-word
;;;(setq ispell-dictionary "en")
;;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;;;(set-background-color "black")
;;;(set-foreground-color "white")
;;;(set-frame-parameter (selected-frame) 'alpha '(90 90))         ; transpararent background
;;;(setq backup-by-copying-when-linked t)    ; to avoid problem with hard links, soft links might be good without this option

(message "Still running ~/.emacs.d/init.el")




;; backup options
(setq
   backup-by-copying t      ; don't clobber symlinks
   ;; backup-directory-alist '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 5
   kept-old-versions 5
   version-control t)       ; use versioned backups

;; externally open certain files
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "open" (file))
                              ("\\.djvu\\'" "open" (file))))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t trash-directory "~/.Trash/emacs")

;; yas snippet
(add-hook 'java-mode-hook #'yas-minor-mode)              ; switch on yas snippet
(add-hook 'python-mode-hook #'yas-minor-mode)            ; switch on yas snippet
 
;; latex
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(setq TeX-save-query nil)					 ; don't ask before TeXing
(setq TeX-view-program-list		
      '(("MacOS-PDF-viewer" "open -a Skim %o")		  ; most likely Preview or Skim
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
(elpy-enable)
(setq elpy-rpc-virtualenv-path "global")
(eval-after-load 'python '(define-key python-mode-map (kbd "<s-return>") 'elpy-shell-send-region-or-codecell))
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
;;(eval-after-load 'python '(define-key inferior-python-mode-map (kbd "s-t") (kbd "s-`")))
;; (defun python-shell-select-cell ()
;;   (interactive)
;;   (let ((cell-start (save-excursion (end-of-line) (re-search-backward "^##" nil t)))
;;  (cell-end   (save-excursion (end-of-line) (re-search-forward  "^##" nil t))))
;;     (if (not cell-start) (setq cell-start (point-min)))
;;     (if (not cell-end) (setq cell-end (point-max))
;;       (setq cell-end (- cell-end 3)))
;;     (when (and cell-start cell-end)
;;       (push-mark cell-start)
;;       (goto-char cell-end)
;;       (activate-mark)
;;       )))
;; (defun python-shell-send-region-or-defun ()
;;   "send region if region active, otherwise send defun"
;;   (interactive)
;;   (setq pmi (point-min))      ; there's a weird bug w/o widen otherwise
;;   (setq pma (point-max))
;;   (if (> pmi 1) (widen))
;;   (if (use-region-p) (python-shell-send-region (region-beginning) (region-end))
;;     (python-shell-send-defun))
;;   (if (> pmi 1) (narrow-to-region pmi pma))
;;   (if (> pmi 1) (deactivate-mark))
;;   )
;; (eval-after-load 'python '(define-key python-mode-map (kbd "s-t")	   ; switch
;; 		     (lambda () (interactive)
;; 		       (or (python-shell-get-buffer)
;; 			   (run-python))
;; 		       (switch-to-buffer (python-shell-get-buffer)))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "s-r")	   ; save buffer, run it, switch
;; 		     (lambda () (interactive)
;; 		       (save-buffer)
;; 		       (python-shell-send-buffer)
;; 		       (python-shell-switch-to-shell))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<S-return>")  ; send region or defun, no switch
;; 		     (lambda () (interactive)
;; 		       (python-shell-send-region-or-defun))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<M-return>")  ; select and send cell, switch
;; 		     (lambda () (interactive)
;; 		       (python-shell-select-cell)
;; 		       (python-shell-send-region-or-defun)
;; 		       (deactivate-mark)
;; 		       (python-shell-switch-to-shell))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<s-return>")  ; select and send cell, no switch
;; 		     (lambda () (interactive)
;; 		       (save-excursion
;; 			 (python-shell-select-cell)
;; 			 (python-shell-send-region-or-defun)
;; 			 (deactivate-mark)
;; 		       ))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<S-s-return>")  ; select and send cell, and advance to next cell, no switch
;; 		     (lambda () (interactive)
;; 		       (save-excursion
;; 			 (python-shell-select-cell)
;; 			 (python-shell-send-region-or-defun)
;; 			 (deactivate-mark)
;; 		         )
;;                        (re-search-forward "^##" nil t 1)
;;                        )))


;; (eval-after-load 'python '(define-key python-mode-map (kbd "<next>")      ; next cell as slide
;;                             (lambda () (interactive)
;;                               (widen)
;;                               (re-search-forward "^##" nil t 1)
;;                               (python-shell-select-cell)
;;                               (narrow-to-region (region-beginning) (region-end))
;;                               (deactivate-mark))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<prior>")      ; previous cell as slide
;;                             (lambda () (interactive)
;;                               (widen)
;;                               (if (not (re-search-backward "^##" nil t 2)) (beginning-of-buffer))
;;                               (python-shell-select-cell)
;;                               (narrow-to-region (region-beginning) (region-end))
;;                               (deactivate-mark))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<end>")      ; start slideshow
;;                             (lambda () (interactive)
;;                               (python-shell-select-cell)
;;                               (narrow-to-region (region-beginning) (region-end))
;;                               (deactivate-mark))))
;; (eval-after-load 'python '(define-key python-mode-map (kbd "<home>")      ; stop slideshow
;;                             (lambda () (interactive)
;;                               (widen)
;;                               (deactivate-mark))))


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






;;;   ;; function to create right-aligned comments
;;;   (setq comment-start ";")                   ; commonly used variable for the comment symbol
;;;   (defvar right-align-column 90
;;;     "The column number to which to right-align comments.")
;;;   (defun right-align-comment ()
;;;     "Insert or remove space to right align comments."
;;;     (interactive)
;;;     (move-beginning-of-line 1)            ; check whether the comment starts a the beginning
;;;     (while (char-equal (char-after) 32)
;;;       (right-char))                                     ; move over white space to the right
;;;     (unless (string= (char-to-string (char-after)) comment-start)
;;;       (let ((b (point)))
;;;         (move-end-of-line 1)                                       ; jump to the end of line
;;;         (while (char-equal (char-before) 32)
;;;           (backward-delete-char 1))                       ; delete trailing space characters
;;;         (let ((n (- right-align-column (current-column)))); store the position of the end of line
;;;           (if (search-backward (concat comment-start " ") b t); search for the comment start
;;;               (if (> n 0)                                          ; delete or insert space?
;;;                   (insert-char 32 n)                                          ; insert space
;;;                 (while (and (< n 0) (char-equal (char-before) 32))            ; delete space
;;;                   (backward-delete-char 1)
;;;                   (setq n (+ n 1)))))                         ; increment n until we reach 0
;;;           (move-beginning-of-line 1)))))                 ; jump to the beginning of the line
;;;   (defun right-align-comment-region (top bottom)
;;;     "Right align comments in region."
;;;     (interactive "r")
;;;     (deactivate-mark)
;;;     (save-excursion
;;;       (goto-char top)
;;;       (dotimes (n (count-lines top bottom))
;;;         (right-align-comment)
;;;         (forward-line))))
;;;   (defun right-align-comment-buffer ()
;;;     "Right align comment in buffer."
;;;     (interactive)
;;;     (save-excursion
;;;       (right-align-comment-region (point-min) (point-max))))





;; markdown-mode
;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;(setq markdown-css-paths (expand-file-name "~/work/bluenotes/bluenote.css")); standard style sheet
;(setq markdown-css-paths (expand-file-name "~/work/css/github.css")); standard style sheet
;(setq markdown-css-paths (expand-file-name "~/work/css/markdown.css")); standard style sheet
;(setq markdown-css-paths `(,(expand-file-name "~/work/css/github-light.css")))
;(setq markdown-css-paths `(,(expand-file-name "~/work/css/github.css")))
;(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
;;(eval-after-load 'markdown '(define-key markdown-mode-map (kbd "s-r") (lambda () (interactive) (save-buffer) (python-shell-send-buffer) (switch-to-buffer "*Python*"))))
;; (defun markdown-slideshow-start () (interactive)
;;      (text-scale-set 6)               ;; increase font size
;;      (set-window-margins nil 10 10)   ;; left and right margin
;;      (fringe-mode 0)                  ;; no weird fringe
;;      (setq header-line-format " ")    ;; top margin, next line sets the size 
;;      (set-face-attribute 'header-line nil :foreground "white"  :background "white" :height 3.0)  ;; change top margin
;;      (auto-fill-mode -1)
;;      ;(set-face-attribute 'markdown-pre-face nil :foreground "black")
;;      (blink-cursor-mode -1)           ;; stop blinking the cursor
;;      (setq cursor-type 'box)          ;; `hollow `box `hbar `bar
;;      ;(toggle-frame-fullscreen)        ;; fullscreen!
;;      (narrow-to-defun)
;;      )
;; (defun markdown-slideshow-stop () (interactive)
;;        ;(toggle-frame-fullscreen)      ;; undo fullscreen
;;        (widen)                        ;; show all
;;        (text-scale-set 0)             ;; reset font size
;;        (set-window-margins nil 0 0)   ;; left and right margin
;;        (fringe-mode nil)              ;; again the fringe
;;        (setq header-line-format nil)  ;; top margin, next line resets the size
;;        (set-face-attribute 'header-line nil :foreground "white" :background "white" :height 1.0)  ;; change top margin
;;        (blink-cursor-mode 1)          ;; blink the cursor
;;        (setq cursor-type 'box)        ;; `hollow `box `hbar `bar
;;        )
;; (defun markdown-slideshow-next () (interactive)
;;        (widen)
;;        (markdown-outline-next)
;;        (narrow-to-defun)
;;        )
;; (defun markdown-slideshow-previous () (interactive)
;;        (widen)
;;        (markdown-outline-previous)
;;        (narrow-to-defun)
;;        )
;; (defun enable-my-markdown-settings ()
;;   (local-set-key (kbd "M-p") (kbd "→"))
;;   (local-set-key (kbd "<end>")   'markdown-slideshow-start)
;;   (local-set-key (kbd "<home>")  'markdown-slideshow-stop)
;;   (local-set-key (kbd "<next>")  'markdown-slideshow-next)
;;   (local-set-key (kbd "<prior>") 'markdown-slideshow-previous))
;; (add-hook 'markdown-mode-hook 'enable-my-markdown-settings)


;;;    
;;;    
;;;   ;; cmake-mode
;;;   (require 'cmake-mode)
;;;   (setq auto-mode-alist
;;;         (append '(("CMakeLists\\.txt\\'" . cmake-mode)
;;;                   ("\\.cmake\\'" . cmake-mode))
;;;                 auto-mode-alist))


;; ;; multi-term (improves term.el)
;; (setq multi-term-program "/bin/zsh")                                    ; my default shell
;; ;;;(setq multi-term-program-switches "--login")        ; ensures that ~/.profile is sourced
;; (global-set-key (kbd "s-T") 'multi-term)                           ; create a new terminal
;; ;;(global-set-key (kbd "s-t") 'multi-term-next)             ; create or switch to a terminal
;; (defun term-send-filename ()
;;   "Insert the dragged filename into buffer."
;;   (interactive)
;;   (term-send-raw-string (concat "\"" (pop ns-input-file) "\"")))
;; ;;; see http://dea.googlecode.com/svn-history/r1380/trunk/my-lisps/multi-term-settings.el
;; ;;; and https://github.com/russell/dotfiles/blob/master/emacs.d/el-get-init/init-multi-term.el
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (add-to-list 'term-bind-key-alist '("s-[" . multi-term-prev))
;;             (add-to-list 'term-bind-key-alist '("s-]" . multi-term-next))
;;             (add-to-list 'term-bind-key-alist '("<prior>" . scroll-down-command))
;;             (add-to-list 'term-bind-key-alist '("<next>" . scroll-up-command))
;;             (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
;;             (add-to-list 'term-bind-key-alist '("<ns-drag-file>" . term-send-filename))))
;; (add-hook 'term-exec-hook
;;           (function
;;            (lambda ()
;;              (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))
 
 
;; to get the prompt correctly run the following command once 
;; (from http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal)
;; tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
;;;    
;;;    
;;;   ;; maximize the frame if on a window system
;;;   ;;; NOT WORKING
;;;   ;;;(when window-system
;;;   ;;;  (let (
;;;   ;;;        (px (display-pixel-width))
;;;   ;;;        (py (display-pixel-height))
;;;   ;;;        (fx (frame-char-width))
;;;   ;;;        (fy (frame-char-height))
;;;   ;;;        tx ty
;;;   ;;;        )
;;;   ;;;    (setq tx (- (/ (/ px 2) fx) 3))                   ; offsets must be chosen empirically
;;;   ;;;    (setq ty (- (/ py fy) 3))
;;;   ;;;    (setq initial-frame-alist '((top . 2) (left . 2)))
;;;   ;;;    (add-to-list 'initial-frame-alist (cons 'width tx))
;;;   ;;;    (add-to-list 'initial-frame-alist (cons 'height ty))))
;;;    
;;;    
;;;   ;; julia programming language (use ess)
;;;   (require 'julia-mode)
;;;   ;;(load "~/.emacs.d/elpa/ess-20131130.859/lisp/ess-site.el")


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

;;;   ;; Emacs server to avoid multiple instances of Emacs
;;;   (require 'server)
;;;   (unless (server-running-p) (server-start))
;;;    
;;;   ;; Run at full power please
;;;    
;;;   ;; auto-dictionary
;;;   (require 'auto-dictionary)
;;;   (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))
;;;    
;;;    
;;;   ;; re-indent whole buffer
;;;   (defun indent-buffer ()
;;;     (interactive)
;;;     (save-excursion
;;;       (indent-region (point-min) (point-max) nil)))
;;;   (global-set-key (kbd "M-Q") 'indent-buffer)


;; deft (notational velocity)
(require 'deft)
(setq deft-default-extension "org")
(setq deft-directory "~/work/notes")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval 100.0)
(setq deft-extensions '("txt" "md" "org"))
;(setq deft-text-mode 'markdown-mode)
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
;(setq haskell-process-type 'cabal-repl)

;(add-hook 'literate-haskell-mode-hook
;          (lambda ()
;            (latex-mode)
;            (mmm-ify-by-class 'literate-haskell-latex)
;            (interactive-haskell-mode)))

;;; Latex and literate Haskell
;(require 'mmm-auto)
;(setq mmm-global-mode 'true)
;(mmm-add-classes
; '((literate-haskell-bird
;    :submode text-mode
;    :front "^[^>]"
;    :include-front true
;    :back "^>\\|$"
;    )
;   (literate-haskell-latex
;    :submode haskell-mode
;    :front "^\\\\begin{code}"
;    :front-offset (end-of-line 1)
;    :back "^\\\\end{code}"
;    :include-back nil
;    :back-offset (beginning-of-line -1)
;    )))
;(mmm-add-mode-ext-class nil "\\.lhs'" 'literate-haskell-latex)

;; prefer prolog over perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))


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
;        (set-face-attribute 'default nil :family "SF Mono" :height 180)
        )))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(csv-separators (quote ("," "	")))
 '(cua-mode t nil (cua-base))
 '(debug-on-error t)
 '(doc-view-continuous t)
 '(ledger-reports
   (quote
    (("register" "ledger ")
     ("balance" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(line-spacing 0.2)
 '(org-agenda-files
   (quote
    ("~/work/notes/index.org" "~/work/notes/syllabus-2019-deep-learning.org" "~/work/notes/syllabus-2019-masterseminar.org" "~/work/notes/syllabus-2019-causality.org" "~/work/notes/students.org")))
 '(package-selected-packages
   (quote
    (multiple-cursors csv-mode writeroom-mode elpy peep-dired ghc magit yasnippet-snippets exec-path-from-shell expand-region java-snippets yasnippet matlab-mode openwith markdown-mode deft auctex)))
 '(safe-local-variable-values
   (quote
    ((TeX-command-extra-options . "--enable-write18")
     (TeX-file-line-error . t))))
 '(show-paren-mode t))

;; '(fringe-mode 10 nil (fringe))  no fringes
;; '(linum-format " %6d ")
;; '(pr-gv-command "open")
;; '(sr-speedbar-right-side nil)


(put 'narrow-to-region 'disabled nil)

;; TRASH THIS?
;;(defun kill-last-output ()
;;  (interactive)
;;  (save-window-excursion
;;    (push-mark)
;;    (comint-previous-prompt 1)
;;    (kill-ring-save (region-beginning) (region-end))))


; pinbar
(require 'pinbar)
(global-set-key "\M-0" 'pinbar-add)
(pinbar-mode t)
; Run pinbar-mode
; Execute `M-x pinbar-mode`.
; Press (kbd “M-0”) to put a tab associated with current buffer to the header line.
; Press (kbd “M-- M-0”) to remove all the tabs associated with current buffer.
; Press M - 1~9 to visit the buffer associated with the tab shown on the header line.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; writeroom
(setq
 writeroom-bottom-divider-width 0
 writeroom-width 90)
(global-set-key (kbd "M-w") 'writeroom-mode)
