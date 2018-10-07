;;-------------------------------------------------------------------------------- 
;; Init
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("emacs-pe" . "https://emacs-pe.github.io/packages/"))

(package-initialize)
(fringe-mode 4)

;;-------------------------------------------------------------------------------- 
;; General
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'alt))

(desktop-save-mode 1)
(menu-bar-mode 1) ;; This allows native full-screen on mac
(tool-bar-mode 0)
(setq ring-bell-function 'ignore)
(when window-system (scroll-bar-mode -1))
(global-prettify-symbols-mode t)
; (paredit-mode 1)
(setq ediff-split-window-function 'split-window-horizontally)

(add-hook 'org-mode-hook
	  (lambda ()
	    (progn (org-bullets-mode 1)
		   (setq line-spacing 0.25))))

;;-------------------------------------------------------------------------------- 

(use-package projectile
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package org
  :ensure t
  :bind ("C-c c" . 'org-store-link)
  :bind ("C-c a" . 'org-agenda)
  :bind ("C-c c" . 'org-capture)
  :bind ("C-c b" . 'org-switchb)
  :config
  (setq org-startup-indented t
	;;org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
	org-ellipsis " ▼ " ;; folding symbol
	org-pretty-entities t
	org-hide-emphasis-markers t
	;; show actually italicized text instead of /italicized text/
	org-agenda-block-separator ""
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t))

(use-package org-projectile
  :ensure t)

(use-package calfw
  :ensure t
  :config
  (setq cfw:fchar-junction ?╋
	cfw:fchar-vertical-line ?┃
	cfw:fchar-horizontal-line ?━
	cfw:fchar-left-junction ?┣
	cfw:fchar-right-junction ?┫
	cfw:fchar-top-junction ?┯
	cfw:fchar-top-left-corner ?┏
	cfw:fchar-top-right-corner ?┓)
  (setq cfw:org-overwrite-default-keybinding t
	calendar-week-start-day 1))

(use-package calfw-org
  :ensure t)

(defun open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "#ffffff")  ; orgmode source
    ;; (cfw:howm-create-source "Blue")  ; howm source
    ;; (cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
   )))

(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("○")))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-want-abbrev-expand-on-insert-exit nil) ; abbrevs are very annoying when in coq
  (setq abbrev-expand-function #'ignore)
  (evil-leader-mode t)
  (setq evil-leader/leader "<SPC>")
  (global-evil-leader-mode 1)
  (define-key evil-emacs-state-map [escape] nil)
  (setq evil-emacs-state-modes '(magit-mode help-mode magit-popup-mode org-agenda-mode))
  (setq evil-motion-state-modes nil)
  (evil-paredit-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-key
  "ee"	'eval-last-sexp
  "ev"	'open-dotemacs
  "p"	'helm-ls-git-ls
  "f"	'helm-do-grep-ag
  "aa"	'align-regexp
  "TAB" 'helm-buffers-list
  "SPC" 'helm-projectile
  ;; Git
  "gs"  'magit-status
  "gp"  'magit-pull
  "gu"  'magit-push-popup
  "gc"  'magit-commit
  "gb"  'magit-blame
  "gd"  'magit-diff-buffer-file
  "ga"  'magit-stage
  "ghp" 'diff-hl-diff-goto-hunk
  "ghu" 'diff-hl-revert-hunk
  ;; Org
  "op"  'org-latex-export-to-pdf
  ;; Toggles
  "tr"	'linum-relative-mode
  "tn"	'linum-mode
  "l,"	'eyebrowse-rename-window-config
  ))

(use-package evil-paredit
  :ensure t)

(use-package magit
  :ensure t)

(use-package shackle
  :ensure t
  :config
  (shackle-mode 1)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)
			(compilation-mode :noselect t))))

(use-package company
  :ensure t
  :config
  (company-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package helm
  :ensure t
  :bind ("M-x" . helm-M-x)
  :diminish helm-mode
  :commands helm-mode
  :init
  ;; Note: these settings break "C-h m" in helm (which is not too
  ;; useful anyway)
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode nil)
  (setq helm-buffer-max-length 40)
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  )

;; Clear eshell buffer (just like in normal shell)
(defun eshell-clear (&optional arg)
  (interactive "p")
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(defun eshell/vim (file)
  (find-file-other-window file))

(add-hook 'eshell-mode-hook
          '(lambda () (progn
		   (define-key eshell-mode-map (kbd "C-l") 'eshell-clear)
		   (define-key eshell-mode-map (kbd "C-r") 'helm-eshell-history) ;; doesn't work
		   (add-to-list 'eshell-visual-commands "fzf")
		   )))

(use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (evil-define-key 'normal pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
    (evil-define-key 'normal pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
    (evil-define-key 'normal pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
    (evil-define-key 'normal pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
    (evil-define-key 'normal pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
    (evil-define-key 'normal pdf-view-mode-map (kbd "j") 'pdf-view-next-page)
    (evil-define-key 'normal pdf-view-mode-map (kbd "k") 'pdf-view-previous-page)
    (evil-define-key 'normal pdf-view-mode-map (kbd "u") 'pdf-view-revert-buffer)
    (evil-define-key 'normal pdf-view-mode-map (kbd "/") 'pdf-occur)
    (evil-define-key 'normal pdf-view-mode-map (kbd "+") 'pdf-view-enlarge)
    (evil-define-key 'normal pdf-view-mode-map (kbd "-") 'pdf-view-shrink)
    (use-package org-pdfview
      :ensure t))

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode))

(use-package company
  :ensure t
  :config
  (company-mode 1)
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("C-l" . company-complete-selection)))

(use-package linum-relative
  :ensure t
  :config
;; Use `display-line-number-mode` as linum-mode's backend for smooth performance
  (setq linum-relative-backend 'display-line-numbers-mode)
  (setq linum-relative-format "%3s ")
  )

;; (setq coq-prog-name "/Applications/CoqIDE_8.6.1.app/Contents/Resources/bin/coqtop")

;;-------------------------------------------------------------------------------- 
;; Key bindings

(defmacro map (key binding)
  `(global-set-key (kbd ,key) ,binding))

(defmacro emap (mode key binding)
  `(define-key ,(intern (concat "evil-" mode "-state-map")) (kbd ,key) ,binding))

(defmacro nmap (key binding)
  `(emap "normal" ,key ,binding))

(defmacro imap (key binding)
  `(emap "insert" ,key ,binding))

(defmacro vmap (key binding)
  `(emap "visual" ,key ,binding))

;(map  "M-n" 'org-capture)
;(nmap "C-a" 'evil-numbers/inc-at-pt)
;(nmap "C-X" 'evil-numbers/dec-at-pt)
;(nmap "C-u" 'evil-scroll-page-up)
(nmap "C-k" 'previous-match)
(nmap "C-j" 'next-match)
(nmap "C-x C-f" 'helm-find-files)
(nmap "A-f" 'helm-recentf)
(imap "C-g" 'evil-normal-state)

(defun open-dotemacs (&optional arg)
  (interactive "p") (find-file "~/.emacs"))

;;-------------------------------------------------------------------------------- 
;; Haskell stuff

(use-package haskell-mode
  :ensure t)

;; Haskell keybindings
(add-hook 'haskell-mode-hook
	  (lambda () (local-set-key (kbd "<f8>") 'haskell-navigate-imports)))

;; (setq ghc "/Users/cs/.stack/programs/x86_64-osx/ghc-8.2.1/bin/ghc")

;;-------------------------------------------------------------------------------- 
;; Coq stuff

;;(require 'proof-site "~/.emacs.d/lisp/PG/generic/proof-site")
;;(add-hook 'coq-mode-hook
;;          (lambda ()
;;            (company-coq-mode)
;;            (evil-define-key 'normal coq-mode-map (kbd "<down>") 'proof-assert-next-command-interactive)
;;            (evil-define-key 'normal coq-mode-map (kbd "<up>") 'proof-undo-last-successful-command)
;;            (evil-define-key 'normal coq-mode-map (kbd "<return>") 'company-coq-proof-goto-point)
;;            (abbrev-mode 0)
;;	    (setq proof-three-window-mode-policy 'hybrid)
;;	    (setq proof-follow-mode 'ignore)
;;	    (setq proof-splash-enable nil)
;;	    (defface proof-locked-face
;;	    (proof-face-specs
;;	    ;; This colour is quite subjective and may be best chosen according
;;	    ;; to the type of display you have.
;;	    (:background "#eaf8ff")
;;	    (:background "#222288")
;;	    (:underline t))
;;	    "*Face for locked region of proof script (processed commands)."
;;	    :group 'proof-faces)))

;; (setq TeX-auto-save t)
;; (setq TeX-pare-self t)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; (setq TeX-PDF-mode t)

;; (add-hook 'LaTeX-mode-hook (lambda ()
;; 			     (push
;; 			      '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;; 				:help "Run latexmk on file")
;; 			      TeX-command-list)))
;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; (setq flycheck-dafny-executable "/Users/cs/Dev/BASE-DIRECTORY/dafny/Binaries/dafny")

;; ;(load-file (let ((coding-system-for-read 'utf-8))
;; ;                (shell-command-to-string "/Users/cs/.local/bin/agda-mode locate")))



;; (shell-command-to-string "/bin/echo hello")

;; ;;;-------------------------------------------------------------------------------- 

;; (setq reftex-default-bibliography '("~/Dev/haskell/meng/report/bibliography.bib"))

;; ;; see org-ref for use of these variables
;; (setq org-ref-bibliography-notes "~/org/papers.org"
;;       org-ref-default-bibliography '("~/Dev/haskell/meng/report/bibliography.bib")
;;       org-ref-pdf-directory "~/org/bibliography/bibtex-pdfs/")

;; (require 'org-ref)

;; (setq helm-bibtex-bibliography "~/Dev/haskell/meng/report/bibliography.bib" ;; where your references are stored
;;       helm-bibtex-library-path "~/lib/" ;; where your pdfs etc are stored
;;       helm-bibtex-notes-path "~/org/papers.org" ;; where your notes are stored
;;       bibtex-completion-bibliography "~/Dev/haskell/meng/report/bibliography.bib" ;; writing completion
;;       bibtex-completion-notes-path "~/org/papers.org")

;; ;;; -------------------------------------------------------------------------------
;; (defun eshell-mode-hook-func ()
;;   (setq eshell-path-env (concat "/~/cs/bin:" eshell-path-env))
;;   (setenv "PATH" (concat "/~/cs/bin:" (getenv "PATH")))
;;   (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))

;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

;;;-------------------------------------------------------------------------------- 

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("c259628fbeed876031537c380f3f2ebe084fe5107f5db63edd4fc1bbdab9cba7" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" default)))
;;  '(exec-path
;;    (quote
;;     ("/Users/cs/.cargo/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Library/TeX/texbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/Users/cs/.local/bin" "/Users/cs/.cabal/bin")))
;;  '(haskell-hasktags-path "/Users/cs/.local/bin/hasktags")
;;  '(haskell-mode-hook
;;    (quote
;;     (highlight-uses-mode turn-on-haskell-unicode-input-method)))
;;  '(haskell-mode-stylish-haskell-path "/Users/cs/.local/bin/stylish-haskell")
;;  '(haskell-process-args-ghci (quote ("-ferror-spans -fobject-code")))
;;  '(haskell-process-args-stack-ghci
;;    (quote
;;     ("--ghci-options=\"-ferror-spans -fobject-code\"" "--no-build" "--no-load" "")))
;;  '(haskell-process-path-cabal "/Users/cs/.cabal/bin/cabal")
;;  '(haskell-process-path-ghci "/Users/cs/.stack/programs/x86_64-osx/ghc-8.2.1/bin/ghci")
;;  '(haskell-process-path-stack "/Users/cs/.local/bin/stack")
;;  '(package-selected-packages
;;    (quote
;;     (helm-
;;      bibtex interleave org-pdfview popwin helm-ghc vdiff-magit pdf-tools evil-magit evil-indent-plus evil-expat evil-cleverparens evil-org evil-surround boogie-friends auctex scribble-mode eyebrowse company-ghc stack-mode slack racket-mode company-coq 0blayout psci use-package psc-ide purescript-mode markdown-mode markdown-mode+ projectile vimrc-mode nlinum-relative linum-relative which-key magit company evil-leader haskell-mode swiper paredit-everywhere evil-paredit evil-numbers sexy-monochrome-theme monochrome-theme evil)))
;;  '(proof-prog-name
;;    "/Applications/CoqIDE_8.6.1.app/Contents/Resources/bin/coqtop -emacs"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#000000" "light gray" "dark gray" "light slate gray"])
 '(diff-hl-draw-borders nil)
 '(diff-hl-side (quote left))
 '(exec-path
   (quote
    ("/Users/cs/.cargo/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Library/TeX/texbin" "/Library/Frameworks/Mono.framework/Versions/Current/Commands" "/Applications/Wireshark.app/Contents/MacOS" "/Users/cs/Dev/emacs-mac/lib-src" "/Users/cs/.cabal/bin")))
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(org-agenda-files
   (quote
    ("~/.notes" "~/Temporary/misc.org" "~/Temporary/bills.org" "~/PhD/notes.org")))
 '(org-babel-load-languages (quote ((shell . t) (haskell . t))))
 '(org-capture-templates nil)
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-entities-user (quote (("mathbb" "mathbb" nil "" "*" "" ""))))
 '(org-icalendar-include-todo t)
 '(package-selected-packages
   (quote
    (org-projectile helm-ag fill-column-indicator calfw calfw-org sexy-monochrome-theme evil-paredit-mode helm-itunes epresent monochrome-theme window-purpose shackle helm-projectile fzf exec-path-from-shell writeroom-mode reveal-in-osx-finder ag ivy org-ref minimal-theme org-bullets which-key vimrc-mode vdiff-magit use-package stack-mode slack scribble-mode projectile paredit-everywhere org-pdfview markdown-mode linum-relative interleave helm-ls-git helm-ghc eyebrowse evil-tabs evil-surround evil-paredit evil-org evil-numbers evil-magit evil-leader evil-indent-plus evil-expat evil-cleverparens diff-hl company-ghc company-coq boogie-friends auctex 0blayout)))
 '(pdf-view-midnight-colors (quote ("#eeeeee" . "#000000")))
 '(projectile-mode t nil (projectile))
 '(proof-toolbar-enable t))

;; Org mode latex stuff
(add-to-list 'org-entities-user
             '("leadsto" "\\leadsto{}" t "~>" "~>" "~>" "↝"))
(add-to-list 'org-entities-user
             '("uapp" "\\mathop{\\texttt{@@}}" f "@@" "@@" "@@" "@"))
(add-to-list 'org-entities-user
             '("Qcal" "\\mathcal{Q}" t "Q" "Q" "Q" "𝑄"))
(add-to-list 'org-entities-user
             '("Dbb" "\\mathbb{D}" f "D" "D" "D" "𝔻"))
(add-to-list 'org-entities-user
             '("Tbb" "\\mathbb{T}" f "T" "T" "T" "𝕋"))
(add-to-list 'org-entities-user
             '("Fbb" "\\mathbb{F}" f "F" "F" "F" "𝔽"))
(add-to-list 'org-entities-user
             '("vdash" "\\vdash{}" f "|-" "|-" "|-" "⊢"))
(add-to-list 'org-entities-user
             '("Vdash" "\\Vdash{}" f "||-" "||-" "||-" "⊩"))
(add-to-list 'org-entities-user
             '("mapsto" "\\mapsto{}" f "|->" "|->" "|->" "↦"))
(setq org-latex-caption-above nil)

(defun what-face (position)
  "https://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs/1242366#1242366"
  (interactive "d")
  (let ((face (or (get-char-property position 'read-face-name)
                  (get-char-property position 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" position))))
(put 'narrow-to-region 'disabled nil)

;; Theme
(deftheme monochrome)
(let ((class '((class color) (min-colors 89)))
       (fg1 "#d4d4d4")
       (fg2 "#c3c3c3")
       (fg3 "#b2b2b2")
       (fg4 "#a1a1a1")
       (bg1 "#121212")
       (bg2 "#141414")
       (bg3 "#292929")
       (bg4 "#3d3d3d")
       (builtin "#d4d4d4")
       (keyword "#d4d4d4")
       (const   "#d4d4d4")
       (comment "#808080")
       (func    "#d4d4d4")
       (str     "#00af87")
       (type    "#d4d4d4")
       (var     "#d4d4d4")
       (warning "#fa8072")
       (warning2 "#ff8800"))
   (custom-theme-set-faces
   'monochrome
        `(default ((,class (:background ,bg1 :foreground ,fg1))))
        `(font-lock-builtin-face ((,class (:foreground ,builtin))))
        `(font-lock-comment-face ((,class (:foreground ,comment))))
	`(font-lock-negation-char-face ((,class (:foreground ,const))))
	`(font-lock-reference-face ((,class (:foreground ,const))))
	`(font-lock-constant-face ((,class (:foreground ,const))))
        `(font-lock-doc-face ((,class (:foreground ,comment))))
        `(font-lock-function-name-face ((,class (:foreground ,func ))))
        `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
        `(font-lock-string-face ((,class (:foreground ,str))))
        `(font-lock-type-face ((,class (:foreground ,type ))))
        `(font-lock-variable-name-face ((,class (:foreground ,var))))
        `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
        `(region ((,class (:background ,fg1 :foreground ,bg1))))
        `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
	`(hl-line ((,class (:background  ,bg2))))
	`(fringe ((,class (:background ,bg1 :foreground ,fg4))))
	`(cursor ((,class (:background ,bg1))))
        `(show-paren-match-face ((,class (:background ,warning))))
        `(isearch ((,class (:bold t :foreground ,warning :background ,bg3))))
        `(mode-line ((,class (:bold t :foreground ,fg4 :background ,bg4))))
        `(mode-line-inactive ((,class (:foreground ,var :background ,bg2 :weight normal))))
        `(mode-line-buffer-id ((,class (:bold t :foreground ,func :background nil))))
	`(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
        `(mode-line-emphasis ((,class (:foreground ,fg1))))
	`(vertical-border ((,class (:foreground ,bg4))))
        `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
        `(default-italic ((,class (:italic t))))
	`(link ((,class (:foreground ,const :underline t))))
	`(org-code ((,class (:foreground ,fg2))))
	`(org-hide ((,class (:foreground ,fg4))))
        `(org-level-1 ((,class (:bold t :foreground ,fg1))))
        `(org-level-2 ((,class (:bold t :foreground ,fg1))))
        `(org-level-3 ((,class (:bold t :foreground ,fg1))))
        `(org-level-4 ((,class (:bold t :foreground ,fg1))))
        `(org-headline-done ((,class (:bold nil :foreground ,fg2))))
        `(org-date ((,class (:underline t :foreground ,var) )))
        `(org-footnote  ((,class (:underline t :foreground ,fg4))))
        `(org-link ((,class (:underline t :foreground ,type ))))
        `(org-special-keyword ((,class (:foreground ,func))))
        `(org-block ((,class (:foreground ,fg3))))
        `(org-quote ((,class (:inherit org-block :slant italic))))
        `(org-verse ((,class (:inherit org-block :slant italic))))
        `(org-todo ((,class (:foreground ,warning :bold t))))
        `(org-table ((,class (:foreground ,fg1 :bold nil))))
        `(org-done ((,class (:bold t :foreground ,fg4))))
        `(org-warning ((,class (:underline nil :foreground ,warning))))
        `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
        `(org-agenda-date ((,class (:foreground ,var))))
        `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
        `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
        `(org-agenda-done ((,class (:foreground ,fg4))))
	`(org-scheduled ((,class (:foreground ,type))))
        `(org-scheduled-today ((,class (:foreground ,func :weight bold))))
	`(org-ellipsis ((,class (:foreground ,builtin))))
	`(org-verbatim ((,class (:foreground ,fg4))))
        `(org-document-info-keyword ((,class (:foreground ,func))))
	`(font-latex-bold-face ((,class (:foreground ,type))))
	`(font-latex-italic-face ((,class (:foreground ,var :italic t))))
	`(font-latex-string-face ((,class (:foreground ,str))))
	`(font-latex-match-reference-keywords ((,class (:foreground ,const))))
	`(font-latex-match-variable-keywords ((,class (:foreground ,var))))
	`(ido-only-match ((,class (:foreground ,warning))))
	`(org-sexp-date ((,class (:foreground ,fg4))))
	`(ido-first-match ((,class (:foreground ,keyword :bold t))))
	`(gnus-header-content ((,class (:foreground ,keyword))))
	`(gnus-header-from ((,class (:foreground ,var))))
	`(gnus-header-name ((,class (:foreground ,type))))
	`(gnus-header-subject ((,class (:foreground ,func :bold t))))
	`(mu4e-view-url-number-face ((,class (:foreground ,type))))
	`(mu4e-cited-1-face ((,class (:foreground ,fg2))))
	`(mu4e-cited-7-face ((,class (:foreground ,fg3))))
	`(mu4e-header-marks-face ((,class (:foreground ,type))))
	`(ffap ((,class (:foreground ,fg4))))
	`(js2-private-function-call ((,class (:foreground ,const))))
	`(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
	`(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
	`(js2-external-variable ((,class (:foreground ,type  ))))
        `(js2-function-param ((,class (:foreground ,const))))
        `(js2-jsdoc-value ((,class (:foreground ,str))))
        `(js2-private-member ((,class (:foreground ,fg3))))
        `(js3-warning-face ((,class (:underline ,keyword))))
        `(js3-error-face ((,class (:underline ,warning))))
        `(js3-external-variable-face ((,class (:foreground ,var))))
        `(js3-function-param-face ((,class (:foreground ,fg2))))
        `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
        `(js3-instance-member-face ((,class (:foreground ,const))))
	`(warning ((,class (:foreground ,warning)))) 
	`(ac-completion-face ((,class (:underline t :foreground ,keyword))))
	`(info-quoted-name ((,class (:foreground ,builtin))))
	`(info-string ((,class (:foreground ,str))))
	`(icompletep-determined ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
        `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
        `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
	`(slime-repl-inputed-output-face ((,class (:foreground ,type))))
        `(trailing-whitespace ((,class :foreground nil :background ,warning)))
        `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
        `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
        `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
        `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
        `(magit-item-highlight ((,class :background ,bg3)))
        `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
        `(magit-hunk-heading           ((,class (:background ,bg3))))
        `(magit-section-highlight      ((,class (:background ,bg2))))
        `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
        `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
        `(magit-diffstat-added   ((,class (:foreground ,type))))
        `(magit-diffstat-removed ((,class (:foreground ,var))))
        `(magit-process-ok ((,class (:foreground ,func :weight bold))))
        `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
        `(magit-branch ((,class (:foreground ,const :weight bold))))
        `(magit-log-author ((,class (:foreground ,fg3))))
        `(magit-hash ((,class (:foreground ,fg2))))
        `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
        `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
        `(term ((,class (:foreground ,fg1 :background ,bg1))))
        `(term-color-monochrome ((,class (:foreground ,bg3 :background ,bg3))))
        `(term-color-blue ((,class (:foreground ,func :background ,func))))
        `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
        `(term-color-green ((,class (:foreground ,type :background ,bg3))))
        `(term-color-yellow ((,class (:foreground ,var :background ,var))))
        `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
        `(term-color-cyan ((,class (:foreground ,str :background ,str))))
        `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
        `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
        `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
        `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
        `(helm-selection ((,class (:background ,bg2 :underline nil))))
        `(helm-selection-line ((,class (:background ,bg2))))
        `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
        `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
        `(helm-separator ((,class (:foreground ,type :background ,bg1))))
        `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
        `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
        `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
        `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
        `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
        `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
        `(helm-ff-executable ((,class (:foreground ,var :background ,bg1 :weight normal))))
        `(helm-ff-invalid-symlink ((,class (:foreground ,warning2 :background ,bg1 :weight bold))))
        `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
        `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
        `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
        `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
        `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
        `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
        `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
        `(helm-bookmark-w3m ((,class (:foreground ,type))))
        `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
        `(company-preview ((,class (:background ,bg1 :foreground ,var))))
        `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
        `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
        `(company-scrollbar-bg ((,class (:background ,bg3))))
        `(company-scrollbar-fg ((,class (:foreground ,keyword))))
        `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
        `(company-tooltop-annotation ((,class (:foreground ,const))))
        `(company-tooltip-common ((,class ( :foreground ,fg3))))
        `(company-tooltip-common-selection ((,class (:foreground ,str))))
        `(company-tooltip-mouse ((,class (:inherit highlight))))
        `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
        `(company-template-field ((,class (:inherit region))))
        `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
        `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
        `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
        `(web-mode-keyword-face ((,class (:foreground ,keyword))))
        `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
        `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
        `(web-mode-string-face ((,class (:foreground ,str))))
        `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
        `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
        `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
        `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
        `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
        `(jde-java-font-lock-package-face ((t (:foreground ,var))))
        `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
        `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
        `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
        `(jde-java-font-lock-modifier-face ((t (:foreground ,fg2))))
        `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
        `(jde-java-font-lock-number-face ((t (:foreground ,var))))
	`(cfw:face-day-title ((t nil)))
	`(cfw:face-grid ((t (:foreground ,bg4))))
	`(cfw:face-header ((t (:foreground ,fg1 :weight bold))))
	`(cfw:face-select ((t (:background ,bg4))))
	`(cfw:face-sunday ((t (:foreground ,fg1 :weight bold))))
	`(cfw:face-title ((t (:inherit variable-pitch :foreground "white" :weight bold :height 2.0))))
	`(cfw:face-today-title ((t (:background ,bg4 :weight bold))))
	`(cfw:face-today ((t (:background ,bg4 :weight bold))))
	`(cfw:face-toolbar ((t (:foreground "Steelblue4"))))
	`(cfw:face-holiday ((t (:foreground "Steelblue4"))))
	`(cfw:face-toolbar-button-off ((t (:foreground "white smoke" :weight bold))))
        `(ediff-odd-diff-A ((,class (:background ,bg3))))
        `(ediff-even-diff-A ((,class (:background ,bg3))))
        `(ediff-odd-diff-B ((,class (:background ,bg3))))
        `(ediff-even-diff-B ((,class (:background ,bg3))))
        `(ediff-odd-diff-C ((,class (:background ,bg3))))
        `(ediff-even-diff-C ((,class (:background ,bg3))))
	`(custom-face-tag ((t (:inherit nil))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#121212" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Iosevka Term")))))
