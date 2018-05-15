;;-------------------------------------------------------------------------------- 
;; Init
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("emacs-pe" . "https://emacs-pe.github.io/packages/"))

(package-initialize)

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
(paredit-mode 1)

;; Font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Slab" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

(setq org-startup-indented t
      org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
      org-ellipsis " ↴ " ;; folding symbol
      org-pretty-entities t
      ;; org-hide-emphasis-markers
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

;;-------------------------------------------------------------------------------- 
;; Packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
(use-package org-pretty-table)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-want-abbrev-expand-on-insert-exit nil) ; abbrevs are very annoying when in coq
  (evil-leader-mode t)
  (setq evil-leader/leader "<SPC>")
  (global-evil-leader-mode 1)
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  (setq evil-emacs-state-modes nil)
  (setq evil-motion-state-modes nil)
  (evil-paredit-mode 1))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

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
  :init
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*helm" (* not-newline) "*" eos)
		 (display-buffer-in-side-window)
		 (inhibit-same-window . t)
		 (window-height . 0.3)))

  :config
  (helm-mode 1))

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
    (use-package org-pdfview
      :ensure t))

;; (use-package sexy-monochrome-theme
;;   :ensure t
;;   :config
;;   (load-theme 'sexy-monochrome t))

(use-package minimal-theme
  :ensure t
  :config
  (load-theme 'minimal-light t))

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode))

(use-package company
  :ensure t
  :config
  (company-mode 1)
  :bind (:map company-active-map
	      ("C-j" . company-select-next)
	      ("C-k" . company-select-previous)
	      ("C-l" . company-complete-selection)))

;; (setq coq-prog-name "/Applications/CoqIDE_8.6.1.app/Contents/Resources/bin/coqtop")

(define-key org-mode-map (kbd "C-c p") 'org-latex-export-to-pdf)

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

(map  "M-n" 'org-capture)
;(nmap "C-a" 'evil-numbers/inc-at-pt)
;(nmap "C-X" 'evil-numbers/dec-at-pt)
;(nmap "C-u" 'evil-scroll-page-up)
(nmap "C-p" 'helm-find)
(nmap "A-f" 'helm-recentf)
(imap "C-g" 'evil-normal-state)

(evil-leader/set-key
  "ee" 'eval-last-sexp
  "ev" 'open-dotemacs
  "p" 'helm-ls-git-ls
  "f" 'helm-do-grep-ag
  "tr" 'linum-relative-mode
  "tn" 'linum-mode
  "TAB" 'helm-buffers-list)

;;-------------------------------------------------------------------------------- 
;; Haskell stuff

;; Haskell keybindings
(add-hook 'haskell-mode-hook
	  (lambda () (local-set-key (kbd "<f8>") 'haskell-navigate-imports)))

;; (require 'proof-site "~/.emacs.d/lisp/PG/generic/proof-site")
;; (add-hook 'coq-mode-hook
;;           (lambda ()
;;             (company-coq-mode)
;;             (evil-define-key 'normal coq-mode-map (kbd "<down>") 'proof-assert-next-command-interactive)
;;             (evil-define-key 'normal coq-mode-map (kbd "<up>") 'proof-undo-last-successful-command)
;;             (evil-define-key 'normal coq-mode-map (kbd "<return>") 'company-coq-proof-goto-point)
;;             (abbrev-mode 0)))
;; (setq proof-three-window-mode-policy 'hybrid)
;; (setq proof-follow-mode 'ignore)
;; (setq proof-splash-enable nil)

;; (setq ghc "/Users/cs/.stack/programs/x86_64-osx/ghc-8.2.1/bin/ghc")

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
 '(custom-safe-themes
   (quote
    ("cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "a2dd771a05705be2a6e6adb6ddbc7a27ebf49edab1dffdbefe243096becba7c9" "a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "c259628fbeed876031537c380f3f2ebe084fe5107f5db63edd4fc1bbdab9cba7" default)))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-entities-user (quote (("mathbb" "mathbb" nil "" "*" "" ""))))
 '(package-selected-packages
   (quote
    (ag ivy org-ref minimal-theme white-theme white-sand-theme org-bullets which-key vimrc-mode vdiff-magit use-package stack-mode slack sexy-monochrome-theme scribble-mode projectile popwin paredit-everywhere org-pdfview markdown-mode linum-relative interleave helm-ls-git helm-ghc eyebrowse evil-tabs evil-surround evil-paredit evil-org evil-numbers evil-magit evil-leader evil-indent-plus evil-expat evil-cleverparens diff-hl company-ghc company-coq boogie-friends auctex 0blayout))))

;; Org mode latex stuff
(add-to-list 'org-entities-user
             '("mathbb" "\\mathbb{}" t "" "" "" "*"))
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
(setq org-latex-caption-above nil)
