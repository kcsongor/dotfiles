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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :height 120))))
 '(org-block ((t (:family "Iosevka"))))
 '(org-level-1 ((t (:family "ETBembo" :height 1.6))))
 '(org-level-2 ((t (:family "ETBembo" :height 1.3))))
 '(org-level-3 ((t (:family "ETBembo" :height 1.2))))
 '(org-level-4 ((t (:family "ETBembo" :height 1.1))))
 '(org-meta-line ((t (:family "Iosevka" :height 1))))
 '(org-table ((t (:family "Iosevka" :height 1))))
 '(org-tag ((t (:family "ETBembo" :height 0.6))))
 '(org-todo ((t (:family "Iosevka" :height 1))))
 '(shm-current-face ((t (:background "#f0f0f0"))))
 '(shm-quarantine-face ((t (:background "#a0a0a0")))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (face-remap-add-relative 'default :family "ETBembo" :height 130)))

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
(add-to-list 'load-path "~/Dev/haskell/structured-haskell-mode/elisp/")
(use-package org-pretty-table)
(load "rogue")
(require 'shm)

(use-package evil
  :ensure 
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
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  )

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
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("C-l" . company-complete-selection)))

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

(map  "M-n" 'org-capture)
;(nmap "C-a" 'evil-numbers/inc-at-pt)
;(nmap "C-X" 'evil-numbers/dec-at-pt)
;(nmap "C-u" 'evil-scroll-page-up)
(nmap "C-p" 'helm-find-files)
(nmap "A-f" 'helm-recentf)
(imap "C-g" 'evil-normal-state)

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
  "gd"  'magit-diff
  "ga"  'magit-stage
  "ghp" 'diff-hl-diff-goto-hunk
  "ghu" 'diff-hl-revert-hunk
  ;; Org
  "op"  'org-latex-export-to-pdf
  ;; Toggles
  "tr"	'linum-relative-mode
  "tn"	'linum-mode
  )

(defun open-dotemacs (&optional arg)
  (interactive "p") (find-file "~/.emacs"))

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
    ("39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "48d9bc7ab7f35488dc3e6376ae19eea20223bafeda2b662c4c519c328423a8d2" "8e13d909a2a7aba5d4b77511d3920733f5d45d5463ddc233e7aa77a95f4dfa6d" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "9fcac3986e3550baac55dc6175195a4c7537e8aa082043dcbe3f93f548a3a1e0" "242527ce24b140d304381952aa7a081179a9848d734446d913ca8ef0af3cef21" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "a2dd771a05705be2a6e6adb6ddbc7a27ebf49edab1dffdbefe243096becba7c9" "a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "c259628fbeed876031537c380f3f2ebe084fe5107f5db63edd4fc1bbdab9cba7" default)))
 '(exec-path
   (quote
    ("/Users/cs/.cargo/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Library/TeX/texbin" "/Library/Frameworks/Mono.framework/Versions/Current/Commands" "/Applications/Wireshark.app/Contents/MacOS" "/Users/cs/Dev/emacs-mac/lib-src" "/Users/cs/.cabal/bin")))
 '(fci-rule-color "#858FA5" t)
 '(haskell-process-type (quote cabal-new-repl))
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-entities-user (quote (("mathbb" "mathbb" nil "" "*" "" ""))))
 '(package-selected-packages
   (quote
    (monochrome-theme window-purpose shackle helm-projectile fzf exec-path-from-shell writeroom-mode reveal-in-osx-finder ag ivy org-ref minimal-theme org-bullets which-key vimrc-mode vdiff-magit use-package stack-mode slack scribble-mode projectile paredit-everywhere org-pdfview markdown-mode linum-relative interleave helm-ls-git helm-ghc eyebrowse evil-tabs evil-surround evil-paredit evil-org evil-numbers evil-magit evil-leader evil-indent-plus evil-expat evil-cleverparens diff-hl company-ghc company-coq boogie-friends auctex 0blayout)))
 '(pdf-view-midnight-colors (quote ("#eeeeee" . "#000000")))
 '(projectile-mode t nil (projectile))
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ab4642")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858"))

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

(setenv "PATH" (concat "/Users/cs/.local/bin:/Users/cs/.cabal/bin:" (getenv "PATH")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:family "Iosevka" :background "white" :foreground "grey20"))))
 '(org-block ((((class color) (min-colors 89)) (:background "grey98"))))
 '(org-level-1 ((((class color) (min-colors 89)) (:foreground "grey20" :height 1.6))))
 '(org-level-2 ((((class color) (min-colors 89)) (:foreground "grey20" :height 1.5))))
 '(org-level-3 ((((class color) (min-colors 89)) (:foreground "grey20" :height 1.4))))
 '(org-level-4 ((((class color) (min-colors 89)) (:foreground "grey20" :height 1.3))))
 '(org-meta-line ((t (:family "Iosevka" :height 1))))
 '(org-table ((((class color) (min-colors 89)) (:background "grey98"))))
 '(org-tag ((((class color) (min-colors 89)) (:background "grey98" :foreground "grey20"))))
 '(org-todo ((((class color) (min-colors 89)) (:background "grey90" :foreground "grey20" :weight bold))))
 '(shm-current-face ((t (:background "#f0f0f0"))))
 '(shm-quarantine-face ((t (:background "#a0a0a0")))))
