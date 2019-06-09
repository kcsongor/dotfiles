;;-------------------------------------------------------------------------------- 
;; Init
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("emacs-pe" . "https://emacs-pe.github.io/packages/"))

(package-initialize)
(fringe-mode 4)

(org-babel-load-file
 (expand-file-name "configuration.org"
                   user-emacs-directory))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   (quote
    ("9ff0f72a872e642a27a8e8d6508ec2c63e0c1565bc37227c4d1739d86afc291b" "ec2e60d07ab444839d49408de314ead3ee1fca4ef6df9bf72a7bd7e3a678fd43" "fc3a7ae1fd0f547f6664fa833037797073f4c15cda5f80e3b0371224b19414e3" "121acc0ad4d52397476577af41b2b5b05c9830d0c06f5b4899102d1a831da964" "c709addc567db60a31e26e1cb352d261205a5657147933262f7fc63bd0ce20bb" "bfb1f11d57d4773a1d78e75d84baf500973a70821d437e72fd917d1f8e9c7b8f" "fe6f97dff184afa66421043709e4a12577a8023246a5739d6f3a26c268d0fa87" "855da6051f0be0d847c9584392d551c6d598df419942747a6210d141d4d01796" "75bc3476eb74f8930e2e8dfd2be53651fb974ff17df13b2820ceb008c9ccff04" "a4ad88801d70e3101a5e96997ad694a49b6e72f931bc6123f19906f53e6c32a1" "31f3f2721df12f3411fe66c88891a1e2ecbdce473e6af6d650b8c339696fd6b2" "433152575e09815739a29782380efd6d5e0559e75aa5d0ca8ccc0f3365d474cb" "49666d2c6b51e2fa24ad382341e81eb2697f6da41cedb1f3cab2b46ae0b9103f" "c3322a24faa9f930bd2cfbacc1711bd93948af8ac75e97dc5827a23941731537" "d78c6d60703a9cf625bba2e7cf8d00b21a376c384b0d1c56a099ad570ada4ee2" "3891a9e3cae5e9d5d7e4d4dc76a91f60ec581b8b23af9aa16fd723e18263f6d6" "a0cde16454c3ba3954fbeae214729ab434d77ee67df2290a633ab01020178d67" "e3c7dad1c9b0ee14f145c93b26a30d9ad29e3d6a1b2af0a2e00bd1bac84f6f3f" "61a4e0a3f1bb69ab056775b3696b356a7bdccd2a996ba1781f807327e73b4dcf" "a7a961ab228a43525edfb037d9d9fdebbcb18f6fe97982b2be737505a9898439" "09c92c571b156d33b6fb3be018b841013c4182352d7fe7ad8cd73f3fbf0b4aea" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" default)))
 '(diff-hl-draw-borders nil)
 '(diff-hl-side (quote left))
 '(exec-path
   (quote
    ("/Users/cs/.cargo/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Library/TeX/texbin" "/Library/Frameworks/Mono.framework/Versions/Current/Commands" "/Applications/Wireshark.app/Contents/MacOS" "/Users/cs/Dev/emacs-mac/lib-src" "/Users/cs/.cabal/bin" "/Users/cs/.nix-profile/bin")))
 '(haskell-compile-cabal-build-command "cd %s && cabal new-build --ghc-option=-ferror-spans")
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-log t)
 '(haskell-process-path-cabal "~/.nix-profile/bin/cabal")
 '(haskell-process-path-ghci "/usr/local/bin/ghc")
 '(haskell-process-type (quote cabal-new-repl))
 '(ivy-virtual-abbreviate (quote full))
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(org-babel-load-languages (quote ((shell . t) (haskell . t))))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-entities-user (quote (("mathbb" "mathbb" nil "" "*" "" ""))))
 '(org-icalendar-include-todo t)
 '(package-selected-packages
   (quote
    (mmm-mode mmm tidal diminish ivy-hydra yasnippet-snippets ivy-rich rainbow-mode emamux ensime scala-mode magithub perspective counsel-projectile which-key org-protocol counsel swiper org-pdfview pdf-tools htmlize org-projectile fill-column-indicator calfw calfw-org evil-paredit-mode helm-itunes epresent monochrome-theme window-purpose shackle fzf exec-path-from-shell writeroom-mode reveal-in-osx-finder ag ivy org-ref minimal-theme org-bullets vimrc-mode vdiff-magit use-package stack-mode slack scribble-mode projectile paredit-everywhere markdown-mode linum-relative interleave helm-ls-git helm-ghc eyebrowse evil-tabs evil-surround evil-paredit evil-org evil-numbers evil-magit evil-leader evil-indent-plus evil-expat evil-cleverparens diff-hl company-ghc company-coq boogie-friends auctex 0blayout)))
 '(pdf-view-midnight-colors (quote ("#eeeeee" . "#000000")))
 '(projectile-mode t nil (projectile))
 '(proof-toolbar-enable t)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix))))
 '(send-mail-function (quote sendmail-send-it)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:background "white" :foreground "black")))))
