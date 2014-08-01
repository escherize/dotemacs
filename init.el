;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: https://github.com/dimitri/emacs-kicker
;; Created: 2011-04-15
;; Keywords: emacs setup el-get kick-start starter-kit
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

(require 'cl)       ; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '(
   (:name ack-and-a-half)               ; for ack projectile C-c p s a
   (:name align-cljlet)
   (:name auto-complete)          ; complete as you type with overlays
   (:name clojure-mode)           ; clojure
   (:name clj-refactor)           ; clojure pro stuff
   (:name color-theme)            ; color themes
   ;;   (:name color-theme-sanityinc)
   (:name color-theme-solarized)
   ;;   (:name color-theme-tomorrow)
   (:name dash)
   (:name pkg-info)
   (:name ido-better-flex)              ; better matching
   (:name ido-ubiquitous)               ; ido everywhere
   (:name s)
   (:name yasnippet)                    ; powerful snippet mode

   ;; Requires No Setup ^^
   ;; Requires Setup vvvvvvv

   ;; (:name ac-nrepl
   ;;        :after (progn (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
   ;;                      (add-hook 'cider-mode-hook 'ac-nrepl-setup)
   ;;                      (eval-after-load "auto-complete"
   ;;                        '(add-to-list 'ac-modes 'cider-repl-mode))
   ;;                      (defun set-auto-complete-as-completion-at-point-function ()
   ;;                        (setq completion-at-point-functions '(auto-complete)))
   ;;                      (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
   ;;                      (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
   ;;                      (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)))
   (:name cider
          :type github
          :pkgname "clojure-emacs/cider"
          :checkout "v0.6.0"
          :after (progn
                   (add-hook 'cider-repl-mode-hook 'paredit-mode)
                   (add-hook 'clojure-mode-hook 'cider-mode)
                   (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
                   (setq nrepl-hide-special-buffers t)
                   (setq cider-popup-stacktraces nil)
                   (setq nrepl-buffer-name-show-port t)
                   (setq cider-prompt-save-file-on-load nil)
                   (setq cider-known-endpoints '(("eccentrica-reporting" "127.0.0.1" "5111")
						 ("eccentrica-api" "127.0.0.1" "5101")))
                   (setq cider-repl-history-size 1000)
                   (setq cider-repl-history-file "~/.emacs.d/cider_repl_hist.txt")))
   (:name diff-hl
	  :after (progn (global-diff-hl-mode)))
   (:name exec-path-from-shell
          :after (progn
                   (when (memq window-system '(mac ns))
                     (exec-path-from-shell-initialize))))
   (:name expand-region
          :after (progn
                   (global-set-key (kbd "C-=") 'er/expand-region)))
   ;; (:name git-gutter-fringe
   ;;  :after (progn (git-gutter-mode 1)))
   (:name goto-last-change          ; move pointer back to last change
          :after (progn
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name highlight-parentheses
          :after (progn 
                   (setq hl-paren-colors '("Red" "Orange" "Yellow"
					   "Green" "Cyan" "Blue"
					   "Purple" "Purple" "Purple"
					   "Purple" "Purple" "Purple" "Purple"))
                   (setq hl-paren-background-colors '("white" "white" "black"
						      "black" "black" "white"
						      "white" "white" "white"
						      "white" "white" "white" "white"))
                   (define-globalized-minor-mode global-highlight-parentheses-mode
                     highlight-parentheses-mode
                     (lambda ()
                       (highlight-parentheses-mode t)))
                   (global-highlight-parentheses-mode t)))
   (:name ido-hacks
          :after (progn
                   (ido-mode t)
                   (setq ido-everywhere t)
                   (ido-hacks-mode t)
                   (setq ido-save-directory-list-file "~/.emacs.d/.ido.last")))
   (:name magit                        ; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "C-c g") 'magit-status)))
   (:name multiple-cursors
          :after (progn
                   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
   (:name paredit                       ; balance parens
          :after (progn
                   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
                   (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
                   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
		   (add-hook 'text-mode-hook             #'enable-paredit-mode)
		   (add-hook 'prog-mode-hook             #'enable-paredit-mode)
                   (defun bry-indent-buffer () ;; indent + align-cljlet with M-q
                     "Indent the currently visited buffer."
                     (interactive)
                     (indent-region (point-min) (point-max))
                     (untabify (point-min) (point-max))
                     (delete-trailing-whitespace))
                   (global-set-key (kbd "M-q") 'bry-indent-buffer)))
   (:name projectile
          :after (progn 
		   (projectile-global-mode)
		   (setq projectile-enable-caching t)))
   (:name rainbow-delimiters            ; pretty and useful
	  :after (progn
		   (global-rainbow-delimiters-mode)))
   (:name smex
          :after (progn
		   (smex-auto-update nil)
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name volatile-highlights           ; see what you undo'd
          :after (progn
                   (volatile-highlights-mode t)))))

;; some el-get stuff.
(setq my:el-get-packages '(el-get))
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (ignore-errors (el-get-executable-find "cvs"))
  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs
(when (ignore-errors (el-get-executable-find "svn"))
  (loop for p in '(psvn       ; M-x svn-status
                   )
        do (add-to-list 'my:el-get-packages p)))
(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))
;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the common-sense visual settings
(setq inhibit-splash-screen t)    ; no splash screen, thanks
(line-number-mode 1)      ; have line numbers and
(column-number-mode 1)      ; column numbers in the mode line
(tool-bar-mode -1)      ; no tool bar with icons
(scroll-bar-mode -1)      ; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (or (set-face-font 'default "mplus-1m-light-14")
        (set-face-font 'default "Monaco-13"))
  (set-face-font 'default "Monospace-10"))

(global-hl-line-mode)     ; highlight current line
(global-linum-mode 1)     ; add line numbers on the left

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; under mac, have Command as Super and Alt as Meta
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'none)
  (setq mac-option-modifier 'meta))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with shift-<arrows>
(windmove-default-keybindings 'shift)
(setq windmove-wrap-around t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; use ido for minibuffer completion
(require 'ido)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)


;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; scratch message
(setq initial-scratch-message "")

;; Always show line numbers on left
(global-linum-mode t)

;; Mode line shows line numbers
(line-number-mode 1)

;; Tab width of 2
(setq-default tab-width 2)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; regex searches with C-M-[s|r]
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;; dont beep outloud, thats rude.
(setq ring-bell-function (lambda () (message "*beep*")))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))


)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vhl/default-face ((t (:background "#6b0000")))))

;; load theme after they're safe!
(load-theme 'solarized-dark)
