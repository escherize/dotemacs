;; Original Author: Dimitri Fontaine <dim@tapoueh.org>
;; Heavily Edited: Bryan Maass <bryan.maass@gmail.com>
;; URL: https://github.com/escherize/dotemacs
;; Keywords: emacs setup el-get kick-start starter-kit clojure
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/


(require 'cl)       ; common lisp goodies, loop
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/blob/5.1/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '(
   ;; General
   (:name ack-and-a-half)        ; for ack-in-project: s-a
   (:name align-cljlet)          ; align clojure lets..
   (:name auto-complete)         ; complete as you type with overlays
   (:name clojure-mode)          ; clojure
   (:name clj-refactor)          ; clojure pro stuff
   (:name color-theme)           ; color themes
   (:name color-theme-solarized) ; http://ethanschoonover.com/solarized
   (:name dash)                  ; a dependancy
   (:name pkg-info)              ; a dependancy
   (:name ido-ubiquitous)        ; ido everywhere
   (:name s)                     ; a string library
   (:name yasnippet)             ; powerful snippet mode
   ;; Requires No Setup ^^^^
   ;; Requires Setup vvvvvvv
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
   (:name flx
          :type github
          :pkgname "lewang/flx"
          :checkout "v0.5"
          :after
          (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil)))
   (:name goto-last-change          ; move pointer back to last change
          :after (progn
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name highlight-parentheses
          :after (progn
                   (setq hl-paren-colors
                         '("black" "black" "black"
                           "black" "black" "black"
                           "black"))
                   (setq hl-paren-background-colors
                         '("#dc322f" "#cb4b16" "#b58900"
                           "#859900" "#2aa198" "#268bd2"
                           "#d33682"))
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
                   (setq ido-save-directory-list-file "~/.ido.last")))
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
                   (autoload 'enable-paredit-mode "paredit"
                     "Turn on pseudo-structural editing of Lisp code." t)
                   (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
                   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
                   (add-hook 'prog-mode-hook             #'enable-paredit-mode)
                   (defun bry-indent-buffer () ;; indent++
                     (interactive)
                     (indent-region (point-min) (point-max))
                     (untabify (point-min) (point-max))
                     (delete-trailing-whitespace))
                   (global-set-key (kbd "M-q") 'bry-indent-buffer)
                   (eval-after-load 'paredit
                     '(define-key paredit-mode-map (kbd "M-q") nil))))
   (:name projectile
          :after (progn
                   (projectile-global-mode)
                   (global-set-key (kbd "s-f") 'projectile-find-file)
                   (global-set-key (kbd "s-g") 'projectile-grep)
                   (global-set-key (kbd "s-a") 'projectile-ack)
                   (global-set-key (kbd "s-p") 'projectile-switch-project)))
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
(setq my:el-get-packages '(el-get))

;; Some recipes require extra tools to be installed
;; Note: el-get-install requires git, so we know we have at least that.
(when (ignore-errors (el-get-executable-find "cvs"))
  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs
(when (ignore-errors (el-get-executable-find "svn"))
  (loop for p in '(psvn)
        do (add-to-list 'my:el-get-packages p)))
(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))
;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;; end of el-get  ;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)                           ; no splash screen, thanks
(line-number-mode 1)                                     ; have line numbers and
(column-number-mode 1)                                   ; column numbers in the mode line
(tool-bar-mode -1)                                       ; no tool bar with icons
(scroll-bar-mode -1)                                     ; no scroll bars
(global-hl-line-mode)                                    ; highlight current line
(global-linum-mode 1)                                    ; add line numbers on the left
(setq x-select-enable-clipboard t)                       ; Use the system clipboard
(global-auto-revert-mode 1)                              ; pickup external file changes (i.e. git)
(setq-default sh-basic-offset 2)                         ; shell
(setq-default sh-indentation 2)                          ; shell indentation
(setq-default tab-width 2)                               ; Tab width of 2
(fset 'yes-or-no-p 'y-or-n-p)                            ; enable y/n answers
(setq initial-scratch-message "")                        ; empty scratch message
(setq ring-bell-function (lambda () (message "*beep*"))) ; dont beep outloud, thats rude.

;; Navigate windows with shift-<arrows>
(windmove-default-keybindings 'shift)
(setq windmove-wrap-around t)
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

;; under mac, have Command as Super and Alt as Meta
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; regex searches with C-M-[s|r]
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-test-error-face ((t (:background "yellow4"))))
 '(clojure-test-failure-face ((t (:background "brown4"))))
 '(vhl/default-face ((t (:background "#6b0000")))))

(when (file-exists-p "~/.emacs.d/user.el")
  (load (expand-file-name "~/.emacs.d/user.el")))

;; one must load themes only after they're declared safe!
(load-theme 'solarized-dark)
