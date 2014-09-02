;; Author: Bryan Maass <bryan.maass@gmail.com>
;; URL: https://github.com/escherize/dotemacs
;; Keywords: emacs clojure setup el-get

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Sensable Builtins ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen 1)          ; no splash screen, thanks
(line-number-mode 1)                    ; have line numbers and
(column-number-mode 1)                  ; column numbers in the mode line
(tool-bar-mode -1)                      ; no tool bar with icons
(scroll-bar-mode -1)                    ; no scroll bars
(global-hl-line-mode)                   ; highlight current line
(global-linum-mode 1)                   ; add line numbers on the left
(setq x-select-enable-clipboard 1)      ; Use the system clipboard
(global-auto-revert-mode 1)             ; pickup external file changes (i.e. git)
(setq-default sh-basic-offset 2)        ; shell
(setq-default sh-indentation 2)         ; shell indentation
(setq-default tab-width 2)              ; Tab width of 2
(fset 'yes-or-no-p 'y-or-n-p)           ; enable y/n answers
(setq initial-scratch-message "")       ; empty scratch message
(setq ring-bell-function
      (lambda () (message "*beep*")))   ; dont beep outloud, thats rude.
(setq auto-window-vscroll nil)          ; better scrolling
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Navigate windows with shift-<arrows>
(windmove-default-keybindings 'shift)
(setq windmove-wrap-around 1)

(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drawn, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

;; under mac, have Command as Super and Alt as Meta
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing 1)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;  el-get  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)       ; common lisp goodies, loop
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   ;; nail down version of el-get.
   "https://raw.githubusercontent.com/dimitri/el-get/c827925bd48ac42a16065490ca7c1f1a2d317ea6/el-get-install.el"
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
   (:name align-cljlet)          ; align clojure lets..
   (:name auto-complete)         ; complete as you type with overlays
   (:name color-theme)           ; color themes
   (:name dash)                  ; a dependancy
   (:name pkg-info)              ; a dependancy
   (:name protobuf-mode)         ; .proto files
   (:name ido-ubiquitous)        ; ido everywhere
   (:name s)                     ; a string library
   (:name yasnippet)             ; powerful snippet mode
   ;; Requires No Setup ^^^^
   ;; Requires Setup vvvvvvv
   (:name cider
          :type github
          :pkgname "clojure-emacs/cider"
          :checkout "v0.7.0"
          :after (progn
                   (add-hook 'cider-repl-mode-hook 'paredit-mode)
                   (add-hook 'clojure-mode-hook 'cider-mode)
                   (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
                   (setq cider-repl-pop-to-buffer-on-connect nil)
                   (setq nrepl-hide-special-buffers 1)
                   (setq cider-popup-stacktraces nil)
                   (setq nrepl-buffer-name-show-port 1)
                   (setq cider-prompt-save-file-on-load nil)
                   (setq cider-repl-history-size 1000)
                   (setq cider-repl-history-file "~/.emacs.d/cider_repl_hist.txt")))
   (:name clj-refactor
          :description "A collection of simple clojure refactoring functions"
          :type github
          :checkout "0.12.0"
          :depends (dash s clojure-mode yasnippet paredit multiple-cursors)
          :pkgname "magnars/clj-refactor.el")
   (:name clojure-mode             ; clojure
          :after (progn
                   (add-hook
                    'clojure-mode-hook
                    (lambda ()
                      (clj-refactor-mode 1)
                      (define-key clj-refactor-map (kbd "M-C->") 'cljr-thread)
                      (define-key clj-refactor-map (kbd "M-C-<") 'cljr-unwind)))))
   (:name color-theme-solarized
          :description "Emacs highlighting using Ethan Schoonover's Solarized color scheme"
          :type github
          :pkgname "sellout/emacs-color-theme-solarized"
          :depends color-theme
          :checkout "6a2c7ca0181585858e6e8054cb99db837e2ef72f"
          :prepare (progn
                     (add-to-list 'custom-theme-load-path default-directory)
                     (autoload 'color-theme-solarized-light "color-theme-solarized"
                       "color-theme: solarized-light" t)
                     (autoload 'color-theme-solarized-dark "color-theme-solarized"
                       "color-theme: solarized-dark" t)))

   (:name diff-hl
          :after (progn (global-diff-hl-mode)))
   (:name exec-path-from-shell
          :after (progn
                   (when (memq window-system '(mac ns))
                     (exec-path-from-shell-initialize))))
   (:name expand-region
          :after (progn
                   (global-set-key (kbd "C-=") 'er/expand-region)))
   (:name floobits
          :website "https://floobits.com"
          :description "Remote pair programming done right"
          :type github
          :checkout "1.5.9"
          :pkgname "Floobits/floobits-emacs")
   (:name flx
          :type github
          :pkgname "lewang/flx"
          :checkout "v0.5"
          :after
          (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching 1)
            (setq ido-use-faces nil)
            (recentf-mode 1)
            (defun recentf-ido-find-file ()
              "Find a recent file using Ido."
              (interactive)
              (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
                (when file
                  (find-file file))))
            (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)))
   (:name gist
          :type github
          :pkgname "defunkt/gist.el"
          :checkout "v1.1.0"
          :depends (gh tabulated-list)
          :description "Emacs integration for gist.github.com"
          :website "http://github.com/defunkt/gist.el")
   (:name github-browse-file
          :type github
          :pkgname "osener/github-browse-file"
          :checkout "1478670dfa1f6925d75b520f10f35feb1e6f2f2c")
   (:name git-timemachine
          :description "Step through historic versions of git controlled files"
          :type github
          :checkout "1.3"
          :pkgname "pidu/git-timemachine")
   (:name goto-last-change          ; move pointer back to last change
          :after (progn
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name hl-sexp
          :description "Highlight the current sexp"
          :type http
          :url "http://edward.oconnor.cx/elisp/hl-sexp.el"
          :features hl-sexp)
   (:name highlight-symbol
          :description "Quickly highlight a symbol throughout the buffer and cycle through its locations."
          :type github
          :pkgname "nschum/highlight-symbol.el"
          :checkout "1.2"
          :after (progn
                   (global-set-key [(control f3)] 'highlight-symbol-at-point)
                   (global-set-key [f3] 'highlight-symbol-next)
                   (global-set-key [(shift f3)] 'highlight-symbol-prev)
                   (global-set-key [(meta f3)] 'highlight-symbol-query-replace)))
   (:name highlight-parentheses
          :after (progn
                   (setq hl-paren-colors
                         '("black" "black" "black"
                           "black" "black" "black"))
                   (setq hl-paren-background-colors
                         '("#2aa198" "#268bd2" "#d33682"
                           "#cb4b16" "#b58900" "#859900"))
                   (define-globalized-minor-mode global-highlight-parentheses-mode
                     highlight-parentheses-mode
                     (lambda ()
                       (highlight-parentheses-mode 1)))
                   (global-highlight-parentheses-mode 1)))
   (:name ido-hacks
          :after (progn
                   (ido-mode 1)
                   (setq ido-everywhere 1)
                   (ido-hacks-mode 1)
                   (setq ido-save-directory-list-file "~/.ido.last")))
   (:name js2-mode
          :website "https://github.com/mooz/js2-mode#readme"
          :description "An improved JavaScript editing mode"
          :type github
          :pkgname "mooz/js2-mode"
          :prepare (autoload 'js2-mode "js2-mode" nil t))
   (:name magit                        ; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "C-c g") 'magit-status)))
   (:name markdown-mode
          :description "Major mode to edit Markdown files in Emacs"
          :website "http://jblevins.org/projects/markdown-mode/"
          :type github
          :checkout "v2.0"
          :pkgname "defunkt/markdown-mode" ;mirror of jblevins
          :prepare (add-to-list 'auto-mode-alist
                                '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode)))
   (:name multiple-cursors
          :after (progn
                   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
   (:name neotree
          :website "https://github.com/jaypei/emacs-neotree"
          :description "An Emacs tree plugin like NerdTree for Vim."
          :type github
          :checkout "0.2"
          :pkgname "jaypei/emacs-neotree"
          :after (progn (global-set-key [f8] 'neotree-toggle)))
   (:name org-mode
          :website "http://orgmode.org/"
          :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
          :type git
          :url "git://orgmode.org/org-mode.git"
          :info "doc"
          :build/berkeley-unix `,(mapcar
                                  (lambda (target)
                                    (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                  '("oldorg"))
          :build `,(mapcar
                    (lambda (target)
                      (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                    '("oldorg"))
          :load-path ("." "contrib/lisp" "lisp")
          :load ("lisp/org-loaddefs.el")
          :after (progn
                   (defun notes () "Switch to notes dir."
                     (interactive)
                     (ido-find-file-in-dir "~/notes"))))
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
          :description "Project jumping, searching, finding functions"
          :after (progn
                   (projectile-global-mode)))
   (:name rainbow-delimiters            ; pretty and useful
          :after (progn
                   (global-rainbow-delimiters-mode)))
   (:name rainbow-mode
          :description "Colorize color names in buffers"
          :type github
          :checkout "2298c419aec2a6cac85f94e9627fec4c0d373c5f"
          :pkgname "emacsmirror/rainbow-mode"
          :type elpa)
   (:name smex
          :after (progn
                   (smex-auto-update nil)
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name switch-window
          :description "A *visual* way to choose a window to switch to"
          :type github
          :pkgname "dimitri/switch-window"
          :checkout "453fbc74c07479ef46e3449ee8432231a9537a8f"
          :features switch-window
          :after (progn
                   (global-set-key (kbd "C-x o") 'switch-window)))
   (:name wgrep
          :type github
          :pkgname "mhayashi1120/Emacs-wgrep"
          :checkout "8e91b932d9c64c5525a1c0a8c770ec55e213e790"
          :after (progn
                   ;; C-c C-p to enter edit mode in grep view
                   (setq wgrep-auto-save-buffer t)))
   (:name undo-tree
          :description "Treat undo history as a tree"
          :website "http://www.dr-qubit.org/emacs.php"
          :type github
          :pkgname "akhayyat/emacs-undo-tree"
          :checkout "a3e81b682053a81e082139300ef0a913a7a610a2"
          :after (progn
                   (global-undo-tree-mode 1)))
   (:name volatile-highlights           ; see what you undo'd
          :after (progn
                   (volatile-highlights-mode 1)))))
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

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; regex searches with C-M-[s|r]
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

(defun save-macro (name)
  "save a macro. Take a name as argument
   and save the last defined macro under
   this name at the end of your .emacs"
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file "~/.emacs.d/user.el") ;; user-init-file
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

(defun date ()
  (interactive)
  (insert (format-time-string "%a, %b %e, %Y")))

(defun stand ()
  (interactive)
  (find-file
   (concat "~/notes/standup"
           (format-time-string "-%e-%m-%Y")
           ".org")))
(global-set-key [f9] 'stand)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-test-error-face ((t (:background "yellow4"))) t)
 '(clojure-test-failure-face ((t (:background "brown4"))) t)
 '(vhl/default-face ((t (:background "#6b0000")))))
;; get bg color: (face-attribute 'default :background)

;; one must load themes only after they're declared safe!
(load-theme 'solarized-dark)

(when (file-exists-p "~/.emacs.d/user.el")
  (load (expand-file-name "~/.emacs.d/user.el")))
