;;;; init.el - kiasaki's emacs configuration

;;;; package.el
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Download the ELPA archive description if needed.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(arjen-grey-theme
    auto-complete
    ensime
    evil
    evil-magit
    evil-paredit
    exec-path-from-shell
    flycheck
    flycheck-flow
    go-complete
    go-mode
    go-rename
    gruvbox-theme
    helm
    helm-projectile
    js2-mode
    json-mode
    magit
    projectile
    scss-mode
    smart-tab
    tss
    typescript-mode
    web-mode
    yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
          '(progn ,@body)))

;;;; generic
(setq is-mac (equal system-type 'darwin))
(add-to-list 'load-path (concat user-emacs-directory "config"))
(desktop-save-mode 1)
; Better word wrapping
(visual-line-mode 1)
; Simpler title
(setq frame-title-format "Emacs")
; Enforce new lines
(setq require-final-newline t)
; right alt is no meta
(setq ns-right-alternate-modifier 'none)
(when (fboundp 'mac-option-key-is-meta) (setq mac-option-key-is-meta t))
(when (fboundp 'mac-right-option-modifier) (setq mac-right-option-modifier nil))
; No lockfile
(setq create-lockfiles nil)
; No audible bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)
; Easier to write answers
(defalias 'yes-or-no-p 'y-or-n-p)
; Use a bigger mark ring
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)
; Automagically load changes on disk to file
(global-auto-revert-mode 1)
; Don't litter tree with saves
(defvar my-auto-save-folder "~/.emacs.d/.saves/")
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t)))
; use ido mode globally
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
; column 80
(after 'whitespace
    (setq whitespace-style '(face empty tabs lines-tail trailing))
    (global-whitespace-mode t))

(when window-system
  (set-face-attribute 'mode-line nil
                      :family "Source Code Pro"
                      :weight 'light
                      :height 100))

;;;; gui
; No startup message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)
; No toolbar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
; No menu bar (in terminal mode)
(when (not (display-graphic-p))
  (menu-bar-mode -1))
; No tabs
(setq-default indent-tabs-mode nil)
; No right fringe
(when (fboundp 'set-fringe-style) (set-fringe-style '(8 . 0)))
; No test in initial scratch buffer
(setq initial-scratch-message nil)
; No scroll bars
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; Column numbers in status line
(column-number-mode)
; Line number in left fringe
(global-linum-mode t)
; Highlights matching parenthesis
(show-paren-mode 1)
; Highlight current line
(global-hl-line-mode 1)
; Make tabs 4 wide not 8
(setq tab-width 4)
; Set theme
;(load-theme 'misterioso)
;(load-theme 'quasi-monochrome t)
;(load-theme 'smyx t)
;(load-theme 'github t)
(load-theme 'gruvbox t)


;;;; keybindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-set)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "RET") 'newline-and-indent)


;;;; functions
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when is-mac
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;;; evil
(after "evil-autoloads"
    (evil-mode 1)

    (setq evil-want-C-u-scroll t)
    (setq evil-want-fine-undo t)
    (setq evil-want-change-word-to-end t)
    (setq evil-search-module 'isearch)
    (setq evil-magic 'very-magic)
    (setq evil-vsplit-window-right t)

    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    (after "helm-autoloads"
        (define-key evil-normal-state-map (kbd "SPC a") 'helm-find-files)
        (define-key evil-normal-state-map (kbd "SPC f") 'helm-projectile)
        (define-key evil-normal-state-map (kbd "SPC x") 'helm-M-x)))

;;;; helm
(after "helm-autoloads"
    (after 'helm-config
        (setq helm-buffers-fuzzy-matching t)
        (setq helm-recentf-fuzzy-match t)
        (setq helm-M-x-fuzzy-match t)

        (global-set-key (kbd "M-x") 'helm-M-x)
        (global-set-key (kbd "C-x C-f") 'helm-find-files)
        (global-set-key (kbd "C-x b") 'helm-mini)))

;;;; Projectile
(after "projectile-autoloads"
    (projectile-global-mode))

;;;; smart-tab
(after "smart-tab-autoloads"
    ;(global-smart-tab-mode 1)
    )

(after "tss-autoloads"
  (tss-config-default))

(after "auto-complete-autoloads"
    (ac-config-default)
    ;(setq-default ac-sources '(ac-source-words-in-all-buffer))
    (setq ac-use-menu-map t)
    (define-key ac-mode-map (kbd "C-TAB") 'auto-complete)
    (define-key ac-completing-map "\C-n" 'ac-next)
    (define-key ac-completing-map "\C-p" 'ac-previous)
    )

;;;; flycheck
(require 'flycheck-flow)
(after "flycheck-autoloads"
    (global-flycheck-mode)
    (setq-default flycheck-temp-prefix ".flycheck")
    (setq-default flycheck-disabled-checkers
        (append flycheck-disabled-checkers '(javascript-jshint json-jsonlist)))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

;;;; magit
(after "magit-autoloads"
    (global-set-key (kbd "C-x g") 'magit-status)
    (after "evil-autoloads"
        (define-key evil-normal-state-map (kbd "SPC g") 'magit-status)))

;;;; evil-magit
(after "evil-magit-autoloads"
    (evil-magit-init)
    (setq evil-magit-want-horizontal-movement t)
    (setq evil-magit-state 'motion))

;;;; typescript-mode
(after "typescript-mode-autoloads"
    (add-to-list 'auto-mode-alist '("\\.tsx?$" . typescript-mode)))

;;;; j2-mode
(after "js2-mode-autoloads"
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;;; web-mode
(after "web-mode-autoloads"
    (defun my-web-mode-hook ()
        "Hooks for Web mode. Adjust indents"
        (setq web-mode-markup-indent-offset 4)
        (setq web-mode-css-indent-offset 4)
        (setq web-mode-code-indent-offset 4))

    (add-hook 'web-mode-hook  'my-web-mode-hook)

    (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))

;;;; go-mode
(after "go-mode-autoloads"
  (setenv "GOROOT" (concat (getenv "HOME") "/code/dev/go"))
  (setenv "GOPATH" (concat (getenv "HOME") "/code/go"))

  (defun my-go-mode-hook ()
    ; go get golang.org/x/tools/cmd/goimports
    (setq gofmt-command "goimports")

    (add-hook 'before-save-hook 'gofmt-before-save)

    (setq indent-tabs-mode t)
    (setq tab-width 4)
    ;(setq tab-stop-list (number-sequence 4 200 4))
    ;(setq indent-line-function 'insert-tab)

    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
            "go generate && go build -v && go test -v && go vet")))

  (add-hook 'go-mode-hook 'my-go-mode-hook)

  (after "evil-autoloads"
    (define-key evil-normal-state-map (kbd "SPC k") 'go-import-add)
    (define-key evil-normal-state-map (kbd "SPC i") 'godef-jump)))


;;;; go-complete
(after "go-complete-autoloads"
  (add-hook 'completion-at-point-functions 'go-complete-at-point))

;;;; go-eldoc
(after "go-eldoc-autoloads"
  (add-hook 'go-mode-hook 'go-eldoc-setup))


;;;; init.el end of file

(provide 'my-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a164837cd2821475e1099911f356ed0d7bd730f13fa36907895f96a719e5ac3e" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "436d21e38033b3eec44927e99d0442bc3be5f67889df5f7f674439294256ee6f" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
