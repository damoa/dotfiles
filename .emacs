(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

; disable startup screen
(setq inhibit-startup-screen t)

; line numbers
(add-hook 'find-file-hook 'linum-mode)

; no automatic newline
(setq require-final-newline nil)
(setq next-line-add-newline nil)
(setq-default require-final-newline nil)

; use-package
(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'use-package)

; evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode t)

; evil surround
(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

; evil nerd commenter
(use-package evil-nerd-commenter)
(add-to-list 'load-path "~/.emacs.d/evil-nerd-commenter")

; flycheck
(add-to-list 'load-path "~/.emacs.d/flycheck")
(require 'flycheck)
(global-flycheck-mode t)

; evil matchit
(add-to-list 'load-path "~/.emacs.d/evil-matchit")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (helm-projectile exec-path-from-shell json-mode js2-mode rjsx-mode browse-at-remote diff-hl yaml-mode magit web-mode git-gutter-fringe git-gutter counsel-projectile auto-complete-auctex wgrep sass-mode robe mmm-mode helm-ag evil-nerd-commenter evil elscreen counsel auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

; evil-leader
(add-to-list 'load-path "~/.emacs.d/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

; helm-projectile
(use-package helm-projectile)
(require 'helm-projectile)
(evil-leader/set-key
  "f" 'helm-projectile-find-file)
; (setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)

(setq ruby-insert-encoding-magic-comment nil)
(setq gc-cons-threshold 100000000)

; evil nerd commenter shortcut
(global-evil-leader-mode)
(evil-leader/set-key
  "/" 'evilnc-comment-or-uncomment-lines)

; swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(evil-leader/set-key
  "a" 'counsel-projectile-ag)
(global-set-key (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode)
(global-set-key (kbd "C-c C-c") 'wgrep-finish-edit)

; NeoTree
(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

; AutoComplete
(ac-config-default)

; emacs async
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; git-gutter-fringe
(use-package git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode +1)
(setq-default right-fringe-width 10)
(setq git-gutter-fr:side 'right-fringe)

; web-mode
(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))

; revert buffers for branch changing
(defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in EMACS will not be reverted.  They
will be reverted though if they were modified outside EMACS.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

; occur mode hook (thanks Aaron Bieber)
(evil-add-hjkl-bindings occur-mode-map 'emacs
  (kbd "/")       'evil-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "C-d")     'evil-scroll-down
  (kbd "C-u")     'evil-scroll-up
  (kbd "C-w C-w") 'other-window)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(menu-bar-mode -1)
(tool-bar-mode -1)

; Use tab to cycle tabs
(define-key evil-normal-state-map (kbd "<tab>") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "<backtab>") 'evil-prev-buffer)

; remove unused buffers
;; Makes *scratch* empty.
(setq initial-scratch-message "")
;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)
;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

; Tabular
(evil-leader/set-key
  "t" 'align-regexp)

; remember cursor position when reopening file
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

; tab like vim
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name)

(defun BdelOnly ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

; rainbow-delimiters
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; INDENTATION START

; shift/indent text
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default c-basic-offset 2 c-default-style "bsd")

; dirt-indent
(add-to-list 'load-path "~/.emacs.d/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)

(define-key global-map (kbd "RET") 'newline-and-indent)

; indent in new line
(define-key global-map (kbd "RET") 'newline-and-indent)

; evil shift
(add-hook 'ruby-mode-hook
  (function (lambda ()
          (setq evil-shift-width ruby-indent-level))))
(add-hook 'js2-mode-hook
  (function (lambda ()
          (setq evil-shift-width 2)
          (setq js-indent-level 2)
          (set (make-local-variable 'tab-width) 2)
          (set (make-local-variable 'indent-tabs-mode) t))))

;; This gives you a tab of 2 spaces
(add-hook 'coffee-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'indent-tabs-mode) t)))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-js-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
; INDENTATION END

; ES6 START
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers '(javascript-jshint)))
(setq flycheck-checkers '(javascript-eslint))
(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun my/configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (-when-let (checker (cond
                       ((string= web-mode-content-type "jsx")
                        'javascript-eslint)))
    (flycheck-mode)
    (flycheck-select-checker checker)))

(add-hook 'web-mode-hook #'my/configure-web-mode-flycheck-checkers)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
; ES6 END

(setq-default show-trailing-whitespace t)

; zeal-at-point setup
(with-eval-after-load 'zeal-at-point
  (add-to-list 'zeal-at-point-mode-alist '(coffee-mode . ("coffee" "backbone" "underscore"))))
; Add any setup before the previous lines
