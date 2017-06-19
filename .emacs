(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

; line numners
(add-hook 'prog-mode-hook 'linum-mode)

; evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode t)

; evil surround
(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

; evil nerd commenter
(add-to-list 'load-path "~/.emacs.d/evil-nerd-commenter")

; evil matchit
(add-to-list 'load-path "~/.emacs.d/evil-matchit")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;(coffee-args-compile (quote ("-c" "--no-header" "--bare")))
;  '(coffee-tab-width 2)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (web-mode git-gutter-fringe git-gutter counsel-projectile auto-complete-auctex wgrep sass-mode robe mmm-mode helm-projectile helm-ag flycheck evil-nerd-commenter evil elscreen counsel coffee-mode auto-complete))))
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
(require 'helm-projectile)
(evil-leader/set-key
  "f" 'helm-projectile-find-file)
; (setq projectile-enable-caching t)

; flycheck
(global-flycheck-mode)

(setq ruby-insert-encoding-magic-comment nil)

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

; counsel-projectile
(counsel-projectile-on)

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
