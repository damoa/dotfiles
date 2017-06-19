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
