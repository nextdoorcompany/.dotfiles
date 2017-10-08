(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq inhibit-startup-screen t)

(setq initial-scratch-message nil)

(setq column-number-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(add-to-list 'completion-ignored-extensions ".pyc")

(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system
  (scroll-bar-mode -1))
(when window-system
  (global-hl-line-mode 1))

(delete-selection-mode t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)

(setq linum-format "%d ")

(add-hook 'python-mode-hook
     (lambda ()
       (linum-mode 1)))

(defun wsl-copy ()
  (interactive)
  (let ((default-directory "/mnt/c/"))
    (shell-command-on-region (point-min) (point-max) "clip.exe")))

(defun new-scratch-buffer ()
    "Create and switch to a temporary scratch buffer with a random
     name."
    (interactive)
    (switch-to-buffer (make-temp-name "scratch-"))
    (org-mode))
