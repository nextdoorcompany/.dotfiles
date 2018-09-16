(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package diminish
  :ensure t
  )

(setq visible-bell t)
(setq confirm-kill-emacs #'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq visual-line-fringe-indicators t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (global-hl-line-mode 1))

(setq column-number-mode t)
(delete-selection-mode t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq linum-format "%d ")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(when (member "Triplicate T4c" (font-family-list))
  (set-face-attribute 'default nil :font "Triplicate T4c"))
(set-face-attribute 'default nil :height 240)

(use-package flyspell
  :ensure t
  :diminish flyspell-mode

  :config
  (setq ispell-program-name "aspell"
        ispell-local-dictionary "en_US"
        ispell-dictionary "american"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-list-command "--list"))

(use-package eldoc
  :diminish eldoc-mode)

(defun js/new-buffer-checked ()
    "Create and switch to a temporary scratch org buffer with a random
     name.  Include spell checking."
    (interactive)
    (switch-to-buffer (make-temp-name "scratch-"))
    (org-mode)
    (flyspell-mode)
    (visual-line-mode))

(global-set-key (kbd "C-c f") 'js/new-buffer-checked)

(defun js/new-scratch-buffer ()
    "Create and switch to a temporary scratch org buffer with a random
     name."
    (interactive)
    (switch-to-buffer (make-temp-name "scratch-"))
    (org-mode))

(defcustom calendar-copy-as-kill-format "%Y-%m-%d"
  "Format string for formatting calendar dates with `format-time-string'."
  :type 'string
  :group 'calendar)

(defun calendar-copy-as-kill ()
  "Copy date at point as kill if region is not active.
Delegate to `kill-ring-save' otherwise."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (let ((date (calendar-cursor-to-date)))
      (when date
        (setq date (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
        (kill-new (format-time-string calendar-copy-as-kill-format date))))))

(defun my-calendar-mode-hook-fun ()
  "Let \[kill-ring-save] copy the date at point if region is not active."
  (local-set-key [remap kill-ring-save] #'calendar-copy-as-kill))

(add-hook 'calendar-mode-hook #'my-calendar-mode-hook-fun)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(add-to-list 'completion-ignored-extensions ".pyc")

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(add-hook 'python-mode-hook
     (lambda ()
       (linum-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))

(defun js/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-c C-<down>") 'js/duplicate-line)

(defun js/delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(global-set-key (kbd "C-c <deletechar>") 'js/delete-current-line)

(defun js/wsl-copy ()
  (interactive)
  (let ((default-directory "/mnt/c/"))
    (shell-command-on-region (point-min) (point-max) "clip.exe")))

(setq org-agenda-files (quote ("~/org/ephemera.org" "~/org/kanban.org")))

(setq org-cycle-separator-lines 1)

(setq org-publish-project-alist
      '(("keyboard"
         :base-directory "~/notes/"
         :exclude "org"
         :include ("keyboard.org")
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/org/html")))

(use-package avy
  :ensure t
  :bind ("C-c j" . avy-goto-char-timer))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package auto-yasnippet
  :ensure t
  :bind (("C-c w" . aya-create)
         ("C-c y" . aya-expand)))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("S-<return>" . crux-smart-open-line)))

(use-package try
  :ensure t)

(use-package helpful
  :ensure t)

(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :config
  (wrap-region-global-mode t))
