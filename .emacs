(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(load-theme 'manoj-dark)

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
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (set-face-attribute 'default nil :height 240))

(delete-selection-mode t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(setq linum-format "%d ")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'python-mode-hook
     (lambda ()
       (linum-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))

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

(defun duplicate-line (arg)
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

(global-set-key (kbd "C-c C-<down>") 'duplicate-line)

(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(global-set-key (kbd "C-c <deletechar>") 'delete-current-line)

(setq org-agenda-files (quote ("~/org/ephemera.org" "~/org/kanban.org")))

(setq org-cycle-separator-lines 1)


(setq org-publish-project-alist
      '(("keyboard"
         :base-directory "~/notes/"
         :exclude "org"
         :include ("keyboard.org")
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/org/html")))

