(when (file-exists-p "~/.config.org")
  (org-babel-load-file (file-truename "~/.config.org")))
(put 'dired-find-alternate-file 'disabled nil)
