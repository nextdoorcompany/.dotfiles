(when (file-exists-p "~/.config.org")
  (org-babel-load-file (file-truename "~/.config.org")))
(when (file-exists-p "~/emacs_local.org")
  (org-babel-load-file (file-truename "~/emacs_local")))

