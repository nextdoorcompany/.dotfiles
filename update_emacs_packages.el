(defun js/update_emacs_packages ()
  (let ((package-menu-async nil))
    (package-list-packages))
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute 'noquery)
    (user-error nil)))
