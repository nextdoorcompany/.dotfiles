((org-mode . ((eval . (progn
                        (make-variable-buffer-local 'after-save-hook)
                        (add-hook 'after-save-hook 'js/commit-on-save))))))
