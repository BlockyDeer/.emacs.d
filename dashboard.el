(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-banner-logo-title "Welcome to Ahoge Emacs!")
(setq dashboard-startup-banner "~/.emacs.d/ahoge_logo.png")

(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents   . 10)
                        (bookmarks . 5)))
(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items))

      
(setq dashboard-item-names '(("Recent Files:"  . "Recently opened files:")
                             ("Bookmarks:"     . "Bookmarks")))

(setq dashboard-navigation-cycle t)

(defun dashboard-goto-recent-files ()
  "Move point to the Recent Files section in dashboard."
  (when (and (derived-mode-p 'dashboard-mode)
             (not (minibufferp)))
    (goto-char (point-min))
    (when (search-forward "Recent Files" nil t)
      (forward-line 1)
      (beginning-of-line))))

(add-hook 'dashboard-after-initialize-hook
          #'dashboard-goto-recent-files)

(provide 'dashboard)
