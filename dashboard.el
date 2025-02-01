(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-banner-logo-title "Welcome to Ahoge Emacs!")
(setq dashboard-startup-banner "~/.emacs.d/ahoge_logo.png")

(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents   . 5)
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

(provide 'dashboard)
