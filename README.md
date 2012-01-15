# Setup

Add the following to your `.emacs`.

    (add-to-list 'load-path "~/.emacs.d/plugins/drupal-mode-el/")
    (require 'drupal-mode)
    (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
    (add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|inc\\)$" . drupal-mode))
    (add-to-list 'auto-mode-alist '("\\.info" . conf-mode))

## Emacs Shortcuts

* C-c C-a:     Search Drupal API for string at point.  
* C-c C-f:     Search php.net for string at point.
* C-c C-c C-a: drush cc all
* C-c C-s:     drush status
