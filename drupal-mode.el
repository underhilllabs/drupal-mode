;;; drupal-mode.el --- Major mode for developing Drupal modules

;;; Commentary:
;;


;;; History:
;;

;;; Code:

;;(require 'w3m)
(require 'drupal-drush-commands)

(defgroup drupal nil
  "Drupal Development."
  :group 'programming)

(defcustom drupal-default-api "7"
  "Drupal API to Search."
  :type 'string
  :group 'drupal)

(defcustom drupal-drush-version "5"
  "Version of Drush installed."
  :type 'string
  :group 'drupal)

(defcustom drupal-api-url "http://api.drupal.org/"
  "Drupal API URL."
  :type 'string
  :group 'drupal)

(defvar drupal-search-url (concat "http://api.drupal.org/api/search/" drupal-default-api "/")
  "URL at which to search for documentation on a word.")

(defcustom drupal-manual-path ""
  "local manual path"
  :type 'string
  :group 'drupal)

(defcustom drupal-hook-docstring
  "/**
 * Implements %s()
 */"
  "Documentation string for hook."
  :type 'string
  :group 'drupal)

(defcustom drupal-file-docstring
  "/**
 * @file
 * %s()
 */"
  "Documentation string for file."
  :type 'string
  :group 'drupal)

(defcustom drupal-function-docstring
  "/**
 * %s()
 */"
  "Documentation string for function."
  :type 'string
  :group 'drupal)

;; source: http://drupal.org/node/59868
(defcustom drupal-php-style
  '((c-offsets-alist . ((case-label . +)
                        (arglist-intro . +) ; for FAPI arrays and DBTNG
                        (arglist-cont-nonempty . c-lineup-math) ; for DBTNG fields and values
                        (arglist-close . c-lineup-close-paren) ; correct arglist closing parenthesis
                        )))
  "Drupal coding style."
  :group 'drupal)

(defcustom drupal-api-buffer-name
  "*drupal-api*"
  "Buffer name for temporary API lookup."
  :type 'string
  :group 'drupal)

(defun drupal-hook-implement (hook-name)
  "Insert API code for HOOK-NAME at point."
  (interactive (list (read-string "Hook name: ")))
  (let ((module-name (drupal-module-name))
        (docstring (format drupal-hook-docstring hook-name))
        (url (concat drupal-api-url hook-name)))
    (insert
     (concat docstring (with-current-buffer (get-buffer-create drupal-api-buffer-name)
                         (if (w3m-process-with-wait-handler
                               (w3m-retrieve-and-render url nil nil nil nil handler))
                             (let* ((beg (search-forward "<?php" nil t))
                                    (end (- (search-forward "?>" nil t) 2))
                                    (content (buffer-substring-no-properties beg end)))
                               (replace-regexp-in-string "^function \\(hook\\)_" module-name content nil nil 1))
                           (error "Failed to fetch page.")))))))

;; based on sacha chua's idea
(defun drupal-module-name ()
  "Return the Drupal module name for .module and .install files."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(define-derived-mode drupal-mode
  php-mode "Drupal"
  "Major mode for working with Drupal.
\\{drupal-mode-map}"
  (c-set-style "drupal-php-style")
  (set 'tab-width 2)
  (set 'c-basic-offset 2)
  (set 'indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq show-tab t))

(define-key drupal-mode-map "\C-cda" 'drupal-search-documentation)
(define-key drupal-mode-map "\C-cdb" 'drupal-browse-api)
(define-key drupal-mode-map "\C-cdc" 'drupal-drush-cc-all)
(define-key drupal-mode-map "\C-cds" 'drupal-drush-status)
(define-key drupal-mode-map "\C-cdvg" 'drupal-drush-variable-get)

(c-add-style "drupal-php-style" drupal-php-style)

(defun drupal-search-local-documentation ()
  "Search the local PHP documentation (i.e. in `php-manual-path')
for the word at point.  The function returns t if the requested
documentation exists, and nil otherwise."
  (interactive)
  (flet ((php-function-file-for (name)
                                (expand-file-name
                                 (format "function.%s.html"
                                         (replace-regexp-in-string "_" "-" name))
                                 php-manual-path)))
    (let ((doc-file (php-function-file-for (current-word))))
      (and (file-exists-p doc-file)
           (browse-url doc-file)))))

;; Define function documentation function
(defun drupal-search-documentation ()
  "Search drupal api documentation for the word at point.  If
`drupal-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the
requested documentation does not exist it will fallback to
searching the drupal api website."
  (interactive)
  (flet ((drupal-search-web-documentation ()
                                       (browse-url (concat drupal-search-url (current-word)))))
    (if (and (stringp drupal-manual-path)
             (not (string= drupal-manual-path "")))
        (or (drupal-search-local-documentation)
            (drupal-search-web-documentation))
      (drupal-search-web-documentation))))

;; Define function for browsing manual
(defun drupal-browse-api ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url drupal-api-url))

(provide 'drupal-mode)

;;; drupal-mode.el ends here
