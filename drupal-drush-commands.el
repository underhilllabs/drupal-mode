;;; drupal-drush.el --- Emacs Interface to Drush Commands

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(defun drupal-drush-cc-all (&rest args)
  (interactive)
  (message "Clearing caches.. one moment.")
  (call-process "drush" nil "*Drush*" nil "cc" "all"))                                                                                    

(defun drupal-drush-status (&rest args)
  (interactive)
  (message "Checking status of site.. one moment.")
  (call-process "drush" nil "*Drush*" nil "status"))

(defun drupal-drush-update-security (&rest args)
  (interactive)
  (call-process "drush" nil "*Drush*" t "pm-update" "--security-only"))

(defun drupal-drush-update-check (&rest args)
  (interactive)
  (call-process "drush" nil "*Drush*" t "pm-update" "--no"))

(defun drupal-drush-core-requirements (&rest args)
  (interactive)
  (call-process "drush" nil "*Drush*" t "core-requirements"))

(defun drupal-drush-variable-get (n)
  "Call drush variable-get and write result to *Drush* buffer.  If called with a prefix argument,
  it will ask for a specific variable to display."
  (interactive "p")
  (if (= n 4)
      (progn 
        (let ((variable (read-string (message "Which Variable do you want to get: "))))
          (call-process "drush" nil "*Drush*" t "variable-get" variable)))
      (call-process "drush" nil "*Drush*" t "variable-get")))

(defun drupal-drush-alias-add (name uri root) 
  "Open the drush aliases file in a buffer."
  (interactive "sAlias Name: \nsURI: \nsRoot: ") 
  (drupal-drush-process-alias-file drush-default-alias-file name uri root))

(defun drupal-drush-process-alias-file (file name uri root)
  "Read the contents of a file into a temp buffer and then do
 something there."
  (when (file-readable-p file)
    (let (mybuffer)
      (setq mybuffer (find-file file))
      (goto-char (point-max))
      (insert (format "$aliases['%s'] = array(\n" name))
      (insert (format "  'uri' => '%s',\n" uri))
      (insert (format "  'root' => '%s',\n" root))
      (insert (format ");\n"))
      )))

(provide 'drupal-drush-commands)
