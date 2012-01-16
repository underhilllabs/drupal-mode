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
  "Call drush variable get and write result to *Drush* buffer.  If called with a prefix argument, 
  it will ask for a specific variable to display."
  (interactive "p")
  (if (= n 4)
      (progn 
        (let ((variable (read-string (message "Which Variable do you want to get: "))))
          (call-process "drush" nil "*Drush*" t "variable-get" variable)))
      (call-process "drush" nil "*Drush*" t "variable-get")))


(provide 'drupal-drush-commands)
