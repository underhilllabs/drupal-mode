;;; drupal-drush.el --- Emacs Interface to Drush Commands

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(defun drupal-drush-cc-all (&rest args)
  (interactive)
  (call-process "drush" nil "*Drush*" nil "cc" "all"))                                                                                    

(defun drupal-drush-cc-all (&rest args)
  (interactive)
  (call-process "drush" nil "*Drush*" nil "cc" "all"))

(defun drupal-drush-status (&rest args)
  (interactive)
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

(provide 'drupal-drush-commands)