;;; safe.el --- A extendable fuzzy searcher for Emacs. -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs))
;; Homepage: https://github.com/SpringHan/safe.git
;; Keywords: Extendable Fuzzy search


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Safe is a extendable fuzzy searcher.

;;; Code:

(require 'safe-core)

;;; Basic Extensions

(defun safe-buffer-get-buffers (input)
  "Get the buffers."
  (let* ((buffers (buffer-list))
         visible-buffers)
    (mapc #'(lambda (b)
              (setq b (buffer-name b))
              (unless (or (string= b "*Safe*")
                          (string= b "*Safe Result*")
                          (string= (substring b 0 1) " "))
                (add-to-list 'visible-buffers b)))
          buffers)
    (setq buffers '())
    (dolist (buffer visible-buffers)
      (when (safe-match-p input buffer)
        (add-to-list 'buffers buffer)))
    buffers))

(defun safe-buffer-icon (buffer)
  "The buffer's icon."
  (with-current-buffer buffer
    (if (eq major-mode 'eaf-mode)
        (all-the-icons-faicon "html5" :height 180)
      (all-the-icons-icon-for-buffer))))

(safe-define-extension safe-buffer-switch "BUFFER"
  "The buffer extension for safe."
  :prefix "#"
  :get safe-buffer-get-buffers
  :enter
  (lambda (e)
    (safe-close)
    (switch-to-buffer e))
  :icon safe-buffer-icon)

;;; Imenu
(require 'imenu)

(safe-define-extension safe-imenu "IMENU"
  "The imenu extension for safe."
  :prefix "@"
  :get
  (lambda (g)
    (ignore-errors
      (let ((imenu-results (with-current-buffer safe-previous-buffer
                             (imenu--make-index-alist t)))
            result)
        (setq imenu-results (delete '("*Rescan*" . -99) imenu-results))
        (mapc #'(lambda (i)
                  (when (safe-match-p g (car i))
                    (add-to-list 'result (car i))))
              imenu-results)
        result)))
  :enter
  (lambda (e)
    (safe-close)
    (imenu e)))

;; (safe-define-extension safe-command "COMMAND"
;;   "The command extension for safe."
;;   :prefix ">"
;;   :get
;;   (lambda (g)
;;     (let ((commands)))))

(provide 'safe)

;;; safe.el ends here
