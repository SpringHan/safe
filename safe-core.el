;;; safe-core.el --- A extendable fuzzy searcher for Emacs. -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs "26.3"))
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

(require 'all-the-icons)

(defgroup safe nil
  "The group for `safe'."
  :group 'applications)

(defcustom safe-extension-alist nil
  "The extensions alist."
  :type 'list
  :group 'safe)

(defcustom safe-autoload-extension-alist nil
  "The autoload extensions."
  :type 'list
  :group 'safe)

(defcustom safe-icon-alist nil
  "The icon alist."
  :type 'list
  :group 'safe)

(defcustom safe-prefix-alist nil
  "The prefix list."
  :type 'list
  :group 'safe)

(defcustom safe-run-timer nil
  "The timer."
  :group 'safe)

(defcustom safe-previous-buffer nil
  "Buffer before starting safe."
  :type 'buffer
  :group 'safe)

(defcustom safe-previous-directory nil
  "Buffer before starting safe."
  :type 'string
  :group 'safe)

(defcustom safe-last-search ""
  "Last search content."
  :type 'string
  :group 'safe)

(defcustom safe-current-item nil
  "The current item's line number."
  :type 'number
  :group 'safe)

(defcustom safe-current-extension nil
  "The current extension."
  :type 'symbol
  :group 'safe)

(defcustom safe-current-input-point 0
  "The column number for input."
  :type 'number
  :group 'safe)

(defcustom safe-update-content nil
  "If need to update the contents."
  :type 'boolean
  :group 'safe)

(defcustom safe-result-list nil
  "Result list."
  :type 'list
  :group 'safe)

(defcustom safe-update-result nil
  "If need to update result."
  :type 'boolean
  :group 'safe)

(defcustom safe-current-item-overlay nil
  "The overlay for current item."
  :group 'safe)

(defconst safe-buffer "*Safe*"
  "Safe buffer.")

(defconst safe-result-buffer "*Safe Result*"
  "Safe result buffer.")

(defface safe-input-face
  '((t (:height 250)))
  "Face for input."
  :group 'safe)

(defface safe-result-face
  '((t (:height 200)))
  "The face for result."
  :group 'safe)

(defvar safe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'safe-close)
    (define-key map (kbd "C-n") 'safe-next-item)
    (define-key map (kbd "C-p") 'safe-previous-item)
    (define-key map (kbd "M-b") 'safe-previous-extension)
    (define-key map (kbd "M-f") 'safe-next-extension)
    (define-key map (kbd "RET") 'safe-return)
    ;; (define-key map (kbd "DEL") 'safe-delete)
    map)
  "The safe mode map.")

(defcustom safe-mode-hook nil
  "Safe mode hook."
  :type 'hook
  :group 'safe)

(define-derived-mode safe-mode nil "Safe"
  "The major mode for `safe'."
  :syntax-table nil
  :abbrev-table nil
  :group 'safe
  (kill-all-local-variables)
  (use-local-map safe-mode-map)

  ;; Close some modes
  (when (featurep 'company)
    (company-mode -1)))

(defun safe (&optional search-type object)
  "The main function for `safe'."
  (interactive)
  (if (get-buffer safe-buffer)
      (safe-close)

    ;; Initialize the buffer
    (setq safe-previous-buffer (current-buffer)
          safe-previous-directory default-directory)

    (tab-bar-new-tab)
    ;; Input buffer init
    (safe-input-buffer-init)
    (safe-result-buffer-init)
    (safe-select-input-window)

    ;; Start searching...
    (safe-search-init (if object
                     object
                   "")
                 (when search-type search-type))))

(defun safe-search-init (input &optional extension)
  "Prepare for the search."
  (if extension
      (when (safe--extension-exists-p extension)
        (setq safe-current-extension extension))
    (let ((prefix (safe--get-prefix input)))
      (when prefix
        (setq safe-current-extension prefix))))
  (setq safe-run-timer (run-with-timer 0 0.1
                                       'safe-search)))

(defun safe-search ()
  "Search for the results."
  (let ((input (safe--get-input))
        (extensions (if safe-current-extension
                        safe-current-extension
                      safe-extension-alist)))
    (when extensions
      (if (symbolp extensions)
          (funcall extensions input)
        (dolist (extension extensions)
          (funcall (car extension) input))
        ))
    (safe-update-result-buffer)))

(defun safe-update-result (name value)
  "Update the VALUE of extension NAME."
  (let* ((result-index (safe--get-index name safe-result-list t))
        (result-name (car (nth result-index safe-result-list))))
    (setf (cdr (nth result-index safe-result-list)) value)
    (setq safe-update-result t)))

(defun safe-update-result-buffer ()
  "Update content buffer."
  (when safe-update-result
    (with-current-buffer safe-result-buffer
      (erase-buffer)

      (setq safe-current-item-overlay (make-overlay (point) (point) (current-buffer) t))
      (overlay-put safe-current-item-overlay 'face '((t)))

      (dolist (result safe-result-list)
        (when (and (not (safe--result-empty-p (cdr result)))
                   (or (null safe-current-extension)
                       (safe-current-extension-p (car result) safe-extension-alist)
                       (safe-current-extension-p (car result) safe-autoload-extension-alist)))
          (insert (propertize (car result) 'face '(:foreground "#CC7700")) "\n")
          (mapc #'(lambda (r)
                    (insert (format "\t%s\t%s\n"
                                    (safe--get-icon (car result) r)
                                    (propertize r 'face '(:height 180)))))
                (cdr result))
          (insert "\n"))))
    (setq safe-update-result nil)))

(defun safe-select-input-window ()
  "Select the input window."
  (select-window (get-buffer-window safe-buffer)))

(defun safe-input-buffer-init ()
  "Initialize the buffer."
  (switch-to-buffer safe-buffer)
  (safe-mode)
  (with-current-buffer safe-buffer
    (erase-buffer)
    (buffer-face-set 'safe-input-face)
    (when (featurep 'evil)
      (evil-change-state 'insert))
    (safe-close-settings)))

(defun safe-result-buffer-init ()
  "Initialize the result buffer."
  (unless (get-buffer safe-result-buffer)
    (split-window nil (safe--float-to-int (* (window-size) 0.95)) 'above)
    (switch-to-buffer safe-result-buffer))
  (with-current-buffer safe-result-buffer
    (erase-buffer)
    (buffer-face-set 'safe-result-face)
    (safe-close-settings t)
    (setq-local tab-width 1)))

(defun safe-enter ()
  "Enter action."
  (interactive))

(defun safe-close ()
  "Close and kill the safe."
  (interactive)
  (when (get-buffer safe-buffer)
    (tab-bar-close-tab)
    (kill-buffer safe-buffer)
    (kill-buffer safe-result-buffer))
  (switch-to-buffer safe-previous-buffer)
  (cancel-timer safe-run-timer)
  (setq safe-previous-buffer nil
        safe-previous-directory nil
        safe-current-item nil
        safe-current-extension nil
        safe-current-input-point 0
        safe-run-timer nil))

(defun safe-close-settings (&optional none-cursor)
  "Close some options."
  (when display-line-numbers
    (setq-local display-line-numbers nil))
  (when (version< "27.0" emacs-version)
    (setq-local tab-line-format nil))
  (setq-local mode-line-format nil
              header-line-format nil)
  (when none-cursor
    (setq-local cursor-type nil)))

(defun safe--float-to-int (float)
  "Convert the FLOAT to int."
  (let ((number (number-to-string float)))
    (string-to-number (progn
                        (string-match "^\\(.*\\)\\." number)
                        (substring (match-string 0 number) 0 -1)))))

(defun safe--extension-exists-p (extension)
  "Check if the extension is exists."
  (let (result)
    (mapc #'(lambda (e)
              (when (equal (car e) extension)
                (setq result t))))
    result))

(defun safe--get-prefix (string)
  "Get the prefix. If STRING has prefix, return it. Otherwise return nil."
  (let ((prefix (if (= (length string) 0)
                    nil
                  (substring string 0 1)))
        prefix-function)
    (when prefix
      (mapc #'(lambda (p)
                (when (string= prefix (car p))
                  (setq prefix-function (cdr p))))
            safe-prefix-alist))
    prefix-function))

(defun safe--get-index (item seq &optional cons value)
  "Get the earliest index of ITEM in SEQ.
If CONS is t, it'll get the earliest cons' index."
  (let ((index nil)
        (indexf 0))
    (if (null seq)
        nil
      (dolist (ele seq)
        (if (equal item (if cons
                            (car ele)
                          ele))
            (setq index indexf)
          (setq indexf (+ 1 indexf))))
      (if value
          (nth index seq)
        index))))

(defun safe-current-extension-p (name list)
  "Check if the NAME of extension is the current extension."
  (when safe-current-extension
    (let ((result
           (string= name
                    (cdr (safe--get-index safe-current-extension
                                          list
                                          t
                                          t)))))
      result)))

(defun safe--get-icon (name result)
  "Get the NAME's icon."
  (let (icon)
    (mapc #'(lambda (i)
              (when (equal name (car i))
                (setq icon (funcall (cdr i) result))))
          safe-icon-alist)
    (if icon
        icon
      "")))

(defun safe--get-input ()
  "Get input."
  (let* ((input (with-current-buffer safe-buffer
                  (substring (buffer-string) safe-current-input-point)))
         (prefix (safe--get-prefix input)))
    (when (and prefix (= safe-current-input-point 0))
      (setq safe-current-input-point 1
            safe-current-extension prefix
            input (substring input 1)
            safe-update-result t))
    (when (and (string= input "") (/= safe-current-input-point 0))
      (setq safe-current-input-point 0
            safe-current-extension nil
            safe-update-result t))
    input))

(defun safe--result-empty-p (result)
  "Check if the result is empty."
  (when (or (equal result '(nil))
            (null result))
    t))

(defun safe-result-exists-p (result)
  "Check if RESULT is exists."
  (let (re)
    (mapc #'(lambda (r)
              (when (string= result (car r))
                (setq re t)))
          safe-result-list)
    re))

(defun safe-match-p (input string)
  "If the STRING is fuzzyly matched by INPUT, return non-nil."
  (if (string= input "")
      t
    (string-match-p (regexp-quote input) string)))

(defmacro safe-define-extension (extension name &rest body)
  "The macro to define extension.

Keywords:

`prefix': The prefix for quickly change to the extension.

`get': A lambda function, you can use it to

\(fn EXTENSION NAME [DOCSTRING] [ARGS..] &rest OTHERS)"
  (declare (indent 2))
  (let (doc prefix get enter auto icon)
    (when (stringp (car-safe body))
      (setq doc (pop body)))
    (while (keywordp (car-safe body))
      (pcase (pop body)
        (:prefix (setq prefix (pop body)))
        (:get (setq get (pop body)))
        (:enter (setq enter (pop body)))
        (:icon (setq icon (pop body)))
        (:auto (setq auto (pop body)))))
    `(progn
       ,@body
       (defun ,extension (&optional get enter)
         ,(when doc doc)
         (when get
           (funcall 'safe-update-result
                    ,name
                    (funcall ',get get)))
         (when enter
           (funcall ',enter enter)))
       (when ,prefix
         (add-to-list 'safe-prefix-alist (cons ,prefix ',extension)))
       (unless (safe-result-exists-p ,name)
         (add-to-list 'safe-result-list (list ,name) t))
       (if ,auto
           (add-to-list 'safe-autoload-extension-alist (cons ',extension ,name))
         (add-to-list 'safe-extension-alist (cons ',extension ,name)))
       (when ',icon
         (add-to-list 'safe-icon-alist (cons ,name ',icon))))))

(provide 'safe-core)

;;; safe-core.el ends here
