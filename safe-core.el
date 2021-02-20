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

(defcustom safe-prefix-alist nil
  "The prefix list."
  :type 'list
  :group 'safe)

(defcustom safe-cursor-type 'bar
  "The cursor type."
  :type 'symbol
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

(defcustom safe-last-search nil
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

(defcustom safe-update-result nil
  "If need to update result."
  :type 'boolean
  :group 'safe)

(defcustom safe-select-item-overlay nil
  "The overlay for current item."
  :group 'safe)

(defcustom safe-select-extension nil
  "The selected extension."
  :type 'string
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

(defface safe-extension-title-face
  '((t (:foreground "#CC7700")))
  "The face for extension's title."
  :group 'safe)

(defface safe-select-item-face
  '((t (:background "grey48")))
  "The face for select item.")

(defvar safe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'safe-close)
    (define-key map (kbd "C-n") 'safe-next-item)
    (define-key map (kbd "C-p") 'safe-previous-item)
    (define-key map (kbd "M-b") 'safe-previous-extension)
    (define-key map (kbd "M-f") 'safe-next-extension)
    (define-key map (kbd "RET") 'safe-return)
    (define-key map (kbd "DEL") 'safe-delete)
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
  (setq-local major-mode 'safe-mode
              mode-name "Safe")
  (use-local-map safe-mode-map)

  ;; Close some modes
  (when (featurep 'company)
    (company-mode -1)))

(defun safe (&optional search-type object))

(defun safe--extension-title-p (string)
  "Check if the STRING is the extension title."
  (not (string= (substring string 0 1) "\t")))

(defun safe--empty-line-p ()
  "Check if the current line is empty."
  (= (point-at-bol) (point-at-eol)))

(defun safe-select-input-window ()
  "Select the input window."
  (select-window (get-buffer-window safe-buffer)))

(defun safe-input-buffer-init ()
  "Initialize the buffer."
  (switch-to-buffer safe-buffer)
  (with-current-buffer safe-buffer
    (erase-buffer)
    (safe-mode)
    (buffer-face-set 'safe-input-face)
    (if (not (featurep 'evil))
        (setq-local cursor-type safe-cursor-type)
      (setq-local evil-emacs-state-cursor safe-cursor-type)
      (evil-change-state 'emacs))
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

(defun safe-close ()
  "Close and kill the safe."
  (interactive)
  (let ((auto-name (safe--get-index safe-current-extension
                                    safe-autoload-extension-alist
                                    'car 'cdr)))
    (safe--result auto-name 'delete))
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
        safe-select-item-overlay nil
        safe-select-extension nil
        safe-run-timer nil
        safe-last-search nil))

(defun safe-close-settings (&optional none-cursor)
  "Close some options."
  (when display-line-numbers
    (setq-local display-line-numbers nil))
  (when (version< "27.0" emacs-version)
    (setq-local tab-line-format nil))
  (setq-local mode-line-format nil
              header-line-format nil)
  (if none-cursor
      (setq-local cursor-type nil)))

(defun safe--float-to-int (float)
  "Convert the FLOAT to int."
  (let ((number (number-to-string float)))
    (string-to-number (progn
                        (string-match "^\\(.*\\)\\." number)
                        (substring (match-string 0 number) 0 -1)))))

(defun safe--extension-exists-p (extension)
  "Check if the extension is exists."
  (eval-or (safe--get-index extension var 'car)
    var safe-extension-alist safe-autoload-extension-alist))

(defun safe--set-input (input)
  "Set the input."
  (with-current-buffer safe-buffer
    (erase-buffer)
    (insert input)))

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

(defun safe--get-index (item seq &optional action value)
  "Get the earliest index of ITEM in SEQ.
If CONS is t, it'll get the earliest cons' index."
  (let ((index nil)
        (indexf 0))
    (if (null seq)
        nil
      (dolist (ele seq)
        (if (equal item (if action
                            (ignore-errors (funcall action ele))
                          ele))
            (setq index indexf)
          (setq indexf (1+ indexf))))
      (when index
        (cond ((eq value t) (nth index seq))
              ((eq value nil) index)
              (t (funcall value (nth index seq))))))))

(defun safe-current-extension-p (name list)
  "Check if the NAME of extension is the current extension."
  (when safe-current-extension
    (let ((result
           (string= name
                    (safe--get-index safe-current-extension
                                     list
                                     'car
                                     'cdr))))
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
    (if (and (not (string= input "")) (string= input safe-last-search))
        (list input nil)
      (setq safe-last-search input)
      (when prefix
        (setq input (substring input 1))
        (when (null safe-current-extension)
          (setq safe-current-extension prefix
                safe-update-result t)))
      (when (and (not (null safe-current-extension)) (null prefix))
        (setq safe-current-extension nil
              safe-update-result t))
      input)))

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

(defun safe--result (name action)
  "Do the ACTION  with NAME."
  (let ((result (safe--get-index name safe-result-list 'car t)))
    (cond ((and result (eq action 'delete))
           (setq safe-result-list (delete result safe-result-list)))
          ((and (null result) (eq action 'add))
           (setq safe-result-list (append safe-result-list (list (list name))))))))

(defun safe--list-match (input string)
  "Check if the INPUT in the list are all matched by STRING."
  (let ((input-list (split-string input " " t)))
    (if (catch 'not-match
          (dolist (i input-list)
            (unless (string-match-p (regexp-quote i) string)
              (throw 'not-match t))))
        nil
      t)))

;;; Advice

(advice-add 'push-mark
            :around
            (lambda (orig &optional location nomsg activite)
              "Do not notice 'Mark Set' in safe."
              (funcall orig location (if (string= (buffer-name (current-buffer)) safe-result-buffer)
                                         t
                                       nomsg)
                       activite)))

;;; Functions for User

(defun safe-delete-extension (extension &optional alist)
  "Delete EXTENSION in `safe-extension-alist' or ALIST."
  (let ((name (safe--get-index extension (if alist
                                             (symbol-value alist)
                                           safe-extension-alist)
                               'car 'cdr)))
    (unless alist
      (setq alist 'safe-extension-alist))
    (if (null name)
        (message "%S is not included by %S!" extension alist)
      (set alist (eval (delete (cons extension name) (symbol-value alist)))))))

(defun safe-move-extension (extension from to)
  "Move EXTENSION from FROM to TO."
  (let ((name (safe--get-index extension (symbol-value from) 'car 'cdr)))
    (if (null name)
        (message "%S is not included by %S!" extension from)
      (safe-delete-extension extension from)
      (add-to-list to (cons extension name) t)
      (safe--result name (if (eq to 'safe-extension-alist)
                             'add
                           'delete)))))

(defun safe-match-p (input string)
  "If the STRING is fuzzyly matched by INPUT, return non-nil."
  (if (string= input "")
      t
    (safe--list-match input string)))

(defmacro eval-or (s-ex var o1 o2)
  "Eval the O1 and O2 with S-EX, replace the VAR in S-EX with o1 and o2.
Return the value which is non-nil.
If the two results are all nil, return nil."
  (declare (indent 1))
  (let ((o-index (safe--get-index `,var `,s-ex)))
    (if (null o-index)
        (error "eval-or: s-ex need var!")
      `(let ((s ',s-ex)
             result)
         (setf (nth ,o-index s) ',o1
               result (ignore-errors (eval s)))
         (if result
             result
           (setf (nth ,o-index s) ',o2
                 result (ignore-errors (eval s)))
           result)))))

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
