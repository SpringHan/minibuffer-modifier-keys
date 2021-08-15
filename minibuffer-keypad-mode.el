;;; minibuffer-keypad-mode.el --- A package allows you to avoid Ctrl,Meta in minibuffer -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/SpringHan/minibuffer-keypad-mode.git
;; Keywords: tools


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

;; A package allows you to avoid Ctrl,Meta in minibuffer

;;; Code:

(defcustom minibuffer-keypad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'minibuffer-keypad-mode-self-insert)
    (define-key map (kbd "SPC") 'minibuffer-keypad-mode-start-or-stop)
    map)
  "The keymap for minibuffer-keypad state."
  :type 'keymap
  :group 'minibuffer-keypad-mode)

(defcustom minibuffer-keypad-mode-on nil
  "If the minibuffer-keypad mode is opened."
  :type 'boolean
  :group 'minibuffer-keypad-mode)

(defcustom minibuffer-keypad-mode-prefix "C-"
  "The prefix for minibuffer-keypad."
  :type 'string
  :group 'minibuffer-keypad-mode)

(defcustom minibuffer-keypad-mode-first-start t
  "If this is the first time to start minibuffer."
  :type 'boolean
  :group 'minibuffer-keypad-mode)

(defcustom minibuffer-keypad-mode-open-timer nil
  "The timer for opening minibuffer-keypad at the first time to start it."
  :type 'timer
  :group 'minibuffer-keypad-mode)

;;;###autoload
(define-minor-mode minibuffer-keypad-mode
  "Minibuffer keypad mode."
  nil nil minibuffer-keypad-mode-map)

(defun minibuffer-keypad-mode-setup (enable)
  "Setup or disable minibuffer-keypad-mode.
Enable the mode when ENABLE is non-nil."
  (if enable
      (add-hook 'minibuffer-setup-hook
                (lambda ()
                  (minibuffer-keypad-mode t)))
    (remove-hook 'minibuffer-setup-hook
                 (lambda ()
                   (minibuffer-keypad-mode t)))))

(defun minibuffer-keypad-mode--index (ele list)
  "Get the index of ELE in LIST."
  (catch 'result
    (dotimes (i (length list))
      (when (equal (nth i list) ele)
        (throw 'result i)))))

(defun minibuffer-keypad-mode--convert-prefix (prefix)
  "Convert PREFIX from string to char or from char to string."
  (let* ((prefix-string '("C-" "M-" "C-M-"))
         (prefix-char '(44 46 47))
         (from (if (stringp prefix)
                   prefix-string
                 prefix-char))
         (to (if (stringp prefix)
                 prefix-char
               prefix-string))
         index)
    (setq index (minibuffer-keypad-mode--index prefix from))
    (when index
      (nth index to))))

(defun minibuffer-keypad-mode-keypad (external-char)
  "Execute the keypad command.
EXTERNAL-CHAR is the entrance for minibuffer-keypad mode."
  (interactive)
  (let ((key (when (and (memq external-char '(44 46 47))
                        (/= (minibuffer-keypad-mode--convert-prefix
                             minibuffer-keypad-mode-prefix)
                            external-char))
               (setq-local minibuffer-keypad-mode-prefix
                           (minibuffer-keypad-mode--convert-prefix external-char))
               (setq external-char t)))
        tmp command prefix-used-p)
    (unless (stringp key)
      (setq key (concat minibuffer-keypad-mode-prefix
                        (when (numberp external-char)
                          (concat (char-to-string external-char) " ")))))

    (message key)
    (catch 'stop
      (when (and (numberp external-char)
                 (commandp (setq command (key-binding (read-kbd-macro (substring key 0 -1))))))
        (throw 'stop nil))
      (while (setq tmp (read-char))
        (if (= tmp 127)
            (setq key (substring key 0 -2))
          (when (= tmp 59)
            (keyboard-quit))
          (setq key (concat key
                            (cond ((and (= tmp 44)
                                        (null prefix-used-p))
                                   (setq prefix-used-p t)
                                   "C-")
                                  ((and (= tmp 46)
                                        (null prefix-used-p))
                                   (setq prefix-used-p t)
                                   "M-")
                                  ((and (= tmp 47)
                                        (null prefix-used-p))
                                   (setq prefix-used-p t)
                                   "C-M-")
                                  ((= tmp 32)
                                   (setq prefix-used-p t)
                                   "")
                                  (t
                                   (when prefix-used-p
                                     (setq prefix-used-p nil))
                                   (concat (char-to-string tmp) " "))))))
        (message key)
        (when (commandp (setq command (key-binding (read-kbd-macro (substring key 0 -1)))))
          (throw 'stop nil))))
    (call-interactively command)))

(defun minibuffer-keypad-mode-start-or-stop ()
  "Start or stop the minibuffer-keypad mode."
  (interactive)
  (if current-input-method
      (if (and (= (char-before) 32)
               (not (= (point) (line-beginning-position))))
          (progn
            (setq-local minibuffer-keypad-mode-on
                        (if minibuffer-keypad-mode-on
                            nil
                          t))
            (call-interactively (key-binding (read-kbd-macro (char-to-string 127)))))
        (self-insert-command 1 32))
    (self-insert-command 1 32)
    (let ((char (read-char)))
      (if (= 32 char)
          (progn
            (setq-local minibuffer-keypad-mode-on
                        (if minibuffer-keypad-mode-on
                            nil
                          t))
            (call-interactively (key-binding (read-kbd-macro (char-to-string 127)))))
        (if (and minibuffer-keypad-mode-on
                 (memq char '(44 46 47)))
            (progn
              (call-interactively (key-binding (read-kbd-macro (char-to-string 127))))
              (minibuffer-keypad-mode-keypad char))
          (minibuffer-keypad-mode-self-insert))))))

(defun minibuffer-keypad-mode-self-insert ()
  "The function to insert the input key or execute the function."
  (interactive)
  (if minibuffer-keypad-mode-on
      (minibuffer-keypad-mode-keypad last-input-event)
    (if (or (symbolp last-input-event)
            (< last-input-event 33)
            (> last-input-event 126))
        (progn
          (minibuffer-keypad-mode -1)
          (let (command)
            (if (commandp (setq command
                                (key-binding
                                 (vector last-input-event))))
                (let ((last-command-event last-input-event))
                  (ignore-errors
                    (call-interactively command)))
              (execute-kbd-macro (vector last-input-event))))
          (minibuffer-keypad-mode t))
      (let ((last-command-event last-input-event))
        (call-interactively #'self-insert-command)))))

(provide 'minibuffer-keypad-mode)

;;; minibuffer-keypad-mode.el ends here
