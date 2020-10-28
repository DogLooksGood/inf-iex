;;; inf-iex-send.el --- send code to IEx session     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shi Tianshu

;; Author: Shi Tianshu <doglooksgood@gmail.com
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide functions to send code to IEx.

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'emamux)
(require 'dash)

(require 'inf-iex-util)

(defvar inf-iex-send-target
  'process
  "Can be `process' or `tmux'.")

(defun inf-iex--send-string-async (string &optional prompt)
  "Send STRING to internal hy process asynchronously."
  (let ((output-buffer " *Comint Inf IEx Redirect Work Buffer*")
        (proc (inf-iex--get-process)))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process string output-buffer proc t t)

      (set-buffer (process-buffer proc))
      (while (and (null comint-redirect-completed)
                  (accept-process-output proc nil 10000000 t)))
      (set-buffer output-buffer)
      (ansi-color-apply (buffer-string)))))

(defun inf-iex-toggle-send-target ()
  "Toggle sending target, between `process' and `tmux'."
  (interactive)
  (message "Set inf-iex send target to %s"
           (if (eq 'process inf-iex-send-target)
               (setq inf-iex-send-target 'tmux)
             (setq inf-iex-send-target 'process))))

(defun inf-iex--tmux-send (input)
  "Send code to iex in tmux."
  (interactive)
  (emamux:check-tmux-running)
  (condition-case nil
      (progn
        (if (or current-prefix-arg (not (emamux:set-parameters-p)))
            (emamux:set-parameters))
        (let ((target (emamux:target-session)))
          (setq emamux:last-command input)
          (emamux:reset-prompt target)
          (emamux:send-keys input)))
    (quit (emamux:unset-parameters))))

(defun inf-iex--send (string)
  "Send code to iex in process."
  (ignore-errors
    (cond
     ((eq 'process inf-iex-send-target)
      (comint-send-string (inf-iex--get-process) (format "%s\n" string)))
     ((eq 'tmux inf-iex-send-target)
      (inf-iex--tmux-send string)))))

(provide 'inf-iex-send)
;;; inf-iex-send.el ends here
