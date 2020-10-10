;;; inf-iex-send.el --- send code to IEx session     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: tianshu <tianshu@tianshu-manjaro>
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

;;

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'emamux)
(require 'dash)

(require 'inf-iex-util)

(defvar inf-iex-send-target
  'process
  "Can be `process' or `tmux'.")

(defvar inf-iex--shell-output-filter-in-progress nil)

(defun inf-iex--shell-end-of-output? (string)
  "Return non-nil if STRING ends with the prompt."
  (when (string-match inf-iex--comint-prompt-regexp string)
    (match-string 1 string)))

(defun inf-iex--shell-output-filter (string)
  "If STRING ends with input prompt then set filter in progress done."
  (when-let ((prompt (inf-iex--shell-end-of-output? string)))
    (setq inf-iex--shell-output-filter-in-progress nil)
    prompt))

(defun inf-iex--internal-send-string (string)
  "Internal implementation of shell send string functionality."
  (let ((process (inf-iex--get-process))
        (inf-iex--shell-output-filter-in-progress t))
    (comint-send-string process string)
    (while inf-iex--shell-output-filter-in-progress
      (accept-process-output process))))

(defun inf-iex--send-string-no-output (string)
  "Send STRING to hy PROCESS and inhibit printing output."
  (-let [comint-preoutput-filter-functions
         '(inf-iex--shell-output-filter)]
    (ansi-color-apply
     (inf-iex--internal-send-string string))))

(defun inf-iex--send-string (string)
  "Send STRING to hy PROCESS."
  (-let ((comint-output-filter-functions
          '(inf-iex--shell-output-filter)))
    (inf-iex--internal-send-string string)))

(defun inf-iex--send-string-async (string)
  "Send STRING to internal hy process asynchronously."
  (let ((output-buffer " *Comint Inf IEx Redirect Work Buffer*")
        (proc (inf-iex--get-process)))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process string output-buffer proc nil t)

      (set-buffer (process-buffer proc))
      (while (and (null comint-redirect-completed)
                  (accept-process-output proc nil 1000 t)))
      (set-buffer output-buffer)
      (ansi-color-apply (buffer-string)))))

(defun inf-iex-toggle-send-target ()
  (interactive)
  (message "Set inf-iex send target to %s"
           (if (eq 'process inf-iex-send-target)
               (setq inf-iex-send-target 'tmux)
             (setq inf-iex-send-target 'process))))

(defun inf-iex--tmux-send (input)
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
  (ignore-errors
    (cond
     ((eq 'process inf-iex-send-target)
      (comint-send-string (inf-iex--get-process) (format "%s\n" string)))
     ((eq 'tmux inf-iex-send-target)
      (inf-iex--tmux-send string)))))

(provide 'inf-iex-send)
;;; inf-iex-send.el ends here
