;;; inf-iex-eval.el --- Eval functionalities for inf-iex  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shi Tianshu

;; Author: Shi Tianshu <doglooksgood@gmail.com
;; Keywords: tools, elixir, iex

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

;; Provide commands for sending code to IEx.

;;; Code:

(require 'dash)
(require 'inf-iex-util)
(require 'inf-iex-send)
(require 'inf-iex-parser)

(defun inf-iex--make-exp-for-time-measure (code)
  "Wrap the string CODE into a :timer.tc call."
  (format ":timer.tc(fn -> %s end) |> elem(0)" code))

(defun inf-iex--make-exp-for-reload (mod)
  "Return the code to reload module MOD."
  (message "r %s" mod))

(defun inf-iex--make-exp-for-compile (file)
  "Return the code to compile FILE."
  (message "c \"%s\"" file))

(defun inf-iex--make-exp-for-inspect (term)
  "Return the code to inpect TERM."
  (format "i %s" term))

(defun inf-iex--make-exp-for-eval-with (binding code)
  "Return the statement that wrap CODE with BINDING."
  (format "with %s do\n%s\nend" binding code))

(defun inf-iex-i ()
  "Inspect current symbol."
  (interactive)
  (let ((thing
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (if-let ((sym (thing-at-point 'symbol)))
               sym
             (error "No symbol at point!")))))
    (inf-iex--send (inf-iex--make-exp-for-inspect thing))))

(defun inf-iex-respawn-context ()
  "Respawn a new context.

Will also do `inf-iex-setup-context' for current module."
  (interactive)
  (if-let ((mod (inf-iex--module-name)))
      (let* ((code (inf-iex--make-setup-code mod t)))
        (inf-iex--send code))
    (message "Can't get module name in this buffer!")))

(defun inf-iex-setup-context ()
  "Execute the import, require and aliases statements in this module."
  (interactive)
  (if-let ((mod (inf-iex--module-name)))
      (let* ((code (inf-iex--make-setup-code mod nil)))
        (inf-iex--send code))
    (message "Can't get module name in this buffer!")))

(defun inf-iex-eval (_arg)
  (interactive "P")
  (-let* ((raw (inf-iex--get-code-to-eval))
          (code-to-eval (inf-iex--format-eval-code raw)))
    (inf-iex--send code-to-eval)))

(defvar inf-iex-eval-with-binding-history nil)

(defun inf-iex-eval-with-binding ()
  "Send code with binding that read from minibuffer prompt."
  (interactive)
  (-let* ((raw (inf-iex--get-code-to-eval))
          (code (inf-iex--format-eval-code raw))
          (binding (read-from-minibuffer "Eval with: " "" nil nil inf-iex-eval-with-binding-history)))
    (inf-iex--send (inf-iex--make-exp-for-eval-with binding code))))

(defun inf-iex-eval-measure-time ()
  "Send code wrapped with a :timer.tc call."
  (interactive)
  (-let* (((beg . end) (if (region-active-p)
                          (car (region-bounds))
                         (cons (line-beginning-position) (line-end-position))))
          (raw (buffer-substring-no-properties beg end))
          (code (inf-iex--format-eval-code raw)))
    (inf-iex--send
     (inf-iex--make-exp-for-time-measure code))))

(defun inf-iex-compile ()
  "Compile this file."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (inf-iex--send
   (inf-iex--make-exp-for-compile (inf-iex--proj-file-name))))

(defun inf-iex-reload ()
  "Reload this file."
  (interactive)
  (let ((inhibit-redisplay t)
        (module-name (inf-iex--module-name)))
    (when (buffer-modified-p) (save-buffer))
    (if (not module-name)
        (message "Can't get module name in this file!")
      (inf-iex--send (inf-iex--make-exp-for-reload module-name)))))

(provide 'inf-iex-eval)
;;; inf-iex-eval.el ends here
