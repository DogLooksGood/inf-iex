;;; inf-iex.el --- Run IEx in Emacs                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: Shi Tianshu
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1") (dash "2.12.0") (cl-lib "0.6.1") (emamux "0.14"))
;; Version: 0.0.1

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
;; Enable `inf-iex-minor-mode' in `elixir-mode'
;;
;; (add-hook 'elixir-mode-hook 'inf-iex-minor-mode)
;;
;; Available Shortcuts:
;;
;; C-c C-v           - Switch between sending target, tmux or comint buffer.
;; C-c C-r           - Send current region to target.
;; C-c C-l           - Send current line to target.
;; C-c C-k           - Reload current module.
;; C-c C-c k         - Compile current file.
;; C-c M-p p         - Add Pry button to above line.
;; C-c M-p k         - Remove Pry button in this file.
;; C-c M-p l         - Goto Pry button.
;; C-c C-z           - Start IEx comint buffer.

;;; Code:

(require 'project)
(require 'comint)
(require 'emamux)
(require 'dash)
(require 'cl-lib)

(require 'inf-iex-eval)
(require 'inf-iex-parser)

(defvar inf-iex-minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-v") 'inf-iex-toggle-send-target)
    (define-key keymap (kbd "C-c C-r") 'inf-iex-eval)
    (define-key keymap (kbd "C-c C-l") 'inf-iex-eval-line)
    (define-key keymap (kbd "C-c C-k") 'inf-iex-reload)
    (define-key keymap (kbd "C-c C-c C-k") 'inf-iex-compile)
    (define-key keymap (kbd "C-c C-z") 'inf-iex-start)
    (define-key keymap (kbd "C-c M-p p") 'inf-iex-set-pry)
    (define-key keymap (kbd "C-c M-p k") 'inf-iex-unset-pry)
    (define-key keymap (kbd "C-c M-p l") 'inf-iex-goto-pry)
    (define-key keymap (kbd "C-c C-i") 'inf-iex-i)
    (define-key keymap (kbd "C-c M-t") 'inf-iex-measure-time)
    keymap)
  "Keymap for interaction with IEx buffer.")

(defvar inf-iex-send-target
  'process
  "Can be `process' or `tmux'.")

(defvar-local inf-iex--pry-overlay
  nil
  "The overlay of pry button.")

(defface inf-iex-pry-face
  '((((class color) (background dark))
     (:box t))
    (((class color) (background light))
     (:box t)))
  ""
  :group 'inf-iex)

(defvar inf-iex-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; (define-key keymap (kbd "TAB") 'completion-at-point)
    keymap)
  "Keymap for IEx buffer.")

(define-minor-mode inf-iex-minor-mode
  "Minor mode for Interaction with IEx."
  nil
  "inf-IEx"
  inf-iex-minor-mode-map)

(define-derived-mode inf-iex-mode comint-mode "inf-IEx"
  "Major mode for IEx session buffer."
  nil
  "IEx"
  inf-iex-mode-map)

(defun inf-iex--proj-file-name ()
  "Return relative file name of current buffer in current project.

Will only work when we are in a project."
  (when (and (buffer-file-name (current-buffer))
             (project-current))
    (file-relative-name
     (buffer-file-name (current-buffer))
     (cdr (project-current)))))

(defun inf-iex-i ()
  (interactive)
  (let ((thing
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (if-let ((sym (thing-at-point 'symbol)))
               sym
             (error "No symbol at point!")))))
    (inf-iex--send (format "i %s" thing))))

(defun inf-iex-unset-pry ()
  (interactive)
  (goto-char (overlay-start inf-iex--pry-overlay))
  (delete-overlay inf-iex--pry-overlay)
  (delete-region (line-beginning-position) (line-end-position))
  (join-line)
  (setq inf-iex--pry-overlay nil)
  (inf-iex-reload))

(defun inf-iex--click-pry-button (ignored)
  (inf-iex-unset-pry))

(defun inf-iex-goto-pry ()
  (interactive)
  (if inf-iex--pry-overlay
      (goto-char (overlay-start inf-iex--pry-overlay))
    (message "No pry in this file.")))

(defun inf-iex-set-pry ()
  (interactive)
  (when inf-iex--pry-overlay
    (save-mark-and-excursion
      (goto-char (overlay-start inf-iex--pry-overlay))
      (delete-overlay inf-iex--pry-overlay)
      (delete-region (line-beginning-position) (line-end-position))
      (join-line)))
  (let (end beg)
    (goto-char (line-beginning-position))

    (indent-for-tab-command)
    (insert-button "require IEx;IEx.pry"
                   'action #'inf-iex--click-pry-button)
    (setq end (point)
          beg (- end (length "require IEx;IEx.pry")))
    (insert "\n")
    (indent-for-tab-command)
    (setq inf-iex--pry-overlay
          (make-overlay beg end))
    (overlay-put inf-iex--pry-overlay 'face 'inf-iex-pry-face)
    (inf-iex-reload)))

(defun inf-iex--measure-time (raw)
  (interactive)
  (format ":timer.tc(fn -> %s end) |> elem(0)" raw))

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

(defun inf-iex--format-eval (s)
  (if-let ((module-name (inf-iex--relative-module-name)))
      (inf-iex--parse-eval-code module-name s)
    (format "(%s)" s)))

(defun inf-iex-eval (arg)
  (interactive "P")
  (let* ((raw (->> (buffer-substring-no-properties (region-beginning) (region-end))
                   (string-remove-prefix "# ")))
         (code (if arg (inf-iex--measure-time raw) raw))
         (code-to-eval (->> code
                            (replace-regexp-in-string "^ *#" "")
                            (replace-regexp-in-string "\n#" "\n")
                            (replace-regexp-in-string " +" " ")
                            (inf-iex--format-eval))))
    (message "Eval: %s" code-to-eval)
    (inf-iex--send code-to-eval)))

(defun inf-iex-eval-line (arg)
  (interactive "P")
  (let* ((raw (->> (buffer-substring-no-properties (save-mark-and-excursion
                                                     (back-to-indentation)
                                                     (point))
                                                   (line-end-position))
                   (string-remove-prefix "# ")))
         (code (if arg (inf-iex--measure-time raw) raw))
         (code-to-eval (replace-regexp-in-string
                        " +"
                        " "
                        (inf-iex--format-eval code))))
    (message "Eval: %s" code-to-eval)
    (inf-iex--send code-to-eval)))

(defun inf-iex-compile ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (inf-iex--send (message "c \"%s\"\n" (inf-iex--proj-file-name))))

(defun inf-iex--relative-module-name ()
  (save-mark-and-excursion
    (re-search-backward
     "defmodule \\([[:graph:]]+\\)"
     nil t 1)
    (match-string 1)))

(defun inf-iex--module-name ()
  (save-mark-and-excursion
    (goto-char (point-min))
    (re-search-forward
     "defmodule \\([[:graph:]]+\\)")
    (match-string 1)))

(defun inf-iex-reload ()
  (interactive)
  (let ((inhibit-redisplay t)
        (module-name (inf-iex--module-name)))
    (when (buffer-modified-p) (save-buffer))
    (if (not module-name)
        (message "Can't get module name in this file!")
      (inf-iex--send (message "r %s\n" module-name)))))

(defun inf-iex--make-iex-buffer-name ()
  (format "IEx[%s]" (cdr (project-current))))

(defun inf-iex--get-process ()
  (get-process (inf-iex--make-iex-buffer-name)))

(defun inf-iex-start ()
  (interactive)
  (let ((inf-iex-buffer (inf-iex--make-iex-buffer-name)))
    (if (and inf-iex-buffer (comint-check-proc inf-iex-buffer))
        (pop-to-buffer inf-iex-buffer)
      (let* ((proj-root (cdr (project-current)))
             (name (inf-iex--make-iex-buffer-name))
             (cmd (split-string (read-from-minibuffer "Command to start IEx session: " "iex -S mix")))
             (exe (car cmd))
             (args (cdr cmd))
             (comint-buffer
              (let ((default-directory proj-root))
                (apply #'make-comint-in-buffer name (generate-new-buffer name) exe nil args))))
        (set-buffer comint-buffer)
        (inf-iex-mode)
        (pop-to-buffer (current-buffer))))))

(provide 'inf-iex)
;;; inf-iex.el ends here
