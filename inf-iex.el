;;; inf-iex.el --- Run IEx in Emacs                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shi Tianshu

;; Author: Shi Tianshu <doglooksgood@gmail.com>
;; Keywords: tools, elixir
;; Package-Requires: ((emacs "27.0") (dash "2.12.0") (cl-lib "0.6.1") (emamux "0.14"))
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
;; How to use:
;;
;; Enable `inf-iex-minor-mode' in `elixir-mode'
;;
;; (add-hook 'elixir-mode-hook 'inf-iex-minor-mode)
;;
;; Available Shortcuts:
;;
;; |--------------+--------------------------------------------------------|
;; | Key Binding  | Functionality                                          |
;; |--------------+--------------------------------------------------------|
;; | C-c C-z      | Start IEx comint buffer.                               |
;; | C-c C-v      | Switch between sending target, tmux or comint buffer.  |
;; | C-c C-c      | Send region or current line to target.                 |
;; | C-c C-n      | Eval and wrap code with :timer.tc, return nanoseconds. |
;; | C-c C-w      | Eval with bindings those read from minibuffer.         |
;; | C-c C-k      | Reload current module.                                 |
;; | C-c C-l      | Compile current file.                                  |
;; | C-c M-p p    | Add Pry section to above line.                         |
;; | C-c M-p k    | Remove Pry section in this file.                       |
;; | C-c M-p l    | Goto Pry section.                                      |
;; | C-c C-s c    | Query process state from process list.                 |
;; | C-c C-s s    | Query process state from Swarm registry.               |
;; | C-c C-i      | Send i {thing-at-point} to IEx.                        |
;; | C-c M-c      | Import this module and its imports & requires.         |
;; | C-c M-r      | Like ~C-c M-c~, but ~respawn~.                         |


;;; Code:

(require 'project)
(require 'comint)
(require 'emamux)
(require 'dash)
(require 'cl-lib)

(require 'inf-iex-eval)
(require 'inf-iex-parser)
(require 'inf-iex-util)
(require 'inf-iex-send)
(require 'inf-iex-observer)
(require 'inf-iex-pry)

(defvar inf-iex-minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-v") 'inf-iex-toggle-send-target)
    (define-key keymap (kbd "C-c C-c") 'inf-iex-eval)
    (define-key keymap (kbd "C-c C-w") 'inf-iex-eval-with-binding)
    (define-key keymap (kbd "C-c C-n") 'inf-iex-eval-measure-time)
    (define-key keymap (kbd "C-c C-s s") 'inf-iex-query-state-swarm)
    (define-key keymap (kbd "C-c C-s c") 'inf-iex-query-state-common)
    (define-key keymap (kbd "C-c C-k") 'inf-iex-reload)
    (define-key keymap (kbd "C-c C-l") 'inf-iex-compile)
    (define-key keymap (kbd "C-c C-z") 'inf-iex-start)
    (define-key keymap (kbd "C-c M-p p") 'inf-iex-set-pry)
    (define-key keymap (kbd "C-c M-p k") 'inf-iex-unset-pry)
    (define-key keymap (kbd "C-c M-p l") 'inf-iex-goto-pry)
    (define-key keymap (kbd "C-c C-i") 'inf-iex-i)
    (define-key keymap (kbd "C-c M-c") 'inf-iex-setup-context)
    (define-key keymap (kbd "C-c M-r") 'inf-iex-respawn-context)
    keymap)
  "Keymap for interaction with IEx buffer.")

(defvar inf-iex-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-s s") 'inf-iex-query-state-swarm)
    (define-key keymap (kbd "C-c C-s c") 'inf-iex-query-state-common)
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
  inf-iex-mode-map
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp inf-iex--comint-prompt-regexp))

(defun inf-iex-start ()
  "Start IEx session in Emacs."
  (interactive)
  (let ((inf-iex-buffer (inf-iex--make-iex-buffer-name)))
    (if (and inf-iex-buffer (comint-check-proc inf-iex-buffer))
        (pop-to-buffer inf-iex-buffer)
      (let* ((proj-root (cdr (project-current)))
             (name (inf-iex--make-iex-buffer-name))
             (cmd (split-string (read-from-minibuffer "Command to start IEx session: " "iex -S mix")))
             (env (read-from-minibuffer "Environment: " "dev"))
             (exe (car cmd))
             (args (cdr cmd))
             (comint-buffer
              (let ((default-directory proj-root)
                    (process-environment (nconc (list (format "MIX_ENV=%s" env)) process-environment)))
                (apply #'make-comint-in-buffer name (generate-new-buffer name) exe nil args))))
        (set-buffer comint-buffer)
        (inf-iex-mode)
        (pop-to-buffer (current-buffer))))))

(provide 'inf-iex)
;;; inf-iex.el ends here
