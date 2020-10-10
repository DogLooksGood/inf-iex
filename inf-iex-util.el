;;; inf-iex-util.el --- Utilities for inf-iex        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: tianshu <tianshu@tianshu-manjaro>
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

;;

;;; Code:

(require 'project)

(defvar inf-iex--comint-prompt-regexp "\\(iex(.+)[0-9]*>\\)")

(defun inf-iex--make-iex-buffer-name ()
  (format "IEx[%s]" (cdr (project-current))))

(defun inf-iex--get-process ()
  (get-process (inf-iex--make-iex-buffer-name)))

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

(defun inf-iex--proj-file-name ()
  "Return relative file name of current buffer in current project.

Will only work when we are in a project."
  (when (and (buffer-file-name (current-buffer))
             (project-current))
    (file-relative-name
     (buffer-file-name (current-buffer))
     (cdr (project-current)))))

(defun inf-iex--create-button (text face action)
  (let ((b (insert-button text 'action action)))
    (-> (make-overlay (button-start b) (button-end b))
        (overlay-put 'face face))))

(provide 'inf-iex-util)
;;; inf-iex-util.el ends here
