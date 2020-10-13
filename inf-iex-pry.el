;;; inf-iex-pry.el --- Pry support for INF IEx       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  DESKTOP-E16H44U

;; Author: DESKTOP-E16H44U <tianshu@DESKTOP-E16H44U>
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


(require 'inf-iex-util)
(require 'inf-iex-send)
(require 'inf-iex-eval)
(require 'dash)
(require 'subr-x)

(defface inf-iex-pry-face
  '((((class color) (background dark))
     (:box t))
    (((class color) (background light))
     (:box t)))
  "The button face for Pry statement."
  :group 'inf-iex)


(defvar-local inf-iex--pry-overlay
  nil
  "The overlay of pry button.

Currently only one Pry statement per buffer is supported.")

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

(defun inf-iex-goto-pry ()
  (interactive)
  (if inf-iex--pry-overlay
      (goto-char (overlay-start inf-iex--pry-overlay))
    (message "No pry in this file.")))

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

(provide 'inf-iex-pry)
;;; inf-iex-pry.el ends here
