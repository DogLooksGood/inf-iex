;;; inf-iex-parser.el --- inf-iex code buffer parser     -*- lexical-binding: t; -*-

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

;; Functions for parsing code buffer.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'dash)

(defun inf-iex--in-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun inf-iex--exp-end-p ()
  "If we are at the end of expression."
  (and (zerop (car (syntax-ppss)))
       (save-mark-and-excursion
         (while (eq (char-before) 32)
           (backward-char 1))
         (not (looking-back "[,:]" 10)))))

(defun inf-iex--bound-of-current-exp ()
  "Return the bounds of current expression."
  (let ((beg (save-mark-and-excursion (back-to-indentation) (point)))
        end)
    (save-mark-and-excursion
      (goto-char (line-end-position))
      (while (not (inf-iex--exp-end-p))
        (forward-line 1)
        (goto-char (line-end-position)))
      (setq end (point)))
    (cons beg end)))

(defun inf-iex--remove-text-properties (text)
  "Remove properties for TEXT."
  (set-text-properties 0 (length text) nil text)
  text)

(defun inf-iex--replace-module-constant (mod code)
  "Replace __MODULE__ with current module name."
  (replace-regexp-in-string "__MODULE__" mod code t t))

(defun inf-iex--parse-alias (mod &optional buf)
  "Extract alias in BUF, consider module name is MOD."
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\_<alias\\_>" nil t)
          (unless (inf-iex--in-comment-p)
            (let* ((bound (inf-iex--bound-of-current-exp))
                   (code (->> (buffer-substring-no-properties (car bound) (cdr bound))
                              (inf-iex--replace-module-constant mod))))
              (push code result))))))
    result))

(defun inf-iex--parse-import (mod &optional buf)
  "Extract imports in BUF, consider module name is MOD."
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\_<import\\_>" nil t)
          (unless (inf-iex--in-comment-p)
            (let* ((bound (inf-iex--bound-of-current-exp))
                   (code (->> (buffer-substring-no-properties (car bound) (cdr bound))
                              (inf-iex--replace-module-constant mod))))
              (push code result))))))
    result))

(defun inf-iex--parse-requires (mod &optional buf)
  "Extract requires in BUF, consider module name is MOD."
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\_<require\\_>" nil t)
          (unless (inf-iex--in-comment-p)
            (let* ((bound (inf-iex--bound-of-current-exp))
                   (code (->> (buffer-substring-no-properties (car bound) (cdr bound))
                              (inf-iex--replace-module-constant mod))))
              (push code result))))))
    result))

(defun inf-iex--format-eval-code (code)
  "Format the CODE for sending to IEx.

Remove # and iex> for those code in comment.
Wrap with parenthese to support multiple lines.
"
  (let* ((lines (split-string code "\n"))
         (lines (mapcar (lambda (s) (string-trim s)) lines)))
    (->> (string-join lines "\n")
         (string-remove-prefix "# ")
         (replace-regexp-in-string "^ *iex> *" "")
         (replace-regexp-in-string "^ *#" "")
         (replace-regexp-in-string "\n#" "\n")
         (format "(%s)"))))

(defun inf-iex--make-setup-code (mod respawn &optional buf)
  "Make the setup code for BUF or current buffer.

Arguments:
MOD - current module name.
RESPAWN - If we should respawn a new session.
"
  (let* ((imports (inf-iex--parse-import mod buf))
         (imports (if mod (cons (format "import %s" mod) imports) imports))
         (aliases (inf-iex--parse-alias mod buf))
         (requires (inf-iex--parse-requires mod buf)))
    (concat (if respawn
                "respawn\n"
              "")
            (format "%s\n%s\n%s"
                    (string-join aliases "\n")
                    (string-join requires "\n")
                    (string-join imports "\n")))))

(provide 'inf-iex-parser)
;;; inf-iex-parser.el ends here
