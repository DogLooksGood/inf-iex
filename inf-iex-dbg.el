;;; inf-iex-pry.el --- DBG support for INF IEx       -*- lexical-binding: t; -*-

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
(require 'inf-iex-parser)

(defvar-local inf-iex--dbg-current-project nil
  "Current project path, used in dbg list buffer.")

(defvar-local inf-iex--dbg-tp-list nil
  "TP list, used in dbg list buffer.")

(defun inf-iex--make-dbg-list-buffer-name (proj)
  "Buffer name to display dbg list."
  (format "*IEx DBG List[%s]*" proj))

(defvar inf-iex--dbg-output-buffer-name
  "*IEx DBG Output*")

(defun inf-iex--make-dbg-output-buffer ()
  (get-buffer-create
   (inf-iex--make-dbg-output-buffer-name)))

(defun inf-iex--make-dbg-output-buffer-name ()
  (format "*IEx DBG Output[%s]*" (inf-iex--project-root)))

(defun inf-iex-dbg-list ()
  (interactive)
  (let* ((proj (inf-iex--project-root))
         (buf (get-buffer (inf-iex--make-dbg-list-buffer-name proj))))
    (if buf
        (pop-to-buffer buf t)
      (let ((buf (get-buffer-create (inf-iex--make-dbg-list-buffer-name proj))))
        (with-current-buffer buf
          (inf-iex-dbg-list-mode)
          (setq-local inf-iex--dbg-current-project proj))
        (pop-to-buffer buf t)))))

(defun inf-iex--dbg-list-redisplay ()
  (let* ((buf (get-buffer-create
               (inf-iex--make-dbg-list-buffer-name inf-iex--dbg-current-project)))
         (pos (point)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (erase-buffer)
        (cl-loop for it in inf-iex--dbg-tp-list do
                 (insert
                  (propertize
                   (format "[%s] %s.%s\n"
                           (if (cadr it) "X" " ")
                           (caar it)
                           (cdar it))
                   'item it)))
        (goto-char (min pos (point-max)))))))

(defun inf-iex-dbg-add-tp ()
  "Get the full name of current function, add it to the dbg list."
  (interactive)
  (let* ((proj (inf-iex--project-root))
         (mod (inf-iex--relative-module-name))
         (fun (save-mark-and-excursion
                (goto-char (car (bounds-of-thing-at-point 'defun)))
                (when (re-search-forward "defp? \\([^( ]+\\)")
                  (inf-iex--remove-text-properties (match-string 1)))))
         (buf (get-buffer-create (inf-iex--make-dbg-list-buffer-name proj))))
    (message "Add %s.%s to dbg list." mod fun)
    (with-current-buffer buf
      (add-to-list 'inf-iex--dbg-tp-list
                   (list (cons mod fun) t))
      (inf-iex--dbg-list-redisplay))))

(defun inf-iex-dbg-kill-all-output-buffer ()
  "Kill all dbg output buffers."
  (interactive)
  (let ((buf-name (inf-iex--make-dbg-output-buffer-name)))
    (cl-loop for buf in (buffer-list) do
             (when (string-prefix-p buf-name (buffer-name buf))
               (kill-buffer buf)))))

(defun inf-iex-dbg-remove-tp ()
  "Remove function from dbg list."
  (interactive)
  (let ((item (get-text-property (point) 'item)))
    (setq inf-iex--dbg-tp-list (delete item inf-iex--dbg-tp-list))
    (inf-iex--dbg-list-redisplay)))

(defun inf-iex-dbg-toggle-tp ()
  "Toggle function in dbg list."
  (interactive)
  (let ((item (get-text-property (point) 'item)))
    (setq inf-iex--dbg-tp-list
          (--replace-where (equal item it)
                           (-update-at 1 #'not item)
                           inf-iex--dbg-tp-list))
    (inf-iex--dbg-list-redisplay)))

(defvar inf-iex-dbg-list-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") 'forward-line)
    (define-key keymap (kbd "p") 'previous-line)
    (define-key keymap (kbd "k") 'inf-iex-dbg-remove-tp)
    (define-key keymap (kbd "t") 'inf-iex-dbg-toggle-tp)
    keymap)
  "Keymap for `inf-iex-dbg-list-mode'.")

(defun inf-iex--dbg-list-mode-init ()
  (setq buffer-read-only t))

(defvar inf-iex-dbg-list-mode-hook nil)

(define-derived-mode inf-iex-dbg-list-mode
  special-mode " DBG List"
  "Major mode for display DBG list in inf-iex."
  (inf-iex--dbg-list-mode-init))

(defun inf-iex--item-to-tp-exp (item)
  (-let ((((mod . fun) enable) item))
    (when enable
      (format ":dbg.tpl({%s, :%s, :_}, :x)"
              mod fun))))

(defvar inf-iex--format-indent-level nil)

(defun inf-iex--format-dbg-line (ln)
  (string-match "^\\(:call\\|:return_from\\){\\([^,]+\\), :\\([^,]+\\), \\([^}]+\\)}\\(.+\\)?$"
                ln)
  (let ((mod (match-string 2 ln))
        (fun (match-string 3 ln)))
    (if (string-equal ":call" (match-string 1 ln))
        ;; call
        (let* ((args-list (match-string 4 ln))
               (args (substring args-list 1 (1- (length args-list))))
               (unhandled (or (match-string 5 ln) ""))
               (s (format "#%s Call:\n%s.%s(%s%s)"
                          (->> (-cycle '(" ▶"))
                               (-take inf-iex--format-indent-level)
                               (string-join))
                          mod fun args unhandled)))
          (cl-incf inf-iex--format-indent-level)
          s)
      ;; return_from
      (let* ((arity (match-string 4 ln))
             (ret (match-string 5 ln))
             (s (format "#%s %s.%s/%s:\n%s"
                        (->> (-cycle '(" ◀"))
                             (-take (cl-decf inf-iex--format-indent-level))
                             (string-join))
                        mod fun arity ret)))
        s))))

(defun inf-iex--dbg-create-output-buffer (output-string)
  (-let* ((output-string (replace-regexp-in-string "\\.\\.\\..+?> *" "" output-string))
          (lines (split-string output-string "\n"))
          (grouped-lines (--group-by (string-prefix-p "__DBG_OUTPUT__" it) lines))
          (dbg-lines (alist-get t grouped-lines))
          (io-lines (alist-get nil grouped-lines))
          (buf (inf-iex--make-dbg-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq inf-iex--format-indent-level 1)
      (cl-loop for ln in dbg-lines do
               (let* ((ln (substring ln 14)))
                 (insert (inf-iex--format-dbg-line ln) "\n")))
      (insert "\n# OUTPUT:\n")
      (cl-loop for ln in io-lines do
               (unless (string-empty-p ln)
                 (insert ln "\n")))
      (elixir-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buf t)))

(defun inf-iex--eval-with-dbg (code dbg-list-buf)
  (with-current-buffer dbg-list-buf
    (let* ((tp-exps (-> (--keep (inf-iex--item-to-tp-exp it)
                                inf-iex--dbg-tp-list)
                        (string-join ";")))
           (_ (when (string-blank-p tp-exps)
                (error "DBG List is empty!")))
           (exp (format "try do :dbg.start();:dbg.tracer(:process, {fn msg, _ ->
IO.puts(case msg do
 {_, _, :call, fma} -> \"\\n__DBG_OUTPUT__:call#{inspect fma, charlists: :as_lists}\"
 {_, _, :return_from, fma, ret} -> \"\\n__DBG_OUTPUT__:return_from#{inspect fma}#{inspect ret, limit: :infinity, charlists: :as_lists}\"
 end)
end, 0});%s;:dbg.p(:all, :c);%s after :dbg.stop_clear() end"
                        tp-exps
                        code)))
      (inf-iex--dbg-create-output-buffer
       (inf-iex--send-string-async exp)))))

(defun inf-iex-dbg-eval ()
  (interactive)
  (unless (equal inf-iex-send-target 'process)
    (error "DBG eval is only supported for send target `process'."))
  (let* ((raw (inf-iex--get-code-to-eval))
         (code (inf-iex--format-eval-code raw))
         (proj (inf-iex--project-root))
         (buf (inf-iex--make-dbg-list-buffer-name proj)))
    (inf-iex--eval-with-dbg code buf)))

(defun inf-iex-dbg-eval-with-binding ()
  "Send code with binding that read from minibuffer prompt."
  (interactive)
  (unless (equal inf-iex-send-target 'process)
    (error "DBG eval is only supported for send target `process'."))
  (-let* ((raw (inf-iex--get-code-to-eval))
          (code (inf-iex--format-eval-code raw))
          (binding (read-from-minibuffer "Eval with: " ""))
          (proj (inf-iex--project-root))
          (buf (inf-iex--make-dbg-list-buffer-name proj)))
    (inf-iex--eval-with-dbg (inf-iex--make-exp-for-eval-with binding code) buf)))


(provide 'inf-iex-dbg)
