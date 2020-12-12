;;; inf-iex-observer.el --- Observer in Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shi Tianshu

;; Author: Shi Tianshu <doglooksgood@gmail.com>
;; Keywords: tools, elixir

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

;; Process states inspector.

;;; Code:

(require 'elixir-mode)
(require 'inf-iex-send)
(require 'dash)

(declare-function inf-iex-minor-mode "inf-iex")

(defface inf-iex--back-button
  '((((class color) (background dark))
     (:box t :inherit font-lock-comment-face))
    (((class color) (background light))
     (:box t :inherit font-lock-comment-face)))
  "The button face for back to iex in inspector."
  :group 'inf-iex)

(defface inf-iex--define-var-button
  '((((class color) (background dark))
     (:box t :inherit font-lock-constant-face))
    (((class color) (background light))
     (:box t :inherit font-lock-constant-face)))
  "The button face for define var in inspector."
  :group 'inf-iex)

(defface inf-iex--refresh-button
  '((((class color) (background dark))
     (:box t :inherit font-lock-string-face))
    (((class color) (background light))
     (:box t :inherit font-lock-string-face)))
  "The button face for refresh in inspector."
  :group 'inf-iex)

(defface inf-iex--kill-button
  '((((class color) (background dark))
     (:box t :inherit font-lock-warning-face))
    (((class color) (background light))
     (:box t :inherit font-lock-warning-face)))
  "The button face for kill process in inspector."
  :group 'inf-iex)

(defvar inf-iex--inspector-buffer-name "*INF IEx Value Inspector*"
  "Buffer name for inspector view.")

(defvar inf-iex--state-variable-name "state"
  "The default variable name when you define state as a variable.")

(defvar inf-iex-inspect-font-lock-limit 10000
  "The limit buffer size for applying font-lock on inspect buffer.")

(defvar inf-iex--common-query
  '("Process Information" .  "Process.list|>Stream.map(&({Keyword.get(Process.info(&1), :registered_name), &1}))|>Enum.filter(&elem(&1, 0))")
  "Default query for get pid list with Process.list.")

(defvar inf-iex--swarm-query
  '("Swarm" . "Swarm.registered()")
  "Query for get pid list with Swarm.")

(defun inf-iex--trim-find-result (s)
  "Trim useless output from iex output S."
  (string-trim-right s ":ok[\n ]+\\(?:nil[\n ]+\\)?\\(?:iex.+>[\n ]+\\)?"))

(defun inf-iex--make-exp-for-query-process (query-exp)
  "Make statement for query with QUERY-EXP."
  (concat query-exp "|>Enum.each(fn {n, pid} -> IO.puts \"#{inspect n}\t#{inspect pid}\" end)"))

(defun inf-iex--make-exp-for-print-state (pid-str)
  "Make statement for print state with PID-STR."
  (format "IO.puts inspect(:sys.get_state(:erlang.list_to_pid('%s')), pretty: true, limit: :infinity)\n" pid-str))

(defun inf-iex--make-exp-for-kill-process (pid-str)
  "Make statement for kill process with PID-STR."
  (format "Process.exit(:erlang.list_to_pid('%s'), :kill); \"#PID%s is killed!\"" pid-str pid-str))

(defun inf-iex--make-exp-for-define-state-var (pid-str)
  "Make statement for define state variable with PID-STR."
  (format "%s = :sys.get_state(:erlang.list_to_pid('%s')); \"The state of #PID%s is defined as variable `%s`.\""
          inf-iex--state-variable-name pid-str pid-str inf-iex--state-variable-name))

(defun inf-iex--query-state-execute (opt query)
  "Execute state query.

Arguments:
OPT - The user's selection from process list, considered as the name of process.
QUERY - A list like `inf-iex--common-query'."
  (let ((iex-buf (inf-iex--make-iex-buffer-name)))
    (if (not iex-buf)
        (message "IEx session not available for this project!")
      (let* ((items (inf-iex--query-items query))
             (pid-str (-some-> (--first (string-equal opt (car it)) items)
                        (cadr)
                        (string-trim-left "#PID")))
             (_ (unless pid-str (error "Process is not exist or terminated!")))
             (state (inf-iex--send-string-async
                     (format "%s" (inf-iex--make-exp-for-print-state pid-str)))))
        (with-current-buffer (get-buffer-create inf-iex--inspector-buffer-name)
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (format "# Query via %s\n" (car query)))
          (insert (format "# The state of %s\n" opt))
          (insert (format "# #PID%s\n" pid-str))
          (insert "\n")
          (inf-iex--create-button "[Define Variable]" 'inf-iex--define-var-button
                                  (lambda (_ignored)
                                    (inf-iex--send
                                     (inf-iex--make-exp-for-define-state-var pid-str))))
          (insert " ")
          (inf-iex--create-button "[Refresh Inspector]" 'inf-iex--refresh-button
                                  (lambda (_ignored)
                                    (let ((pos (point)))
                                      (inf-iex--query-state-execute opt query)
                                      (ignore-errors (goto-char pos)))))
          (insert " ")
          (inf-iex--create-button "[Diff Refresh]" 'inf-iex--refresh-button
                                  (lambda (_ignored)
                                    (diff-buffers
                                     (get-buffer-create "*inf iex diff-old*")
                                     (get-buffer-create "*inf iex diff-new*"))))
          (insert " ")
          (inf-iex--create-button "[Kill Process]" 'inf-iex--kill-button
                                  (lambda (_ignored)
                                    (message "Kill process #PID%s" pid-str)
                                    (inf-iex--send
                                     (inf-iex--make-exp-for-kill-process pid-str))))
          (insert "\n\n")
          (inf-iex--create-button "[Back To IEx]" 'inf-iex--back-button
                                  (lambda (_ignored)
                                    (switch-to-buffer iex-buf)))
          (insert " ")
          (inf-iex--create-button "[Kill Buffer]" 'inf-iex--back-button
                                  (lambda (_ignored)
                                    (kill-buffer inf-iex--inspector-buffer-name)))
          (insert " ")
          (insert "\n\n")
          (let ((result (inf-iex--trim-find-result state)))
            (with-current-buffer (get-buffer-create "*inf iex diff-old*")
              (erase-buffer)
              (insert
               (with-current-buffer (get-buffer-create "*inf iex diff-new*")
                 (buffer-substring (point-min) (point-max)))))
            (insert result)
            (with-current-buffer (get-buffer-create "*inf iex diff-new*")
              (erase-buffer)
              (insert result))
            (goto-char (point-min))
            (setq buffer-read-only t))
          (if (or (not (featurep 'elixir-mode))
                  (> (point-max) inf-iex-inspect-font-lock-limit))
              (text-mode)
            (elixir-mode))
          (inf-iex-minor-mode))
        (unless (equal (buffer-name) inf-iex--inspector-buffer-name)
          (pop-to-buffer iex-buf)
          (switch-to-buffer inf-iex--inspector-buffer-name))))))

(defun inf-iex--query-items (query)
  "List items from QUERY."
  (let* ((resp (-> (inf-iex--make-exp-for-query-process (cdr query))
                   (inf-iex--send-string-async )
                   (inf-iex--trim-find-result)))
         (lines (split-string resp "\n")))
    (->> (mapcar (lambda (line)
                   (split-string line "\t"))
                 lines))))

(defun inf-iex--pick-item (items)
  "Ask user to pick an item from ITEMS."
  (let ((names (mapcar #'car items)))
    (completing-read "Query:" names nil t)))

(defun inf-iex-query-state-common (&optional query)
  "Query state with QUERY or `inf-iex--common-query'."
  (interactive)
  (if (eq inf-iex-send-target 'tmux)
      (message "Query state is not available for tmux target!")
    (let* ((query (or query inf-iex--common-query))
           (items (inf-iex--query-items query))
           (opt (inf-iex--pick-item items)))
      (inf-iex--query-state-execute opt query))))

(defun inf-iex-query-state-swarm ()
  "Query state from Swarm registry."
  (interactive)
  (inf-iex-query-state-common inf-iex--swarm-query))

(provide 'inf-iex-observer)
;;; inf-iex-observer.el ends here
