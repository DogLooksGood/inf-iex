;;; inf-iex-observer.el --- Observer in Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: tianshu <tianshu@tianshu-manjaro>
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

;;; Code:

(require 'inf-iex-send)

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
  "The button face for define var in inspector."
  :group 'inf-iex)

(defface inf-iex--kill-button
  '((((class color) (background dark))
     (:box t :inherit font-lock-warning-face))
    (((class color) (background light))
     (:box t :inherit font-lock-warning-face)))
  "The button face for kill process in inspector."
  :group 'inf-iex)

(defvar inf-iex--state-variable-name "state"
  "The default variable name when you define state as a variable.")

(defvar inf-iex-inspect-font-lock-limit 10000
  "The limit buffer size for applying font-lock on inspect buffer.")

(defvar inf-iex--common-query
  '("Process Information" .  "Process.list|>Stream.map(&({Keyword.get(Process.info(&1), :registered_name), &1}))|>Enum.filter(&elem(&1, 0))"))

(defvar inf-iex--swarm-query
  '("Swarm" . "Swarm.registered()"))

(defun inf-iex--trim-find-result (s)
  (string-trim-right s ":ok[\n ]+\\(?:nil[\n ]+\\)?\\(?:iex.+>[\n ]+\\)?"))

(defun inf-iex--make-exp-for-query-process (query-exp)
  (concat query-exp "|>Enum.each(fn {n, pid} -> IO.puts \"#{inspect n}\t#{inspect pid}\" end)"))

(defun inf-iex--make-exp-for-print-state (pid-str)
  (format "IO.puts inspect(:sys.get_state(:erlang.list_to_pid('%s')), pretty: true, limit: :infinity)\n" pid-str))

(defun inf-iex--make-exp-for-kill-process (pid-str)
  (format "Process.exit(:erlang.list_to_pid('%s'), :kill); \"#PID%s is killed!\"" pid-str pid-str))

(defun inf-iex--make-exp-for-define-state-var (pid-str)
  (format "%s = :sys.get_state(:erlang.list_to_pid('%s')); \"The state of #PID%s is defined as variable `state`.\""
          inf-iex--state-variable-name pid-str pid-str))

(defun inf-iex--query-state-execute (opt query)
  (let* ((items (inf-iex--query-items query))
         (pid-str (-> (--first (string-equal opt (car it)) items)
                      (cadr)
                      (string-trim-left "#PID")))
         (state (inf-iex--send-string-async
                 (message "%s" (inf-iex--make-exp-for-print-state pid-str)))))
    (with-current-buffer (get-buffer-create "*INF IEx Value Inspector*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "# Query via %s\n" (car query)))
      (insert (format "# Get state from %s\n" opt))
      (insert (format "# #PID%s\n" pid-str))
      (insert "\n")
      (inf-iex--create-button "[Define Variable]" 'inf-iex--define-var-button
                              (lambda (_ignored)
                                (inf-iex--send
                                 (inf-iex--make-exp-for-define-state-var pid-str))))
      (insert "\n")
      (inf-iex--create-button "[Refresh Inspector]" 'inf-iex--refresh-button
                              (lambda (_ignored)
                                (inf-iex--query-state-execute opt query)))
      (insert "\n")
      (inf-iex--create-button "[Kill Process]" 'inf-iex--kill-button
                              (lambda (_ignored)
                                (inf-iex--send
                                 (inf-iex--make-exp-for-kill-process pid-str))))
      (insert "\n\n")
      (insert (inf-iex--trim-find-result state))
      (if (> (point-max) inf-iex-inspect-font-lock-limit)
          (text-mode)
        (elixir-mode))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer)))))

(defun inf-iex--query-items (query)
  (let* ((resp (-> (inf-iex--make-exp-for-query-process (cdr query))
                   (inf-iex--send-string-async )
                   (inf-iex--trim-find-result)))
         (lines (split-string resp "\n")))
    (->> (mapcar (lambda (line)
                   (split-string line "\t"))
                 lines))))

(defun inf-iex--pick-item (items)
  (let ((names (mapcar #'car items)))
    (completing-read "Query:" names nil t)))

(defun inf-iex-query-state-common (&optional query)
  (interactive)
  (if (eq inf-iex-send-target 'tmux)
      (message "Query state is not available for tmux target!")
    (let* ((query (or query inf-iex--common-query))
           (items (inf-iex--query-items query))
           (opt (inf-iex--pick-item items)))
      (inf-iex--query-state-execute opt query))))

(defun inf-iex-query-state-swarm ()
  (interactive)
  (inf-iex-query-state-common inf-iex--swarm-query))

(provide 'inf-iex-observer)
;;; inf-iex-observer.el ends here
