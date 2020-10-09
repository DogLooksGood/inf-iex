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

(defvar inf-iex--common-query "")

(defvar inf-iex--swarm-query "Swarm.registered() |> Enum.each(fn {n, pid} -> IO.puts \"#{inspect n}\t#{inspect pid}\" end)")

(defun inf-iex--trim-find-result (s)
  (string-trim-right s ":ok[\n ]+\\(?:nil[\n ]+\\)?\\(?:iex.+>[\n ]+\\)?"))

(defun inf-iex-query-state-common (&optional query)
  (interactive)
  (if (eq inf-iex-send-target 'tmux)
      (message "Query state is not available for tmux target!")
    (let* ((resp (-> (inf-iex--send-string-async (or query inf-iex--common-query))
                     (inf-iex--trim-find-result)))
           (lines (split-string resp "\n"))
           (items (->> (mapcar (lambda (line)
                                 (split-string line "\t"))
                               lines)))
           (names (mapcar #'car items))
           (q (completing-read "Query:" names nil t))
           (pid-str (-> (--first (string-equal q (car it)) items)
                        (cadr)
                        (string-trim-left "#PID")))
           (state (inf-iex--send-string-async
                   (format "IO.puts inspect(:sys.get_state(:erlang.list_to_pid('%s')), pretty: true, limit: :infinity)\n" pid-str))))
      (with-current-buffer (get-buffer-create "*INF IEx Value Inspector*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (inf-iex--trim-find-result state))
        (if (> (point-max) 8000)
            (text-mode)
          (elixir-mode))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (pop-to-buffer (current-buffer))))))

(defun inf-iex-query-state-swarm ()
  (interactive)
  (inf-iex-query-state-common inf-iex--swarm-query))

(provide 'inf-iex-observer)
;;; inf-iex-observer.el ends here
