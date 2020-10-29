;;; inf-iex-pry.el --- Tracer support for INF IEx       -*- lexical-binding: t; -*-

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
(require 'dash)

(defconst inf-iex--find-caller-buffer
  "*INF IEx Callers*")

(defun inf-iex--make-find-caller-exp (prefixs)
  "Return the expression for defining tracer."
  (let ((prefix-value (-> (--map (format "\"Elixir.%s.\"" it) prefixs)
                          (string-join ",")
                          (format "[%s]"))))
    (format
     "defmodule InfIExCallerFinder do
  use Agent

  def start_link(_) do
    Agent.start_link(fn -> [] end, name: __MODULE__)
  end

  def run() do
    __MODULE__.start_link(fn -> [] end)
    Mix.Task.clear()
    Mix.Task.rerun(\"compile\", [\"--force\", \"--ignore-module-conflict\", \"--tracer\", __MODULE__])
    result = Agent.get(__MODULE__, & &1)
    Agent.stop(__MODULE__)
    Enum.join(result, \"\\n\")
    |> IO.puts
  end

  def trace({:remote_function, _meta, module, fun, arity}, env) do
    mod = to_string(module)
    if String.starts_with?(mod, %s)  do
      Agent.update(__MODULE__, fn state ->
        [\"__INF_IEX__#{env.file}\\t#{env.line}\\t#{inspect(module)}.#{fun}/#{arity}\" | state]
      end)
    end
    :ok
  end

  def trace(_, _), do: :ok
end
InfIExCallerFinder.run()" prefix-value)))

(defvar-local inf-iex--tracer-items nil)
(defvar-local inf-iex--tracer-current-project nil)
(defvar-local inf-iex--tracer-current-prefix nil)

(defun inf-iex--tracer-redisplay ()
  (erase-buffer)
  (cl-loop for group in inf-iex--tracer-items do
           (-let* (((fma . items) group))
             (insert fma "\n")
             (cl-loop for item in items do
                      (-let* (((file line-no fma) item))
                        (insert "  |- ")
                        (insert-button
                         (format "%s:%s" (file-relative-name file inf-iex--tracer-current-project) line-no)
                         'follow-link t
                         'action (lambda (_) (find-file file) (goto-line (string-to-number line-no))))
                        (insert "\n")))))
  (goto-char (point-min))
  (inf-iex-tracer-next))

(defun inf-iex--handle-find-callers-result (result proj-root)
  (let* ((lines (->> (split-string result "\n")
                     (--filter (string-prefix-p "__INF_IEX__" it))
                     (--map (substring it 11))))
         (buf (get-buffer-create inf-iex--find-caller-buffer)))
    (with-current-buffer buf
      (inf-iex-tracer-mode)
      (let ((buffer-read-only nil))
        (setq inf-iex--tracer-current-project proj-root)
        (setq inf-iex--tracer-items
              (->> (--map (split-string it "\t") lines)
                   (-group-by #'caddr)
                   (-sort (lambda (x y) (string< (car x) (car y))))))
        (inf-iex--tracer-redisplay)))
    buf))

(defvar inf-iex-tracer-prefix-history nil)

(defun inf-iex-tracer ()
  (interactive)
  (let* ((proj-root (inf-iex--project-root))
         (prefixs (-> (read-from-minibuffer "Module prefixs(separate by comma): " "" nil nil inf-iex-tracer-prefix-history)
                      (split-string ","))))
    (setq inf-iex--tracer-current-prefix prefixs)
    (-> (inf-iex--make-find-caller-exp prefixs)
        (inf-iex--format-eval-code)
        (inf-iex--send-string-async)
        (inf-iex--handle-find-callers-result proj-root)
        (pop-to-buffer t))))

(defun inf-iex--trace-mode-init ())

(defun inf-iex-tracer-next ()
  (interactive)
  (forward-line 1)
  (unless (equal 32 (char-after (line-beginning-position)))
    (forward-line 1))
  (goto-char (+ 5 (line-beginning-position))))

(defun inf-iex-tracer-prev ()
  (interactive)
  (forward-line -1)
  (unless (equal 32 (char-after (line-beginning-position)))
    (forward-line -1))
  (goto-char (+ 5 (line-beginning-position))))

(defvar inf-iex-tracer-search-history nil)

(defun inf-iex-tracer-search ()
  (interactive)
  (let ((s (completing-read "Search: "
                            (-map #'car inf-iex--tracer-items)
                            nil
                            t
                            nil
                            inf-iex-tracer-search-history)))
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote s)) nil t 1)
    (inf-iex-tracer-next)))

(defun inf-iex-tracer-refresh ()
  (interactive)
  (-> (inf-iex--make-find-caller-exp inf-iex--tracer-current-prefix)
      (inf-iex--format-eval-code)
      (inf-iex--send-string-async)
      (inf-iex--handle-find-callers-result inf-iex--tracer-current-project)))

(defvar inf-iex-tracer-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") 'inf-iex-tracer-next)
    (define-key keymap (kbd "p") 'inf-iex-tracer-prev)
    (define-key keymap (kbd "j") 'inf-iex-tracer-next)
    (define-key keymap (kbd "k") 'inf-iex-tracer-prev)
    (define-key keymap (kbd "s") 'inf-iex-tracer-search)
    (define-key keymap (kbd "r") 'inf-iex-tracer-refresh)
    keymap))

(define-derived-mode inf-iex-tracer-mode
  special-mode " inf-IEx Tracer"
  "Major mode for display tracer result in inf-iex."
  :keymap inf-iex-tracer-mode-map
  (inf-iex--trace-mode-init))

(provide 'inf-iex-tracer)
