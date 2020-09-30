;;; inf-iex-eval.el --- Eval functionalities for inf-iex  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: tianshu <tianshu@tianshu-manjaro>
;; Keywords: elixir, tools

;; Utilities

(defun inf-iex--make-eval-fn (code)
  "Wrap the string CODE into a special named function definition.

Return the definition in string format."
  (format "def __iex_inf_eval do\n%s\nend" code))

(defun inf-iex--make-time-measure (code)
  "Wrap the string CODE into a :timer.tc call."
  (format ":timer.tc(fn -> %s end) | >elem(0)"))

(defun inf-iex--make-reload (mod)
  "Return the code to reload module MOD."
  (format "r %s" mod))

(defun inf-iex--make-compile (file)
  "Return the code to compile FILE."
  (format "c \"%s\"" file))

(defun inf-iex--make-inspect (term)
  "Return the code to inpect TERM."
  (format "i %s" term))

(provide 'inf-iex-eval)
;;; inf-iex-eval.el ends here
