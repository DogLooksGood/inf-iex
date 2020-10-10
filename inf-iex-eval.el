;;; inf-iex-eval.el --- Eval functionalities for inf-iex  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: tianshu <tianshu@tianshu-manjaro>
;; Keywords: elixir, tools

;; Utilities


(require 'dash)
(require 'inf-iex-util)
(require 'inf-iex-send)
(require 'inf-iex-parser)

(defun inf-iex--make-exp-for-time-measure (code)
  "Wrap the string CODE into a :timer.tc call."
  (format ":timer.tc(fn -> %s end) |> elem(0)" code))

(defun inf-iex--make-exp-for-reload (mod)
  "Return the code to reload module MOD."
  (format "r %s" mod))

(defun inf-iex--make-exp-for-compile (file)
  "Return the code to compile FILE."
  (format "c \"%s\"" file))

(defun inf-iex--make-exp-for-inspect (term)
  "Return the code to inpect TERM."
  (format "i %s" term))

(defun inf-iex--make-exp-for-eval-with (binding code)
  (format "with %s do\n%s\nend" binding code))

(defun inf-iex-i ()
  (interactive)
  (let ((thing
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (if-let ((sym (thing-at-point 'symbol)))
               sym
             (error "No symbol at point!")))))
    (inf-iex--send (inf-iex--make-exp-for-inspect thing))))

(defun inf-iex-respawn-context ()
  (interactive)
  (if-let ((mod (inf-iex--module-name)))
      (let* ((code (inf-iex--make-setup-code mod t)))
        (inf-iex--send code))
    (message "Can't get module name in this buffer!")))

(defun inf-iex-setup-context ()
  (interactive)
  (if-let ((mod (inf-iex--module-name)))
      (let* ((code (inf-iex--make-setup-code mod nil)))
        (inf-iex--send code))
    (message "Can't get module name in this buffer!")))

(defun inf-iex-eval (_arg)
  (interactive "P")
  (-let* (((beg . end) (if (region-active-p)
                          (car (region-bounds))
                         (cons (line-beginning-position) (line-end-position))))
          (raw (buffer-substring-no-properties beg end))
          (code-to-eval (inf-iex--format-eval-code raw)))
    (inf-iex--send code-to-eval)))

(defun inf-iex-eval-with-binding ()
  (interactive)
  (-let* (((beg . end) (if (region-active-p)
                          (car (region-bounds))
                         (cons (line-beginning-position) (line-end-position))))
          (raw (buffer-substring-no-properties beg end))
          (code (inf-iex--format-eval-code raw))
          (binding (read-from-minibuffer "Eval with: " "")))
    (inf-iex--send (inf-iex--make-exp-for-eval-with binding code))))

(defun inf-iex-eval-measure-time ()
  (interactive)
  (-let* (((beg . end) (if (region-active-p)
                          (car (region-bounds))
                         (cons (line-beginning-position) (line-end-position))))
          (raw (buffer-substring-no-properties beg end))
          (code (inf-iex--format-eval-code raw)))
    (inf-iex--send
     (inf-iex--make-exp-for-time-measure code))))

(defun inf-iex-compile ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (inf-iex--send
   (inf-iex--make-exp-for-compile (inf-iex--proj-file-name))))

(defun inf-iex-reload ()
  (interactive)
  (let ((inhibit-redisplay t)
        (module-name (inf-iex--module-name)))
    (when (buffer-modified-p) (save-buffer))
    (if (not module-name)
        (message "Can't get module name in this file!")
      (inf-iex--send (inf-iex--make-exp-for-reload module-name)))))

(provide 'inf-iex-eval)
;;; inf-iex-eval.el ends here
