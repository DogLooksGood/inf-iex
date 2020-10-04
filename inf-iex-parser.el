;;; -*- lexical-binding: t -*-

(require 'subr-x)
(require 'cl-lib)

(defun inf-iex--remove-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun inf-iex--parse-alias (&optional buf)
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "alias \\([A-Z][.A-Za-z0-9]*\\)\\(?:, ?as: \\([A-Z][A-Za-z0-9]*\\)\\)?"
                nil t)
          (let* ((mod (inf-iex--remove-text-properties (match-string 1)))
                 (alias (or (inf-iex--remove-text-properties (match-string 2))
                            (car (reverse (split-string mod "\\."))))))
            (push (cons alias mod) result)))))
    result))

(defun inf-iex--parse-import (&optional buf)
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "import \\([A-Z][.A-Za-z0-9]*\\(?:, ?only: \\(\\[.+\\]\\)\\)?\\)"
                nil t)

          (let ((mod (inf-iex--remove-text-properties (match-string 1))))
            (push mod result)))))
    result))

(defun inf-iex--parse-requires (&optional buf)
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "require \\([A-Z][.A-Za-z0-9]*\\)"
                nil t)
          (let ((mod (inf-iex--remove-text-properties (match-string 1))))
            (push mod result)))))
    result))

(defun inf-iex--replace-code-with-aliases (code aliases)
  (let ((case-fold-search nil)
        (code code))
    (cl-loop for alias in aliases do
             (setq code
                   (replace-regexp-in-string
                    (format "\\([^.]\\)\\_<%s\\_>" (regexp-quote (car alias)))
                    (lambda (s)
                      (format "%s%s" (match-string 1 s) (cdr alias)))
                    (concat " " code)
                    t
                    t)))
    code))

(defun inf-iex--parse-eval-code (code &optional buf)
  (let* ((aliases (inf-iex--parse-alias buf)))
    (inf-iex--replace-code-with-aliases code aliases)))

(defun inf-iex--make-setup-code (mod respawn &optional buf)
  "Make the setup code for BUF or current buffer."
  (let* ((imports (inf-iex--parse-import buf))
         (imports (if mod (cons mod imports) imports))
         (requires (inf-iex--parse-requires buf)))
    (concat (if respawn
                "respawn\n"
              "")
            (format "%s %s"
                    (string-join
                     (mapcar (lambda (s) (format "import %s;" s)) imports)
                     " ")
                    (string-join
                     (mapcar (lambda (s) (format "require %s;" s)) requires)
                     " ")))))

(provide 'inf-iex-parser)
;;; inf-iex-parser.el ends here
