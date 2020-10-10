;;; -*- lexical-binding: t -*-

(require 'subr-x)
(require 'cl-lib)

(defun inf-iex--in-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun inf-iex--exp-end-p ()
  (and (zerop (car (syntax-ppss)))
       (save-mark-and-excursion
         (while (eq (char-before) 32)
           (backward-char 1))
         (not (looking-back "[,:]")))))

(defun inf-iex--bound-of-current-exp ()
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
  (set-text-properties 0 (length text) nil text)
  text)

(defun inf-iex--replace-module-constant (mod code)
  (replace-regexp-in-string "__MODULE__" mod code t t))

(defun inf-iex--parse-alias (mod &optional buf)
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
  "Format the code for sending to IEx.

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
  "Make the setup code for BUF or current buffer."
  (let* ((imports (inf-iex--parse-import mod buf))
         (imports (if mod (cons (format "import %s" mod) imports) imports))
         (aliases (inf-iex--parse-alias mod buf))
         (requires (inf-iex--parse-requires mod buf)))
    (concat (if respawn
                "respawn\n"
              "")
            (message "%s\n%s\n%s"
                     (string-join aliases "\n")
                     (string-join requires "\n")
                     (string-join imports "\n")))))

(provide 'inf-iex-parser)
;;; inf-iex-parser.el ends here
