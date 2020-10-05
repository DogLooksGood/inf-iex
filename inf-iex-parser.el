;;; -*- lexical-binding: t -*-

(require 'subr-x)
(require 'cl-lib)

(defun inf-iex--remove-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun inf-iex--parse-attributes (&optional buf)
  (let ((buf (or buf (current-buffer)))
        (result ())
        (case-fold-search nil))
    (with-current-buffer buf
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "\\(@[_a-z0-9]+\\) +\\(.+\\)" nil t)
          (let* ((attr (inf-iex--remove-text-properties (match-string 1)))
                 (val (inf-iex--remove-text-properties (match-string 2))))
            (unless (member attr
                            '("@impl" "@moduledoc" "@doc" "@behaviour"
                              "@before_compile"))
              (push (cons attr val) result))))))
    result))


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

(defun inf-iex--format-eval-code (code)
  "Currently do nothing."
  (let* ((lines (split-string code "\n"))
         (lines (mapcar (lambda (s) (string-trim s)) lines)))
    (->> (string-join lines "\n")
         (replace-regexp-in-string "^ *#" "")
         (replace-regexp-in-string "\n#" "\n")
         (format "(%s)"))))

(defun inf-iex--make-setup-code (mod respawn &optional buf)
  "Make the setup code for BUF or current buffer."
  (let* ((imports (inf-iex--parse-import buf))
         (imports (if mod (cons mod imports) imports))
         (aliases (inf-iex--parse-alias buf))
         (requires (inf-iex--parse-requires buf)))
    (concat (if respawn
                "respawn\n"
              "")
            (format "%s %s %s"
                    (string-join
                     (mapcar (lambda (s) (format "import %s;" s)) imports)
                     " ")
                    (string-join
                     (mapcar (lambda (s) (format "require %s;" s)) requires)
                     " ")
                    (string-join
                     (mapcar (lambda (s) (format "alias %s, as: %s;" (cdr s) (car s)))
                             aliases)
                     " ")))))

(provide 'inf-iex-parser)
;;; inf-iex-parser.el ends here
