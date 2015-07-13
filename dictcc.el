;;; dictcc --- Look up translations on dict.cc  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Marten Lienen

;; Author: Marten Lienen <marten.lienen@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((s "1.0") (dash "2.0") (helm))

;;; Commentary:

;; Look up translations on dict.cc. You then pick one of them through a helm
;; interface and it is inserted at point.

;;; Code:

(require 'dash)
(require 's)
(require 'helm)

(defun dictcc--request (word)
  "Send the request to look up WORD on dict.cc."
  (let ((buffer (current-buffer)))
    (url-retrieve (format "http://www.dict.cc/?s=%s" (url-encode-url word))
                  (lambda (log)
                    (let ((translations (dictcc--parse-http-response)))
                      (save-excursion
                        (switch-to-buffer buffer)
                        (dictcc--select-translation word translations)))))))

(defun dictcc--parse-http-response ()
  "Parse the HTTP response and extract the translations."
  (search-forward "\n\n")
  (let* ((doc (libxml-parse-html-region (point) (point-max)))
         (rows (dictcc--find-translation-rows doc))
         (texts (mapcar #'dictcc--extract-translations rows))
         (english (mapcar #'car texts)))
    english))

(defun dictcc--find-translation-rows (doc)
  "Find all table rows with translations in them in DOC.

At the moment all translations are elements of the form <tr
id='trXXX'></tr>."
  (let ((rows nil)
        (elements (list doc)))
    (while elements
      (let ((element (pop elements)))
        (when (listp element)
          (let* ((tag (car element))
                 (attributes (cadr element))
                 (children (cddr element))
                 (id (cdr (assq 'id attributes)))
                 (is-translation
                  (and (eq tag 'tr)
                       (stringp id)
                       (string-equal (substring id 0 2) "tr"))))
            (if is-translation
                (push element rows)
              (dolist (child children)
                (push child elements)))))))
    rows))

(defun dictcc--extract-translations (row)
  "Extract translation texts from table ROW."
  (let* ((cells (cddr row))
         (c1 (nth 1 cells))
         (c2 (nth 2 cells)))
    (list (dictcc--cell-to-text c1)
          (dictcc--cell-to-text c2))))

(defun dictcc--cell-to-text (cell)
  "Extract the text from a single table CELL."
  (let* ((children (cddr cell))
         (links (-filter #'dictcc--translation-tag-p children))
         (words (mapcar #'dictcc--tag-to-text links)))
    (s-join " " words)))

(defun dictcc--translation-tag-p (tag)
  "Check, if TAG is part of the translation.

This is the case, if tag is either <a>...</a> or
<a><b>...</b></a>."
  (and (listp tag)
       (eq (car tag) 'a)
       (let ((child (caddr tag)))
         (or (stringp child)
             (eq (car child) 'b)))))

(defun dictcc--tag-to-text (tag)
  "Concatenate the string contents of TAG and its children."
  (if (stringp tag)
      tag
    (let* ((children (cddr tag))
           (texts (mapcar #'dictcc--tag-to-text children)))
      (s-join "" texts))))

(defun dictcc--select-translation (word translations)
  "Select one from TRANSLATIONS and insert it into the buffer."
  (let ((source `((name . ,(format "Translations for %s" word))
                  (candidates . ,translations)
                  (action . ,(lambda (w)
                               (message w)
                               (insert w))))))
    (helm :sources (list source))))

;;;###autoload
(defun dictcc (word)
  "Look up translations of WORD and insert into the buffer."
  (interactive "sWord: \n")
  (dictcc--request word))

(provide 'dictcc)
;;; dictcc.el ends here
