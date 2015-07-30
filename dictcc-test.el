;;; dictcc-test --- Tests for `dictcc'

;;; Commentary:

;; We are testing various aspects of how `dictcc' parses the translations from
;; the HTML results.

;;; Code:

(require 'dictcc)
(require 'dash)
(require 'f)

(defun dictcc-test--parse-html-string (string)
  "Parse STRING as an HTML document."
  (let ((doc (with-temp-buffer
              (insert string)
              (libxml-parse-html-region (point-min) (point-max)))))
    (caddr (caddr doc))))

(ert-deftest dictcc-parses-cell-to-translation ()
  (let* ((cell (dictcc-test--parse-html-string (f-read-text "test/cell.html")))
         (translation (dictcc--translation-from-cell cell)))
    (should (equal (dictcc--translation-text translation)
                   "math"))
    (should (-same-items-p (dictcc--translation-tags translation)
                           (list "Am." "coll." "math." "educ.")))))

(ert-deftest dictcc-parsing-ignores-spaces ()
  "This should not error."
  (let ((cell (dictcc-test--parse-html-string "<td> </td>")))
    (dictcc--translation-from-cell cell)))

(ert-deftest dictcc-multiword-tags-should-be-parsed-as-one ()
  (let* ((cell (dictcc-test--parse-html-string
                (f-read-text "test/split-tags.html")))
         (translation (dictcc--translation-from-cell cell)))
    (should (equal (dictcc--translation-tags translation)
                   (list "also ironic")))))

(ert-deftest dictcc-tags-are-parsed-correctly ()
  (should (equal (dictcc--tags-from-string "educ. [Am.] {var} [also ironic]")
                 (list "educ." "Am." "var" "also ironic"))))

(provide 'dictcc-test)
;;; dictcc-test.el ends here
