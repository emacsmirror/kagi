;;; kagi-test.el --- Kagi API tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023 - 2024 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 11 Feb 2024
;; Package-Version: 0.1
;; Package-Requires: ((emacs "28.2") (shell-maker "0.44.1"))
;; Keywords: convenience
;; URL: https://codeberg.org/bram85/kagi.el

;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2023 - 2024 Bram Schoenmakers

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; API documentation: https://help.kagi.com/kagi/api/fastgpt.html

;;; Code:

(require 'buttercup)
(require 'kagi)

(defun kagi-test--dummy-output (text &optional references)
  "Construct a fictitious result from the Kagi FastGPT API.

TEXT is the output text, optionally with a list of REFERENCES."
  (json-encode (list (cons "data" (append
                                   (list (cons "output" text))
                                   (when references
                                     (list (cons "references" references))))))))

(buttercup-define-matcher-for-binary-function
    :to-be-equal-including-properties equal-including-properties
  :expect-match-phrase "Expected `%A' to be equal (incl. properties) to %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' not to be equal (incl. properties) to %b, but `%A' was %a.")

(describe "kagi.el"
  (describe "Kagi FastGPT"
    :var ((dummy-output "text"))
    (before-each
      (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output dummy-output)))
    (describe "kagi-fastgpt-prompt"
      (before-each
        (spy-on #'message)
        (spy-on #'kagi--fastgpt-display-result))
      (it "converts *bold* markup to a bold face"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output "**bold**"))
        (expect (kagi-fastgpt-prompt "foo")
                :to-be-equal-including-properties
                (propertize "bold" 'font-lock-face 'kagi-bold)))
      (it "converts <b>bold</b> markup to a bold face"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output "<b>bold</b>"))
        (expect (kagi-fastgpt-prompt "foo")
                :to-be-equal-including-properties
                (propertize "bold" 'font-lock-face 'kagi-bold)))
      (it "converts $italic$ markup to an italic face"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output "$italic$"))
        (expect (kagi-fastgpt-prompt "foo")
                :to-be-equal-including-properties
                (propertize "italic" 'font-lock-face 'kagi-italic)))
      (it "converts ```code``` markup to a code face"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output "```echo $*```"))
        (expect (kagi-fastgpt-prompt "foo")
                :to-be-equal-including-properties
                (propertize "echo $*" 'font-lock-face 'kagi-code)))
      (it "formats references properly"
        (spy-on #'kagi--call-api
                :and-return-value
                (kagi-test--dummy-output
                 "Main text"
                 '(((title . "First title")
                    (snippet . "**Snippet 1**")
                    (url . "https://www.example.org"))
                   ((title . "Second title")
                    (snippet . "Snippet $2$")
                    (url . "https://www.example.com")))))
        (expect (kagi-fastgpt-prompt "foo")
                :to-be-equal-including-properties
                (format "Main text

%s First title
%s
https://www.example.org

%s Second title
Snippet %s
https://www.example.com"
                        (propertize "[1]" 'font-lock-face 'kagi-bold)
                        (propertize "Snippet 1" 'font-lock-face 'kagi-bold)
                        (propertize "[2]" 'font-lock-face 'kagi-bold)
                        (propertize "2" 'font-lock-face 'kagi-italic))))
      (it "inserts the output when requested"
        (spy-on #'insert)
        (kagi-fastgpt-prompt "foo" t)
        ;; one additional insert call is to fill the temporary buffer for POST data
        (expect #'insert :to-have-been-called-times 2)
        (expect #'insert :to-have-been-called-with dummy-output))
      (it "does not insert the output by default"
        (spy-on #'insert)
        (kagi-fastgpt-prompt "foo")
        ;; one insert call is to fill the temporary buffer for POST data
        (expect #'insert :to-have-been-called-times 1))
      (it "shows short output in the echo area when called interactively"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output dummy-output))
        (kagi-fastgpt-prompt "foo" nil t)
        (expect #'message :to-have-been-called-with dummy-output)
        (expect #'kagi--fastgpt-display-result :not :to-have-been-called))
      (it "shows longer output in a separate buffer when called interactively"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output (format "%s\n%s" dummy-output dummy-output)))
        (kagi-fastgpt-prompt "foo" nil t)
        (expect #'message :not :to-have-been-called)
        (expect #'kagi--fastgpt-display-result :to-have-been-called)))
    (describe "kagi-translate"
      (before-each
        (spy-on #'kagi-fastgpt-prompt))
      (it "returns output on minimal input"
        (kagi-translate "foo" "English")
        (expect #'kagi-fastgpt-prompt :to-have-been-called-times 1)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          ;; has English in the prompt
          (expect (nth 0 args) :to-match "English")
          ;; called non-interactively
          (expect (nth 2 args) :to-equal nil)))
      (it "returns output with a source language"
        (kagi-translate "foo" "English" "Spanish")
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          ;; has English in the prompt
          (expect (nth 0 args) :to-match "English")
          ;; has Spanish in the prompt
          (expect (nth 0 args) :to-match "Spanish")
          ;; called non-interactively
          (expect (nth 2 args) :to-equal nil)))
      (it "calls kagi-fastgpt-prompt with interactive flag when called interactively"
        (kagi-translate "foo" "English" nil t)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          ;; called interactively
          (expect (nth 2 args) :to-equal t)))))

  (xdescribe "Kagi Summarizer"
    (before-each
      (spy-on #'kagi--call-api))
    (it "contains a spec with an expectation"
      (expect t :to-be t))))

;;; kagi-test.el ends here
