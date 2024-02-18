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
  :var ((dummy-output "text"))
  (before-each
    (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output dummy-output)))
  (describe "FastGPT"
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
        ;; one additional insert call is to fill the temporary buffer
        ;; for POST data
        (expect #'insert :to-have-been-called-times 2)
        (expect #'insert :to-have-been-called-with dummy-output))
      (it "does not insert the output by default"
        (spy-on #'insert)
        (kagi-fastgpt-prompt "foo")
        ;; one insert call is to fill the temporary buffer for POST
        ;; data
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
        (expect #'kagi--fastgpt-display-result :to-have-been-called))
      (it "makes exactly one API call"
        (kagi-fastgpt-prompt "foo")
        (expect #'kagi--call-api :to-have-been-called-times 1)))
    (describe "kagi-translate"
      (before-each
        (spy-on #'kagi-fastgpt-prompt))
      (it "calls kagi-fastgpt-prompt non-interactively with target language in prompt"
        (kagi-translate "hello" "toki pona")
        (expect #'kagi-fastgpt-prompt :to-have-been-called-times 1)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          ;; not going to test the exact phrasing of the prompt, but
          ;; at least 'toki pona' has to appear.
          (expect (nth 0 args) :to-match "toki pona")
          ;; called non-interactively
          (expect (nth 2 args) :to-equal nil)))
      (it "calls kagi-fastgpt-prompt non-interactively with source and target language in prompt"
        (kagi-translate "bonjour" "toki pona" "French")
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          ;; has 'toki pona' in the prompt
          (expect (nth 0 args) :to-match "toki pona")
          ;; and has French in the prompt
          (expect (nth 0 args) :to-match "French")
          ;; called non-interactively
          (expect (nth 2 args) :to-equal nil)))
      (it "passes the region text to kagi-fastgpt-prompt, if active"
        (spy-on #'use-region-p :and-return-value t)
        (spy-on #'buffer-substring-no-properties :and-return-value "region text")
        (spy-on #'region-beginning)
        (spy-on #'region-end)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (call-interactively #'kagi-translate)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "region text")
          (expect (nth 0 args) :to-match "toki pona")
          (expect (nth 2 args) :to-equal t)))
      (it "passes the user input if the region is inactive"
        (spy-on #'use-region-p :and-return-value nil)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (spy-on #'read-buffer :and-return-value "user text")
        (spy-on #'get-buffer :and-return-value nil)
        (call-interactively #'kagi-translate)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "user text")
          (expect (nth 2 args) :to-equal t)))
      (it "passes the buffer text if buffer is selected"
        (spy-on #'use-region-p :and-return-value nil)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (spy-on #'read-buffer)
        (spy-on #'get-buffer :and-return-value t)
        (spy-on #'set-buffer)
        (spy-on #'buffer-string :and-return-value "buffer text")
        (call-interactively #'kagi-translate)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "buffer text")
          (expect (nth 2 args) :to-equal t)))
      (it "raises an error when no text is given"
        (spy-on #'use-region-p :and-return-value nil)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (spy-on #'read-buffer :and-return-value "")
        (spy-on #'get-buffer :and-return-value nil)
        (expect (call-interactively #'kagi-translate) :to-throw)))
    (describe "kagi-proofread"
      (before-each
        (spy-on #'kagi-fastgpt-prompt))
      (it "calls kagi-fastgpt-prompt"
        (kagi-proofread "foo")
        (expect #'kagi-fastgpt-prompt :to-have-been-called)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "foo")))
      (it "passes the region text to kagi-fastgpt-prompt, if active"
        (spy-on #'use-region-p :and-return-value t)
        (spy-on #'buffer-substring-no-properties :and-return-value "region text")
        (spy-on #'region-beginning)
        (spy-on #'region-end)
        (call-interactively #'kagi-proofread)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "region text")
          (expect (nth 2 args) :to-equal t)))
      (it "passes the user input if the region is inactive"
        (spy-on #'use-region-p :and-return-value nil)
        (spy-on #'read-buffer :and-return-value "user text")
        (spy-on #'get-buffer :and-return-value nil)
        (call-interactively #'kagi-proofread)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "user text")
          (expect (nth 2 args) :to-equal t)))
      (it "passes the buffer text if buffer is selected"
        (spy-on #'use-region-p :and-return-value nil)
        (spy-on #'read-buffer)
        (spy-on #'get-buffer :and-return-value t)
        (spy-on #'set-buffer)
        (spy-on #'buffer-string :and-return-value "buffer text")
        (call-interactively #'kagi-proofread)
        (let ((args (spy-calls-args-for #'kagi-fastgpt-prompt 0)))
          (expect (nth 0 args) :to-match "buffer text")
          (expect (nth 2 args) :to-equal t)))
      (it "raises an error when no text is given"
        (spy-on #'use-region-p :and-return-value nil)
        (spy-on #'read-buffer :and-return-value "")
        (spy-on #'get-buffer :and-return-value nil)
        (expect (call-interactively #'kagi-proofread) :to-throw))))

  (describe "Summarizer"
    :var ((just-enough-text-input nil)
          (just-too-little-text-input nil)
          (dummy-https-url "https://www.example.com")
          (dummy-http-url "http://www.example.com")
          (dummy-ftp-url "ftp://example.com")
          ;; make pattern matching case sensitive
          (case-fold-search nil))
    (before-all
      (dotimes (_ 50) (push "a" just-enough-text-input))
      (setq just-too-little-text-input (string-join (cdr just-enough-text-input) " "))
      (setq just-enough-text-input (string-join just-enough-text-input " ")))
    (before-each
      (spy-on #'kagi--call-summarizer :and-call-through))
    (describe "kagi-summarize"
      :var ((kagi-summarizer-default-language))
      (before-each
        (setq kagi-summarizer-default-language "NL"))
      (it "returns a summary on minimal text input"
        (expect (kagi-summarize just-enough-text-input) :to-equal dummy-output))
      (it "makes exactly one API call"
        (kagi-summarize just-enough-text-input)
        (expect #'kagi--call-api :to-have-been-called-times 1))
      (it "throws on just too little text output"
        (expect (kagi-summarize just-too-little-text-input) :to-throw))
      (it "throws an error on too little text input"
        (expect (kagi-summarize "foo") :to-throw))
      (it "throws an error on empty input"
        (expect (kagi-summarize "") :to-throw))
      (it "throws an error on missing input"
        (expect (kagi-summarize nil) :to-throw))
      (it "returns a summary for a valid language code"
        (expect (kagi-summarize just-enough-text-input "NL" :to-equal dummy-output))
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "NL")))
      (it "returns a summary for a valid language code with wrong capitalization"
        (expect (kagi-summarize just-enough-text-input "nL" :to-equal dummy-output))
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "NL")))
      (it "returns a summary for a valid language name"
        (expect (kagi-summarize just-enough-text-input "Dutch" :to-equal dummy-output))
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "NL")))
      (it "returns a summary for a valid language name with different capitalization"
        (expect (kagi-summarize just-enough-text-input "dUtch" :to-equal dummy-output))
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "NL")))
      (it "falls back to the default language for invalid language codes"
        (expect (kagi-summarize just-enough-text-input "VL") :to-equal dummy-output)
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "NL")))
      (it "falls back to the default language for invalid language names"
        (expect (kagi-summarize just-enough-text-input "Valyrian") :to-equal dummy-output)
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "NL")))
      (it "falls back to English if the default language is invalid"
        (setq kagi-summarizer-default-language "XY")
        (expect (kagi-summarize just-enough-text-input "Valyrian") :to-equal dummy-output)
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "target_language") :to-equal "EN")))
      (it "returns a summary for an HTTPS URL"
        (expect (kagi-summarize dummy-https-url) :to-equal dummy-output))
      (it "returns a summary for an uppercase HTTPS URL"
        (expect (kagi-summarize (upcase dummy-https-url)) :to-equal dummy-output))
      (it "returns a summary for an HTTP URL"
        (expect (kagi-summarize dummy-http-url) :to-equal dummy-output))
      (it "throws for an unsupported URL scheme"
        (expect (kagi-summarize dummy-ftp-url) :to-throw))
      (it "return a summary for a valid engine with different capitalization"
        (expect (kagi-summarize dummy-https-url nil "Daphne") :to-equal dummy-output)
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "engine") :to-equal "daphne"))
        )
      (it "uses kagi-summarizer-engine variable for invalid engine values"
        (setq kagi-summarizer-engine "Daphne")
        (expect (kagi-summarize dummy-https-url nil "bram") :to-equal dummy-output)
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "engine") :to-equal "daphne")))
      (it "uses the cecil engine when an invalid engine is configured"
        (setq kagi-summarizer-engine "steve")
        (expect (kagi-summarize dummy-https-url) :to-equal dummy-output)
        (let ((args (car (spy-calls-args-for #'kagi--call-summarizer 0))))
          (expect (map-elt args "engine") :to-equal "cecil"))))))

;;; kagi-test.el ends here
