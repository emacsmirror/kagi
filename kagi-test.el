;;; kagi-test.el --- Kagi API tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023 - 2024 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 11 Feb 2024
;; Package-Version: 0.4
;; Package-Requires: ((emacs "29.1") (shell-maker "0.46.1"))
;; Keywords: terminals wp
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

(defun kagi-test--error-output ()
  "Construct a fictitious erroneous result from the Kagi API."
  (json-encode
   '((data . ((output . nil)))
     (error . (((code . 42)
                (msg . "Too bad")))))))

(buttercup-define-matcher-for-binary-function
    :to-be-equal-including-properties equal-including-properties
  :expect-match-phrase "Expected `%A' to be equal (incl. properties) to %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' not to be equal (incl. properties) to %b, but `%A' was %a.")

(defmacro kagi-test--expect-arg (function-symbol num &rest expect-args)
  "Check the argument NUM of the first call of FUNCTION-SYMBOL.

The EXPECT-ARGS correspond to the arguments passed to the `expect' macro."
  `(let ((args (spy-calls-args-for ,function-symbol 0)))
     (expect (nth ,num args) ,@expect-args)))

(defmacro kagi-test--expect-object (function-symbol key &rest expect-args)
  "Check the argument NUM of the first call of FUNCTION-SYMBOL.

The EXPECT-ARGS correspond to the arguments passed to the `expect' macro."
  `(let ((args (car (spy-calls-args-for ,function-symbol 0))))
     (expect (map-elt args ,key) ,@expect-args)))

(describe "kagi.el"
  :var ((dummy-output "text"))
  (before-each
    (spy-on #'kagi--call-api :and-return-value (kagi-test--dummy-output dummy-output)))
  (it "throws an error for invalid tokens"
    (setq kagi-api-token 42)
    (expect (kagi--curl-flags "foo") :to-throw))
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
        (expect #'kagi--call-api :to-have-been-called-times 1))
      (it "handles empty output and returned errors from the API gracefully"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--error-output))
        (spy-on #'kagi--fastgpt :and-call-through)
        (expect (kagi-fastgpt-prompt "foo") :to-throw)
        (expect (spy-context-thrown-signal
                 (spy-calls-most-recent #'kagi--fastgpt))
                :to-equal '(error "Too bad (42)"))))
    (describe "kagi-translate"
      (before-each
        (spy-on #'kagi-fastgpt-prompt))
      (it "calls kagi-fastgpt-prompt non-interactively with target language in prompt"
        (kagi-translate "hello" "toki pona")
        (expect #'kagi-fastgpt-prompt :to-have-been-called-times 1)
        ;; not going to test the exact phrasing of the prompt, but
        ;; at least 'toki pona' has to appear.
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "toki pona")
        ;; called non-interactively
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal nil))
      (it "calls kagi-fastgpt-prompt non-interactively with source and target language in prompt"
        (kagi-translate "bonjour" "toki pona" "French")
        ;; has 'toki pona' in the prompt
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "toki pona")
        ;; and has French in the prompt
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "French")
        ;; called non-interactively
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal nil))
      (it "passes the region text to kagi-fastgpt-prompt, if active"
        (spy-on #'use-region-p :and-return-value t)
        (spy-on #'buffer-substring-no-properties :and-return-value "region text")
        (spy-on #'region-beginning)
        (spy-on #'region-end)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (call-interactively #'kagi-translate)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "region text")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "toki pona")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal t))
      (it "passes the user input if the region is inactive"
        (spy-on #'use-region-p)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (spy-on #'read-buffer :and-return-value "user text")
        (spy-on #'get-buffer)
        (call-interactively #'kagi-translate)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "user text")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal t))
      (it "passes the buffer text if buffer is selected"
        (spy-on #'use-region-p)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (spy-on #'read-buffer)
        (spy-on #'get-buffer :and-return-value t)
        (spy-on #'set-buffer)
        (spy-on #'buffer-string :and-return-value "buffer text")
        (call-interactively #'kagi-translate)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "buffer text")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal t))
      (it "raises an error when no text is given"
        (spy-on #'use-region-p)
        (spy-on #'kagi--read-language :and-return-value "toki pona")
        (spy-on #'read-buffer :and-return-value "")
        (spy-on #'get-buffer)
        (expect (call-interactively #'kagi-translate) :to-throw)))
    (describe "kagi-proofread"
      (before-each
        (spy-on #'kagi-fastgpt-prompt))
      (it "calls kagi-fastgpt-prompt"
        (kagi-proofread "foo")
        (expect #'kagi-fastgpt-prompt :to-have-been-called)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "foo"))
      (it "passes the region text to kagi-fastgpt-prompt, if active"
        (spy-on #'use-region-p :and-return-value t)
        (spy-on #'buffer-substring-no-properties :and-return-value "region text")
        (spy-on #'region-beginning)
        (spy-on #'region-end)
        (call-interactively #'kagi-proofread)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "region text")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal t))
      (it "passes the user input if the region is inactive"
        (spy-on #'use-region-p)
        (spy-on #'read-buffer :and-return-value "user text")
        (spy-on #'get-buffer)
        (call-interactively #'kagi-proofread)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "user text")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal t))
      (it "passes the buffer text if buffer is selected"
        (spy-on #'use-region-p)
        (spy-on #'read-buffer)
        (spy-on #'get-buffer :and-return-value t)
        (spy-on #'set-buffer)
        (spy-on #'buffer-string :and-return-value "buffer text")
        (call-interactively #'kagi-proofread)
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 0 :to-match "buffer text")
        (kagi-test--expect-arg #'kagi-fastgpt-prompt 2 :to-equal t))
      (it "raises an error when no text is given"
        (spy-on #'use-region-p)
        (spy-on #'read-buffer :and-return-value "")
        (spy-on #'get-buffer)
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
      (spy-on #'kagi--call-summarizer :and-call-through)
      (spy-on #'kagi--display-summary))
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
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "NL"))
      (it "returns a summary for a valid language code with wrong capitalization"
        (expect (kagi-summarize just-enough-text-input "nL" :to-equal dummy-output))
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "NL"))
      (it "returns a summary for a valid language name"
        (expect (kagi-summarize just-enough-text-input "Dutch" :to-equal dummy-output))
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "NL"))
      (it "returns a summary for a valid language name with different capitalization"
        (expect (kagi-summarize just-enough-text-input "dUtch" :to-equal dummy-output))
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "NL"))
      (it "falls back to the default language for invalid language codes"
        (expect (kagi-summarize just-enough-text-input "VL") :to-equal dummy-output)
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "NL"))
      (it "falls back to the default language for invalid language names"
        (expect (kagi-summarize just-enough-text-input "Valyrian") :to-equal dummy-output)
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "NL"))
      (it "falls back to English if the default language is invalid"
        (setq kagi-summarizer-default-language "XY")
        (expect (kagi-summarize just-enough-text-input "Valyrian") :to-equal dummy-output)
        (kagi-test--expect-object #'kagi--call-summarizer "target_language" :to-equal "EN"))
      (it "returns a summary for an HTTPS URL"
        (expect (kagi-summarize dummy-https-url) :to-equal dummy-output))
      (it "returns a summary for an uppercase HTTPS URL"
        (expect (kagi-summarize (upcase dummy-https-url)) :to-equal dummy-output))
      (it "returns a summary for an HTTP URL"
        (expect (kagi-summarize dummy-http-url) :to-equal dummy-output))
      (it "throws for an unsupported URL scheme"
        (expect (kagi-summarize dummy-ftp-url) :to-throw))
      (it "returns a summary for a valid engine with different capitalization"
        (expect (kagi-summarize dummy-https-url nil "Daphne") :to-equal dummy-output)
        (kagi-test--expect-object #'kagi--call-summarizer "engine" :to-equal "daphne"))
      (it "uses kagi-summarizer-engine variable for invalid engine values"
        (setq kagi-summarizer-engine "Daphne")
        (expect (kagi-summarize dummy-https-url nil "bram") :to-equal dummy-output)
        (kagi-test--expect-object #'kagi--call-summarizer "engine" :to-equal "daphne"))
      (it "uses the cecil engine when an invalid engine is configured"
        (setq kagi-summarizer-engine "steve")
        (expect (kagi-summarize dummy-https-url) :to-equal dummy-output)
        (kagi-test--expect-object #'kagi--call-summarizer "engine" :to-equal "cecil"))
      (it "returns a summary when the summary style is requested"
        (expect (kagi-summarize just-enough-text-input nil nil 'summary) :to-equal dummy-output))
      (it "returns a summary when the take-away style is requested"
        (expect (kagi-summarize just-enough-text-input nil nil 'takeaway) :to-equal dummy-output))
      (it "uses the summary style when an invalid format is given"
        (kagi-summarize just-enough-text-input nil nil 'invalid)
        (kagi-test--expect-object #'kagi--call-summarizer "summary_type" :to-equal 'summary))
      (it "uses the summary style when an invalid format is configured"
        (setq kagi-summarizer-default-summary-format 'invalid)
        (kagi-summarize just-enough-text-input)
        (kagi-test--expect-object #'kagi--call-summarizer "summary_type" :to-equal 'summary))
      (it "lets Kagi cache by default"
        (kagi-summarize just-enough-text-input)
        (kagi-test--expect-object #'kagi--call-summarizer "cache" :to-equal t))
      (it "does not let Kagi cache when no-cache is set"
        (kagi-summarize just-enough-text-input nil nil nil t)
        (kagi-test--expect-object #'kagi--call-summarizer "cache" :to-equal nil))
      (it "lets the no-cache argument override the configured value"
        (setq kagi-summarizer-cache t)
        (kagi-summarize just-enough-text-input nil nil nil t)
        (kagi-test--expect-object #'kagi--call-summarizer "cache" :to-equal nil))
      (it "does not let Kagi cache if configured"
        (setq kagi-summarizer-cache nil)
        (kagi-summarize just-enough-text-input)
        (kagi-test--expect-object #'kagi--call-summarizer "cache" :to-equal nil))
      (it "caches by default for an invalid configuration value"
        (setq kagi-summarizer-cache 'invalid)
        (kagi-summarize just-enough-text-input)
        (kagi-test--expect-object #'kagi--call-summarizer "cache" :to-equal t))
      (it "handles empty output and returned errors from the API gracefully"
        (spy-on #'kagi--call-api :and-return-value (kagi-test--error-output))
        (spy-on #'kagi-summarize :and-call-through)
        (expect (kagi-summarize just-enough-text-input) :to-throw)
        (expect (spy-context-thrown-signal
                 (spy-calls-most-recent #'kagi-summarize))
                :to-equal '(error "Too bad (42)"))))
    (describe "kagi-summarize-buffer"
      (before-each
        (spy-on #'read-buffer)
        (spy-on #'set-buffer)
        (spy-on #'buffer-name :and-return-value "dummy-buffer")
        (spy-on #'kagi-summarize :and-return-value dummy-output)
        (spy-on #'kagi--insert-summary))
      (it "returns the summary when called non-interactively"
        (expect (kagi-summarize-buffer "dummy") :to-be dummy-output)
        (expect #'kagi-summarize :to-have-been-called)
        (expect #'kagi--display-summary :not :to-have-been-called)
        (expect #'kagi--insert-summary :not :to-have-been-called))
      (it "inserts the summary when requested, non-interactively"
        (kagi-summarize-buffer "dummy" t)
        (expect #'kagi-summarize :to-have-been-called)
        (expect #'kagi--display-summary :not :to-have-been-called)
        (expect #'kagi--insert-summary :to-have-been-called))
      (it "displays the summary when called interactively"
        (call-interactively #'kagi-summarize-buffer)
        (expect #'kagi-summarize :to-have-been-called)
        (expect #'kagi--display-summary :to-have-been-called)
        (expect #'kagi--insert-summary :not :to-have-been-called))
      (it "inserts the summary when requested, interactively"
        (spy-on #'kagi--get-summarizer-parameters :and-return-value '(t nil nil))
        (call-interactively #'kagi-summarize-buffer)
        (expect #'kagi-summarize :to-have-been-called)
        (expect #'kagi--display-summary :not :to-have-been-called)
        (expect #'kagi--insert-summary :to-have-been-called))
      (it "passes arguments to kagi-summary"
        (spy-on #'kagi--get-summarizer-parameters :and-return-value '(t lang bram random maybe))
        (call-interactively #'kagi-summarize-buffer)
        (expect #'kagi-summarize :to-have-been-called)
        (expect #'kagi--display-summary :not :to-have-been-called)
        (expect #'kagi--insert-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi-summarize 1 :to-equal 'lang)
        (kagi-test--expect-arg #'kagi-summarize 2 :to-equal 'bram)
        (kagi-test--expect-arg #'kagi-summarize 3 :to-equal 'random)
        (kagi-test--expect-arg #'kagi-summarize 4 :to-equal 'maybe)))
    (describe "kagi-summarize-region"
      (before-each
        (spy-on #'region-beginning)
        (spy-on #'region-end)
        (spy-on #'kagi--get-summarizer-parameters :and-return-value '(lang bram random maybe))
        (spy-on #'kagi-summarize :and-return-value dummy-output)
        (spy-on #'buffer-name :and-return-value "buffer-name")
        (spy-on #'buffer-substring-no-properties))
      (it "passes arguments to kagi-summary"
        (call-interactively #'kagi-summarize-region)
        (kagi-test--expect-arg #'kagi--display-summary 1 :to-equal "buffer-name (summary)")
        (expect #'kagi-summarize :to-have-been-called)
        (kagi-test--expect-arg #'kagi-summarize 1 :to-equal 'lang)
        (kagi-test--expect-arg #'kagi-summarize 2 :to-equal 'bram)
        (kagi-test--expect-arg #'kagi-summarize 3 :to-equal 'random)
        (kagi-test--expect-arg #'kagi-summarize 4 :to-equal 'maybe))
      (it "opens a buffer with the summary"
        (call-interactively #'kagi-summarize-region)
        (expect #'kagi--display-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 0 :to-equal dummy-output)))
    (describe "kagi-summarize-url"
      (before-each
        (spy-on #'kagi-summarize :and-return-value dummy-output)
        (spy-on #'read-string :and-return-value "https://www.example.com")
        (spy-on #'kagi--get-summarizer-parameters :and-return-value '(nil lang bram random))
        (spy-on #'kagi--insert-summary))
      (it "passes arguments to kagi-summary"
        (call-interactively #'kagi-summarize-url)
        (kagi-test--expect-arg #'kagi-summarize 1 :to-equal 'lang)
        (kagi-test--expect-arg #'kagi-summarize 2 :to-equal 'bram)
        (kagi-test--expect-arg #'kagi-summarize 3 :to-equal 'random)
        (kagi-test--expect-arg #'kagi-summarize 4 :to-equal nil))
      (it "opens a buffer with the summary"
        (call-interactively #'kagi-summarize-url)
        (expect #'kagi--display-summary :to-have-been-called)
        (expect #'kagi--insert-summary :not :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 0 :to-equal dummy-output))
      (it "opens a buffer with the domain name for an HTTPS URL"
        (call-interactively #'kagi-summarize-url)
        (expect #'kagi--display-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 1 :to-equal "example.com (summary)"))
      (it "opens a buffer with the domain name for an HTTP URL"
        (spy-on #'read-string :and-return-value "http://www.example.com")
        (call-interactively #'kagi-summarize-url)
        (expect #'kagi--display-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 1 :to-equal "example.com (summary)"))
      (it "opens a buffer with the domain name for an HTTPS URL ending with /"
        (spy-on #'read-string :and-return-value "https://www.example.com/")
        (call-interactively #'kagi-summarize-url)
        (expect #'kagi--display-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 1 :to-equal "example.com (summary)"))
      (it "opens a buffer with the domain name for an HTTPS URL ending with path"
        (spy-on #'read-string :and-return-value "https://www.example.com/aaa/")
        (call-interactively #'kagi-summarize-url)
        (expect #'kagi--display-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 1 :to-equal "example.com (summary)"))
      (it "opens a buffer with the domain name for an HTTPS URL with subdomain"
        (spy-on #'read-string :and-return-value "https://abc.example.com/")
        (call-interactively #'kagi-summarize-url)
        (expect #'kagi--display-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--display-summary 1 :to-equal "abc.example.com (summary)"))
      (it "inserts the summary when requested non-interactively"
        (kagi-summarize-url "https://www.example.com" t)
        (expect #'kagi--display-summary :not :to-have-been-called)
        (expect #'kagi--insert-summary :to-have-been-called)
        (kagi-test--expect-arg #'kagi--insert-summary 0 :to-equal dummy-output)))))

;;; kagi-test.el ends here
