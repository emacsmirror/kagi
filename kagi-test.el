;;; kagi-test.el --- Kagi API tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Bram Schoenmakers

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

(require 'kagi)

(defun kagi-test--dummy-output (text)
  (format "{\"data\":{\"output\":\"%s\"}}" text))

(buttercup-define-matcher-for-binary-function
    :to-be-equal-including-properties equal-including-properties
  :expect-match-phrase "Expected `%A' to be equal (incl. properties) to %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' not to be equal (incl. properties) to %b, but `%A' was %a.")

(describe "kagi.el"
  (describe "Kagi FastGPT"
    (it "converts bold markup ** to a bold face"
      (spy-on #'kagi--call-fastgpt :and-return-value (kagi-test--dummy-output "**bold**"))
      (expect (kagi-fastgpt-prompt "foo")
              :to-be-equal-including-properties
              (propertize "bold" 'font-lock-face 'kagi-bold))))

  (xdescribe "Kagi Summarizer"
    (it "contains a spec with an expectation"
      (expect t :to-be t))))

;;; kagi-test.el ends here
