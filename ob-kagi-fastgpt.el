;;; ob-kagi-fastgpt.el --- Kagi FastGPT integration for Org Babel -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 12 April 2024
;; Package-Version: 0.4
;; Package-Requires: ((emacs "29.1") (shell-maker "0.46.1"))
;; Keywords: terminals wp
;; URL: https://codeberg.org/bram85/kagi.el

;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2024 Bram Schoenmakers

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

;; This package provides Org Babel support for Kagi FastGPT blocks.

;;; Code:

(require 'kagi)
(require 'ob)
(require 'org)

(defvar org-babel-default-header-args:kagi-fastgpt
  '((:results . "output"))
  "Default values for the kagi-fastgpt header arguments.")

(defun org-babel-execute:kagi-fastgpt (prompt _params)
  "Execute a PROMPT in an Org Babel block with Kagi FastGPT."
  (kagi-fastgpt-prompt prompt))

(defun ob-kagi-fastgpt-setup ()
  "Set up support for Kagi FastGPT in Org Babel."
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((kagi-fastgpt . t))))
  (add-to-list 'org-src-lang-modes '("kagi-fastgpt" . text)))

(provide 'ob-kagi-fastgpt)

;;; ob-kagi-fastgpt.el ends here)
