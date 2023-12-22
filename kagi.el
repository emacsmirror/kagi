;;; kagi.el --- Kagi API integration with Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 16 Dec 2023
;; Package-Version: 0.1
;; Package-Requires: ((emacs "28.2") "shell-maker") ; TODO
;; Keywords: convenience
;; URL: https://codeberg.org/bram85/kagi.el

;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2023 Bram Schoenmakers

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

;; TODO

;; API documentation: https://help.kagi.com/kagi/api/fastgpt.html

;;; Code:

(require 'shell-maker)

(defcustom kagi-api-token nil
  "The Kagi API token.

The token can be generated inside your account at https://kagi.com/settings?p=api"
  :type '(choice string function))

(defcustom kagi-api-fastgpt-url "https://kagi.com/api/v0/fastgpt"
  "The Kagi FastGPT API entry point."
  :type '(choice string function))

(defvar kagi--canned-response "{\"meta\":{\"id\":\"2c6ea22b-16ea-4529-87c0-612474125724\",\"node\":\"europe-west4\",\"ms\":3099,\"api_balance\":4.805},\"data\":{\"output\":\"Based on the context provided:\\n\\nKagi search is a premium search engine created by Kagi to be faster than other search engines like Google. Some context mentions using Kagi search instead of Google [1][2]. Emacs allows customization and extension through editing Lisp code. Kagi search could potentially be integrated into Emacs to provide search capabilities without leaving the editor, allowing users to seamlessly search from within Emacs. [3]\",\"tokens\":1036,\"references\":[{\"title\":\"View topic - can dnsmasq suppress AAAA/IPv6 queries? - DD-WRT\",\"snippet\":\"<b>Well</b>, that was <b>a</b> google nightmare! (Actually I use <b>kagi</b>.com now instead of google/startpage, fwiw.) I found nothing except the dnsmasq man ...\",\"url\":\"https://forum.dd-wrt.com/phpBB2/viewtopic.php?t=335561&sid=ca1d16c9ffe0db1a7ddd02ecc57f5a3e\"},{\"title\":\"View topic - [SOLVED: no] can dnsmasq suppress AAAA ... - DD-WRT\",\"snippet\":\"<b>Well</b>, that was <b>a</b> google nightmare! (Actually I use <b>kagi</b>.com now instead of google/startpage, fwiw.) I found nothing except the dnsmasq man ...\",\"url\":\"https://forum.dd-wrt.com/phpBB2/viewtopic.php?p=1294379&sid=cdc0b6cb7e86d7f3a66211f7f81d060f\"},{\"title\":\"Orion is a web browser based on Safari that supports both Chrome ...\",\"snippet\":\" It's from <b>Kagi</b> who are creating <b>a</b> \\\"Premium <b>search</b> engine\\\" currently ... Maybe I will <b>give</b> it <b>a go</b> in the future. Upvote 2. Downvote Reply reply\",\"url\":\"https://www.reddit.com/r/macapps/comments/sat03p/orion_is_a_web_browser_based_on_safari_that/\"}]}}"
  "A test response for debugging purposes.")

(defcustom kagi-debug nil
  "Enable debugging statements."
  :type 'boolean)

(defun kagi--build-curl-command (prompt)
  (let* ((input-obj `((query . ,prompt)))
         (data (json-encode input-obj))
         (token (cond ((functionp kagi-api-token) (funcall kagi-api-token))
                      ((stringp kagi-api-token) kagi-api-token)
                      (t (error "No API token configured in variable kagi-api-token"))))
         (curl-flags `("--silent"
                       ,(format "--header \"Authorization: Bot %s\"" token)
                       "--header \"Content-Type: application/json\""
                       ,(format "--data '%s'" data))))
    (format "curl %s %s"
            (string-join curl-flags " ")
            kagi-api-fastgpt-url)))

(defun kagi--format-output (output)
  ;; Replace Bold tags
  (replace-regexp-in-string "</b>" "\e[0m"
			    (replace-regexp-in-string "<b>" "\e[1m" output)))

(defun kagi--format-references (references)
  (string-join (seq-map-indexed (lambda (ref i)
                                  (let ((title (gethash "title" ref))
                                        (snippet (gethash "snippet" ref))
                                        (url (gethash "url" ref)))
                                    (format "\e[1m[%d]\e[0m %s\n%s\n%s" (1+ i) title (kagi--format-output snippet) url)))
                                references)
               "\n\n"))

(defun kagi--call-fastgpt (prompt)
  (let ((command (kagi--build-curl-command prompt)))
    (if kagi-debug
        kagi--canned-response
      (shell-command-to-string command))))

(defun kagi--process-prompt (prompt)
  (let* ((response (kagi--call-fastgpt prompt))
         (parsed-response (json-parse-string response))
         (data (gethash "data" parsed-response))
         (output (gethash "output" data))
         (references (gethash "references" data)))
    (format "%s\n\n%s" (kagi--format-output output) (kagi--format-references references))))

(defvar kagi-fastgpt--config
  (make-shell-maker-config
   :name "FastGPT"
   :prompt "fastgpt > "
   :execute-command (lambda (command _history callback error-callback)
                      (funcall callback (kagi--process-prompt command) nil)))
  "The FastGPT shell configuration for shell-maker.")

(defun kagi-fastgpt-shell ()
  (interactive)
  (shell-maker-start kagi-fastgpt--config))

(provide 'kagi)

;;; kagi.el ends here
