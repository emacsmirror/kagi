;;; kagi.el --- Kagi API integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 16 Dec 2023
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (shell-maker "0.44.1"))
;; Keywords: terminals wp
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

;; This Emacs package provides the following functionalities from the
;; Kagi search engine:

;; FastGPT
;;       Kagi's open source LLM offering, as a shell inspired by
;;       [xenodium's chatgpt-shell].
;; Universal Summarizer
;;       Summarizes texts, webpages, videos and more.

;; Both functions are accessed through an [API].

;; [xenodium's chatgpt-shell] <https://github.com/xenodium/chatgpt-shell>

;; [API] <https://help.kagi.com/kagi/api/overview.html>

;;; Code:

(require 'shell-maker)

(defcustom kagi-api-token nil
  "The Kagi API token.

The token can be generated inside your account at
https://kagi.com/settings?p=api"
  :type '(choice string function)
  :group 'kagi)

(defcustom kagi-api-fastgpt-url "https://kagi.com/api/v0/fastgpt"
  "The Kagi FastGPT API entry point."
  :type '(choice string function)
  :group 'kagi)

(defcustom kagi-api-summarizer-url "https://kagi.com/api/v0/summarize"
  "The Kagi Summarizer API entry point."
  :type '(choice string function)
  :group 'kagi)

(defcustom kagi-summarizer-engine "cecil"
  "Which summary engine to use.

- cecil :: Friendly, descriptive, fast summary.
- agnes :: Formal, technical, analytical summary.
- daphne :: Informal, creative, friendly summary.
- muriel :: Best-in-class summary using Kagi's enterprise-grade model.

Note that the muriel model is enterprise grade and has different
pricing. Refer to the API documentation for more info at
https://help.kagi.com/kagi/api/summarizer.html."
  :type '(choice
          (const "agnes")
          (const "cecil")
          (const "daphne")
          (const "muriel"))
  :group 'kagi)

(defcustom kagi-summarize-default-language nil
  "Default target language of the summary."
  :type '(choice
          (const :tag "Document language" nil)
          (const :tag "Bulgarian" "BG")
          (const :tag "Czech" "CZ")
          (const :tag "Danish" "DA")
          (const :tag "German" "DE")
          (const :tag "Greek" "EL")
          (const :tag "English" "EN")
          (const :tag "Spanish" "ES")
          (const :tag "Estonian" "ET")
          (const :tag "Finnish" "FI")
          (const :tag "French" "FR")
          (const :tag "Hungarian" "HU")
          (const :tag "Indonesian" "ID")
          (const :tag "Italian" "IT")
          (const :tag "Japanese" "JA")
          (const :tag "Korean" "KO")
          (const :tag "Lithuanian" "LT")
          (const :tag "Latvian" "LV")
          (const :tag "Norwegian" "NB")
          (const :tag "Dutch" "NL")
          (const :tag "Polish" "PL")
          (const :tag "Portuguese" "PT")
          (const :tag "Romanian" "RO")
          (const :tag "Russian" "RU")
          (const :tag "Slovak" "SK")
          (const :tag "Slovenian" "SL")
          (const :tag "Swedish" "SV")
          (const :tag "Turkish" "TR")
          (const :tag "Ukrainian" "UK")
          (const :tag "Chinese (simplified)" "ZH"))
  :group 'kagi)

(defcustom kagi-summarizer-cache t
  "Determines whether the Summarizer should cache results.

Repetitive queries won't be charged if caching is enabled (the
default). For sensitive texts, you may opt for disabling the
cache by setting this item to nil (but subsequent queries on the
same text will be charged.)"
  :type 'boolean
  :group 'kagi)

(defface kagi-bold '((t :inherit bold))
  "Face for bold parts in the Kagi output."
  :group 'kagi)

(defface kagi-code '((t :inherit fixed-pitch))
  "Face for code parts in the Kagi output."
  :group 'kagi)

(defconst kagi--markup-to-face
  '(("<b>" "</b>" 'kagi-bold)
    ("```" "```" 'kagi-code))
  "Contains a mapping from markup elements to faces.")

(defun kagi--convert-markup-to-faces (string)
  "Convert markup elements inside STRING to faces.

Fun fact: the initial version of this function was generated by
FastGPT with the following prompt:

  write an Emacs Lisp function that accepts a string with html
  bold tags, and returns a string with bold face text properties
  applied for the tags content and the tags removed"
  (with-temp-buffer
    (insert string)
    (dolist (entry kagi--markup-to-face)
      (cl-destructuring-bind (start end face) entry
        (goto-char (point-min))
        (let ((regexp (rx (seq (literal start)
                               (group (*? any))
                               (literal end)))))
          (while (re-search-forward regexp nil t)
            (let ((escaped-replacement (string-replace "\\" "\\\\" (match-string 1))))
              (replace-match (propertize escaped-replacement 'font-lock-face face) t nil))))))
    (buffer-string)))

(defun kagi--format-output (output)
  "Format the OUTPUT by replacing markup elements to proper faces."
  (kagi--convert-markup-to-faces output))

(defun kagi--format-reference-index (i)
  "Format the index of reference number I."
  (propertize (format "[%d]" i) 'font-lock-face 'bold))

(defun kagi--format-references (references)
  "Format the REFRENCES as a string.

The REFERENCES is a part of the JSON response, see
https://help.kagi.com/kagi/api/fastgpt.html for more information."
  (string-join
   (seq-map-indexed (lambda (ref i)
                      (let ((title (gethash "title" ref))
                            (snippet (gethash "snippet" ref))
                            (url (gethash "url" ref)))
                        (format "%s %s\n%s\n%s"
                                (kagi--format-reference-index (1+ i))
                                title
                                (kagi--convert-markup-to-faces snippet) url)))
                    references)
   "\n\n"))

(defun kagi--curl-flags ()
  "Collect flags for a `curl' command to call the Kagi API."
  (let ((token (cond ((functionp kagi-api-token) (funcall kagi-api-token))
                     ((stringp kagi-api-token) kagi-api-token)
                     (t (error "No API token configured in variable kagi-api-token")))))
    `("--silent"
      "--header" ,(format "Authorization: Bot %s" token)
      "--header" "Content-Type: application/json"
      "--data" "@-")))

(defun kagi--call-fastgpt (prompt)
  "Submit the given PROMPT to the FastGPT API.

Returns the JSON response as a string. See
https://help.kagi.com/kagi/api/fastgpt.html for the
interpretation."
  (with-temp-buffer
    (insert (json-encode `((query . ,prompt))))
    (let* ((call-process-flags '(nil nil "curl" t t nil))
           (curl-flags (kagi--curl-flags))
           (all-flags (append call-process-flags
                              curl-flags
                              (list kagi-api-fastgpt-url)))
           (return (apply #'call-process-region all-flags)))
      (if (zerop return)
          (buffer-string)
        (error "Call to FastGPT API returned with status %s" return)))))

(defun kagi--call-summarizer (obj)
  "Submit a request to the Summarizer API.

The given OBJ is encoded to JSON and used as the request's POST data.

Returns the JSON response as a string. See
https://help.kagi.com/kagi/api/summarizer.html for the
interpretation."
  (with-temp-buffer
    (insert (json-encode obj))
    (let* ((call-process-flags '(nil nil "curl" t t nil))
           (curl-flags (kagi--curl-flags))
           (all-flags (append call-process-flags
                              curl-flags
                              (list kagi-api-summarizer-url)))
           (return (apply #'call-process-region all-flags)))
      (if (zerop return)
          (buffer-string)
        (error "Call to Summarizer API returned with status %s" return)))))

(defun kagi--build-summarizer-request-object (items)
  "Build a request object for a summary.

Common request elements are returned based on the package's
configuration. The given ITEMS are appended to it, which is a
list of conses."
  (append items
          `(("engine" . ,kagi-summarizer-engine)
            ("summary_type" . "summary")
            ("cache" . ,kagi-summarizer-cache))

          ;; prevent a nil in the result list, causing (json-encode)
          ;; to generate a wrong request object.
          (when kagi-summarize-default-language
            `(("target_language" . ,kagi-summarize-default-language)))))

(defun kagi--call-text-summarizer (text)
  "Return a response object from the Summarizer with the TEXT summary."
  (let ((request-obj (kagi--build-summarizer-request-object
                      `(("text" . ,text)))))
    (kagi--call-summarizer request-obj )))

(defun kagi--call-url-summarizer (url)
  "Return a response object from the Summarizer with the URL summary."
  (let ((request-obj (kagi--build-summarizer-request-object
                      `(("url" . ,url)))))
    (kagi--call-summarizer request-obj)))

(defun kagi--display-summary (summary buffer-name)
  "Display the SUMMARY in a buffer called BUFFER-NAME."
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert summary)
    (goto-char 0)
    (text-mode)
    (display-buffer buffer-name)))

(defun kagi--process-prompt (prompt)
  "Submit a PROMPT to FastGPT and process the API response.

Returns a formatted string to be displayed by the shell."
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
   :execute-command
   (lambda (command _history callback error-callback)
     (condition-case err
         (funcall callback (kagi--process-prompt command) nil)
       (json-parse-error (funcall error-callback
                                  (format "Could not parse the server response %s" (cdr err))))
       (error (funcall error-callback (format "An error occurred during the request %s" (cdr err)))))))
  "The FastGPT shell configuration for shell-maker.")

;;; FastGPT shell

;;;###autoload
(defun kagi-fastgpt-shell ()
  "Start an FastGPT shell."
  (interactive)
  (shell-maker-start kagi-fastgpt--config))

;;; Summarizer

(defun kagi--get-domain-name (url)
  "Return the domain name of the given URL."
  (string-match
   (rx (seq bos
            (? (seq "http"
                    (? "s")
                    "://"))
            (? "www.")
            ;; the domain name
            (group (seq (+ (not "/"))))))
   url)

  (if-let ((domain-name (match-string 1 url)))
      domain-name
    "URL"))

(defun kagi--summary-buffer-name (hint)
  "Generate a name for the summary buffer, HINT will be part of the name."
  (format "%s (summary)" hint))

(defun kagi--url-p (s)
  "Non-nil if string S is a URL."
  (string-match-p (rx (seq "http" (? "s") "://")) s))

;;;###autoload
(defun kagi-summarize (text-or-url)
  "Return the summary of the given TEXT-OR-URL."
  (let* ((response (if (kagi--url-p text-or-url)
                       (kagi--call-url-summarizer text-or-url)
                     (kagi--call-text-summarizer text-or-url)))
         (parsed-response (json-parse-string response))
         (data (gethash "data" parsed-response))
         (output (gethash "output" data)))
    (kagi--format-output output)))

;;;###autoload
(defun kagi-summarize-buffer (buffer)
  "Summarize the BUFFER's content and show it in a new window."
  (interactive "b")
  (with-current-buffer buffer
    (kagi--display-summary
     (kagi-summarize (buffer-string))
     (kagi--summary-buffer-name (buffer-name)))))

;;;###autoload
(defun kagi-summarize-region (begin end)
  "Summarize the region's content marked by BEGIN and END positions.

Shows the summary in a new window."
  (interactive "r")
  (kagi--display-summary
   (kagi-summarize (buffer-substring-no-properties begin end))
   (kagi--summary-buffer-name (buffer-name))))

;;;###autoload
(defun kagi-summarize-url (url)
  "Show the summary of the content behind the given URL.

According to the API documentation, the following media types are
supported:

- Text web pages, articles, and forum threads
- PDF documents (.pdf)
- PowerPoint documents (.pptx)
- Word documents (.docx)
- Audio files (mp3/wav)
- YouTube URLs
- Scanned PDFs and images (OCR)"
  (interactive "sURL: ")
  (kagi--display-summary
   (kagi-summarize url)
   (kagi--summary-buffer-name (kagi--get-domain-name url))))

(provide 'kagi)

;;; kagi.el ends here
