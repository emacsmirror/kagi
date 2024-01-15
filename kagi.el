;;; kagi.el --- Kagi API integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 - 2024 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 16 Dec 2023
;; Package-Version: 0.1
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

(defcustom kagi-stubbed-responses nil
  "Whether the package should return a stubbed response.

To be used for testing purposes, such that no credits are spent
on dummy data."
  :type 'boolean
  :group 'kagi)

(defcustom kagi-fastgpt-api-url "https://kagi.com/api/v0/fastgpt"
  "The Kagi FastGPT API entry point."
  :type '(choice string function)
  :group 'kagi)

(defcustom kagi-summarizer-api-url "https://kagi.com/api/v0/summarize"
  "The Kagi Summarizer API entry point."
  :type '(choice string function)
  :group 'kagi)

(defvar kagi--summarizer-engines
  '(("agnes" . "Friendly, descriptive, fast summary.")
    ("cecil" . "Formal, technical, analytical summary.")
    ("daphne" . "Informal, creative, friendly summary.")
    ("muriel" . "Best-in-class summary using Kagi's enterprise-grade model (at different pricing)."))
  "List of Kagi Summarizer engines.

See `kagi-summarizer-engine' for a brief description per engine.")

(defcustom kagi-summarizer-engine "cecil"
  "Which summary engine to use.

Note that the muriel model is enterprise grade and has different
pricing. Refer to the API documentation for more info at
https://help.kagi.com/kagi/api/summarizer.html."
  :type (append '(choice)
                (mapcar (lambda (engine) `(const :doc ,(cdr engine) ,(car engine)))
                        kagi--summarizer-engines))
  :group 'kagi)

(defvar kagi--summarizer-languages '(("Document language" . nil)
                                     ("Bulgarian [BG]" . "BG")
                                     ("Czech [CZ]" . "CZ")
                                     ("Danish [DA]" . "DA")
                                     ("German [DE]" . "DE")
                                     ("Greek [EL]" . "EL")
                                     ("English [EN]" . "EN")
                                     ("Spanish [ES]" . "ES")
                                     ("Estonian [ET]" . "ET")
                                     ("Finnish [FI]" . "FI")
                                     ("French [FR]" . "FR")
                                     ("Hungarian [HU]" . "HU")
                                     ("Indonesian [ID]" . "ID")
                                     ("Italian [IT]" . "IT")
                                     ("Japanese [JA]" . "JA")
                                     ("Korean [KO]" . "KA")
                                     ("Lithuanian [LT]" . "LT")
                                     ("Latvian [LV]" . "LV")
                                     ("Norwegian [NB]" . "NB")
                                     ("Dutch [NL]" . "NL")
                                     ("Polish [PL]" . "PL")
                                     ("Portuguese [PT]" . "PT")
                                     ("Romanian [RO]" . "RO")
                                     ("Russian [RU]" . "RU")
                                     ("Slovak [SK]" . "SK")
                                     ("Slovenian [SL]" . "SL")
                                     ("Swedish [SV]" . "SV")
                                     ("Turkish [TR]" . "TR")
                                     ("Ukrainian [UK]" . "UK")
                                     ("Chinese (simplified) [ZH]" . "ZH"))
  "Supported languages by the Kagi Universal Summarizer.")

(defcustom kagi-summarizer-default-language nil
  "Default target language of the summary."
  :type (append '(choice)
                (mapcar (lambda (lang) `(const :tag ,(car lang) ,(cdr lang)))
                        kagi--summarizer-languages))
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

(defun kagi--gethash (hash &rest keys)
  "Get the value inside a (nested) HASH following the sequence of KEYS."
  (let ((value hash))
    (dolist (key keys)
      (when (hash-table-p value)
        (setq value (gethash key value))))
    (if (eq value :null)
        nil
      value)))

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

(defvar kagi--fastgpt-stubbed-response
  "{\"data\":{\"output\":\"<b>Test</b> response.\"}}"
  "Stubbed response for the Kagi FastGPT endpoint.")

(defvar kagi--summarizer-stubbed-response
  "{\"data\":{\"output\":\"```Test``` response.\"}}"
  "Stubbed response for the Kagi Summarizer endpoint.")

(defun kagi--call-process-region (&rest args)
  "`call-process-region' wrapper.

Calls `call-process-region' with ARGS, unless
`kagi-stubbed-responses' is non-nil.

In that case, this function will not do an actual API call but
return some dummy data."
  (if (not kagi-stubbed-responses)
      (apply #'call-process-region args)
    (let* ((url (car (last args)))
           (response (cond
                      ((string= url kagi-fastgpt-api-url)
                       kagi--fastgpt-stubbed-response)
                      ((string= url kagi-summarizer-api-url)
                       kagi--summarizer-stubbed-response)
                      (t ""))))
      (erase-buffer)
      (insert response)
      0)))

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
                              (list kagi-fastgpt-api-url)))
           (return (apply #'kagi--call-process-region all-flags)))
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
                              (list kagi-summarizer-api-url)))
           (return (apply #'kagi--call-process-region all-flags)))
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
          (when kagi-summarizer-default-language
            `(("target_language" . ,kagi-summarizer-default-language)))))

(defconst kagi--summarizer-min-input-words 50
  "The minimal amount of words that the text input should have.")

(defun kagi--summarizer-input-valid-p (input)
  "Return t if INPUT is valid for a summary."
  (>= (length (split-string input)) kagi--summarizer-min-input-words))

(defun kagi--call-text-summarizer (text)
  "Return a response object from the Summarizer with the TEXT summary."
  (if (kagi--summarizer-input-valid-p text)
      (let ((request-obj (kagi--build-summarizer-request-object
                          `(("text" . ,text)))))
        (kagi--call-summarizer request-obj))
    (error "Input text is invalid, it may be too short (less than %d words)"
           kagi--summarizer-min-input-words)))

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

(defun kagi--insert-summary (summary)
  "Insert the SUMMARY at point."
  (save-excursion
    (insert (substring-no-properties summary))))

(defun kagi--process-prompt (prompt)
  "Submit a PROMPT to FastGPT and process the API response.

Returns a formatted string to be displayed by the shell."
  (let* ((response (kagi--call-fastgpt prompt))
         (parsed-response (json-parse-string response))
         (output (kagi--gethash parsed-response "data" "output"))
         (references (kagi--gethash parsed-response "data" "references")))
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
  (string-match-p (rx (seq bos "http" (? "s") "://" (+ (not space)) eos)) s))

;;;###autoload
(defun kagi-summarize (text-or-url &optional language engine)
  "Return the summary of the given TEXT-OR-URL.

LANGUAGE is a supported two letter abbreviation of the language,
as defined in `kagi--summarizer-languages'. When nil, the target
is automatically determined.

ENGINE is the name of a supported summarizer engine, as
defined in `kagi--summarizer-engines'."

  (let* ((kagi-summarizer-default-language
          (upcase (or language kagi-summarizer-default-language)))
         (kagi-summarizer-engine
          (downcase (or engine kagi-summarizer-engine))))
    (if-let* ((response (if (kagi--url-p text-or-url)
                            (kagi--call-url-summarizer text-or-url)
                          (kagi--call-text-summarizer text-or-url)))
              (parsed-response (json-parse-string response))
              (output (kagi--gethash parsed-response "data" "output")))
        (kagi--format-output output)
      (if-let ((firsterror (aref (kagi--gethash parsed-response "error") 0)))
          (error (format "%s (%s)"
                         (gethash "msg" firsterror)
                         (gethash "code" firsterror)))
        (error "An error occurred while requesting a summary")))))

(defun kagi--get-summarizer-parameters (&optional prompt-insert-p)
  "Return a list of interactively obtained summarizer parameters.

Not all commands need to insert a summary, so only prompt for
this when PROMPT-INSERT-P is non-nil."
  (append
   (list
    (and prompt-insert-p
         (equal current-prefix-arg '(4))
         (y-or-n-p "Insert summary at point?")))
   (list
    (when (equal current-prefix-arg '(4))
      (alist-get
       (completing-read (format-prompt "Output language" "")
                        kagi--summarizer-languages nil t)
       kagi--summarizer-languages
       (or kagi-summarizer-default-language "EN")
       nil
       #'string=)))
   (list
    (when (equal current-prefix-arg '(4))
      (completing-read (format-prompt "Engine" "")
                       kagi--summarizer-engines nil t kagi-summarizer-engine)))))

;;;###autoload
(defun kagi-summarize-buffer (buffer &optional insert language engine)
  "Summarize the BUFFER's content and show it in a new window.

By default, the summary is shown in a new buffer.

When INSERT is non-nil, the summary will be inserted at point. In
case the current buffer is read-only, the summary will be shown
in a separate buffer anyway.

LANGUAGE is a supported two letter abbreviation of the language,
as defined in `kagi--summarizer-languages'. When nil, the target
is automatically determined.

ENGINE is the name of a supported summarizer engine, as
defined in `kagi--summarizer-engines'.

With a single universal prefix argument (`C-u'), the user is
prompted whether the summary has to be inserted at point, which
target LANGUAGE to use and which summarizer ENGINE to use."
  (interactive (cons
                (read-buffer (format-prompt "Buffer" "") nil t)
                (kagi--get-summarizer-parameters t)))
  (let ((summary (with-current-buffer buffer
                   (kagi-summarize (buffer-string) language engine)))
        (summary-buffer-name (with-current-buffer buffer
                               (kagi--summary-buffer-name (buffer-name)))))
    (if (and insert (not buffer-read-only))
        (kagi--insert-summary summary)
      (kagi--display-summary summary summary-buffer-name))))

;;;###autoload
(defun kagi-summarize-region (begin end &optional language engine)
  "Summarize the region's content marked by BEGIN and END positions.

The summary is always shown in a new buffer.

LANGUAGE is a supported two letter abbreviation of the language,
as defined in `kagi--summarizer-languages'. When nil, the target
is automatically determined.

ENGINE is the name of a supported summarizer engine, as
defined in `kagi--summarizer-engines'.

With a single universal prefix argument (`C-u'), the user is
prompted for which target LANGUAGE to use and which summarizer
ENGINE to use."
  (interactive (append
                (list (region-beginning) (region-end))
                (kagi--get-summarizer-parameters)))
  (kagi--display-summary
   (kagi-summarize (buffer-substring-no-properties begin end)
                   language
                   engine)
   (kagi--summary-buffer-name (buffer-name))))

;;;###autoload
(defun kagi-summarize-url (url &optional insert language engine)
  "Show the summary of the content behind the given URL.

By default, the summary is shown in a new buffer.

When INSERT is non-nil, the summary will be inserted at point. In
case the current buffer is read-only, the summary will be shown
in a separate buffer anyway.

LANGUAGE is a supported two letter abbreviation of the language,
as defined in `kagi--summarizer-languages'. When nil, the target
is automatically determined.

ENGINE is the name of a supported summarizer engine, as
defined in `kagi--summarizer-engines'.

With a single universal prefix argument (`C-u'), the user is
prompted whether the summary has to be inserted at point, which
target LANGUAGE to use and which summarizer ENGINE to use.

According to the Kagi API documentation, the following media
types are supported:

- Text web pages, articles, and forum threads
- PDF documents (.pdf)
- PowerPoint documents (.pptx)
- Word documents (.docx)
- Audio files (mp3/wav)
- YouTube URLs
- Scanned PDFs and images (OCR)"
  (interactive
   (cons
    (read-string (format-prompt "URL" ""))
    (kagi--get-summarizer-parameters t)))
  (let ((summary (kagi-summarize url language engine)))
    (if (and insert (not buffer-read-only))
        (kagi--insert-summary summary)
      (kagi--display-summary
       summary
       (kagi--summary-buffer-name (kagi--get-domain-name url))))))

(provide 'kagi)

;;; kagi.el ends here
