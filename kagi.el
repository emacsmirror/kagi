;;; kagi.el --- Kagi API integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 - 2024 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 16 Dec 2023
;; Package-Version: 0.6
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6") (shell-maker "0.46.1"))
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
;;       Kagi's LLM offering, as a shell inspired by
;;       [xenodium's chatgpt-shell].
;; Universal Summarizer
;;       Summarizes texts, webpages, videos and more.

;; Both functions are accessed through an [API].

;; [xenodium's chatgpt-shell] <https://github.com/xenodium/chatgpt-shell>

;; [API] <https://help.kagi.com/kagi/api/overview.html>

;;; Code:

(require 'markdown-mode)
(require 'shell-maker)

(defcustom kagi-api-token nil
  "The Kagi API token.

The token can be generated inside your account at
https://kagi.com/settings?p=api"
  :type '(choice string function)
  :group 'kagi)

(defcustom kagi-fastgpt-api-url "https://kagi.com/api/v0/fastgpt"
  "The Kagi FastGPT API entry point."
  :type '(choice string function)
  :group 'kagi)

(defcustom kagi-summarizer-api-url "https://kagi.com/api/v0/summarize"
  "The Kagi Summarizer API entry point."
  :type '(choice string function)
  :group 'kagi)

(defvar kagi--fastgpt-prompts '()
  "List of prompts that were defined with `define-kagi-fastgpt-prompt'.")

(defmacro define-kagi-fastgpt-prompt (symbol-name prompt &optional name)
  "Define a command SYMBOL-NAME that executes the given PROMPT.

PROMPT can be a string or a function returning a string. The
function may take one argument: whether the command was called
interactively or not. This can be used to alter the prompt based
on how the command was called. E.g. a non-interactive version
could contain an instruction to say either Yes or No. See
`kagi-proofread' for an example.

When PROMPT contains %s, it will be replaced with the region (if
active), the (narrowed) buffer content of the selected buffer or
a manually entered prompt.

The NAME is also shown as an option when `kagi-fastgpt-prompt' is
called interactively, to select the corresponding prompt. When no
NAME is given, the SYMBOL-NAME is shown instead."
  `(progn
     (push (cons (or ,name (symbol-name ',symbol-name)) ,prompt) kagi--fastgpt-prompts)
     (defun ,symbol-name (text &optional interactive-p)
       (interactive (list (kagi--get-text-for-prompt) t))
       (let* ((prompt-template (if (functionp ,prompt)
                                   (funcall ,prompt interactive-p)
                                 ,prompt))
              (expanded-prompt (kagi--fastgpt-expand-prompt-placeholders
                                prompt-template
                                (lambda () text))))
         (kagi-fastgpt-prompt
          expanded-prompt
          nil
          interactive-p)))))

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

(defvar kagi--languages '(("Bulgarian" . "BG")
                          ("Czech" . "CS")
                          ("Danish" . "DA")
                          ("German" . "DE")
                          ("Greek" . "EL")
                          ("English" . "EN")
                          ("Spanish" . "ES")
                          ("Estonian" . "ET")
                          ("Finnish" . "FI")
                          ("French" . "FR")
                          ("Hungarian" . "HU")
                          ("Indonesian" . "ID")
                          ("Italian" . "IT")
                          ("Japanese" . "JA")
                          ("Korean" . "KO")
                          ("Lithuanian" . "LT")
                          ("Latvian" . "LV")
                          ("Norwegian" . "NB")
                          ("Dutch" . "NL")
                          ("Polish" . "PL")
                          ("Portuguese" . "PT")
                          ("Romanian" . "RO")
                          ("Russian" . "RU")
                          ("Slovak" . "SK")
                          ("Slovenian" . "SL")
                          ("Swedish" . "SV")
                          ("Turkish" . "TR")
                          ("Ukrainian" . "UK")
                          ("Chinese (simplified)" . "ZH")
                          ("Chinese (traditional)" . "ZH-HANT"))
  "Supported languages by the Kagi LLM.")

(defvar kagi--summarizer-languages (append
                                    '(("Document language" . nil))
                                    kagi--languages)
  "Supported languages by the Kagi Universal Summarizer.")

(defvar kagi--language-history nil)

(defcustom kagi-summarizer-default-language nil
  "Default target language of the summary.

The value should be a string of two characters representing the
 language. See variable `kagi--summarizer-languages' for the list
 of language codes."
  :type (append '(choice)
                (mapcar (lambda (lang)
                          `(const :tag ,(format "%s [%s]" (car lang) (cdr lang))
                                  ,(cdr lang)))
                        kagi--summarizer-languages))
  :group 'kagi)

(defcustom kagi-summarizer-cache t
  "Determines whether the Summarizer should cache results.

Repetitive requests on the same text won't be charged if caching
is enabled (the default). For sensitive texts, you may opt for
disabling the cache by setting this item to nil (but subsequent
queries on the same text will be charged.)"
  :type 'boolean
  :group 'kagi)

(defcustom kagi-summarizer-default-summary-format 'summary
  "The summary format that should be returned.

Symbol `summary' returns a paragraph of prose. Symbol `takeaway'
returns a bullet list."
  :type '(choice (const :tag "Paragraph" summary)
                 (const :tag "Bullet-list" takeaway))
  :group 'kagi)

(defface kagi-bold '((t :inherit bold))
  "Face for bold parts in the Kagi output."
  :group 'kagi)

(define-obsolete-face-alias 'kagi-bold nil "0.6")

(defface kagi-italic '((t :inherit italic))
  "Face for italic parts in the Kagi output."
  :group 'kagi)

(define-obsolete-face-alias 'kagi-italic nil "0.6")

(defface kagi-code '((t :inherit fixed-pitch))
  "Face for code parts in the Kagi output."
  :group 'kagi)

(define-obsolete-face-alias 'kagi-code nil "0.6")

(defun kagi--gethash (hash &rest keys)
  "Get the value inside a (nested) HASH following the sequence of KEYS."
  (let ((value hash))
    (dolist (key keys)
      (when (hash-table-p value)
        (setq value (gethash key value))))
    (if (eq value :null)
        nil
      value)))

(defun kagi--format-references (references)
  "Format the REFERENCES as a string.

The REFERENCES is a part of the JSON response, see
https://help.kagi.com/kagi/api/fastgpt.html for more information."
  (string-join
   (seq-map-indexed (lambda (ref i)
                      (let ((title (gethash "title" ref))
                            (snippet (gethash "snippet" ref))
                            (url (gethash "url" ref)))
                        (format "%s %s\n%s\n%s"
                                (format "[%d]" (1+ i))
                                title snippet url)))
                    references)
   "\n\n"))

(defun kagi--apply-markdown-font-lock (s)
  "Apply Markdown formatting with markdown-mode on string S."
  ;; Inspired by this answer at Emacs StackExchange:
  ;; https://emacs.stackexchange.com/a/5408
  (with-temp-buffer
    (insert s)
    (delay-mode-hooks (markdown-mode))
    (font-lock-default-function #'markdown-mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil)
    (buffer-string)))

(defun kagi--fontify-using-faces (s)
  "Set the fontified property in the string S.

This is needed to insert a font-locked string (generated by
`kagi--apply-markdown-font-lock') in a font-lock enabled buffer."
  ;; Inspired by this answer at Emacs StackExchange:
  ;; https://emacs.stackexchange.com/a/5408
  (let ((pos 0)
        (next))
    (while (setq next (next-single-property-change pos 'face s))
      (put-text-property pos next 'font-lock-face (get-text-property pos 'face s) s)
      (setq pos next))
    (add-text-properties 0 (length s) '(fontified t) s)
    s))

(defun kagi--curl-flags ()
  "Collect flags for a `curl' command to call the Kagi API."
  (let ((token (cond ((functionp kagi-api-token) (funcall kagi-api-token))
                     ((stringp kagi-api-token) kagi-api-token)
                     (t (error "No API token configured in variable kagi-api-token")))))
    `("--silent"
      "--header" ,(format "Authorization: Bot %s" token)
      "--header" "Content-Type: application/json"
      "--data" "@-")))

(defun kagi--call-api (object url)
  "Submit the OBJECT to the API end-point at URL.

The OBJECT will be JSON encoded and sent as HTTP POST data."
  (with-temp-buffer
    (insert (json-encode object))
    (let* ((call-process-flags '(nil nil "curl" t t nil))
           (curl-flags (kagi--curl-flags))
           (all-flags (append call-process-flags
                              curl-flags
                              (list url)))
           (return (apply #'call-process-region all-flags)))
      (if (zerop return)
          (buffer-string)
        (error "Call to Kagi API returned with status %s" return)))))

(defun kagi--call-summarizer (obj)
  "Submit a request to the Summarizer API.

The given OBJ is encoded to JSON and used as the request's POST data.

Returns the JSON response as a string. See
https://help.kagi.com/kagi/api/summarizer.html for the
interpretation."
  (kagi--call-api obj kagi-summarizer-api-url))

(defun kagi--build-summarizer-request-object (items)
  "Build a request object for a summary.

Common request elements are returned based on the package's
configuration. The given ITEMS are appended to it, which is a
list of conses."
  (append items
          `(("engine" . ,kagi-summarizer-engine)
            ("summary_type" . ,kagi-summarizer-default-summary-format)
            ("cache" . ,(if kagi-summarizer-cache t nil)))

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
  (let ((new-buffer-name (generate-new-buffer-name buffer-name)))
    (with-current-buffer (get-buffer-create new-buffer-name)
      (insert summary)
      (goto-char 0)
      (text-mode)
      (display-buffer new-buffer-name))))

(defun kagi--insert-summary (summary)
  "Insert the SUMMARY at point."
  (save-excursion
    (insert (substring-no-properties summary))))

(defun kagi--fastgpt (prompt)
  "Submit a PROMPT to FastGPT and return a formatted response string.

This is used by `kagi-fastgpt-prompt' to obtain a result from
FastGPT. Use that function instead for retrieving a result from
Lisp code."
  (let* ((response (kagi--call-api (list (cons 'query prompt)) kagi-fastgpt-api-url))
         (parsed-response (json-parse-string response))
         (output (kagi--gethash parsed-response "data" "output"))
         (references (kagi--gethash parsed-response "data" "references")))
    (if output
        (string-trim (format "%s\n\n%s"
                             output
                             (kagi--format-references references)))
      (if-let ((firsterror (aref (kagi--gethash parsed-response "error") 0)))
          (error (format "%s (%s)"
                         (gethash "msg" firsterror)
                         (gethash "code" firsterror)))
        (error "An error occurred while querying FastGPT")))))

(defun kagi--fastgpt-display-result (result)
  "Display the RESULT of a FastGPT prompt in a new buffer."
  (let ((buffer-name (generate-new-buffer-name "*fastgpt-result*")))
    (with-current-buffer (get-buffer-create buffer-name)
      (save-excursion
        (insert result))
      (markdown-mode)
      (view-mode)
      (display-buffer buffer-name))))

(defun kagi--fastgpt-welcome-message (_config)
  "Return a string to be shown at the start of a new FastGPT shell.

This can be overridden by setting a different function in
`kagi-fastgpt-welcome-function'."
  (format "Welcome to Kagi FastGPT.

- Enter `help' for more info.
- Press `C-c RET' to open a URL.

"))

(defcustom kagi-fastgpt-welcome-function #'kagi--fastgpt-welcome-message
  "A function returning a welcome string.

The function takes one argument: a shell-maker configuration
object (created with `make-shell-maker-config')."
  :type 'function
  :group 'kagi)

(defvar kagi-fastgpt--config
  (make-shell-maker-config
   :name "FastGPT"
   :prompt "fastgpt > "
   :execute-command
   (lambda (command _history callback error-callback)
     (when command
       (condition-case err
           (funcall callback
                    (kagi--fontify-using-faces (kagi-fastgpt-prompt command))
                    nil)
         (json-parse-error (funcall error-callback
                                    (format "Could not parse the server response %s" (cdr err))))
         (error (funcall error-callback (format "An error occurred during the request %s" (cdr err))))))))
  "The FastGPT shell configuration for shell-maker.")

;;; FastGPT shell

;;;###autoload
(defun kagi-fastgpt-shell ()
  "Start an FastGPT shell."
  (interactive)
  (shell-maker-start kagi-fastgpt--config
                     nil
                     kagi-fastgpt-welcome-function))

(defun kagi--get-text-for-prompt ()
  "Return the text to insert in a prompt.

The text is obtained interactively. Typically this is the user
text that gets inserted in a prompt (e.g. translate the
following, proofread the following, etc.).

If the region is active, return the corresponding text.

Otherwise, the user is requested to enter a buffer name or enter
the text manually."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((buffer-or-text (read-buffer (format-prompt "Buffer name or text" nil))))
      (cond ((get-buffer buffer-or-text)
             (with-current-buffer buffer-or-text
               (buffer-string)))
            ((< 0 (length buffer-or-text)) buffer-or-text)
            (t (error "No buffer or text entered"))))))

(defun kagi--fastgpt-expand-prompt-placeholders (prompt text-function)
  "Expand all occurrences of %s in PROMPT with the result of TEXT-FUNCTION.

It gets replaced with the region text, (narrowed) buffer text or
user input."
  (let ((user-text))
    (replace-regexp-in-string (rx (seq "%" anychar))
                              (lambda (match)
                                (pcase match
                                  ("%%" "%")
                                  ("%s" (or user-text
                                            (setq user-text (save-match-data
                                                              (funcall text-function)))))
                                  (_ match)))
                              prompt t t)))

(defvar kagi--fastgpt-prompt-history nil
  "History for `kagi-fastgpt-prompt'.")

(defun kagi--fastgpt-construct-prompt ()
  "Construct a prompt, either a predefined one or entered by the user.

When the selected prompt contains %s, then the value is
interactively obtained from the user (the region, buffer content
or text input)."
  (let* ((prompt-name (completing-read
                       "fastgpt> "
                       kagi--fastgpt-prompts
                       nil nil nil
                       'kagi--fastgpt-prompt-history))
         (prompt-cdr (alist-get prompt-name kagi--fastgpt-prompts prompt-name nil #'string=))
         (prompt-template (if (functionp prompt-cdr) (funcall prompt-cdr) prompt-cdr)))
    (kagi--fastgpt-expand-prompt-placeholders prompt-template (lambda () (kagi--get-text-for-prompt)))))

;;;###autoload
(defun kagi-fastgpt-prompt (prompt &optional insert interactive-p)
  "Feed the given PROMPT to FastGPT.

When PROMPT contains %s, it will be replaced with the region (if
active), the (narrowed) buffer content of the selected buffer or
a manually entered prompt. %s remains unprocessed when
`kagi-fastgpt-prompt' is called non-interactively (when
INTERACTIVE-P is nil). %% becomes % and any other placeholder is
left as-is.

If INSERT is non-nil, the response is inserted at point (if the
buffer is writable).

If INTERACTIVE-P is non-nil, the result is presented either in
the minibuffer for single line outputs, or shown in a separate
buffer.

If INTERACTIVE-P is nil, the result is returned as a
string (suitable for invocations from Emacs Lisp)."
  (interactive (list (kagi--fastgpt-construct-prompt)
                     current-prefix-arg
                     t))
  (let* ((result (kagi--fastgpt prompt))
         (result-lines (length (string-lines result))))
    (cond ((and insert (not buffer-read-only))
           (save-excursion
             (insert result)))
          ((and interactive-p (eql result-lines 1))
           (message result))
          ((and interactive-p (> result-lines 1))
           (kagi--fastgpt-display-result result))
          ((not interactive-p)
           (kagi--apply-markdown-font-lock result)))))

(define-kagi-fastgpt-prompt kagi-fastgpt-prompt-definition
                            "Define the following word: %s"
                            "Definition")

(defun kagi--read-language (prompt)
  "Read a language from the minibuffer interactively.

PROMPT is passed to the corresponding parameters of
`completing-read', refer to its documentation for more info."
  (completing-read prompt kagi--languages
                   nil
                   nil
                   nil
                   kagi--language-history
                   "English"))

;;;###autoload
(defun kagi-translate (text target-language &optional source-language interactive-p)
  "Translate the TEXT to TARGET-LANGUAGE using FastGPT.

The TEXT can be either from the region, a (narrowed) buffer or
entered manually.

With a single universal prefix, also prompt for the SOURCE-LANGUAGE.

When INTERACTIVE-P is nil, the translation is returned as a string.

When non-nil, the translation is shown in the echo area when the
result is short, otherwise it is displayed in a new buffer."
  (interactive
   (list
    (kagi--get-text-for-prompt)
    (kagi--read-language (format-prompt "Target language" nil))
    (when (equal current-prefix-arg '(4))
      (kagi--read-language (format-prompt "Source language" nil)))
    t))
  (let ((prompt (format "Translate the following text %sto %s, return the translation in the target language only:

%s"
                        (if source-language
                            (format "from %s " source-language)
                          "")
                        target-language
                        text)))
    (kagi-fastgpt-prompt prompt nil interactive-p)))

(define-kagi-fastgpt-prompt kagi-proofread
                            (lambda (interactive-p)
                              (format "Proofread the following text for spelling, grammar and stylistic errors. %s


%%s" (if interactive-p "" "Say OK if there are no issues.")))
                            "Proofread")

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

(defun kagi--valid-language-name-p (language)
  "Return non-nil if LANGUAGE is a valid language name."
  (and (stringp language)
       (map-elt kagi--summarizer-languages (capitalize language))))

(defun kagi--valid-language-code-p (language)
  "Return t if LANGUAGE is a valid two letter language code for the summarizer."
  (and (stringp language)
       (seq-contains-p
        (map-values kagi--summarizer-languages)
        (upcase language))))

(defun kagi--summarizer-determine-language (hint)
  "Determine the language for the summary given a language HINT.

The HINT may be a language code (e.g. `DE') or a language
name (e.g. `German'). If as invalid hint is given, it falls back
to `kagi-summarizer-default-language'."
  (cond
   ((kagi--valid-language-code-p hint) (upcase hint))
   ((kagi--valid-language-name-p hint)
    (map-elt kagi--summarizer-languages (capitalize hint)))
   ((kagi--valid-language-code-p kagi-summarizer-default-language)
    kagi-summarizer-default-language)
   (t "EN")))

(defun kagi--valid-engine-p (engine)
  "Return non-nil when the given ENGINE is valid."
  (and (stringp engine)
       (map-elt kagi--summarizer-engines (downcase engine))))

(defun kagi--summarizer-engine (hint)
  "Return a valid engine name based on the name given in HINT."
  (cond ((kagi--valid-engine-p hint) (downcase hint))
        ((kagi--valid-engine-p kagi-summarizer-engine)
         (downcase kagi-summarizer-engine))
        (t "cecil")))

(defun kagi--summarizer-format (hint)
  "Return a valid summary type based on the type given in HINT."
  (let* ((custom-type (cdr (get 'kagi-summarizer-default-summary-format 'custom-type)))
         (choices (mapcar (lambda (e) (car (last e))) custom-type)))
    (cond ((seq-contains-p choices hint) hint)
          ((seq-contains-p choices kagi-summarizer-default-summary-format)
           kagi-summarizer-default-summary-format)
          (t 'summary))))

(defun kagi-summarize (text-or-url &optional language engine format no-cache)
  "Return the summary of the given TEXT-OR-URL.

LANGUAGE is a supported two letter abbreviation of the language,
as defined in `kagi--summarizer-languages'. When nil, the target
is automatically determined.

ENGINE is the name of a supported summarizer engine, as
defined in `kagi--summarizer-engines'.

FORMAT is the summary format, where `summary' returns a paragraph
of text and `takeaway' returns a bullet list.

When NO-CACHE is t, inputs are not retained inside Kagi's
infrastructure. When nil, the default value for
`kagi-summarizer-cache' is used. Set to t for confidential
content."
  (let ((kagi-summarizer-default-language
         (kagi--summarizer-determine-language language))
        (kagi-summarizer-engine
         (kagi--summarizer-engine engine))
        (kagi-summarizer-default-summary-format
         (kagi--summarizer-format format))
        (kagi-summarizer-cache (if no-cache nil kagi-summarizer-cache)))
    (if-let* ((response (if (kagi--url-p text-or-url)
                            (kagi--call-url-summarizer text-or-url)
                          (kagi--call-text-summarizer text-or-url)))
              (parsed-response (json-parse-string response))
              (output (kagi--gethash parsed-response "data" "output")))
        output
      (if-let ((firsterror (aref (kagi--gethash parsed-response "error") 0)))
          (error (format "%s (%s)"
                         (gethash "msg" firsterror)
                         (gethash "code" firsterror)))
        (error "An error occurred while requesting a summary")))))

(defun kagi--get-summarizer-parameters (&optional prompts)
  "Return a list of interactively obtained summarizer parameters.

Some parameters need to be called interactively, however, for
some clients that doesn't make sense. E.g. we don't want to ask
to insert when the region is highlighted. Therefore, PROMPTS is a
list of items that can be prompted interactively. It is
a (possibly empty) list with possible elements \\='prompt-for-insert
or \\='prompt-for-no-cache.

The function returns an alist with parameter names and values, so
each caller can cherry-pick what they need."
  (list
   (cons 'insert
         (and (seq-contains-p prompts 'prompt-for-insert)
              (equal current-prefix-arg '(4))
              (not buffer-read-only)
              (y-or-n-p "Insert summary at point?")))
   (cons 'language
         (when (equal current-prefix-arg '(4))
           (let ((language-table (mapcar (lambda (lang)
                                           (cons
                                            (format "%s" (car lang))
                                            (cdr lang)))
                                         kagi--summarizer-languages)))
             (alist-get
              (completing-read (format-prompt "Output language" "")
                               language-table nil t nil kagi--language-history)
              language-table
              (or kagi-summarizer-default-language "EN")
              nil
              #'string=))))
   (cons 'engine
         (when (equal current-prefix-arg '(4))
           (completing-read (format-prompt "Engine" "")
                            kagi--summarizer-engines nil t kagi-summarizer-engine)))
   (cons 'format
         (when (equal current-prefix-arg '(4))
           (let ((summary-formats '(("Summary" . summary)
                                    ("Bullet-list" . takeaway))))
             (alist-get
              (completing-read (format-prompt "Summary format" "")
                               summary-formats nil t)
              summary-formats
              kagi-summarizer-default-summary-format
              nil
              #'string=))))
   (cons 'no-cache
         (and (seq-contains-p prompts 'prompt-for-no-cache)
              (equal current-prefix-arg '(4))
              (y-or-n-p "Cache the result?")))))

;;;###autoload
(defun kagi-summarize-buffer (buffer &optional insert language engine format no-cache interactive-p)
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

FORMAT is the summary format, where `summary' returns a paragraph
of text and `takeaway' returns a bullet list.

When NO-CACHE is t, inputs are not retained inside Kagi's
infrastructure. When nil, the default value for
`kagi-summarizer-cache' is used. Set to t for confidential
content.

With a single universal prefix argument (`C-u'), the user is
prompted whether the summary has to be inserted at point, which
target LANGUAGE to use, which summarizer ENGINE to use and which
summary FORMAT to use.

INTERACTIVE-P is t when called interactively."
  (interactive (let ((buffer (read-buffer (format-prompt "Buffer" "") nil t))
                     (parameters (kagi--get-summarizer-parameters
                                  '(prompt-for-insert
                                    prompt-for-no-cache))))
                 (list
                  buffer
                  ;; optional parameters
                  (map-elt parameters 'insert)
                  (map-elt parameters 'language)
                  (map-elt parameters 'engine)
                  (map-elt parameters 'format)
                  (map-elt parameters 'no-cache)
                  ;; interactive-p
                  t)))
  (let ((summary (with-current-buffer buffer
                   (kagi-summarize (buffer-string) language engine format no-cache)))
        (summary-buffer-name (with-current-buffer buffer
                               (kagi--summary-buffer-name (buffer-name)))))
    (cond ((and insert (not buffer-read-only)) (kagi--insert-summary summary))
          (interactive-p (kagi--display-summary summary summary-buffer-name))
          (t summary))))

;;;###autoload
(defun kagi-summarize-region (begin end &optional language engine format no-cache)
  "Summarize the region's content marked by BEGIN and END positions.

The summary is always shown in a new buffer.

LANGUAGE is a supported two letter abbreviation of the language,
as defined in `kagi--summarizer-languages'. When nil, the target
is automatically determined.

ENGINE is the name of a supported summarizer engine, as
defined in `kagi--summarizer-engines'.

FORMAT is the summary format, where `summary' returns a paragraph
of text and `takeaway' returns a bullet list.

When NO-CACHE is t, inputs are not retained inside Kagi's
infrastructure. When nil, the default value for
`kagi-summarizer-cache' is used. Set to t for confidential
content.

With a single universal prefix argument (`C-u'), the user is
prompted for which target LANGUAGE to use, which summarizer
ENGINE to use and which summary FORMAT to use."
  (interactive (let ((parameters (kagi--get-summarizer-parameters '(prompt-for-no-cache))))
                 (list (region-beginning) (region-end)
                       (map-elt parameters 'language)
                       (map-elt parameters 'engine)
                       (map-elt parameters 'format)
                       (map-elt parameters 'no-cache))))
  (kagi--display-summary
   (kagi-summarize (buffer-substring-no-properties begin end)
                   language
                   engine
                   format
                   no-cache)
   (kagi--summary-buffer-name (buffer-name))))

;;;###autoload
(defun kagi-summarize-url (url &optional insert language engine format)
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

FORMAT is the summary format, where `summary' returns a paragraph
of text and `takeaway' returns a bullet list.

With a single universal prefix argument (`C-u'), the user is
prompted whether the summary has to be inserted at point, which
target LANGUAGE to use, which summarizer ENGINE to use and which
summary FORMAT to use.

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
   (let ((url (read-string (format-prompt "URL" "")))
         (parameters (kagi--get-summarizer-parameters '(prompt-for-insert))))
     (list
      url
      (map-elt parameters 'insert)
      (map-elt parameters 'language)
      (map-elt parameters 'engine)
      (map-elt parameters 'format))))
  (let ((summary (kagi-summarize url language engine format)))
    (if (and insert (not buffer-read-only))
        (kagi--insert-summary summary)
      (kagi--display-summary
       summary
       (kagi--summary-buffer-name (kagi--get-domain-name url))))))

(provide 'kagi)

;;; kagi.el ends here
