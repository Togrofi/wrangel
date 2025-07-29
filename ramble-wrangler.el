;;; ramble-wrangler.el --- Extract and categorize org todos using gptel.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Charlie

;;; Commentary:

;; This package provides functionality to extract org-mode todos from buffer text
;; using gptel.el and automatically categorize them into appropriate org files.

;;; Code:

(require 'gptel)
(require 'ramble-wrangler-prompts)

(defcustom ramble-wrangler-files
  '(("inbox" . "inbox.org")
    ("journal" . "journal.org") 
    ("goals" . "goals.org"))
  "Alist mapping category names to org file names."
  :type '(alist :key-type string :value-type string)
  :group 'ramble-wrangler)

(defun ramble-wrangler--parse-json-response-generic (response key error-type)
  "Parse JSON RESPONSE and return list of items using KEY.
ERROR-TYPE is used in error messages for context."
  (condition-case err
      (let* ((json-data (json-parse-string response :object-type 'plist))
             (items (plist-get json-data key)))
        (mapcar (lambda (item)
                  (list :text (plist-get item :text)
                        :category (plist-get item :category)))
                items))
    (error
     (message "Error parsing %s JSON response: %s" error-type err)
     nil)))

(defun ramble-wrangler--parse-json-response (response)
  "Parse JSON RESPONSE and return list of todos."
  (ramble-wrangler--parse-json-response-generic response :todos "todo"))

(defun ramble-wrangler--parse-ideas-json-response (response)
  "Parse JSON RESPONSE and return list of ideas."
  (ramble-wrangler--parse-json-response-generic response :ideas "ideas"))

(defun ramble-wrangler--append-to-org-file-generic (filename content-formatter text)
  "Append TEXT to FILENAME using CONTENT-FORMATTER function for org formatting.
Returns the filename."
  (let ((file-path (expand-file-name filename)))
    (with-temp-buffer
      (when (file-exists-p file-path)
        (insert-file-contents file-path))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (funcall content-formatter text))
      (write-region (point-min) (point-max) file-path))
    filename))

(defun ramble-wrangler--append-to-org-file (filename todo-text)
  "Append TODO-TEXT to FILENAME with proper org formatting."
  (ramble-wrangler--append-to-org-file-generic filename
                                               (lambda (text) (format "* %s\n" text))
                                               todo-text))

(defun ramble-wrangler--append-idea-to-file (category idea-text)
  "Append IDEA-TEXT to a file based on CATEGORY."
  (let ((filename (format "ideas-%s.org" category)))
    (ramble-wrangler--append-to-org-file-generic filename
                                                 (lambda (text) (format "* %s\n" text))
                                                 idea-text)))

(defun ramble-wrangler--append-tldr-to-file (tldr-text)
  "Append TLDR-TEXT to tldr.org file."
  (ramble-wrangler--append-to-org-file-generic "tldr.org"
                                               (lambda (text) 
                                                 (format "* TLDR - %s\n%s\n\n" 
                                                         (format-time-string "%Y-%m-%d %H:%M")
                                                         text))
                                               tldr-text))

(defun ramble-wrangler--process-todos (todos)
  "Process TODOS list and append to appropriate org files."
  (dolist (todo todos)
    (let* ((text (plist-get todo :text))
           (category (plist-get todo :category))
           (filename (cdr (assoc category ramble-wrangler-files))))
      (if filename
          (progn
            (ramble-wrangler--append-to-org-file filename text)
            (message "Added todo to %s: %s" filename text))
        (progn
          (ramble-wrangler--append-to-org-file "inbox.org" text)
          (message "Added todo to inbox.org (unknown category '%s'): %s" category text))))))

(defun ramble-wrangler--process-ideas (ideas)
  "Process IDEAS list and append to category-specific org files."
  (dolist (idea ideas)
    (let* ((text (plist-get idea :text))
           (category (or (plist-get idea :category) "general"))
           (filename (ramble-wrangler--append-idea-to-file category text)))
      (message "Added idea to %s: %s" filename text))))

(defun ramble-wrangler--generic-callback (response info type-name parser-fn processor-fn)
  "Generic callback function to process LLM RESPONSE with INFO context.
TYPE-NAME is used in messages, PARSER-FN parses the response, PROCESSOR-FN processes the results."
  (if (not response)
      (message "%s extraction failed: %s" (capitalize type-name) (plist-get info :status))
    (let ((items (funcall parser-fn response)))
      (if items
          (progn
            (funcall processor-fn items)
            (message "Successfully extracted and categorized %d %s" (length items) type-name))
        (message "No %s found or failed to parse response" type-name)))))

(defun ramble-wrangler--callback (response info)
  "Callback function to process LLM RESPONSE with INFO context."
  (ramble-wrangler--generic-callback response info "todos" 
                                     #'ramble-wrangler--parse-json-response
                                     #'ramble-wrangler--process-todos))

(defun ramble-wrangler--ideas-callback (response info)
  "Callback function to process LLM RESPONSE for ideas extraction with INFO context."
  (ramble-wrangler--generic-callback response info "ideas"
                                     #'ramble-wrangler--parse-ideas-json-response
                                     #'ramble-wrangler--process-ideas))

(defun ramble-wrangler--tldr-callback (response info)
  "Callback function to process LLM RESPONSE for TLDR extraction with INFO context."
  (if (not response)
      (message "TLDR extraction failed: %s" (plist-get info :status))
    (let ((tldr-text (string-trim response)))
      (if (not (string-empty-p tldr-text))
          (progn
            (let ((filename (ramble-wrangler--append-tldr-to-file tldr-text)))
              (message "TLDR saved to %s: %s" filename 
                       (if (> (length tldr-text) 60) 
                           (concat (substring tldr-text 0 60) "...")
                         tldr-text))))
        (message "No TLDR content generated")))))

(defun ramble-wrangler--extract-from-buffer-generic (type-name system-prompt callback-fn &optional buffer)
  "Generic function to extract TYPE-NAME from BUFFER using SYSTEM-PROMPT and CALLBACK-FN."
  (let* ((source-buffer (or buffer (current-buffer)))
         (text-content (with-current-buffer source-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p (string-trim text-content))
        (message "Buffer is empty, nothing to %s" type-name)
      (gptel-request text-content
        :system system-prompt
        :callback callback-fn))))

(defun ramble-wrangler--extract-from-region-generic (type-name system-prompt callback-fn start end)
  "Generic function to extract TYPE-NAME from region using SYSTEM-PROMPT and CALLBACK-FN."
  (if (not (use-region-p))
      (message "No region selected")
    (let ((text-content (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim text-content))
          (message "Selected region is empty")
        (gptel-request text-content
          :system system-prompt
          :callback callback-fn)))))

(defun ramble-wrangler--extract-from-text-generic (type-name system-prompt callback-fn text)
  "Generic function to extract TYPE-NAME from TEXT using SYSTEM-PROMPT and CALLBACK-FN."
  (if (string-empty-p (string-trim text))
      (message "No text provided")
    (gptel-request text
      :system system-prompt
      :callback callback-fn)))

;;;###autoload
(defun ramble-wrangler-todo-from-buffer (&optional buffer)
  "Extract todos from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to extract and categorize org todos,
then appends them to appropriate org files."
  (interactive)
  (ramble-wrangler--extract-from-buffer-generic "extract" ramble-wrangler-system-prompt 
                                                #'ramble-wrangler--callback buffer))

;;;###autoload
(defun ramble-wrangler-todo-from-region (start end)
  "Extract todos from region between START and END using gptel.el."
  (interactive "r")
  (ramble-wrangler--extract-from-region-generic "extract" ramble-wrangler-system-prompt 
                                                #'ramble-wrangler--callback start end))

;;;###autoload
(defun ramble-wrangler-todo-from-text (text)
  "Extract todos from TEXT string using gptel.el."
  (interactive "sText to extract todos from: ")
  (ramble-wrangler--extract-from-text-generic "extract" ramble-wrangler-system-prompt 
                                              #'ramble-wrangler--callback text))

;;;###autoload
(defun ramble-wrangler-ideas-from-buffer (&optional buffer)
  "Extract atomic ideas from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to extract discrete ideas,
then appends them to category-specific org files."
  (interactive)
  (ramble-wrangler--extract-from-buffer-generic "extract" ramble-wrangler-ideas-system-prompt 
                                                #'ramble-wrangler--ideas-callback buffer))

;;;###autoload
(defun ramble-wrangler-ideas-from-region (start end)
  "Extract atomic ideas from region between START and END using gptel.el."
  (interactive "r")
  (ramble-wrangler--extract-from-region-generic "extract" ramble-wrangler-ideas-system-prompt 
                                                #'ramble-wrangler--ideas-callback start end))

;;;###autoload
(defun ramble-wrangler-ideas-from-text (text)
  "Extract atomic ideas from TEXT string using gptel.el."
  (interactive "sText to extract ideas from: ")
  (ramble-wrangler--extract-from-text-generic "extract" ramble-wrangler-ideas-system-prompt 
                                              #'ramble-wrangler--ideas-callback text))

;;;###autoload
(defun ramble-wrangler-tldr-from-buffer (&optional buffer)
  "Create TLDR summary from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to create a concise summary,
then appends it to tldr.org file."
  (interactive)
  (ramble-wrangler--extract-from-buffer-generic "summarize" ramble-wrangler-tldr-system-prompt 
                                                #'ramble-wrangler--tldr-callback buffer))

;;;###autoload
(defun ramble-wrangler-tldr-from-region (start end)
  "Create TLDR summary from region between START and END using gptel.el."
  (interactive "r")
  (ramble-wrangler--extract-from-region-generic "summarize" ramble-wrangler-tldr-system-prompt 
                                                #'ramble-wrangler--tldr-callback start end))

;;;###autoload
(defun ramble-wrangler-tldr-from-text (text)
  "Create TLDR summary from TEXT string using gptel.el."
  (interactive "sText to create TLDR from: ")
  (ramble-wrangler--extract-from-text-generic "summarize" ramble-wrangler-tldr-system-prompt 
                                              #'ramble-wrangler--tldr-callback text))

(provide 'ramble-wrangler)

;;; ramble-wrangler.el ends here
