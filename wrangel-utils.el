;;; wrangel-utils.el --- Utility functions for wrangel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Charlie

;;; Commentary:

;; Utility functions used throughout the wrangel package for common operations
;; like text formatting, JSON parsing, file handling, and validation.

;;; Code:

(require 'json)
(require 'cl-lib)

;; Declare variables that are defined in main wrangel file
(defvar wrangel-todo-directory)
(defvar wrangel-files)

(defun wrangel--ensure-directory-exists (directory)
  "Ensure DIRECTORY exists, creating it if necessary."
  (unless (file-directory-p directory)
    (make-directory directory t)))

(defun wrangel--validate-text-content (text)
  "Return non-nil if TEXT is not empty after trimming whitespace."
  (not (string-empty-p (string-trim text))))

(defun wrangel--org-todo-formatter (text)
  "Format TEXT as an org-mode TODO item."
  (format "* %s\n" text))

(defun wrangel--handle-response-failure (info error-context)
  "Handle failed response with INFO context and ERROR-CONTEXT string."
  (message "%s failed: %s" error-context (plist-get info :status)))

(defun wrangel--truncate-message (text max-length)
  "Truncate TEXT to MAX-LENGTH characters, adding ellipsis if needed."
  (if (> (length text) max-length)
      (concat (substring text 0 max-length) "...")
    text))

(defun wrangel--parse-json-response-generic (response key error-type)
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

(defun wrangel--parse-todo-json-response (response)
  "Parse JSON RESPONSE and return list of todos."
  (wrangel--parse-json-response-generic response :todos "todo"))

(defun wrangel--parse-ideas-json-response (response)
  "Parse JSON RESPONSE and return list of ideas with proper field mapping."
  (condition-case err
      (let* ((json-data (json-parse-string response :object-type 'plist))
             (items (plist-get json-data :ideas)))
        (mapcar (lambda (item)
                  (list :title (plist-get item :title)
                        :content (plist-get item :content)
                        :category (plist-get item :category)
                        :tags (plist-get item :tags)))
                items))
    (error
     (message "Error parsing ideas JSON response: %s" err)
     nil)))

(defun wrangel--append-to-org-file-generic (filename content-formatter text &optional directory)
  "Append TEXT to FILENAME using CONTENT-FORMATTER function for org formatting.
Uses DIRECTORY if provided, otherwise uses wrangel-todo-directory.
Returns the filename."
  (let* ((base-dir (or directory wrangel-todo-directory))
         (file-path (expand-file-name filename base-dir)))
    ;; Ensure the directory exists
    (wrangel--ensure-directory-exists base-dir)
    (with-temp-buffer
      (when (file-exists-p file-path)
        (insert-file-contents file-path))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (funcall content-formatter text))
      (write-region (point-min) (point-max) file-path))
    filename))

(defun wrangel--collect-todo-files (todos)
  "Collect filenames that todos were written to and return as list."
  (if (not todos)
      '()
    (let ((files '()))
      (dolist (todo todos)
        (let* ((category (plist-get todo :category))
               (filename (cdr (assoc category wrangel-files))))
          (when filename
            (cl-pushnew filename files :test #'string=))
          (unless filename
            (cl-pushnew "inbox.org" files :test #'string=))))
      (reverse files))))

(defun wrangel--callback-no-save (response info extractor-type parser-fn)
  "Generic callback function to process LLM RESPONSE without saving to files.
 EXTRACTOR-TYPE is used in messages, PARSER-FN processes the response."
  (if (not response)
      (wrangel--handle-response-failure info (format "%s extraction" extractor-type))
    (cond
     ;; TLDR case - response is plain text
     ((eq extractor-type 'tldr)
      (let ((tldr-text (string-trim response)))
        (if (wrangel--validate-text-content tldr-text)
            (message "TLDR: %s" tldr-text)
          (message "No TLDR content generated"))))
     ;; JSON response cases - todos and ideas
     (t
      (let ((items (funcall parser-fn response)))
        (if items
            (progn
              (message "Successfully extracted %d %ss:" (length items) extractor-type)
              (dolist (item items)
                (cond
                 ((eq extractor-type 'todo)
                  (let ((text (plist-get item :text))
                        (category (plist-get item :category)))
                    (message "- [%s] %s" category text)))
                 ((eq extractor-type 'idea)
                  (let ((title (plist-get item :title))
                        (content (plist-get item :content))
                        (category (plist-get item :category)))
                    (message "- [%s] %s: %s" category title 
                             (wrangel--truncate-message content 60)))))))
          (message "No %ss found or failed to parse response" extractor-type)))))))

(defun wrangel--extract-from-text-generic (system-prompt callback-fn text)
  "Generic function to extract from TEXT using SYSTEM-PROMPT and CALLBACK-FN."
  (if (not (wrangel--validate-text-content text))
      (message "No text provided")
    (gptel-request text
      :system system-prompt
      :callback callback-fn)))

(defun wrangel--create-no-save-callback (extractor-type parser-fn)
  "Create a no-save callback function for EXTRACTOR-TYPE using PARSER-FN."
  (lambda (response info)
    (wrangel--callback-no-save response info extractor-type parser-fn)))

(provide 'wrangel-utils)

;;; wrangel-utils.el ends here
