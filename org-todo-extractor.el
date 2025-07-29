;;; org-todo-extractor.el --- Extract and categorize org todos using gptel.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Charlie
;; Keywords: org, todo, llm, gptel
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))

;;; Commentary:

;; This package provides functionality to extract org-mode todos from buffer text
;; using gptel.el and automatically categorize them into appropriate org files.

;;; Code:

(require 'gptel)
(require 'org-todo-extractor-prompts)

(defcustom org-todo-extractor-files
  '(("inbox" . "inbox.org")
    ("journal" . "journal.org") 
    ("goals" . "goals.org"))
  "Alist mapping category names to org file names."
  :type '(alist :key-type string :value-type string)
  :group 'org-todo-extractor)

(defun org-todo-extractor--parse-json-response (response)
  "Parse JSON RESPONSE and return list of todos."
  (condition-case err
      (let* ((json-data (json-parse-string response :object-type 'plist))
             (todos (plist-get json-data :todos)))
        (mapcar (lambda (todo)
                  (list :text (plist-get todo :text)
                        :category (plist-get todo :category)))
                todos))
    (error
     (message "Error parsing JSON response: %s" err)
     nil)))

(defun org-todo-extractor--parse-ideas-json-response (response)
  "Parse JSON RESPONSE and return list of ideas."
  (condition-case err
      (let* ((json-data (json-parse-string response :object-type 'plist))
             (ideas (plist-get json-data :ideas)))
        (mapcar (lambda (idea)
                  (list :text (plist-get idea :text)
                        :category (plist-get idea :category)))
                ideas))
    (error
     (message "Error parsing ideas JSON response: %s" err)
     nil)))

(defun org-todo-extractor--append-to-org-file (filename todo-text)
  "Append TODO-TEXT to FILENAME with proper org formatting."
  (let ((file-path (expand-file-name filename)))
    (with-temp-buffer
      (when (file-exists-p file-path)
        (insert-file-contents file-path))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "* %s\n" todo-text))
      (write-region (point-min) (point-max) file-path))))

(defun org-todo-extractor--append-idea-to-file (category idea-text)
  "Append IDEA-TEXT to a file based on CATEGORY."
  (let ((filename (format "ideas-%s.org" category))
        (file-path (expand-file-name (format "ideas-%s.org" category))))
    (with-temp-buffer
      (when (file-exists-p file-path)
        (insert-file-contents file-path))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "* %s\n" idea-text))
      (write-region (point-min) (point-max) file-path))
    filename))

(defun org-todo-extractor--append-tldr-to-file (tldr-text)
  "Append TLDR-TEXT to tldr.org file."
  (let ((filename "tldr.org")
        (file-path (expand-file-name "tldr.org")))
    (with-temp-buffer
      (when (file-exists-p file-path)
        (insert-file-contents file-path))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "* TLDR - %s\n%s\n\n" 
                      (format-time-string "%Y-%m-%d %H:%M")
                      tldr-text))
      (write-region (point-min) (point-max) file-path))
    filename))

(defun org-todo-extractor--process-todos (todos)
  "Process TODOS list and append to appropriate org files."
  (dolist (todo todos)
    (let* ((text (plist-get todo :text))
           (category (plist-get todo :category))
           (filename (cdr (assoc category org-todo-extractor-files))))
      (if filename
          (progn
            (org-todo-extractor--append-to-org-file filename text)
            (message "Added todo to %s: %s" filename text))
        (progn
          (org-todo-extractor--append-to-org-file "inbox.org" text)
          (message "Added todo to inbox.org (unknown category '%s'): %s" category text))))))

(defun org-todo-extractor--process-ideas (ideas)
  "Process IDEAS list and append to category-specific org files."
  (dolist (idea ideas)
    (let* ((text (plist-get idea :text))
           (category (or (plist-get idea :category) "general"))
           (filename (org-todo-extractor--append-idea-to-file category text)))
      (message "Added idea to %s: %s" filename text))))

(defun org-todo-extractor--callback (response info)
  "Callback function to process LLM RESPONSE with INFO context."
  (if (not response)
      (message "Todo extraction failed: %s" (plist-get info :status))
    (let ((todos (org-todo-extractor--parse-json-response response)))
      (if todos
          (progn
            (org-todo-extractor--process-todos todos)
            (message "Successfully extracted and categorized %d todos" (length todos)))
        (message "No todos found or failed to parse response")))))

(defun org-todo-extractor--ideas-callback (response info)
  "Callback function to process LLM RESPONSE for ideas extraction with INFO context."
  (if (not response)
      (message "Ideas extraction failed: %s" (plist-get info :status))
    (let ((ideas (org-todo-extractor--parse-ideas-json-response response)))
      (if ideas
          (progn
            (org-todo-extractor--process-ideas ideas)
            (message "Successfully extracted and categorized %d ideas" (length ideas)))
        (message "No ideas found or failed to parse response")))))

(defun org-todo-extractor--tldr-callback (response info)
  "Callback function to process LLM RESPONSE for TLDR extraction with INFO context."
  (if (not response)
      (message "TLDR extraction failed: %s" (plist-get info :status))
    (let ((tldr-text (string-trim response)))
      (if (not (string-empty-p tldr-text))
          (progn
            (let ((filename (org-todo-extractor--append-tldr-to-file tldr-text)))
              (message "TLDR saved to %s: %s" filename 
                       (if (> (length tldr-text) 60) 
                           (concat (substring tldr-text 0 60) "...")
                         tldr-text))))
        (message "No TLDR content generated")))))

;;;###autoload
(defun org-todo-extractor-from-buffer (&optional buffer)
  "Extract todos from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to extract and categorize org todos,
then appends them to appropriate org files."
  (interactive)
  (let* ((source-buffer (or buffer (current-buffer)))
         (text-content (with-current-buffer source-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p (string-trim text-content))
        (message "Buffer is empty, nothing to extract")
      (gptel-request text-content
        :system org-todo-extractor-system-prompt
        :callback #'org-todo-extractor--callback))))

;;;###autoload
(defun org-todo-extractor-from-region (start end)
  "Extract todos from region between START and END using gptel.el."
  (interactive "r")
  (if (not (use-region-p))
      (message "No region selected")
    (let ((text-content (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim text-content))
          (message "Selected region is empty")
        (gptel-request text-content
          :system org-todo-extractor-system-prompt
          :callback #'org-todo-extractor--callback)))))

;;;###autoload
(defun org-todo-extractor-from-text (text)
  "Extract todos from TEXT string using gptel.el."
  (interactive "sText to extract todos from: ")
  (if (string-empty-p (string-trim text))
      (message "No text provided")
    (gptel-request text
      :system org-todo-extractor-system-prompt
      :callback #'org-todo-extractor--callback)))

;;;###autoload
(defun org-todo-extractor-ideas-from-buffer (&optional buffer)
  "Extract atomic ideas from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to extract discrete ideas,
then appends them to category-specific org files."
  (interactive)
  (let* ((source-buffer (or buffer (current-buffer)))
         (text-content (with-current-buffer source-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p (string-trim text-content))
        (message "Buffer is empty, nothing to extract")
      (gptel-request text-content
        :system org-todo-extractor-ideas-system-prompt
        :callback #'org-todo-extractor--ideas-callback))))

;;;###autoload
(defun org-todo-extractor-ideas-from-region (start end)
  "Extract atomic ideas from region between START and END using gptel.el."
  (interactive "r")
  (if (not (use-region-p))
      (message "No region selected")
    (let ((text-content (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim text-content))
          (message "Selected region is empty")
        (gptel-request text-content
          :system org-todo-extractor-ideas-system-prompt
          :callback #'org-todo-extractor--ideas-callback)))))

;;;###autoload
(defun org-todo-extractor-ideas-from-text (text)
  "Extract atomic ideas from TEXT string using gptel.el."
  (interactive "sText to extract ideas from: ")
  (if (string-empty-p (string-trim text))
      (message "No text provided")
    (gptel-request text
      :system org-todo-extractor-ideas-system-prompt
      :callback #'org-todo-extractor--ideas-callback)))

;;;###autoload
(defun org-todo-extractor-tldr-from-buffer (&optional buffer)
  "Create TLDR summary from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to create a concise summary,
then appends it to tldr.org file."
  (interactive)
  (let* ((source-buffer (or buffer (current-buffer)))
         (text-content (with-current-buffer source-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p (string-trim text-content))
        (message "Buffer is empty, nothing to summarize")
      (gptel-request text-content
        :system org-todo-extractor-tldr-system-prompt
        :callback #'org-todo-extractor--tldr-callback))))

;;;###autoload
(defun org-todo-extractor-tldr-from-region (start end)
  "Create TLDR summary from region between START and END using gptel.el."
  (interactive "r")
  (if (not (use-region-p))
      (message "No region selected")
    (let ((text-content (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim text-content))
          (message "Selected region is empty")
        (gptel-request text-content
          :system org-todo-extractor-tldr-system-prompt
          :callback #'org-todo-extractor--tldr-callback)))))

;;;###autoload
(defun org-todo-extractor-tldr-from-text (text)
  "Create TLDR summary from TEXT string using gptel.el."
  (interactive "sText to create TLDR from: ")
  (if (string-empty-p (string-trim text))
      (message "No text provided")
    (gptel-request text
      :system org-todo-extractor-tldr-system-prompt
      :callback #'org-todo-extractor--tldr-callback)))

(provide 'org-todo-extractor)

;;; org-todo-extractor.el ends here
