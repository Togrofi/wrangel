;;; wrangel.el --- Extract and categorize org todos using gptel.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Charlie

;;; Commentary:

;; This package provides functionality to extract org-mode todos from buffer text
;; using gptel.el and automatically categorize them into appropriate org files.

;;; Code:

(require 'gptel)
(require 'wrangel-prompts)
(require 'cl-lib)

(defcustom wrangel-files
  '(("inbox" . "inbox.org")
    ("journal" . "journal.org") 
    ("goals" . "goals.org"))
  "Alist mapping category names to org file names."
  :type '(alist :key-type string :value-type string)
  :group 'wrangel)

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

(defun wrangel--parse-json-response (response)
  "Parse JSON RESPONSE and return list of todos."
  (wrangel--parse-json-response-generic response :todos "todo"))

(defun wrangel--parse-ideas-json-response (response)
  "Parse JSON RESPONSE and return list of ideas."
  (wrangel--parse-json-response-generic response :ideas "ideas"))

(defun wrangel--append-to-org-file-generic (filename content-formatter text)
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

(defun wrangel--append-to-org-file (filename todo-text)
  "Append TODO-TEXT to FILENAME with proper org formatting."
  (wrangel--append-to-org-file-generic filename
                                               (lambda (text) (format "* %s\n" text))
                                               todo-text))

(defun wrangel--append-idea-to-file (category idea-text)
  "Append IDEA-TEXT to a file based on CATEGORY."
  (let ((filename (format "ideas-%s.org" category)))
    (wrangel--append-to-org-file-generic filename
                                                 (lambda (text) (format "* %s\n" text))
                                                 idea-text)))

(defun wrangel--append-tldr-to-file (tldr-text)
  "Append TLDR-TEXT to tldr.org file."
  (wrangel--append-to-org-file-generic "tldr.org"
                                               (lambda (text) 
                                                 (format "* TLDR - %s\n%s\n\n" 
                                                         (format-time-string "%Y-%m-%d %H:%M")
                                                         text))
                                               tldr-text))

(defun wrangel--process-todos (todos)
  "Process TODOS list and append to appropriate org files."
  (dolist (todo todos)
    (let* ((text (plist-get todo :text))
           (category (plist-get todo :category))
           (filename (cdr (assoc category wrangel-files))))
      (if filename
          (progn
            (wrangel--append-to-org-file filename text)
            (message "Added todo to %s: %s" filename text))
        (progn
          (wrangel--append-to-org-file "inbox.org" text)
          (message "Added todo to inbox.org (unknown category '%s'): %s" category text))))))

(defun wrangel--process-ideas (ideas)
  "Process IDEAS list and append to category-specific org files."
  (dolist (idea ideas)
    (let* ((text (plist-get idea :text))
           (category (or (plist-get idea :category) "general"))
           (filename (wrangel--append-idea-to-file category text)))
      (message "Added idea to %s: %s" filename text))))

(defun wrangel--generic-callback (response info parser-fn processor-fn)
  "Generic callback function to process LLM RESPONSE with INFO context.
PARSER-FN parses the response, PROCESSOR-FN processes the results."
  (if (not response)
      (message "Extraction failed: %s" (plist-get info :status))
    (let ((items (funcall parser-fn response)))
      (if items
          (progn
            (funcall processor-fn items)
            (message "Successfully extracted and processed %d items" (length items)))
        (message "No items found or failed to parse response")))))

(defun wrangel--callback (response info)
  "Callback function to process LLM RESPONSE with INFO context."
  (wrangel--generic-callback response info
                                     #'wrangel--parse-json-response
                                     #'wrangel--process-todos))

(defun wrangel--ideas-callback (response info)
  "Callback function to process LLM RESPONSE for ideas extraction with
INFO context."
  (wrangel--generic-callback response info
                                     #'wrangel--parse-ideas-json-response
                                     #'wrangel--process-ideas))

(defun wrangel--tldr-callback (response info)
  "Callback function to process LLM RESPONSE for TLDR extraction with INFO context."
  (if (not response)
      (message "TLDR extraction failed: %s" (plist-get info :status))
    (let ((tldr-text (string-trim response)))
      (if (not (string-empty-p tldr-text))
          (progn
            (let ((filename (wrangel--append-tldr-to-file tldr-text)))
              (message "TLDR saved to %s: %s" filename 
                       (if (> (length tldr-text) 60) 
                           (concat (substring tldr-text 0 60) "...")
                         tldr-text))))
        (message "No TLDR content generated")))))

(defun wrangel--extract-from-buffer-generic (system-prompt callback-fn &optional buffer)
  "Generic function to extract from BUFFER using SYSTEM-PROMPT and CALLBACK-FN."
  (let* ((source-buffer (or buffer (current-buffer)))
         (text-content (with-current-buffer source-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p (string-trim text-content))
        (message "Buffer is empty, nothing to extract")
      (gptel-request text-content
        :system system-prompt
        :callback callback-fn))))

(defun wrangel--extract-from-region-generic (system-prompt callback-fn start end)
  "Generic function to extract from region using SYSTEM-PROMPT and CALLBACK-FN."
  (if (not (use-region-p))
      (message "No region selected")
    (let ((text-content (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim text-content))
          (message "Selected region is empty")
        (gptel-request text-content
          :system system-prompt
          :callback callback-fn)))))

(defun wrangel--extract-from-text-generic (system-prompt callback-fn text)
  "Generic function to extract from TEXT using SYSTEM-PROMPT and CALLBACK-FN."
  (if (string-empty-p (string-trim text))
      (message "No text provided")
    (gptel-request text
      :system system-prompt
      :callback callback-fn)))

;;;###autoload
(defun wrangel-todo-from-buffer (&optional buffer)
  "Extract todos from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to extract and categorize org todos,
then appends them to appropriate org files."
  (interactive)
  (wrangel--extract-from-buffer-generic wrangel-system-prompt 
                                                #'wrangel--callback buffer))

;;;###autoload
(defun wrangel-todo-from-region (start end)
  "Extract todos from region between START and END using gptel.el."
  (interactive "r")
  (wrangel--extract-from-region-generic wrangel-system-prompt 
                                                #'wrangel--callback start end))

;;;###autoload
(defun wrangel-todo-from-text (text)
  "Extract todos from TEXT string using gptel.el."
  (interactive "sText to extract todos from: ")
  (wrangel--extract-from-text-generic wrangel-system-prompt 
                                              #'wrangel--callback text))

;;;###autoload
(defun wrangel-ideas-from-buffer (&optional buffer)
  "Extract atomic ideas from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to extract discrete ideas,
then appends them to category-specific org files."
  (interactive)
  (wrangel--extract-from-buffer-generic wrangel-ideas-system-prompt 
                                                #'wrangel--ideas-callback buffer))

;;;###autoload
(defun wrangel-ideas-from-region (start end)
  "Extract atomic ideas from region between START and END using gptel.el."
  (interactive "r")
  (wrangel--extract-from-region-generic wrangel-ideas-system-prompt 
                                                #'wrangel--ideas-callback start end))

;;;###autoload
(defun wrangel-ideas-from-text (text)
  "Extract atomic ideas from TEXT string using gptel.el."
  (interactive "sText to extract ideas from: ")
  (wrangel--extract-from-text-generic wrangel-ideas-system-prompt 
                                              #'wrangel--ideas-callback text))

;;;###autoload
(defun wrangel-tldr-from-buffer (&optional buffer)
  "Create TLDR summary from BUFFER (or current buffer) using gptel.el.
Sends the buffer content to an LLM to create a concise summary,
then appends it to tldr.org file."
  (interactive)
  (wrangel--extract-from-buffer-generic wrangel-tldr-system-prompt 
                                                #'wrangel--tldr-callback buffer))

;;;###autoload
(defun wrangel-tldr-from-region (start end)
  "Create TLDR summary from region between START and END using gptel.el."
  (interactive "r")
  (wrangel--extract-from-region-generic wrangel-tldr-system-prompt 
                                                #'wrangel--tldr-callback start end))

;;;###autoload
(defun wrangel-tldr-from-text (text)
  "Create TLDR summary from TEXT string using gptel.el."
  (interactive "sText to create TLDR from: ")
  (wrangel--extract-from-text-generic wrangel-tldr-system-prompt 
                                              #'wrangel--tldr-callback text))

(defvar wrangel--digest-results nil
  "Internal variable to store digest processing results.")

(defun wrangel--collect-todo-files (todos)
  "Collect filenames that todos were written to and return as list."
  (let ((files '()))
    (dolist (todo todos)
      (let* ((category (plist-get todo :category))
             (filename (cdr (assoc category wrangel-files))))
        (when filename
          (cl-pushnew filename files :test #'string=))
        (unless filename
          (cl-pushnew "inbox.org" files :test #'string=))))
    (reverse files)))

(defun wrangel--digest-callback (response _info step)
  "Callback for digest processing. STEP indicates which command completed."
  (let ((results (or wrangel--digest-results (make-hash-table :test 'equal))))
    (cond
     ((eq step 'tldr)
      (if response
          (puthash 'tldr (string-trim response) results)
        (puthash 'tldr "TLDR generation failed" results)))
     
     ((eq step 'ideas)
      (let ((ideas (wrangel--parse-ideas-json-response (or response ""))))
        (puthash 'ideas ideas results)))
     
     ((eq step 'todos)
      (let ((todos (wrangel--parse-json-response (or response ""))))
        (puthash 'todos todos results)
        (puthash 'todo-files (wrangel--collect-todo-files todos) results))))
    
    (setq wrangel--digest-results results)
    
    ;; Check if all three steps are complete
    (when (and (gethash 'tldr results)
               (gethash 'ideas results)
               (gethash 'todos results))
      (wrangel--write-digest-entry results))))

(defun wrangel--write-digest-entry (results)
  "Write complete digest entry to ramble-digest.org file."
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (tldr-text (gethash 'tldr results))
         (ideas (gethash 'ideas results))
         (todos (gethash 'todos results))
         (todo-files (gethash 'todo-files results))
         (original-text (gethash 'original-text results))
         (digest-file "wrangel-digest.org"))
    
    (with-temp-buffer
      (when (file-exists-p digest-file)
        (insert-file-contents digest-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      
      ;; Write the digest entry
      (insert (format "* %s\n" timestamp))
      (insert "** TLDR\n")
      (insert (format "%s\n\n" tldr-text))
      
      (insert "** Content\n")
      (insert (format "%s\n\n" (or original-text "Original text not captured")))
      
      (insert "** Ideas\n")
      (if ideas
          (dolist (idea ideas)
            (let ((text (plist-get idea :text))
                  (category (plist-get idea :category)))
              (insert (format "- %s (category: %s)\n" text category))))
        (insert "No ideas extracted\n"))
      (insert "\n")
      
      (insert "** TODOs\n")
      (if todos
          (progn
            (insert (format "Generated %d todos in the following files:\n" (length todos)))
            (dolist (file (delete-dups todo-files))
              (insert (format "- [[file:%s][%s]]\n" file file))))
        (insert "No todos extracted\n"))
      (insert "\n")
      
      (write-region (point-min) (point-max) digest-file))
    
    ;; Clear results and notify user
    (setq wrangel--digest-results nil)
    (message "Digest entry written to %s with %d ideas and %d todos" 
             digest-file (length ideas) (length todos))))

;;;###autoload
(defun wrangel-digest-from-text (text)
  "Create a complete digest entry from TEXT by running tldr, ideas, and
todo extraction.
The results are combined into a single org entry in wrangel-digest.org
with links to generated files."
  (interactive "sText to create digest from: ")
  (if (string-empty-p (string-trim text))
      (message "No text provided for digest")
    (progn
      ;; Store original text for the digest entry
      (setq wrangel--digest-results (make-hash-table :test 'equal))
      (puthash 'original-text text wrangel--digest-results)
      
      ;; Start all three extractions
      (gptel-request text
        :system wrangel-tldr-system-prompt
        :callback (lambda (response info)
                    (wrangel--digest-callback response info 'tldr)))
      
      (gptel-request text
        :system wrangel-ideas-system-prompt
        :callback (lambda (response info)
                    (wrangel--digest-callback response info 'ideas)))
      
      (gptel-request text
        :system wrangel-system-prompt
        :callback (lambda (response info)
                    ;; Process todos first to create files
                    (let ((todos (wrangel--parse-json-response (or response ""))))
                      (when todos
                        (wrangel--process-todos todos)))
                    ;; Then handle digest callback
                    (wrangel--digest-callback response info 'todos)))
      
      (message "Starting digest processing for text (%d characters)..." (length text)))))

;;;###autoload
(defun wrangel-digest-from-buffer (&optional buffer)
  "Create a complete digest entry from BUFFER (or current buffer)."
  (interactive)
  (let* ((source-buffer (or buffer (current-buffer)))
         (text-content (with-current-buffer source-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (wrangel-digest-from-text text-content)))

;;;###autoload
(defun wrangel-digest-from-region (start end)
  "Create a complete digest entry from region between START and END."
  (interactive "r")
  (if (not (use-region-p))
      (message "No region selected")
    (let ((text-content (buffer-substring-no-properties start end)))
      (wrangel-digest-from-text text-content))))

(provide 'wrangel)

;;; wrangel.el ends here
