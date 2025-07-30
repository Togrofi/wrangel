;;; wrangel.el --- Extract and categorize org todos using gptel.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Charlie

;;; Commentary:

;; This package provides functionality to extract org-mode todos from buffer text
;; using gptel.el and automatically categorize them into appropriate org files.

;;; Code:

(require 'gptel)
(require 'wrangel-prompts)
(require 'wrangel-utils)
(require 'cl-lib)
(require 'org-id)

(defcustom wrangel-todo-directory "~/org/"
  "Directory path where todo org files should be stored."
  :type 'string
  :group 'wrangel)

(defcustom wrangel-files
  '(("inbox" . "inbox.org")
    ("journal" . "journal.org") 
    ("goals" . "goals.org"))
  "Alist mapping category names to org file names."
  :type '(alist :key-type string :value-type string)
  :group 'wrangel)

(defcustom wrangel-org-nodes-directory "~/org/zettel/"
  "Directory path where org-node files should be stored."
  :type 'string
  :group 'wrangel)

(defcustom wrangel-digest-file "~/org/wrangel-digest.org"
  "File path where digest entries should be stored."
  :type 'string
  :group 'wrangel)

(defun wrangel--process-todos (todos)
  "Process TODOS list and append to appropriate org files."
  (dolist (todo todos)
    (let* ((text (plist-get todo :text))
           (category (plist-get todo :category))
           (filename (cdr (assoc category wrangel-files))))
      (if filename
          (progn
            (wrangel--append-to-org-file-generic filename #'wrangel--org-todo-formatter text)
            (message "Added todo to %s: %s" filename text))
        (progn
          (wrangel--append-to-org-file-generic "inbox.org" #'wrangel--org-todo-formatter text)
          (message "Added todo to inbox.org (unknown category '%s'): %s" category text))))))

(defun wrangel--create-org-node-for-idea (idea)
  "Create an org-node atomic note for a single IDEA and return the node ID."
  (let* ((title (plist-get idea :title))
         (content (plist-get idea :content))
         (category (or (plist-get idea :category) "general"))
         (tags (plist-get idea :tags))
         (node-id (org-id-new))
         (timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" title))
         (filename (format "%s-%s.org" node-id sanitized-title))
         (file-path (expand-file-name filename wrangel-org-nodes-directory)))
    
    ;; Ensure the org-nodes directory exists
    (wrangel--ensure-directory-exists wrangel-org-nodes-directory)
    
    ;; Create the atomic note file (each idea gets its own file)
    (with-temp-buffer
      ;; Insert the atomic note with org-node structure
      (insert (format "* %s\n" title))
      (insert (format ":PROPERTIES:\n"))
      (insert (format ":ID: %s\n" node-id))
      (insert (format ":CREATED: %s\n" timestamp))
      (insert (format ":CATEGORY: %s\n" category))
      (when tags
        (insert (format ":FILETAGS: %s\n" (mapconcat 'identity tags " "))))
      (insert (format ":END:\n\n"))
      (insert (format "%s\n\n" content))
      
      (write-region (point-min) (point-max) file-path))
    
    ;; Return the node ID for linking
    node-id))

(defun wrangel--process-ideas-as-org-nodes (ideas)
  "Process IDEAS list and create org-node atomic notes, returning list of node IDs."
  (let ((node-ids '()))
    (dolist (idea ideas)
      (let* ((node-id (wrangel--create-org-node-for-idea idea))
             (title (plist-get idea :title)))
        (push (list :id node-id :title title) node-ids)
        (message "Created atomic note: %s" title)))
    (reverse node-ids)))

;;;###autoload
(defun wrangel-todo-from-text (text)
  "Extract todos from TEXT string using gptel.el."
  (wrangel--extract-from-text-generic wrangel-system-prompt 
                                              (wrangel--create-no-save-callback 'todo #'wrangel--parse-todo-json-response) 
                                              text))
;;;###autoload
(defun wrangel-ideas-from-text (text)
  "Extract atomic ideas from TEXT string using gptel.el."
  (wrangel--extract-from-text-generic wrangel-ideas-system-prompt 
                                              (wrangel--create-no-save-callback 'idea #'wrangel--parse-ideas-json-response) 
                                              text))
;;;###autoload
(defun wrangel-tldr-from-text (text)
  "Create TLDR summary from TEXT string using gptel.el."
  (wrangel--extract-from-text-generic wrangel-tldr-system-prompt 
                                              (wrangel--create-no-save-callback 'tldr nil) 
                                              text))

(defvar wrangel--digest-results nil
  "Internal variable to store digest processing results.")

(defun wrangel--digest-callback (response _info step)
  "Callback for digest processing. STEP indicates which command completed."
  (let ((results (or wrangel--digest-results (make-hash-table :test 'equal))))
    (cond
     ((eq step 'tldr)
      (if response
          (puthash 'tldr (string-trim response) results)
        (puthash 'tldr "TLDR generation failed" results)))
     
     ((eq step 'ideas)
      (let* ((ideas (wrangel--parse-ideas-json-response (or response "")))
             (org-node-links (when ideas (wrangel--process-ideas-as-org-nodes ideas))))
        (puthash 'ideas ideas results)
        (puthash 'idea-nodes org-node-links results)))
     
     ((eq step 'todos)
      (let ((todos (wrangel--parse-todo-json-response (or response ""))))
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
         (digest-file (expand-file-name wrangel-digest-file)))
    
    ;; Ensure the digest directory exists
    (let ((digest-dir (file-name-directory digest-file)))
      (wrangel--ensure-directory-exists digest-dir))
    
    (with-temp-buffer
      (when (file-exists-p digest-file)
        (insert-file-contents digest-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      
      ;; Write the digest entry
      (insert (format "* %s\n" timestamp))
      (insert "** TLDR\n")
      (insert (format "%s\n" tldr-text))
      (insert "** Content\n")
      (insert (format "%s\n" (or original-text "Original text not captured")))
      (insert "** Atomic Notes\n")
      (let ((idea-nodes (gethash 'idea-nodes results)))
        (if idea-nodes
            (progn
              (insert (format "Generated %d atomic zettelkasten notes:\n" (length idea-nodes)))
              (dolist (node idea-nodes)
                (let ((node-id (plist-get node :id))
                      (title (plist-get node :title)))
                  (insert (format "- [[id:%s][%s]]\n" node-id title)))))
          (insert "No atomic notes created\n")))
      (insert "** TODOs\n")
      (if todos
          (progn
            (insert (format "Generated %d todos in the following files:\n" (length todos)))
            (dolist (file (delete-dups (or todo-files '())))
              (insert (format "- [[file:%s][%s]]\n" file file))))
        (insert "No todos extracted\n"))
      
      (write-region (point-min) (point-max) digest-file))
    
    ;; Clear results and notify user
    (setq wrangel--digest-results nil)
    (message "Digest entry written to %s with %d ideas and %d todos" 
             digest-file 
             (length (or ideas '())) 
             (length (or todos '())))))

;;;###autoload
(defun wrangel-digest-from-text (text)
  "Create a complete digest entry from TEXT by running tldr, ideas, and
todo extraction.
The results are combined into a single org entry in wrangel-digest.org
with links to generated files."
  (if (not (wrangel--validate-text-content text))
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
                    (let ((todos (wrangel--parse-todo-json-response (or response ""))))
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
