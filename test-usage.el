;;; test-usage.el --- Test the org-todo-extractor functionality

;; Load the org-todo-extractor
(load-file "org-todo-extractor.el")

;; Test function to demonstrate usage
(defun test-org-todo-extractor ()
  "Test the org-todo-extractor with sample text."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "test-sample.txt")
    (message "Testing org-todo-extractor with sample text...")
    (org-todo-extractor-from-buffer (current-buffer))))

;; Alternative test with direct text
(defun test-org-todo-extractor-direct ()
  "Test with direct text input."
  (interactive)
  (org-todo-extractor-from-text "I need to finish the project report by Friday and also schedule a dentist appointment. Should also start working on learning Python for data analysis."))

(provide 'test-usage)