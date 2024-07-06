;;; claude.el --- Interact with Claude API in Emacs buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a function to interact with the Anthropic Claude API
;; directly from Emacs buffers.

;;; Code:

(require 'json)
(require 'url)

(defgroup claude nil
  "Customization group for Claude Chat."
  :group 'communication)

(defcustom claude-api-key nil
  "API key for Claude API."
  :type 'string
  :group 'claude)

(defcustom claude-model "claude-3-5-sonnet-20240620"
  "The Claude model to use for chat."
  :type 'string
  :group 'claude)

(defvar claude-buffer "*Claude Chat*"
  "The name of the buffer for Claude chat.")

(defun claude-get-buffer-content ()
  "Get the content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun claude-send-request (prompt buffer-content)
  "Send a request to Claude API with PROMPT and BUFFER-CONTENT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,claude-api-key)
            ("anthropic-version" . "2023-06-01")))
         (url-request-data
          (json-encode
           `((system . ,(format "You are an AI assistant in an Emacs buffer. You and the user will have a discussion about the contents of this buffer. Contents: %s" buffer-content))
             (model . ,claude-model)
             (max_tokens . 4096)
             (messages . [((role . "user")
                           (content . ,prompt))])))))
    (with-current-buffer
        (url-retrieve-synchronously "https://api.anthropic.com/v1/messages")
      (goto-char (point-min))
      ;; (write-region (point-min) (point-max) "/tmp/claude-resp.json" nil)
      (re-search-forward "^$")
      (delete-region (point-min) (point))
      (json-read))))

(defun claude-insert-response (response)
  "Insert Claude's RESPONSE into the chat buffer."
  (with-current-buffer (get-buffer-create claude-buffer)
    (goto-char (point-max))
    (insert "\n\nClaude: ")
    ;; (insert "\nDebug - Full response:\n")
    ;; (insert (format "%S" response))
    ;; (insert "\n\nParsed response:\n")
    (let ((content (cdr (assoc 'content response))))
      ;; (insert (format "Content: %S\n" content))
      (if (and (vectorp content) (> (length content) 0))
          (let ((text (cdr (assoc 'text (aref content 0)))))
            ;; (insert (format "Text: %S\n" text))
            (if text
                (insert text)
              (insert "Error: Unable to extract text from response.")))
        (insert "Error: Unexpected response structure.")))))

(defun claude-prompt (prompt)
  "Send PROMPT to Claude and display the response."
  (interactive "sAsk Claude: ")
  (if (not claude-api-key)
      (message "Please set claude-api-key first")
    (let ((buffer-content (claude-get-buffer-content)))
      (with-current-buffer (get-buffer-create claude-buffer)
        (goto-char (point-max))
        (insert "\n\nYou: " prompt))
      (let ((response (claude-send-request prompt buffer-content)))
        (claude-insert-response response)
        (display-buffer claude-buffer)))))

(provide 'claude)

;;; claude.el ends here
