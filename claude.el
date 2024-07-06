;;; claude.el --- Interact with Claude API in Emacs buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides functions to interact with the Anthropic Claude API
;; directly from Emacs buffers, including rewriting selected regions.

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

(defun claude-send-request (prompt content &optional is-code-rewrite)
  "Send a request to Claude API with PROMPT and CONTENT.
If IS-CODE-REWRITE is non-nil, use a system prompt for code rewriting."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,claude-api-key)
            ("anthropic-version" . "2023-06-01")))
         (system-prompt
          (if is-code-rewrite
              "You are an AI assistant helping to rewrite code in an Emacs buffer. Your response will be directly inserted into the code, replacing the original content. Provide only the modified code without any additional explanations or markdown formatting."
            "You are an AI assistant in an Emacs buffer. You and the user will have a discussion about the contents of this text."))
         (url-request-data
          (json-encode
           `((system . ,system-prompt)
             (model . ,claude-model)
             (max_tokens . 4096)
             (messages . [((role . "user")
                           (content . ,(format "%s\n\nCode or text to modify:\n%s" prompt content)))])))))
    (with-current-buffer
        (url-retrieve-synchronously "https://api.anthropic.com/v1/messages")
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point-min) (point))
      (json-read))))

(defun claude-insert-response (response)
  "Insert Claude's RESPONSE into the chat buffer."
  (with-current-buffer (get-buffer-create claude-buffer)
    (goto-char (point-max))
    (insert "\n\nClaude: ")
    (let ((content (cdr (assoc 'content response))))
      (if (and (vectorp content) (> (length content) 0))
          (let ((text (cdr (assoc 'text (aref content 0)))))
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

(defun claude-rewrite-region (start end prompt)
  "Rewrite the region between START and END based on PROMPT sent to Claude API."
  (interactive "r\nsPrompt for rewriting: ")
  (if (not claude-api-key)
      (user-error "Please set claude-api-key first")
    (let ((region-content (buffer-substring-no-properties start end)))
      (let ((response (claude-send-request prompt region-content t)))  ; Pass t for is-code-rewrite
        (let ((new-content (claude-extract-content response)))
          (if new-content
              (progn
                (delete-region start end)
                (goto-char start)
                (insert new-content)
                (message "Region rewritten based on Claude's response."))
            (message "Failed to get a valid response from Claude.")))))))

(defun claude-extract-content (response)
  "Extract the content from Claude's RESPONSE."
  (let ((content (cdr (assoc 'content response))))
    (when (and (vectorp content) (> (length content) 0))
      (let ((text (cdr (assoc 'text (aref content 0)))))
        (when text
          (string-trim text))))))

(provide 'claude)

;;; claude.el ends here
