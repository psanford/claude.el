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

(defcustom claude-model "claude-3-7-sonnet-latest"
  "The Claude model to use for chat."
  :type 'string
  :group 'claude)

(defcustom claude-include-history t
  "Whether to include conversation history in subsequent messages."
  :type 'boolean
  :group 'claude)

(defvar-local claude-conversation-history nil
  "Buffer-local variable to store conversation history.
Each element is a plist with :role and :content keys.")

(defvar claude-current-request nil
  "The current ongoing request to Claude API.")

(defvar claude-rewrite-region nil
  "The region to be rewritten.")

(defvar claude-rewrite-buffer nil
  "The buffer containing the region to be rewritten.")

(defvar claude-rewrite-overlay nil
  "Overlay used for highlighting the region being rewritten.")

(defun claude-get-buffer-content ()
  "Get the content of the current buffer."
  (encode-coding-string
   (buffer-substring-no-properties (point-min) (point-max))
   'utf-8))

(defun claude-get-chat-buffer-name (source-buffer)
  "Get the name of the chat buffer associated with SOURCE-BUFFER."
  (format "*Claude Chat: %s*" (buffer-name source-buffer)))

(defun claude-get-or-create-chat-buffer (source-buffer)
  "Get or create a chat buffer for SOURCE-BUFFER."
  (let ((chat-buffer-name (claude-get-chat-buffer-name source-buffer)))
    (or (get-buffer chat-buffer-name)
        (with-current-buffer (get-buffer-create chat-buffer-name)
          (setq-local claude-conversation-history nil)
          (current-buffer)))))

(defun claude-send-request (prompt content &optional is-code-rewrite)
  "Send a request to Claude API with PROMPT and CONTENT.
If IS-CODE-REWRITE is non-nil, use a system prompt for code rewriting."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,claude-api-key)
            ("anthropic-version" . "2023-06-01")
            ("anthropic-beta" . "output-128k-2025-02-19,max-tokens-3-5-sonnet-2024-07-15")
            ("Accept" . "text/event-stream")))
         (system-prompt
          (if is-code-rewrite
              "You are an AI assistant helping to rewrite code in an Emacs buffer. Your response will be directly inserted into the code, replacing the original content. Provide only the modified code without any additional explanations or markdown formatting."
            "You are an AI assistant in an Emacs buffer. You and the user will have a discussion about the contents of this text."))
         (messages (if (and claude-include-history claude-conversation-history (not is-code-rewrite))
                      (vconcat claude-conversation-history
                              `[((role . "user")
                                 (content . ,(format "%s\n\nCode or text to modify:\n%s" prompt content)))])
                    `[((role . "user")
                       (content . ,(format "%s\n\nCode or text to modify:\n%s" prompt content)))]))
         (url-request-data
          (encode-coding-string
           (json-encode
            `((system . ,system-prompt)
              (model . ,claude-model)
              (max_tokens . 4096)
              (stream . t)
              (messages . ,messages)))
           'utf-8)))
    (setq claude-current-request
          (url-retrieve "https://api.anthropic.com/v1/messages"
                        (lambda (status)
                          ;; Add error handling here?
                          ;; (message "claud-response-callback %s" status))
                          )
                        nil
                        t))
    (with-current-buffer claude-current-request
      (add-hook 'after-change-functions #'claude-process-response nil t))))

(defun claude-process-response (begin end length)
  "Process the streaming response as it comes in.
This function is called by the `after-change-functions` hook."
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "^data: " end t)
      (let* ((start (point))
             (line-end (line-end-position))
             (line-text (buffer-substring-no-properties start line-end)))
        ;; Only process if we have a complete line
        (when (and (> (length line-text) 0)
                   (string-match-p "\n" (buffer-substring-no-properties line-end (min (+ line-end 1) (point-max)))))
          (let* ((json-object-type 'plist)
                 (json-array-type 'vector)
                 (json-key-type 'keyword)
                 (data (json-read-from-string line-text)))
            (cond
             ((plist-get data :delta)
              (when-let* ((delta (plist-get data :delta))
                          (type (plist-get delta :type))
                          (text (plist-get delta :text))
                          ((string= type "text_delta")))
                (if claude-rewrite-region
                    (claude-stream-rewrite-region text)
                  (with-current-buffer claude-rewrite-buffer
                    (save-excursion
                      (goto-char (point-max))
                      (insert text))))))
             ((plist-get data :type)
              (when (string= (plist-get data :type) "message_stop")
                (claude-finish-response))))))))))

(defun claude-finish-response ()
  "Finish processing the response.
Either finish the rewrite process or update conversation history."
  (if claude-rewrite-region
      (claude-finish-rewrite)
    (with-current-buffer claude-rewrite-buffer
      ;; Get the response text from the last "Claude: " to the end
      (save-excursion
        (goto-char (point-max))
        (search-backward "\n\nClaude: " nil t)
        (forward-char 10)
        (let ((response (buffer-substring-no-properties (point) (point-max))))
          ;; Add the new messages to history
          (setq-local claude-conversation-history
                      (vconcat claude-conversation-history
                              `[((role . "assistant")
                                 (content . ,response))])))))))

(defun claude-finish-rewrite ()
  "Finish the rewrite process by removing the overlay."
  (when (and claude-rewrite-region claude-rewrite-buffer claude-rewrite-overlay)
    (with-current-buffer claude-rewrite-buffer
      (delete-overlay claude-rewrite-overlay)
      (setq claude-rewrite-overlay nil)
      (setq claude-rewrite-region nil)
      (setq claude-rewrite-buffer nil)
      (setq claude-accumulated-text nil))))

(defun claude-stream-rewrite-region (text)
  "Accumulate TEXT and update the region being rewritten."
  (when (and claude-rewrite-region claude-rewrite-buffer claude-rewrite-overlay)
    (with-current-buffer claude-rewrite-buffer
      (setq claude-accumulated-text (concat claude-accumulated-text text))
      (let* ((start (overlay-start claude-rewrite-overlay))
             (end (overlay-end claude-rewrite-overlay)))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert claude-accumulated-text)
          (move-overlay claude-rewrite-overlay start (point)))))))

(defvar-local claude-accumulated-text nil
  "Accumulated text for the current rewrite operation.")

(defface claude-rewrite-face
  '((t :inherit region))
  "Face used for highlighting the region being rewritten by Claude."
  :group 'claude)

(defun claude-rewrite-region (start end prompt)
  "Rewrite the region between START and END based on PROMPT sent to Claude API."
  (interactive "r\nsPrompt for rewriting: ")
  (if (not claude-api-key)
      (user-error "Please set claude-api-key first")
    (let ((region-content (buffer-substring-no-properties start end)))
      (setq claude-rewrite-region (cons start end))
      (setq claude-rewrite-buffer (current-buffer))
      (setq claude-rewrite-overlay (make-overlay start end))
      (overlay-put claude-rewrite-overlay 'face 'claude-rewrite-face)
      (setq-local claude-accumulated-text "")
      (claude-send-request prompt region-content t))))

(defun claude-prompt (prompt)
  "Send PROMPT to Claude and display the response.
If called from a chat buffer, continues the conversation in that buffer."
  (interactive "sAsk Claude: ")
  (if (not claude-api-key)
      (message "Please set claude-api-key first")
    (let* ((source-buf (or (claude-chat-buffer-p) (current-buffer)))
           (chat-buffer (if (claude-chat-buffer-p)
                          (current-buffer)
                        (claude-get-or-create-chat-buffer source-buf)))
           (buffer-content (with-current-buffer source-buf
                           (claude-get-buffer-content))))
      (with-current-buffer chat-buffer
        (goto-char (point-max))
        (insert "\n\nYou: " prompt)
        (insert "\n\nClaude: "))
      (setq claude-rewrite-buffer chat-buffer)
      (display-buffer chat-buffer)
      ;; Add the user message to history before sending
      (with-current-buffer chat-buffer
        (setq-local claude-conversation-history
                    (vconcat claude-conversation-history
                            `[((role . "user")
                               (content . ,prompt))])))
      (claude-send-request prompt buffer-content))))

(defun claude-cancel-request ()
  "Cancel the ongoing Claude API request."
  (interactive)
  (when claude-current-request
    (url-cookie-delete-internal (current-url-cookie-string))
    ;; (kill-buffer (process-buffer claude-current-request))
    (delete-process claude-current-request)
    (setq claude-current-request nil)
    (setq claude-rewrite-region nil)
    (setq claude-rewrite-buffer nil)
    (when claude-rewrite-overlay
      (delete-overlay claude-rewrite-overlay)
      (setq claude-rewrite-overlay nil))
    (message "Claude API request cancelled.")
    (when claude-rewrite-buffer
      (with-current-buffer claude-rewrite-buffer
        (goto-char (point-max))
        (insert "\n\n[Request cancelled]")))))

(defun claude-chat-buffer-p (&optional buffer)
  "Return non-nil if BUFFER (or current buffer) is a Claude chat buffer.
Also returns the source buffer if it is a chat buffer."
  (let* ((buf (or buffer (current-buffer)))
         (name (buffer-name buf)))
    (when (string-match "^\\*Claude Chat: \\(.+\\)\\*$" name)
      (get-buffer (match-string 1 name)))))


(provide 'claude)
;;; claude.el ends here
