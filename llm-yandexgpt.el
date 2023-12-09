;;; llm-yandexgpt.el --- llm module for integrating with YandexGPT -*- lexical-binding: t -*-

;; Author: Max Arnold <arnold.maxim@gmail.com>
;; Homepage: https://github.com/max-arnold/llm-yandexgpt
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file implements the llm functionality defined in llm.el, for YandexGPT
;; API.

(require 'cl-lib)
(require 'llm)
(require 'llm-request)
(require 'json)

(defgroup llm-yandexgpt nil
  "LLM implementation for YandexGPT."
  :group 'llm)

;; (defun llm-yandexgpt-default-api-key-function ()
;;   "Fetch the API key with auth-source."
;;   (auth-source-pick-first-password :host "llm.api.cloud.yandex.net"))

;; (defcustom llm-yandexgpt-api-key #'llm-yandexgpt-default-api-key-function
;;   "YandexGPT key as a string or a function that loads and returns it."
;;   :group 'llm-yandexgpt
;;   :type '(choice (function :tag "Function")
;;                  (string :tag "String")))

(cl-defstruct llm-yandexgpt
 "A provider for the YandexGPT LLM provider.

KEY is the API key for the YandexGPT API

CHAT-MODEL is the name of the chat model to use.

EMEDDING-MODEL is the name of the embedding model to use."
  key
  chat-model
  embedding-model
  )

(defun llm-yandexgpt--chat-request (provider prompt &optional streaming)
  "From PROMPT, create the data for the YandexGPT chat request."
  (let (request-alist options)
    (push `("modelUri" . ,(llm-yandexgpt-chat-model provider)) request-alist)
    (push `("messages" . ,(apply #'vector
                                 (mapcar (lambda (interaction)
                                           `(("role" . ,(pcase (llm-chat-prompt-interaction-role interaction)
                                                         ('user "user")
                                                         ('system "system")
                                                         ('assistant "assistant")))
                                             ("text" . ,(llm-chat-prompt-interaction-content interaction))))
                                         (llm-chat-prompt-interactions prompt))))
          request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) options))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("maxTokens" . ,(llm-chat-prompt-max-tokens prompt)) options))
    (when streaming (push `("stream" . ,t) options))
    (when options (push `("completionOptions" . ,options) request-alist))
    request-alist))


(defun llm-yandexgpt--handle-response (response extractor)
  "If RESPONSE is an error, throw it, else call EXTRACTOR."
  (if (cdr (assoc 'error response))
      (error (llm-yandexgpt--error-message response))
    (funcall extractor response)))


(defun llm-yandexgpt--error-message (err-response)
  "Return a user-visible error message from ERR-RESPONSE."
  (format "Problem calling YandexGPT: %s"
          (assoc-default 'message (assoc-default 'error err-response))
          err-response))

(defun llm-yandexgpt--get-final-chat-response (response)
  "Return chat response from server RESPONSE."
  (let ((final (cl-find-if (lambda (alt)
                             (and (string= (cdr (assoc 'status alt)) "ALTERNATIVE_STATUS_FINAL")
                                  (string= (cdr (assoc 'role (cdr (assoc 'message alt)))) "assistant")))
                           (cdr (assoc 'alternatives (cdr (assoc 'result response)))))))
    (when final
      (cdr (assoc 'text (cdr (assoc 'message final)))))))

(cl-defmethod llm-chat ((provider llm-yandexgpt) prompt)
  (unless (llm-yandexgpt-key provider)
    (error "To call YandexGPT API, the key must have been set"))
  (let ((response (llm-yandexgpt--handle-response
                   (llm-request-sync "https://llm.api.cloud.yandex.net/foundationModels/v1/completion"
                                     :headers `(("Authorization" . ,(format "Api-Key %s" (llm-yandexgpt-key provider))))
                                     :data (llm-yandexgpt--chat-request provider prompt))
                   #'llm-yandexgpt--get-final-chat-response)))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
    response))

(defun llm-yandexgpt--get-partial-chat-response (response)
  "Return the text in the partial chat response from RESPONSE."
  (let ((result nil)
        (lines (split-string response "\r?\n" t)))
    (dolist (line lines)
      (when (string-match "^{.*" line)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (alternatives (cdr (assoc 'alternatives (cdr (assoc 'result
                                                                   (condition-case nil
                                                                       (json-read-from-string line)
                                                                     (error (format "Json error: %s" line)))
                                                                   )))))
               (alt (cl-find-if (lambda (alt)
                                  (string= (cdr (assoc 'role (cdr (assoc 'message alt)))) "assistant"))
                                alternatives)))
          (when alt
            (setq result (cdr (assoc 'text (cdr (assoc 'message alt)))))))))
  result))

(cl-defmethod llm-chat-streaming ((provider llm-yandexgpt) prompt partial-callback response-callback error-callback)
  (unless (llm-yandexgpt-key provider)
    (error "To call YandexGPT API, the key must have been set"))
  (let ((buf (current-buffer)))
    (llm-request-async "https://llm.api.cloud.yandex.net/foundationModels/v1/completion"
                       :headers `(("Authorization" . ,(format "Api-Key %s" (llm-yandexgpt-key provider))))
                       :data (llm-yandexgpt--chat-request provider prompt t)
                       :on-partial (lambda (data)
                                     (when-let ((response (llm-yandexgpt--get-partial-chat-response data)))
                                       (llm-request-callback-in-buffer buf partial-callback response)))
                       :on-success-raw (lambda (data)
                                         (let ((response (llm-yandexgpt--get-partial-chat-response data)))
                                           (setf (llm-chat-prompt-interactions prompt)
                                                 (append (llm-chat-prompt-interactions prompt)
                                                         (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
                                           (llm-request-callback-in-buffer buf response-callback response))
                                         )
                       :on-error (lambda (_ data)
                                   (llm-request-callback-in-buffer buf error-callback 'error
                                                                   (llm-yandexgpt--error-message data))))))


;; (let ((provider (make-llm-yandexgpt :key yandexgpt-api-key :chat-model yandexgpt-chat-model :embedding-model yandexgpt-embedding-model))
;;       (llm-warn-on-nonfree nil))
;;   (llm-chat-streaming provider (llm-make-simple-chat-prompt "Write a simple terms of service doc for a saas")
;;                       (lambda (text) (message (format "PART: %s" text)))
;;                       (lambda (text) (message (format "RESP: %s" text)))
;;                       (lambda (type message) (message (format "ERR: %s %s" type message)))
;;   ))

;; (let ((provider (make-llm-yandexgpt :key yandexgpt-api-key :chat-model yandexgpt-chat-model :embedding-model yandexgpt-embedding-model))
;;       (llm-warn-on-nonfree nil))
;;   (llm-chat provider (llm-make-simple-chat-prompt "Какой редактор мощнее - Emacs или Vim?")
;;  ))

(cl-defmethod llm-nonfree-message-info ((provider llm-yandexgpt))
  (ignore provider)
  (cons "YandexGPT" "https://yandex.ru/legal/yagpt_termsofuse/"))

(provide 'llm-yandexgpt)
