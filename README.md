# YandexGPT LLM provider for Emacs

This is a [Emacs LLM](https://github.com/ahyatt/llm) provider that supports [YandexGPT API](https://cloud.yandex.com/en/services/yandexgpt).

The simplest way to use it is [Ellama](https://github.com/s-kostyaev/ellama):

```elisp
(use-package ellama
  :init
  (require 'llm-yandexgpt)
  (setopt ellama-provider (make-llm-yandexgpt :key yandexgpt-api-key :chat-model yandexgpt-chat-model :embedding-model yandexgpt-embedding-model)))
```

Development status: alpha
