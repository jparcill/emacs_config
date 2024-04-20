;;; private.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq! gptel-api-key "key")
  (setq! gptel-model "gpt-4")
  )

(use-package! counsel-spotify
  :config
  (setq! counsel-spotify-client-id "id")
  (setq! counsel-spotify-client-secret "secret")
  )
