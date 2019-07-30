;; -*- no-byte-compile: t; -*-

(package! d-mode)
(when (featurep! :tools flycheck)
  (package! flycheck-dmd-dub))
