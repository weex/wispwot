(import (gnu packages web)
	(gnu packages guile)
	(gnu packages guile-xyz)
	(guix packages)
	(guix gexp)
	(guix build-system gnu)
	(prefix (guix licenses) license:))

(define-public wispwot
  (package
    (name "wispwot")
    (version "0.1")
   (source (local-file "."))
   (build-system gnu-build-system)
   (propagated-inputs `(
                        ("guile" ,guile-3.0)
                        ("guile-wisp" ,guile-wisp)
                        ))
   (home-page "")
   (synopsis "")
   (description "")
   (license license:gpl3+)))

wispwot

