;;;; the-invaders.asd
#-asdf3.1 (error "requires ASDF 3.1")
(asdf:defsystem #:the-invaders
  :version "1.1.1"
  :description "A remake of the classic game Space Invaders"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL v2"
  :class :package-inferred-system
  :depends-on (#:the-invaders/all)
  )



