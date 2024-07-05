(asdf:defsystem #:the-invaders
  :version "1.1.1"
  :description "A remake of the classic game Space Invaders"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL v2"
  :depends-on ("msys2-for-externals"
	       "lispbuilder-sdl"
	       "lispbuilder-sdl-image"
	       "lispbuilder-sdl-mixer"
	       "lispbuilder-sdl-ttf"
	       "lispbuilder-sdl-mixer")
  :components ((:file "package")
	       (:file "the-invaders")))



