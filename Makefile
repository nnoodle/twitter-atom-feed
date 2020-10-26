NAME = "twitter-atom-feed"
LISP ?= sbcl

build:
	$(LISP) --load $(NAME).asd \
		--eval "(ql:quickload :$(NAME))" \
		--eval "(asdf:make :$(NAME))" \
		--eval "(quit)"
