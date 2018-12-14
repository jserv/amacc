PYTHON = python3
PYTHON := $(shell which $(PYTHON))
ifndef PYTHON
$(error "python3 is required.")
endif
