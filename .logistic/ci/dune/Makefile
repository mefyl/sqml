.PHONY: opam
MAKEFILE=$(abspath $(firstword $(MAKEFILE_LIST)))
DIR=$(patsubst %/,%,$(dir $(MAKEFILE)))
DUNE := dune
WORKSPACE_OPT := $(if $(WORKSPACE),--workspace dune-workspace.$(WORKSPACE),)

-include Makefile.ci

ifdef DOCKERFILE_BASE
DOCKERFILE_SUFFIX=.$(DOCKERFILE_BASE)
endif

DOCKER_DIR := .
ifndef DOCKER_FILE
DOCKER_FILE := $(dir $(firstword $(MAKEFILE_LIST)))Dockerfile.ci$(DOCKERFILE_SUFFIX)
endif

build:
	if test -d /root/node_modules; then ln -s /root/node_modules; fi
	$(DUNE) subst
	time $(DUNE) build --profile release $(WORKSPACE_OPT) --ignore-promoted-rules @install

lint:
	tagref check
	time $(DUNE) build $(WORKSPACE_OPT) @fmt
	time $(DUNE) build --profile pedantic $(WORKSPACE_OPT)
	# Check there is no diff in generated files, like *.opam
	git diff --exit-code


check:
	if test -d /root/node_modules; then ln -s /root/node_modules; fi
	$(DUNE) subst
	time $(DUNE) runtest $(WORKSPACE_OPT)

opam:
	$(DIR)/opam-publish
