SPECNAME = hydrozoa
SPECDIR = specification

.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help               -- show this help"
	@echo "  spec               -- build $(SPECDIR)/$(SPECNAME).pdf"
	@echo "  spec-clean         -- clean the latexmk files in $(SPECDIR)"

.PHONY: spec
spec:
	$(MAKE) -C $(SPECDIR) nix-spec

.PHONY: spec-clean
spec-clean:
	$(MAKE) -C $(SPECDIR) nix-clean

