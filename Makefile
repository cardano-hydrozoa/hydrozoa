
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help               -- show this help"
	@echo "  spec               -- build the specification specification/hydrozoa.pdf"
	@echo "  spec-clean         -- clean the latexmk files in technical-spec"

.PHONY: spec
spec:
	$(MAKE) -C specification nix-spec

.PHONY: spec-clean
spec-clean:
	$(MAKE) -C specification nix-clean

