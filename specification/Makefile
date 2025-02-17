SPECNAME = hydrozoa

.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help           -- show this help"
	@echo "  spec           -- build $(SPECNAME).pdf"
	@echo "  spec-clean     -- clean up latexmk files"
	@echo "  nix-shell      -- open nix shell"
	@echo "  nix-spec       -- open nix shell & make spec"
	@echo "  nix-spec-clean -- open nix shell & make spec-clean"

.PHONY: nix-shell
nix-shell:
	nix develop

.PHONY: spec
spec:
	latexmk -pdf -bibtex $(SPECNAME).tex

.PHONY: spec-clean
spec-clean:
	latexmk -CA
	find . -type f -name "*.aux" -exec rm {} \;
	rm gitHeadLocal.gin

.PHONY: nix-spec
nix-spec:
	nix develop --command $(MAKE) spec

.PHONY: nix-spec-clean
nix-spec-clean:
	nix develop --command $(MAKE) spec-clean
