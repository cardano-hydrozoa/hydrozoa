SPECNAME = hydrozoa

.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help      -- show this help"
	@echo "  shell     -- open nix dev shell"
	@echo "  spec      -- build $(SPECNAME).pdf"
	@echo "  clean     -- clean up latexmk files"
	@echo "  nix-spec  -- open nix shell & spec"
	@echo "  nix-clean -- open nix shell & clean"

.PHONY: shell
shell:
	nix develop

.PHONY: spec
spec:
	latexmk -pdf -bibtex $(SPECNAME).tex

.PHONY: clean
clean:
	latexmk -CA
	find . -type f -name "*.aux" -exec rm {} \;
	rm gitHeadLocal.gin

.PHONY: nix-spec
nix-spec:
	nix develop --command $(MAKE) spec

.PHONY: nix-clean
nix-clean:
	nix develop --command $(MAKE) clean
