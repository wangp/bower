.PHONY: bower
bower:
	@$(MAKE) -C src ../bower

manpage.md: README.md
	cp README.md manpage.md
	patch manpage.md < manpage.patch

bower.1.gz: manpage.md
	pandoc -f markdown_github -t man manpage.md -s -M title=BOWER -M section=1 | gzip > bower.1.gz

.PHONY: manpage
manpage: bower.1.gz
