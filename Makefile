AWK = awk

.PHONY: bower
bower:
	@$(MAKE) -C src ../bower

.PHONY: man
man: bower.1

bower.1: README.md make_man
	$(AWK) -f make_man < README.md | \
	pandoc -f markdown -t man --standalone \
		-M title=bower -M section=1 -o bower.1
