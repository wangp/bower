MMC = mmc
PARALLEL =
files = $(wildcard *.m)

bower: $(files) Mercury.options Mercury.params
	@$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.params:

tags: $(files)
	@mtags $(files)
