MMC = mmc
PARALLEL =
files = $(wildcard *.m)

../bower: bower
	@ln -L -f bower ../bower

bower: $(files) Mercury.options Mercury.params
	@$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.params:

tags: $(files)
	@mtags $(files)
