MMC = mmc
files = $(wildcard *.m)

bower: $(files) Mercury.options Mercury.params
	@$(MMC) --make $@ && touch $@

Mercury.params:

tags: $(files)
	@mtags $(files)
