MMC = mmc
files = $(wildcard *.m)

bower: $(files) Mercury.options Mercury.params
	@$(MMC) --make $@ && touch $@

tags: $(files)
	@mtags $(files)
