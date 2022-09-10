MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)

files = $(wildcard *.m) prog_version.m

../bower: bower
	@ln -L -f bower ../bower

bower: $(files) Mercury.options Mercury.params
	@$(MMC) --make $(PARALLEL) $@ && touch $@

prog_version.m: ../NEWS make_version.sh
	@./make_version.sh

Mercury.params:

tags: $(files)
	@mtags $(files)
