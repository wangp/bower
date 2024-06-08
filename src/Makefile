MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)

MMC_MAKE_FLAGS :=
ifdef WITH_NCURSESW_DIR
	MMC_MAKE_FLAGS := --options-file Mercury.options.ncursesw
endif

files = $(wildcard *.m) prog_version.m

../bower: bower
	@ln -L -f bower ../bower

bower: $(files) Mercury.options Mercury.options.ncursesw Mercury.params
	@$(MMC) --make $(PARALLEL) $(MMC_MAKE_FLAGS) $@ && touch $@

prog_version.m: ../NEWS make_version.sh
	@./make_version.sh

Mercury.params:

tags: $(files)
	@mtags $(files)
