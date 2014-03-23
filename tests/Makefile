MMC = mmc
PARALLEL =
DIFF = diff -u

files = $(wildcard *.m ../*.m)

TESTS = test_rfc5322

.PHONY: default
default: $(addsuffix .runtest,$(TESTS))

$(TESTS): $(files) Mercury.modules
	$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.modules: $(files)
	@$(MMC) -f $(files)

.PHONY: test_rfc5322.runtest
test_rfc5322.runtest: test_rfc5322
	./test_rfc5322 > test_rfc5322.out && \
		$(DIFF) test_rfc5322.exp test_rfc5322.out
