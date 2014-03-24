MMC = mmc
PARALLEL =
DIFF = diff -u

files = $(wildcard *.m ../*.m)

TESTS = test_base64 \
	test_rfc5322

.PHONY: default
default: $(addsuffix .runtest,$(TESTS))

$(TESTS): $(files) Mercury.modules
	$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.modules: $(files)
	@$(MMC) -f $(files)

.PHONY: test_base64.runtest
test_base64.runtest: test_base64
	./test_base64 -e < test_base64.m | \
		./test_base64 -d > test_base64.out && \
		$(DIFF) test_base64.m test_base64.out

.PHONY: test_rfc5322.runtest
test_rfc5322.runtest: test_rfc5322
	./test_rfc5322 > test_rfc5322.out && \
		$(DIFF) test_rfc5322.exp test_rfc5322.out
