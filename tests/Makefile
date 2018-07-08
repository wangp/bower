MMC = mmc
PARALLEL =
DIFF = diff -u

files = $(wildcard *.m ../src/*.m)

TESTS = test_base64 \
	test_rfc5322 \
	test_rfc2047_decode \
	test_rfc2047_encode \
	test_rfc2231 \
	test_rfc3986 \
	test_rfc6068 \
	test_json \
	test_process

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
	@$(RM) test_base64.out

.PHONY: %.runtest
%.runtest: %
	./$(<) > $(<).out && \
		$(DIFF) $(<).exp $(<).out
	@$(RM) $(<).out

# Prevent building targets in parallel to avoid multiple instances of
# mmc trying to make the same target files at once.
test_rfc5322: | test_base64
test_rfc2047_decode: | test_rfc5322
test_rfc2047_encode: | test_rfc2047_decode
test_rfc2231: | test_rfc2047_encode
test_rfc3986: | test_rfc2231
test_rfc6068: | test_rfc3986
