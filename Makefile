.SECONDARY:
.SUFFIXES:
SUFFIXES:=

SHELL:=/bin/bash
UMM:=target/debug/umm

TESTS!=find test -name '*.expect'
TESTS:=$(TESTS:test/%.expect=%)

check: $(TESTS:%=run-test/%.verdict)

target/debug/umm: .forcerebuild
	@cargo build

target/release/umm: .forcerebuild
	@cargo build --release

run-test/%.verdict: run-test/%.out test/%.expect
	@mkdir -p $(@D)
	@echo CHECK $*
	@diff -q $< test/$*.expect

run-test/%.out: test/%.tlang $(UMM)
	@mkdir -p $(@D)
	@echo RUN $*
	@$(UMM) < $< 2>run-test/$*.err | LANG=C.utf8 sort > run-test/$*.out

.PHONY: .forcerebuild
