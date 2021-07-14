TESTS:=eastwoodTests languageServerTests
RUN_TESTS:=$(addprefix run_,$(TESTS))

$(TESTS): .FORCE
	cp $@.prj.default $@.prj
	cpm $@.prj

$(RUN_TESTS): run_%: % .FORCE
	cleantest -r ./$< --junit junit_$<.xml

runTests: $(RUN_TESTS)

.FORCE:

.PHONY: .FORCE runTests $(RUN_TESTS)
