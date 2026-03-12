This will be the main document for our testing.  Testing infrastructure in this repo is very much in development and this document will help point out examples of the design and practices we want to be using.

Our top section will be an explanation of the test runner and an overview of the ERT and Buttercup tests.  The purpose is to show what tests are here and how to run them.
It will include details on the test snapshot, where it's stored etc.
It will include details on the test report and how to create it.
It will include examples of running tests by type, directory, etc.

The next section will describe the types of testing
Unit: test individual functions in isolation.
Integration: test collections of functions in an 'end-to-end' fashion, mocking only external dependencies like emacs primitives or base packages.  config/gptel/test is an example.
Behavioral: test behavioral specs.  May sometimes overlap with integration tests, but are distinct.  A behavioral test may say "can restore a session by opening the file" while an integration test would be more in depth and validate that saving the session and restoring the session shared a compatible interface.  Behavioral tests are human readable and should map to openspec specs.  Example config/bash-parser/test/corpus/runners/test-corpus-file-operations.el
Contract: tests the interface between distant and complex modules using intermediary function an objects which validate producers produce what consumers consume.  Example config/bash-parser/commands/test/complex-commands-spec.el (producer) config/gptel/scope/test/integration/bash-parser-integration-spec.org (consumer)


Testing method when debugging
1. examine the failure and generate a hypothesis on the root cause
2. write a test that fails as a result of the hypothesized root cause.  Don't codify broken behavior in test assertions, if the behavior is wrong the test should fail.
3. run the test and confirm the failure
4. apply the fix
5. run the test to confirm


/bin/run-tests.sh -d config/gptel/scope/test/integration
