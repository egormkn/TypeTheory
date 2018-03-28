#       `make` - build all homework sources
#  `make test` - build and run tests
# `make clean` - clean build directory
#
# Use `opam install ocamlbuild` to install build tool

all: hw1 hw1_reduction hw2_unify hw2_inference

test: test_hw1 test_hw1_reduction test_hw2_unify test_hw2_inference

hw1: hw1.native
hw1_reduction: hw1_reduction.native
hw2_unify: hw2_unify.native
hw2_inference: hw2_inference.native

test_hw1: test_hw1.native
		./_build/bin/test_hw1
test_hw1_reduction: test_hw1_reduction.native
		./_build/bin/test_hw1_reduction
test_hw2_unify: test_hw2_unify.native
		./_build/bin/test_hw2_unify
test_hw2_inference: test_hw2_inference.native
		./_build/bin/test_hw2_inference

%.native:
	@echo "Building <$*>:"
	@ocamlbuild -use-ocamlfind $@
	@echo
	@mkdir --parents ./_build/bin/
	@mv $@ ./_build/bin/$*

clean:
	rm -rf _build

.PHONY: all test clean test_hw1 test_hw1_reduction test_hw2_unify test_hw2_inference
