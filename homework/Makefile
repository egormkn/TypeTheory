all: hw1 hw2

hw1: test_hw1.native
hw1_reduction: test_hw1_reduction.native
hw2_inference: test_hw2_inference.native
hw2_unify: test_hw2_unify.native

test1: hw1 hw1_reduction
	./_build/bin/test_hw1
	./_build/bin/test_hw1_reduction

test2: hw2_inference hw2_unify
	./_build/bin/test_hw2_inference
	./_build/bin/test_hw2_unify

%.native:
	ocamlbuild -use-ocamlfind $@
	@mkdir --parents ./_build/bin/
	@mv $@ ./_build/bin/$*

clean:
	rm -rf _build/*

.PHONY: all test1 test2 clean
