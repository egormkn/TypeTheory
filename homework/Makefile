all: hw1 hw2

hw1: test_hw1.native
	./_build/bin/test_hw1

hw1_reduction: test_hw1_reduction.native
	./_build/bin/test_hw1_reduction

hw2_unify: test_hw2_unify.native
	./_build/bin/test_hw2_unify

hw2_inference: test_hw2_inference.native
	./_build/bin/test_hw2_inference

test1: hw1 hw1_reduction
test2: hw2_unify hw2_inference

%.native:
	ocamlbuild -use-ocamlfind $@
	@mkdir --parents ./_build/bin/
	@mv $@ ./_build/bin/$*

clean:
	rm -rf _build/*

.PHONY: all test1 test2 clean hw1 hw1_reduction hw2_unify hw2_inference
