build:
	dune build --profile=release

run:
	dune exec syntheticML --profile=release

watch:
	dune build -w
