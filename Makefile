build:
	elm make src/Main.elm --output out/index.html
clean:
	rm -rf out/
test:
	elm-test
all:
	make build
	make test
	elm-format src/ tests/ --validate
