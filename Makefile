build:
	elm-make src/Main.elm --yes --warn --output out/Main.html
clean:
	rm -r out/
