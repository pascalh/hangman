build:
	elm make src/Main.elm --output out/Main.html
clean:
	rm -rf out/
deploy:
	git checkout gh-pages
	make
	mv out/Main.html index.html
	git add index.html
	git commit -m "Deploy"
	git push origin gh-pages
	git checkout master
test:
	elm-test
all:
	make build
	make test
	elm-format src/ tests/ --validate
