build:
	elm-make src/Main.elm --yes --warn --output out/Main.html
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
	elm test
