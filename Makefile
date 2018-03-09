dist: src/Main.elm
	mkdir -p dist
	elm-make --yes --output=dist/main.js src/Main.elm
