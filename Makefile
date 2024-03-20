build:
	elm make src/Main.elm --output=public/index.html

elm-live:
	npx elm-live src/Main.elm --  --debug

test:
	npx elm-test-rs