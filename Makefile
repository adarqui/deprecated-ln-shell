all:
	cabal sandbox init
	cabal sandbox add-source ../ln-types
	cabal install -j

test_init:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build

reset:
	rm -rf ./.cabal-sandbox/lib/x86_64-osx-ghc-7.*/ln-yesod-0.*
	rm -f ./.cabal-sandbox/x86_64-osx-ghc-7.*-packages.conf.d/ln-yesod-0.*
	cabal clean

docs:
	cabal haddock --hyperlink-source

build:
	stack build --file-watch

ghci:
	stack ghci ln-shell

ghci2:
	cd ../ln-yesod && stack --stack-yaml ../ln-notes/stack.yaml ghci ln-yesod
