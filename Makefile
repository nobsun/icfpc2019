
all:
	stack build -j4

all-check:
	stack build -j4 --test

install:
	make all
	install -m 755 ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/solve-one/solve-one ./bin/
	install -m 755 ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/solver/solver ./bin/
	install -m 755 ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/merge/merge ./bin/
	install -m 755 ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/view-map/view-map ./bin/
	strip ./bin/solve-one
	strip ./bin/solver
	strip ./bin/merge
	strip ./bin/view-map

install-g:
	make all
	mkdir -p bin
	install -m 755 .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/submit-gateway/submit-gateway ./bin/

clean:
	stack clean
