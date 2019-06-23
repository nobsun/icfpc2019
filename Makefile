
all:
	stack build -j4

all-check:
	stack build -j4 --test

install-g:
	make all
	mkdir -p bin
	install -m 755 .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/submit-gateway/submit-gateway ./bin/

clean:
	stack clean
