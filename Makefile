all: counter server

.PHONEY: clean counter bin live

live:
	live-server --port=8000 --no-browser \
		--wait=300 \
		--watch=counter.html,_build/default/client/\*.js

bin:
	dune build ./server/bin.exe

counter:
	dune build ./client/counter.bc.js

clean:
	dune clean
