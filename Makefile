all:
	./build
	./build test
	./test.native

test:
	./build test

clean:
	./build clean
