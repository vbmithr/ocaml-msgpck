all:
	jbuilder build @install @runtest --dev

clean:
	rm -rf _build
