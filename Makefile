docker-build:
	for dockerfile in dockerfiles/*; do docker build -f $$dockerfile . ; done

all:
	make x86_64-osx/hpack-convert
	make x86_64-linux/hpack-convert

	make upload-osx
	make upload-linux-64

upload-osx: x86_64-osx/hpack-convert
	github-release upload --name hpack-convert_x86_64-osx.tar.gz --label "Pre-built binary for OSX" -u yamadapc -s $$GITHUB_API_TOKEN -r hpack-convert -t 0.14.3 -f ./x86_64-osx/hpack-convert_x86_64-osx.tar.gz

upload-linux-64: x86_64-linux/hpack-convert
	github-release upload --name hpack-convert_x86_64-linux.tar.gz --label "Pre-built binary for Linux 64-bits" -u yamadapc -s $$GITHUB_API_TOKEN -r hpack-convert -t 0.14.3 -f ./x86_64-linux/hpack-convert_x86_64-linux.tar.gz

x86_64-osx/hpack-convert: FORCE
	stack build
	rm -rf ./x86_64-osx
	mkdir -p x86_64-osx
	cp `stack path --dist-dir`/build/hpack-convert/hpack-convert ./x86_64-osx/
	cd ./x86_64-osx; tar -zcvf hpack-convert_x86_64-osx.tar.gz *

x86_64-linux/hpack-convert: FORCE
	stack docker pull
	stack build --docker
	rm -rf ./x86_64-linux
	mkdir -p x86_64-linux
	cp `stack path --docker --dist-dir`/build/hpack-convert/hpack-convert ./x86_64-linux/
	cd ./x86_64-linux; tar -zcvf hpack-convert_x86_64-linux.tar.gz *

FORCE:
