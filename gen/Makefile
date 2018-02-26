.PHONY: clean init build test run

clean:
	rm -fr aws-sdk-js bower_components output

init:
	git clone https://github.com/aws/aws-sdk-js.git

build:
	bower update
	pulp build

test:
	pulp test

run:
	pulp run
