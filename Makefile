.PHONY: build document release test

VERSION := 0.0.$(shell git log --oneline | wc -l | tr -d '[:space:]')

build:
	npm update
	bower update
	pulp build

test:
	pulp test

release:
ifneq ($(shell git rev-parse --abbrev-ref HEAD), master)
	$(error Cannot release: You aren't on master branch)
endif
ifneq ($(shell git status --porcelain),)
	$(error Cannot release: You have unstaged changes)
endif

	cd gen && make clean init build test run
	make build test

	pulp version ${VERSION}
	pulp publish
