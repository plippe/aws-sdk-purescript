.PHONY: build test release

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

	git tag -a ${VERSION} -m "v${VERSION}"
	git push origin ${VERSION}
