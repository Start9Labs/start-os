.PHONY: docs
docs:
	cd projects/start-docs && ./build.sh

.PHONY: clean-docs
clean-docs:
	rm -rf projects/start-docs/docs projects/start-docs/node_modules
