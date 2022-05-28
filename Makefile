.PHONY: clean compile bench bench-trace test test-bundle

SOURCES = $(shell find src/)
BENCH_SOURCES = $(shell find bench/)
TEST_SOURCES = $(shell find test/)
BOWER_SOURCES = $(shell find bower_components/)

all: compile test test-bundle bench

output/Bench.Main/index.js: $(SOURCES) $(BENCH_SOURCES) $(BOWER_SOURCES)
	purs compile 'bench/**/*.purs' 'src/**/*.purs' 'bower_components/*/src/**/*.purs'

bench: output/Bench.Main/index.js
	node --expose-gc -e 'import("./output/Bench.Main/index.js").then(m => m.main())'

bench-trace: output/Bench.Main/index.js
	node --expose-gc --trace-opt --trace-deopt --print-opt-code --code-comments -e  'import("./output/Bench.Main/index.js").then(m => m.main())'

compile: $(SOURCES) $(BOWER_SOURCES)
	purs compile 'src/**/*.purs' 'bower_components/*/src/**/*.purs'

output/Test.Main/index.js: compile $(SOURCES) $(TEST_SOURCES) $(BOWER_SOURCES)
	purs compile 'test/**/*.purs' 'src/**/*.purs' 'bower_components/*/src/**/*.purs'

test: output/Test.Main/index.js
	node -e 'import("./output/Test.Main/index.js").then(m => m.main())'

# No more purs bundle in Purescript v0.15.
# test-bundle: output/Test.Main/index.js
# 	purs bundle 'output/**/*.js' --output test-bundle.js --module Test.Main --main Test.Main
# 	node test-bundle.js

clean:
	rm -rf bower_components/ output/
