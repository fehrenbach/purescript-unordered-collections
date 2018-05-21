.PHONY: compile bench test

SOURCES = $(shell find src/)
BENCH_SOURCES = $(shell find bench/)
TEST_SOURCES = $(shell find test/)
BOWER_SOURCES = $(shell find bower_components/)

all: compile test bench

output/Bench.Main/index.js: $(SOURCES) $(BENCH_SOURCES) $(BOWER_SOURCES)
	purs compile 'bench/**/*.purs' 'src/**/*.purs' 'bower_components/*/src/**/*.purs'

bench: output/Bench.Main/index.js
	node -e 'require("./output/Bench.Main/index.js").main()'

compile: $(SOURCES) $(BOWER_SOURCES)
	purs compile 'src/**/*.purs' 'bower_components/*/src/**/*.purs'

output/Test.Main/index.js: $(SOURCES) $(TEST_SOURCES) $(BOWER_SOURCES)
	purs compile 'test/**/*.purs' 'src/**/*.purs' 'bower_components/*/src/**/*.purs'

test: output/Test.Main/index.js
	node -e 'require("./output/Test.Main/index.js").main()'
