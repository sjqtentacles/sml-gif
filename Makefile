# sml-gif build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make example    render assets/wave.gif + assets/wave_frame.png
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-image (and its own
# vendored sml-inflate + sml-color) are vendored under lib/ and loaded first,
# in dependency order.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
IMGDIR     := lib/github.com/sjqtentacles/sml-image
INFDIR     := lib/github.com/sjqtentacles/sml-inflate
COLDIR     := lib/github.com/sjqtentacles/sml-color
TEST_MLB   := test/test.mlb
SRCS       := $(wildcard $(IMGDIR)/* $(INFDIR)/* $(COLDIR)/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

example: $(BIN)/animate
	mkdir -p assets
	./$(BIN)/animate

$(BIN)/animate: $(SRCS) examples/animate.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own. Load vendored deps first (inflate, color, image), then the gif
# sources, then the test driver.
poly test-poly:
	printf 'use "$(INFDIR)/inflate.sig";\nuse "$(INFDIR)/inflate.sml";\nuse "$(COLDIR)/color.sig";\nuse "$(COLDIR)/color.sml";\nuse "$(IMGDIR)/image.sig";\nuse "$(IMGDIR)/image.sml";\nuse "src/gif.sig";\nuse "src/gif.sml";\nuse "test/harness.sml";\nuse "test/support.sml";\nuse "test/test_structure.sml";\nuse "test/test_roundtrip.sml";\nuse "test/test_quant.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton $(BIN)/animate
