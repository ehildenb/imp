# Settings
# --------

BUILD_DIR     := .build
SUBDEFN_DIR   := .
DEFN_BASE_DIR := $(BUILD_DIR)/defn
DEFN_DIR      := $(DEFN_BASE_DIR)/$(SUBDEFN_DIR)

DEPS_DIR          := deps
K_DIR             := $(abspath $(BUILD_DIR))/k
K_RELEASE_TAR     := $(abspath $(BUILD_DIR))/k.tar.gz
K_RELEASE_TAR_URL := $(shell cat $(abspath $(DEPS_DIR))/k.release)/k-nightly.tar.gz

K_RELEASE ?= $(K_DIR)/k-distribution/target/release/k
K_BIN     := $(K_RELEASE)/bin
K_LIB     := $(K_RELEASE)/lib
export K_RELEASE

PATH := $(K_BIN):$(PATH)
export PATH

# need relative path for `pandoc` on MacOS
PANDOC_TANGLE_SUBMODULE := $(DEPS_DIR)/pandoc-tangle
TANGLER                 := $(PANDOC_TANGLE_SUBMODULE)/tangle.lua
LUA_PATH                := $(PANDOC_TANGLE_SUBMODULE)/?.lua;;
export TANGLER
export LUA_PATH

.PHONY: all clean distclean deps                   \
        build                                      \
        build-imp build-imp-llvm build-imp-haskell \
        build-fun build-fun-llvm build-fun-haskell \
        test test-imp test-fun

all: build

clean:
	rm -rf $(DEFN_BASE_DIR)
	git clean -dffx -- tests/

distclean: clean
	rm -rf $(BUILD_DIR)

# Dependencies
# ------------

deps: $(K_RELEASE)/lib/java/kernel-1.0-SNAPSHOT.jar

$(K_RELEASE)/lib/java/kernel-1.0-SNAPSHOT.jar:
	mkdir -p $(BUILD_DIR)
	rm -rf $(K_DIR) $(K_RELEASE_TAR)
	curl --location --output $(K_RELEASE_TAR) $(K_RELEASE_TAR_URL)
	mkdir -p $(K_RELEASE)
	tar --extract --file $(K_RELEASE_TAR) --strip-components 1 --directory $(K_RELEASE)
	krun --version

# Building
# --------

tangle := .k

build: build-imp build-fun

KOMPILE_OPTS += -ccopt -std=c++14 -O2

### IMP

imp_module := IMP
imp_dir    := $(DEFN_DIR)/imp

imp_llvm_dir      := $(imp_dir)/llvm
imp_llvm_files    := $(imp_llvm_dir)/imp.k
imp_llvm_kompiled := $(imp_llvm_dir)/imp-kompiled/interpreter

imp_haskell_dir      := $(imp_dir)/haskell
imp_haskell_files    := $(imp_haskell_dir)/imp.k
imp_haskell_kompiled := $(imp_haskell_dir)/imp-kompiled/definition.kore

build-imp: build-imp-llvm build-imp-haskell
build-imp-llvm:    $(imp_llvm_kompiled)
build-imp-haskell: $(imp_haskell_kompiled)

$(imp_llvm_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(imp_llvm_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle)" $< > $@

$(imp_haskell_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(imp_haskell_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle)" $< > $@

$(imp_llvm_kompiled): $(imp_llvm_files)
	kompile --main-module $(imp_module) --backend llvm          \
	        --syntax-module $(imp_module) $(imp_llvm_dir)/imp.k \
	        --directory $(imp_llvm_dir) -I $(imp_llvm_dir)      \
	        $(KOMPILE_OPTS)

$(imp_haskell_kompiled): $(imp_haskell_files)
	kompile --main-module $(imp_module) --backend haskell          \
	        --syntax-module $(imp_module) $(imp_haskell_dir)/imp.k \
	        --directory $(imp_haskell_dir) -I $(imp_haskell_dir)   \
	        $(KOMPILE_OPTS)

### FUN

fun_module := FUN
fun_dir    := $(DEFN_DIR)/fun

fun_llvm_dir      := $(fun_dir)/llvm
fun_llvm_files    := $(fun_llvm_dir)/fun.k
fun_llvm_kompiled := $(fun_llvm_dir)/fun-kompiled/interpreter

fun_haskell_dir      := $(fun_dir)/haskell
fun_haskell_files    := $(fun_haskell_dir)/fun.k
fun_haskell_kompiled := $(fun_haskell_dir)/fun-kompiled/definition.kore

build-fun: build-fun-llvm build-fun-haskell
build-fun-llvm:    $(fun_llvm_kompiled)
build-fun-haskell: $(fun_haskell_kompiled)

$(fun_llvm_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(fun_llvm_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle)" $< > $@

$(fun_haskell_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(fun_haskell_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle)" $< > $@

$(fun_llvm_kompiled): $(fun_llvm_files)
	kompile --main-module $(fun_module) --backend llvm          \
	        --syntax-module $(fun_module) $(fun_llvm_dir)/fun.k \
	        --directory $(fun_llvm_dir) -I $(fun_llvm_dir)      \
	        $(KOMPILE_OPTS)

$(fun_haskell_kompiled): $(fun_haskell_files)
	kompile --main-module $(fun_module) --backend haskell          \
	        --syntax-module $(fun_module) $(fun_haskell_dir)/fun.k \
	        --directory $(fun_haskell_dir) -I $(fun_haskell_dir)   \
	        $(KOMPILE_OPTS)

# Test
# ----

CHECK := git --no-pager diff --no-index --ignore-all-space -R

test: test-imp test-fun

### IMP

test_imp_files  := $(wildcard tests/*.imp)
prove_imp_files := $(wildcard tests/*-spec.k)

test-imp: $(test_imp_files:=.run) $(prove_imp_files:=.prove)

tests/%.imp.run: tests/%.imp.out
	$(CHECK) $< tests/$*.imp.k-expected

tests/%-spec.k.prove: tests/%-spec.k.out
	$(CHECK) $< tests/$*-spec.k.expected

.SECONDARY: $(test_imp_files:=.out)
tests/%.imp.out: tests/%.imp $(imp_llvm_kompiled)
	krun --directory $(imp_llvm_dir) $< > $@

.SECONDARY: $(prove_imp_files:=.out)
tests/%-spec.k.out: tests/%-spec.k $(imp_haskell_kompiled)
	kprove --directory $(imp_haskell_dir) $< --def-module VERIFICATION > $@

### FUN

test_fun_files  := $(wildcard tests/*.fun)
prove_fun_files := $(wildcard tests/*-spec.k)

test-fun: $(test_fun_files:=.run)

tests/%.fun.run: tests/%.fun.out
	$(CHECK) $< tests/$*.fun.k-expected

tests/%-spec.k.prove: tests/%-spec.k.out
	$(CHECK) $< tests/$*-spec.k.expected

.SECONDARY: $(test_fun_files:=.out)
tests/%.fun.out: tests/%.fun $(fun_llvm_kompiled)
	krun --directory $(fun_llvm_dir) $< > $@

.SECONDARY: $(prove_fun_files:=.out)
tests/%-spec.k.out: tests/%-spec.k $(fun_haskell_kompiled)
	kprove --directory $(fun_haskell_dir) $< --def-module VERIFICATION > $@
