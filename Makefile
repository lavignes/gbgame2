rwildcard = $(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

ASM_PATH := tools/asm
ASM := $(ASM_PATH)/target/release/gbasm
LINK := $(ASM_PATH)/target/release/gblink
FIX := $(ASM_PATH)/target/release/gbfix

SRCS := $(call rwildcard,src,*.asm)
OBJS := $(SRCS:.asm=.o)
DEPS := $(SRCS:.asm=.d)

LOG_LEVEL := WARN
ASM_FLAGS := -DDEBUG=1 -l $(LOG_LEVEL) -I include
LINK_FLAGS := -c link.toml -l $(LOG_LEVEL) -g game.sym --tags game.tags
FIX_FLAGS := -l $(LOG_LEVEL)

game.gb: link.toml $(DEPS) $(OBJS) $(LINK) $(FIX)
	$(LINK) $(LINK_FLAGS) -o $@ $(OBJS)
	$(FIX) $(FIX_FLAGS) -o $@ $@

%.o: %.asm $(ASM)
	$(ASM) $(ASM_FLAGS) -o $@ $<

%.d: %.asm $(ASM)
	$(ASM) $(ASM_FLAGS) -M -o $@ $<

$(ASM):
	cd $(ASM_PATH) && cargo build --release --bin gbasm

$(LINK):
	cd $(ASM_PATH) && cargo build --release --bin gblink

$(FIX):
	cd $(ASM_PATH) && cargo build --release --bin gbfix

.PHONY: deepclean
deepclean: clean
	cd $(ASM_PATH) && cargo clean

.PHONY: clean
clean:
	rm -f $(call rwildcard,src,*.o)
	rm -f $(call rwildcard,src,*.d)
	rm -f game.gb
	rm -f game.sym
	rm -f game.tags

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
include $(DEPS)
endif

