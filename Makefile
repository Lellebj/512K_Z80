# Make Z80 SBC 
# Copyright LB

SYSINCDIR?=include
SYSLIBDIR?=libs/Build/lib
BUILDLIBDIR?=Build
BUILD_DIR = Build
LIBS=
# DEFINES=-Z80-512K

MAIN=Z80_BOARD


#****************************************************
SOURCES    := $(wildcard *.s)
OBJECTS = $(SOURCES:%$(S_EX)=$(BUILD_DIR)/%.o)

# SOURCES = $(shell find  -maxdepth 1 -name "*$(S_EX)")
# OBJECTS    := $(patsubst %.asm,%.o,$(SOURCES))
APPNAME = $(notdir $(CURDIR))

#****************************************************
# $(info    SRC is $(SOURCES))
# $(info    OBJ is $(OBJECTS))
# $(info    APPNAME is $(APPNAME))


TARGET  = $(BUILD_DIR)/$(APPNAME).$(_EXT)
_EXT=bin
default: $(TARGET)


S_EX=.s
OBJ_EX=o

#	Z88DK linker and assembler
# LDFLAGS=-mz80 -b -l -m  -split-bin
# ASFLAGS=-mz80 -l -m   -o$@  $(DEFINES)
# LD=z80asm
# AS=z80asm

#	Old VASM Z80 assembler and VLINK
LDFLAGS=-T LD/vlink_Z80_pld.ld  -MZ80PLD.map  -b rawbin2  -o $@ -L $(SYSLIBDIR)   
ASFLAGS=-Fvobj -esc -z80asm -o $@ $(F_LIST) $(DEFINES)
LD=vlink
AS=vasmz80_oldstyle


LST=$@.lst
#F_LIST?=
F_LIST = -L $(LST)


RM=rm -f
.SECONDEXPANSION:

DATE := $(shell date +"%Y-%m-%d_%H:%M")
GIT_VERSION := $(shell git describe --long --dirty)
$(info    GIT is $(GIT_VERSION))
# GIT_VERSION := $(shell git describe --long --dirty; git show -s --format='%ci')
# cat $< | sed -e "s/@@DATE@@/$(DATE)/g" -e "s/@@GIT_VERSION@@/$(GIT_VERSION)/g" | z80asm - -o $@ --list=$(basename $@).lst --label=$(basename $@).sym $(ASM_FLAGS)

# test_lev.o : Z80TT*.asm
# 		sed -ri "s/[12][0-9]{3}[-][01][0-9][-][0-3][0-9][_][0-6]{2}[:][0-6]{2}/$(DATE)/g" $^
# 		$(AS)  $(ASFLAGS)  $^


$(OBJECTS) : $$(patsubst $(BUILD_DIR)/%.o, %$(S_EX),$$@)
		mkdir -p $(@D)
		sed -e "s|@@DATE@@|$(DATE)|g" -e "s|@@EEDATE@@|$(DATE)|g" -e "s+@@GIT_VERSION@@+$(GIT_VERSION)+g" $^  >  temp.tmp
		$(AS)  $(ASFLAGS)  temp.tmp
#		mv 	*.lis *.map *.o -t $(BUILD_DIR)
		rm  -f  temp.tmp
		echo

$(TARGET): $(OBJECTS)
	$(LD)  $(LDFLAGS)  $^
	chmod a-x $@



#.PHONY: default

.PHONY: all clean dump

all: $(TARGET)

cp: $(SOURCES)
	cp -f $^ ./Safe_ARC/


.PHONY: all clean load

clean: 
	$(RM) $(OBJECTS)  $(BUILDLIBDIR)/*.bin $(BUILDLIBDIR)/*.map Build/*.* libs/build/*.$(_EXT) *.lis *.o  *.def  *.map t_*.*
