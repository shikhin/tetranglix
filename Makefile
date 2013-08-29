# OUTIMG.
OUTIMG := tetranglix.img

ASMSRC := $(shell find ./ -type f -name "*.asm")

# Make related files.
MAKEDEPS := Makefile

# The default target.
all: $(OUTIMG)

# List phony targets.
.PHONY: all clean dog

$(OUTIMG): $(ASMSRC) $(MAKEDEPS) 
	nasm $(ASMSRC) -fbin -o $(OUTIMG)

# Clean.
clean: 
	-$(RM) $(wildcard $(OUTIMG))

# Dog.
dog:
	$(warning Experimental dog generator. Don't try it out; the default size isn't set, so-)
