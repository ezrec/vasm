# Unix

TARGET =
TARGETEXTENSION = 

CC = gcc
CCOUT = -o 
COPTS = -c -O2 

LD = $(CC)
LDOUT = $(CCOUT)
LDFLAGS = -lm
MKDIR = mkdir -p

RM = rm -f

include make.rules
