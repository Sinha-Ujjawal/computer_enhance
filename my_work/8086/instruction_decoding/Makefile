DEBUG ?= 0
CC ?= cc
CLAGS ?= -Wall -Wextra -pedantic -std=c23 -D_POSIX_C_SOURCE=200809L

ifeq ($(DEBUG), 1)
	CLAGS += -g -O0 -gdwarf-4
else
	CLAGS += -O2
endif

all: dasm8086

dasm8086: dasm8086.c
	$(CC) $(CLAGS) -o dasm8086 dasm8086.c
