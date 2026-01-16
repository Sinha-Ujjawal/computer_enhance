DEBUG ?= 0
CC ?= cc
CLAGS ?= -Wall -Wextra -pedantic

ifeq ($(DEBUG), 1)
	CLAGS += -g -O0
else
	CLAGS += -O2
endif

all: dasm8086.exe

dasm8086.exe: dasm8086.c
	$(CC) $(CLAGS) -o dasm8086.exe dasm8086.c
