FLAGS = -ggdb 
#-masm=intel 
LIBS = -pthread
FLAGS_OPT = $(FLAGS) -o3
FLAG_ASM = -S
SOURCES = main.c CodePatcher.c
INCLUDES = CodePatcher.h
OUT_FILE = main

all: nopt

nopt: $(SOURCES) $(INCLUDES)
	gcc $(FLAGS) $(SOURCES) -o $(OUT_FILE) $(LIBS)

opt: $(SOURCES) $(INCLUDES)
	gcc $(FLAGS_OPT) $(SOURCES) -o $(OUT_FILE) $(LIBS)
