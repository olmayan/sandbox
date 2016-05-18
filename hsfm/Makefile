
PROGS  = hsfm
SOURCES = FileManagerExt.hs

all : $(PROGS)

hsfm : FileManagerExt.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

all: 
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o)
#clean:
	#rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)
HC=ghc
