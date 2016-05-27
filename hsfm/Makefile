
PROGS  = hsfm
SOURCES = Main.hs MainWindow.hs Misc.hs Files.hs

all : $(PROGS)

hsfm : $(SOURCES)
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

all: 
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o)
clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)
HC=ghc
