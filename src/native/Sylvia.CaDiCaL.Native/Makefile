# Build sylvia_cadical.dll — the native interop layer over CaDiCaL.
#
# MUST be built with the same g++ that produced libcadical.a, and against the same CaDiCaL headers:
# ProofCapture subclasses CaDiCaL::Tracer, so its vtable is generated from tracer.hpp at compile
# time. A header/archive mismatch dispatches to the wrong slot — memory corruption at solve time,
# not a link error. `make check-version` compares the two before building.
#
# MSVC is not an option here: libcadical.a is a GNU ar archive, Tracer's vtable follows the Itanium
# C++ ABI, its virtuals take libstdc++'s std::vector, and CaDiCaL has no MSVC support at all
# (_MSC_VER appears nowhere in the tree; six sources include POSIX headers).
#
#   make            build ../../../bin/sylvia_cadical.dll
#   make check      build, then verify the DLL loads and its version matches libcadical.a
#   make clean

CXX          ?= g++
CADICAL_SRC  ?= ../../../reference/projects/cadical-rel-3.0.1
CADICAL_LIB  ?= ../../../bin/libcadical.a
OUTDIR       ?= ../../../bin
TARGET       := $(OUTDIR)/sylvia_cadical.dll

CXXFLAGS ?= -O2 -std=c++11 -Wall -Wextra -Wno-unused-parameter -I$(CADICAL_SRC)/src

# Static libstdc++/libgcc/winpthread makes the DLL self-contained: unlike bin/cadical.dll it needs
# no mingw runtime DLLs alongside it, so it loads from a plain Windows process with nothing on PATH.
# Safe because nothing is thrown across the boundary — every entry point catches (see SC_GUARD).
LDFLAGS ?= -shared -static -static-libgcc -static-libstdc++

.PHONY: all check check-version clean

all: $(TARGET)

$(TARGET): sylvia_cadical.cpp sylvia_cadical.h | check-version
	@mkdir -p $(OUTDIR)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ sylvia_cadical.cpp $(CADICAL_LIB)
	@echo "built $@"

# The mismatch this guards against is silent at link time and fatal at run time, so check it up
# front rather than after a crash.
check-version:
	@test -f $(CADICAL_LIB) || { echo "ERROR: $(CADICAL_LIB) not found"; exit 1; }
	@test -f $(CADICAL_SRC)/src/tracer.hpp || { echo "ERROR: $(CADICAL_SRC)/src/tracer.hpp not found"; exit 1; }
	@src_v=`cat $(CADICAL_SRC)/VERSION`; \
	 lib_v=`strings -a $(CADICAL_LIB) | grep -oE 'cadical-[0-9]+\.[0-9]+\.[0-9]+' | head -1 | sed 's/cadical-//'`; \
	 if [ "$$src_v" != "$$lib_v" ]; then \
	   echo "ERROR: header/archive mismatch — $(CADICAL_SRC) is $$src_v, $(CADICAL_LIB) is $$lib_v."; \
	   echo "       Rebuild libcadical.a from this source tree; a Tracer subclass compiled against"; \
	   echo "       mismatched headers corrupts memory instead of failing to link."; \
	   exit 1; \
	 fi; \
	 echo "version check: both $$src_v"

check: $(TARGET)
	@dotnet fsi ../../../examples/sat/NativeSmoke.fsx

clean:
	rm -f $(TARGET)
