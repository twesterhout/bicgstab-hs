EIGEN_VERSION = 3.4.0
CXX ?= clang++
CLANG_WARNINGS = -Wall -Wextra -Wshadow -Wnon-virtual-dtor \
 -Wold-style-cast -Wcast-align -Wunused -Woverloaded-virtual \
 -Wpedantic -Wconversion -Wsign-conversion  -Wnull-dereference \
 -Wdouble-promotion -Wformat=2 -Wno-gnu-statement-expression 
GCC_WARNINGS = -pedantic -W -Wall -Wextra -Wcast-align -Wcast-qual \
 -Wctor-dtor-privacy -Wdisabled-optimization -Wformat=2 -Winit-self \
 -Wlogical-op -Wmissing-declarations -Wmissing-include-dirs -Wnoexcept \
 -Wold-style-cast -Woverloaded-virtual -Wredundant-decls -Wshadow \
 -Wsign-conversion -Wsign-promo -Wstrict-null-sentinel \
 -Wduplicated-cond -Wduplicated-branches -Wuseless-cast \
 -Wstrict-overflow=5 -Wswitch-default -Wundef -Wno-unused
CXXFLAGS ?= $(CLANG_WARNINGS)


cbits/eigen-$(EIGEN_VERSION).tar.bz2:
	curl -LJO https://gitlab.com/libeigen/eigen/-/archive/$(EIGEN_VERSION)/eigen-$(EIGEN_VERSION).tar.bz2

cbits/eigen-$(EIGEN_VERSION): cbits/eigen-$(EIGEN_VERSION).tar.bz2
	cd cbits && \
	tar -xf eigen-$(EIGEN_VERSION).tar.bz2

cbits/dlpack/dlpack.h:
	cd cbits && \
	mkdir -p dlpack && \
	cd dlpack && \
	curl -LJO https://raw.githubusercontent.com/dmlc/dlpack/main/include/dlpack/dlpack.h

cbits/bicgstab.o: cbits/bicgstab.cpp cbits/dlpack/dlpack.h cbits/eigen-$(EIGEN_VERSION)
	$(CXX) $(CXXFLAGS) -std=c++11 -isystem cbits/eigen-$(EIGEN_VERSION) -c -o $@ $<
