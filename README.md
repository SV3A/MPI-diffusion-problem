Parallelization of the diffusion problem
========================================
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/SV3A/MPI-diffusion-problem/blob/master/LICENSE)

Parallelized Fortran code, using the MPI API, that solves the unsteady two-dimensional [diffusion equation](https://scipython.com/book/chapter-7-matplotlib/examples/the-two-dimensional-diffusion-equation/) in a m x n domain, using a simple finite difference scheme.

<img src="doc/domain.png" alt="Solution domain" width="500" height="348">


## Build
Two makefiles are included: one for the Sun Studio 12 compiler (tested with ver. 5), and another for GFortran.

Just run `make` (sun) or `make --file=makefile_gnu` (GFortran)

This creates a main binary called `bin`, which may be executed with `mpirun -np 8 ./bin` (e.g. for using 8 processors).


### The code
- Discretizes the problem using 2nd finite-differences for the Laplacian.
- Creates a mesh of size (m x n).
- Decomposes the mesh into subset layers (according to number of procs) with ghost layers:

<img src="doc/ghost-layers.png" alt="Ghost layers" width="250" height="217">

- Integrates the solution in time using explicit Euler time integration.


Domains specification etc. are set in the `inputfile.txt` file.
