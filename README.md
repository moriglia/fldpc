# fLDPC

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15221950.svg)](https://doi.org/10.5281/zenodo.15221950)

This is a Fortran library for LDPC decoding.

## Citation

I would be very happy to know that you are using this software for your research.
If so, please consider adding a citation. You can refer to the attached `CITATION.*` files,
or you can add the following entry to your bibliography:

[1] M. Origlia, ‘Fortran LDPC decoder’. Zenodo, Apr. 15, 2025. doi: 10.5281/zenodo.15221950.


## Supported features

- Decoding a noisy codeword of a specific coset by means of a syndrome.
This feature make this project particularly suitable for the problem of reconciliation.

- Classical decoding (just feed the null syndrome)

## Not supported

### Likely in the future

- more flexible construction of the decoder object from a
[sparse matrix object](https://stdlib.fortran-lang.org/module/stdlib_sparse_kinds.html)

### Unlikely anytime soon

- Information bit encoding (feel free to contribute if you need this feature, I work on reconciliation, and I don't need this right now)


## Note

Currently the decoder is constructed by listing the nodes which each edge of the Tanner graph is connected to.

## Example

In the examples folder, you can find an example BPSK transmission.
You can run the example with `fpm` or with `make`
- `fpm run --example`
- `make examples` (you may need to install some external libraries)

## Tests

The library is shipped with a couple of tests, which you can run again with `fpm` or with `make`:
- `fpm test`
- `make test` (you may need to install some external libraries)

## Install

### with FPM

`fpm install [--profile release]`

### with make

`make install` (you may need to install some external libraries)
