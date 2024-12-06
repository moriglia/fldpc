# fLDPC

This is a Fortran library for LDPC decoding.

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

{!examples/bpsk.f90}
