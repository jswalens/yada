# Yada

This is the Yada benchmark, an adapted version of the Yada benchmark of the STAMP benchmark suite [Minh2008] ported to Clojure.

This application does not use transactional futures and was not used for our evaluation [Swalens2016].

## How to run

You can run the program using [Leiningen](https://leiningen.org/).

Run the benchmark as follows (all parameters are optional):

    $ lein run -- -i inputs/spiral.2

Parameters:
* `-a`: minimal angle constraint.
* `-i`: input name prefix.
* `-t`: number of threads.
* `-p`: enable profiling.

(Run `lein run -- -h` to get this description and more.)

Running the program prints the given options and the total execution time to the screen.

## License
Licensed under the MIT license, included in the file `LICENSE`.

## References

[Swalens2016]
J. Swalens, J. De Koster, and W. De Meuter. 2016. "Transactional Tasks: Parallelism in Software Transactions". In _Proceedings of the 30th European Conference on Object-Oriented Programming (ECOOP'16)_.

[Minh2008]
C. C. Minh, J. Chung, C. Kozyrakis, and K. Olukotun. 2008. "STAMP: Stanford Transactional Applications for Multi-Processing". In _Proceedings of the IEEE International Symposium on Workload Characterization (IISWC'08)_.
