# Evaluate Progressions/Regressions w.r.t. a Benchmark

## Dependencies
If you use nix simply run the script as ./Main.hs.
Otherwise use runhaskell with installed libraries megaparsec terminal-progress-bar extra async async-pool cpuinfo.

## Evaluate a Benchmark
The corresponding subcommand is `bench`.
Input: A Benchmark Set (for example TPDB: https://github.com/TermCOMP/TPDB), A KoAT executable

Running `./Main.hs bench path/to/koat/binary path/to/benchmarkset` will evaluate the specified binary on all
koat files in the benchmark set in parallel. A timeout (default: 300s) is used.
Finally a database containing all results is dumped to the working directory.

## Show Regressions/Progressions
To show all regressions/progressions between to result files run

```
./Main.hs diff oldfile newfile
```

