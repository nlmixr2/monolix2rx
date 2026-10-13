# Set the number of threads to 2 for CRAN checks

CRAN requires packages to use at most 2 threads. Its example file sorts
first, so the limit holds for every example that follows. It is not
intended for general use.

## Usage

``` r
aaaCranMonolix2rxThreads()
```

## Value

nothing, called for the side effect of limiting the threads to 2 for
CRAN checks

## Details

When testing with
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html) or
[`testthat::test_package()`](https://testthat.r-lib.org/reference/test_package.html),
`NOT_CRAN` is `"true"`, so the number of threads is not limited.

## Author

Matthew L. Fidler

## Examples

``` r

# Set the number of threads to 2 for CRAN checks
aaaCranMonolix2rxThreads()
```
