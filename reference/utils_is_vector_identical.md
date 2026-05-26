# Check if two vectors are identical in length and content

This function compares two vectors and returns \`TRUE\` if they have the
same length and the same elements (irrespective of order).

## Usage

``` r
utils_is_vector_identical(x, y)
```

## Arguments

- x:

  First vector to compare.

- y:

  Second vector to compare.

## Value

A logical value (\`TRUE\` or \`FALSE\`). Returns \`TRUE\` if both
vectors have the same length and the same elements, otherwise \`FALSE\`.

## Examples

``` r
is_vector_identical(c(1, 2, 3), c(3, 2, 1))         # TRUE
#> Error in is_vector_identical(c(1, 2, 3), c(3, 2, 1)): could not find function "is_vector_identical"
is_vector_identical(c(1, 2), c(1, 2, 3))            # FALSE
#> Error in is_vector_identical(c(1, 2), c(1, 2, 3)): could not find function "is_vector_identical"
is_vector_identical(c("a", "b"), c("a", "b"))       # TRUE
#> Error in is_vector_identical(c("a", "b"), c("a", "b")): could not find function "is_vector_identical"
is_vector_identical(c("a", "b"), c("a", "b", "c"))  # FALSE
#> Error in is_vector_identical(c("a", "b"), c("a", "b", "c")): could not find function "is_vector_identical"
```
