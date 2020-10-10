library(palmerpenguins)

penguins


mean(penguins$bill_length_mm)

str(penguins$bill_length_mm)

# How do I get mean to work is NAs are in a vector?

# Here is a reprex that reproduces the error identified in the question

x <- c(1,2,3,NA,4)
mean(x)


#-----#

``` r
# How do I get mean to work is NAs are in a vector?

# Here is a reprex that reproduces the error identified in the question

x <- c(1,2,3,NA,4)
mean(x)
#> [1] NA
```

<sup>Created on 2020-10-09 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>