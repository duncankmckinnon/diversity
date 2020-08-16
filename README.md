
# diversity

<!-- badges: start -->
<!-- badges: end -->

The diversity package is made up of only a few functions that can be used to
calculate a diversity score for the distribution of rows or entries in data.frames or vectors.
It relies on a formal definition of diversity rooted in information theory as the ratio of the
entropy of the given distribution of groupings to the potential entropy of the same data with 
uniformly distributed groupings.  

The idea is that if all groupings are equally well represented in the data, then the groups will
be uniformly distributed in the data (equal numbers of each group).  Since this corresponds to the
maximum entropy scenario for the given data size and number of groupings, diversity can be extracted by looking
at how the entropy of the true distribution of groupings compares to what we know is the maximum
potential entropy.
For data where the distribution of groups seems to favor some groups over others, diversity scores should be low.  For
data where either each entry is uniquely defined or where groupings are uniformly distributed, the diversity score should
be high.

## Installation

You can install the released version of diversity from the devtools package using github with:

``` r
devtools::install_github("duncankmckinnon/diversity")
```

