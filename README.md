# discerner

R package for ranked preference sorting using binary insertion.

## Overview

`discerner` ranks a set of items from most to least preferred by asking you
pairwise "battle" questions. It uses a binary insertion sort algorithm, so it
needs only O(n log n) comparisons instead of asking about every possible pair.

## Installation

```r
# install.packages("devtools")
devtools::install_github("your-username/discerner")
```

## Usage

```r
library(discerner)

# Interactive: you'll be prompted to pick a winner for each battle
result <- discerner(c("pizza", "tacos", "sushi", "burgers"))
print(result)
#>     item ranking
#> 1  sushi       1
#> 2  tacos       2
#> 3  pizza       3
#> 4 burgers      4

# Get the full comparison log
result <- discerner(c("pizza", "tacos", "sushi"), full_output = TRUE)
result$ranking       # ranked data frame
result$comparisons   # list of winner/loser pairs
result$n_comparisons # total battles fought
```

## Non-interactive / programmatic use

Supply a custom `compare_fn` for scripting or testing:

```r
# Define a comparison function based on a known ordering
my_order <- c("sushi", "tacos", "pizza")
auto_compare <- function(item_a, item_b, battle_num) {
  if (match(item_a, my_order) < match(item_b, my_order)) "1" else "2"
}

discerner(c("pizza", "tacos", "sushi"), compare_fn = auto_compare)
```
