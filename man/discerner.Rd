\name{discerner}
\alias{discerner}
\title{Rank items by preference using binary insertion sort}
\usage{
discerner(items, full_output = FALSE, compare_fn = NULL)
}
\arguments{
\item{items}{A character or numeric vector of items to rank. Must contain at
least 2 unique values.}

\item{full_output}{If \code{TRUE}, returns a list containing both the ranking
and a log of all comparisons made. If \code{FALSE} (the default), returns only
the ranking data frame.}

\item{compare_fn}{An optional comparison function with signature
\code{function(item_a, item_b, battle_num)} that returns \code{"1"} if \code{item_a} is
preferred or \code{"2"} if \code{item_b} is preferred. Defaults to an interactive
console prompt. Supply a custom function for non-interactive use or
testing.}
}
\value{
If \code{full_output = FALSE} (default), a data frame with columns:
\describe{
  \item{item}{The item.}
  \item{ranking}{Integer rank, where 1 is the most preferred.}
}

If \code{full_output = TRUE}, a list with components:
\describe{
  \item{ranking}{The ranking data frame described above.}
  \item{comparisons}{A list of comparison records. Each record is a named
    character vector with \code{winner} and \code{loser}.}
  \item{n_comparisons}{Total number of comparisons made.}
}
}
\description{
Interactively sorts a set of items from most to least preferred by asking
pairwise comparison questions. Uses a binary insertion sort algorithm,
requiring only O(n log n) comparisons instead of comparing every possible
pair.
}
\examples{
# Non-interactive example using a custom comparison function
my_order <- c("cherry", "banana", "apple")
auto_compare <- function(item_a, item_b, battle_num) {
  if (match(item_a, my_order) < match(item_b, my_order)) "1" else "2"
}
discerner(c("apple", "banana", "cherry"), compare_fn = auto_compare)
}
