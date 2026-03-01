#' Interactive pairwise comparison
#'
#' Prompts the user to choose between two items via the console.
#'
#' @param item_a First item to compare.
#' @param item_b Second item to compare.
#' @param battle_num The battle number to display in the prompt.
#'
#' @return `"1"` if the user prefers `item_a`, `"2"` if they prefer `item_b`.
#'
#' @keywords internal
interactive_compare <- function(item_a, item_b, battle_num) {
  answer <- readline(
    paste0("Battle ", battle_num, ": 1) ", item_a, " or 2) ", item_b, " ")
  )

  if (!(answer %in% c("1", "2"))) {
    stop("Response must be 1 or 2.")
  }

  answer
}

#' Insert an item into a sorted vector using binary search
#'
#' Finds the correct position for `new_item` among already-sorted items by
#' performing a binary search, where each comparison is resolved by
#' `compare_fn`. The sorted vector is ordered from most preferred (index 1)
#' to least preferred.
#'
#' @param sorted Character vector of items already sorted by preference
#'   (most preferred first).
#' @param new_item The item to insert.
#' @param compare_fn A function with signature
#'   `function(item_a, item_b, battle_num)` that returns `"1"` if `item_a` is
#'   preferred or `"2"` if `item_b` is preferred.
#' @param battle_num Current battle counter (will be incremented for each
#'   comparison made).
#'
#' @return A list with components:
#'   \describe{
#'     \item{sorted}{Updated sorted vector with `new_item` inserted.}
#'     \item{battle_num}{Updated battle counter.}
#'     \item{comparisons}{A list of comparison records made during insertion.
#'       Each record is a named character vector with `winner` and `loser`.}
#'   }
#'
#' @keywords internal
binary_search_insert <- function(sorted, new_item, compare_fn, battle_num) {
  low <- 1L
  high <- length(sorted)
  comparisons <- list()

  while (low <= high) {
    mid <- (low + high) %/% 2L
    battle_num <- battle_num + 1L

    answer <- compare_fn(new_item, sorted[mid], battle_num)

    if (answer == "1") {
      # User prefers new_item over sorted[mid] -> new_item ranks higher
      comparisons[[length(comparisons) + 1L]] <- c(
        winner = new_item, loser = sorted[mid]
      )
      high <- mid - 1L
    } else {
      # User prefers sorted[mid] over new_item -> new_item ranks lower
      comparisons[[length(comparisons) + 1L]] <- c(
        winner = sorted[mid], loser = new_item
      )
      low <- mid + 1L
    }
  }

  # Insert new_item at position `low`
  sorted <- append(sorted, new_item, after = low - 1L)

  list(sorted = sorted, battle_num = battle_num, comparisons = comparisons)
}
