#' Rank items by preference using binary insertion sort
#'
#' Interactively sorts a set of items from most to least preferred by asking
#' pairwise comparison questions. Uses a binary insertion sort algorithm,
#' requiring only O(n log n) comparisons instead of comparing every possible
#' pair.
#'
#' @param items A character or numeric vector of items to rank. Must contain at
#'   least 2 unique values.
#' @param full_output If `TRUE`, returns a list containing both the ranking
#'   and a log of all comparisons made. If `FALSE` (the default), returns only
#'   the ranking data frame.
#' @param compare_fn An optional comparison function with signature
#'   `function(item_a, item_b, battle_num)` that returns `"1"` if `item_a` is
#'   preferred or `"2"` if `item_b` is preferred. Defaults to an interactive
#'   console prompt. Supply a custom function for non-interactive use or
#'   testing.
#'
#' @return If `full_output = FALSE` (default), a data frame with columns:
#'   \describe{
#'     \item{item}{The item.}
#'     \item{ranking}{Integer rank, where 1 is the most preferred.}
#'   }
#'
#'   If `full_output = TRUE`, a list with components:
#'   \describe{
#'     \item{ranking}{The ranking data frame described above.}
#'     \item{comparisons}{A list of comparison records. Each record is a named
#'       character vector with `winner` and `loser`.}
#'     \item{n_comparisons}{Total number of comparisons made.}
#'   }
#'
#' @examples
#' # Non-interactive example using a custom comparison function
#' my_order <- c("cherry", "banana", "apple")
#' auto_compare <- function(item_a, item_b, battle_num) {
#'   if (match(item_a, my_order) < match(item_b, my_order)) "1" else "2"
#' }
#' discerner(c("apple", "banana", "cherry"), compare_fn = auto_compare)
#'
#' @export
discerner <- function(items, full_output = FALSE, compare_fn = NULL) {
  # --- Input validation ---
  if (!is.character(items) && !is.numeric(items)) {
    stop("`items` must be a character or numeric vector.")
  }

  if (length(items) < 2L) {
    stop("`items` must contain at least 2 elements.")
  }

  if (anyDuplicated(items) > 0L) {
    stop("`items` must not contain duplicates.")
  }

  if (is.null(compare_fn)) {
    if (!interactive()) {
      stop(
        "No `compare_fn` supplied and session is not interactive. ",
        "Provide a `compare_fn` for non-interactive use."
      )
    }
    compare_fn <- interactive_compare
  }

  # --- Binary insertion sort ---
  sorted <- items[1L]
  all_comparisons <- list()
  battle_num <- 0L

  for (i in seq_along(items)[-1L]) {
    result <- binary_search_insert(sorted, items[i], compare_fn, battle_num)
    sorted <- result$sorted
    battle_num <- result$battle_num
    all_comparisons <- c(all_comparisons, result$comparisons)
  }

  # --- Build output ---
  ranking <- data.frame(
    item = sorted,
    ranking = seq_along(sorted),
    stringsAsFactors = FALSE
  )

  if (full_output) {
    return(list(
      ranking = ranking,
      comparisons = all_comparisons,
      n_comparisons = length(all_comparisons)
    ))
  }

  ranking
}
