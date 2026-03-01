# Helper: build a compare function from a known true ordering.
# The first element in `true_order` is the most preferred.
make_compare <- function(true_order) {
  function(item_a, item_b, battle_num) {
    if (match(item_a, true_order) < match(item_b, true_order)) "1" else "2"
  }
}

# --- discerner() ---------------------------------------------------------

test_that("discerner ranks items correctly with 3 items", {
  cmp <- make_compare(c("cherry", "banana", "apple"))
  result <- discerner(c("apple", "banana", "cherry"), compare_fn = cmp)

  expect_equal(result$item, c("cherry", "banana", "apple"))
  expect_equal(result$ranking, 1:3)
})

test_that("discerner ranks items correctly with 5 items", {
  true_order <- c("e", "d", "c", "b", "a")
  cmp <- make_compare(true_order)
  result <- discerner(c("a", "b", "c", "d", "e"), compare_fn = cmp)

  expect_equal(result$item, true_order)
  expect_equal(result$ranking, 1:5)
})

test_that("discerner works with 2 items", {
  cmp <- make_compare(c("B", "A"))
  result <- discerner(c("A", "B"), compare_fn = cmp)

  expect_equal(result$item, c("B", "A"))
  expect_equal(result$ranking, 1:2)
})

test_that("discerner works with numeric items", {
  cmp <- make_compare(c(3, 1, 2))
  result <- discerner(c(1, 2, 3), compare_fn = cmp)

  expect_equal(result$item, c(3, 1, 2))
})

test_that("full_output returns comparisons and count", {
  cmp <- make_compare(c("B", "A"))
  result <- discerner(c("A", "B"), full_output = TRUE, compare_fn = cmp)

  expect_type(result, "list")
  expect_named(result, c("ranking", "comparisons", "n_comparisons"))
  expect_s3_class(result$ranking, "data.frame")
  expect_type(result$comparisons, "list")
  expect_equal(result$n_comparisons, length(result$comparisons))
  expect_true(result$n_comparisons >= 1L)
})

test_that("comparison count is efficient (O(n log n))", {
  # 8 items: binary insertion worst case is sum(ceil(log2(k))) for k=1..7 = 17
  # Exhaustive pairwise would be C(8,2) = 28
  true_order <- letters[1:8]
  cmp <- make_compare(true_order)
  result <- discerner(rev(true_order), full_output = TRUE, compare_fn = cmp)

  expect_equal(result$ranking$item, true_order)
  expect_true(result$n_comparisons <= 17L)
})

# --- Input validation ----------------------------------------------------

test_that("discerner rejects empty input", {
  expect_error(discerner(character(0)), "at least 2 elements")
})

test_that("discerner rejects single item", {
  expect_error(discerner("only_one"), "at least 2 elements")
})

test_that("discerner rejects duplicates", {
  expect_error(discerner(c("a", "b", "a")), "duplicates")
})

test_that("discerner rejects non-character/numeric input", {
  expect_error(discerner(list(1, 2)), "character or numeric")
})

test_that("discerner rejects logical input", {
  expect_error(discerner(c(TRUE, FALSE)), "character or numeric")
})

# --- binary_search_insert() ----------------------------------------------

test_that("binary_search_insert places item correctly at the start", {
  cmp <- make_compare(c("new", "a", "b", "c"))
  result <- discerner:::binary_search_insert(c("a", "b", "c"), "new", cmp, 0L)

  expect_equal(result$sorted, c("new", "a", "b", "c"))
})

test_that("binary_search_insert places item correctly at the end", {
  cmp <- make_compare(c("a", "b", "c", "new"))
  result <- discerner:::binary_search_insert(c("a", "b", "c"), "new", cmp, 0L)

  expect_equal(result$sorted, c("a", "b", "c", "new"))
})

test_that("binary_search_insert places item correctly in the middle", {
  cmp <- make_compare(c("a", "new", "b", "c"))
  result <- discerner:::binary_search_insert(c("a", "b", "c"), "new", cmp, 0L)

  expect_equal(result$sorted, c("a", "new", "b", "c"))
})

test_that("binary_search_insert increments battle_num", {
  cmp <- make_compare(c("a", "b", "new"))
  result <- discerner:::binary_search_insert(c("a", "b"), "new", cmp, 5L)

  expect_true(result$battle_num > 5L)
})

# --- interactive_compare() -----------------------------------------------

test_that("interactive_compare rejects invalid input", {
  # Mock readline to return invalid input
  mockr <- function(prompt) "3"
  result <- tryCatch(
    {
      with_mocked_bindings(
        interactive_compare("a", "b", 1L),
        readline = mockr,
        .package = "base"
      )
    },
    error = function(e) e
  )
  expect_true(inherits(result, "error"))
})
