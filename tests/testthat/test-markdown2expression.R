test_that("input validation works", {
  expect_error(markdown2expression(123))
  expect_error(markdown2expression(NULL))
  expect_error(markdown2expression(list("text")))
})

test_that("plain text returns correct expression", {
  result <- markdown2expression("hello")
  expect_equal(deparse(result), "\"hello\"")
})

test_that("single letter returns symbol expression", {
  result <- markdown2expression("x")
  expect_equal(deparse(result), "x")
})

test_that("bold formatting is converted correctly", {
  result <- markdown2expression("**bold**")
  expect_equal(deparse(result), "bold(\"bold\")")
})

test_that("italic formatting is converted correctly", {
  result <- markdown2expression("*italic*")
  expect_equal(deparse(result), "italic(\"italic\")")
})

test_that("superscript is converted correctly", {
  result <- markdown2expression("x^2^")
  expect_equal(deparse(result), "x^2")
})

test_that("subscript is converted correctly", {
  result <- markdown2expression("H~2~")
  expect_equal(deparse(result), "H[2]")
})

test_that("superscript without base throws error", {
  expect_error(markdown2expression("^2^"))
})

test_that("subscript without base throws error", {
  expect_error(markdown2expression("~2~"))
})

test_that("only first element of vector is used", {
  result1 <- suppressWarnings(markdown2expression(c("**bold**", "*italic*")))
  result2 <- markdown2expression("**bold**")
  expect_equal(deparse(result1), deparse(result2))
})

test_that("nested bold and subscript works", {
  result <- markdown2expression("**H~2~O**")
  expect_type(result, "language")
  expr_str <- deparse(result)
  expect_match(expr_str, "bold")
})

test_that("mixed text and formatting works", {
  result <- markdown2expression("R^2^ value")
  expect_type(result, "language")
  expr_str <- deparse(result)
  expect_match(expr_str, "\\^")
  expect_match(expr_str, "value")
})

test_that("result is usable in plot without error", {
  expect_no_error({
    expr <- markdown2expression("**Title** x^2^")
    grDevices::recordPlot()
    plot(1, main = expr)
  })
})

test_that("result is a language object if markdown is contained", {
  expect_type(markdown2expression("**bold**"), "language")
  expect_type(markdown2expression("x^2^"), "language")
})

test_that("result is character object if no markdown is contained", {
  expect_type(markdown2expression("This is a simple test."), "character")
})

# --- Unclosed delimiters ---
test_that("unclosed bold throws error", {
  expect_error(markdown2expression("**bold"), "Unclosed")
})

test_that("unclosed italic throws error", {
  expect_error(markdown2expression("*italic"), "Unclosed")
})

test_that("unclosed superscript throws error", {
  expect_error(markdown2expression("x^2"), "Unclosed")
})

test_that("unclosed subscript throws error", {
  expect_error(markdown2expression("H~2"), "Unclosed")
})

test_that("unclosed greek throws error", {
  expect_error(markdown2expression("{alpha"), "Unclosed")
})

# --- Escape character ---
test_that("escaped asterisk is treated as literal text", {
  result <- markdown2expression("2 \\* 2")
  expect_match(deparse(result), "\\*")
})

test_that("escaped caret is treated as literal text", {
  result <- markdown2expression("x\\^2")
  expect_match(deparse(result), "\\^")
})

# --- Greek letters ---
test_that("greek letter alpha is converted", {
  result <- markdown2expression("{alpha}")
  expect_equal(deparse(result), "alpha")
})

test_that("greek letter Omega is converted", {
  result <- markdown2expression("{Omega}")
  expect_equal(deparse(result), "Omega")
})

test_that("unknown curly brace content is treated as text", {
  result <- markdown2expression("{notgreek}")
  expect_true(is.character(result))
})

# --- Newline ---
test_that("newline produces atop expression", {
  result <- markdown2expression("line1\nline2")
  expect_true(is.language(result))
  expect_match(deparse(result), "atop")
})

# --- Combinations ---
test_that("subscript with bold base works", {
  result <- markdown2expression("**H~2~O**")
  expect_true(is.language(result))
  expect_match(deparse(result), "bold")
})

test_that("greek with superscript works", {
  result <- markdown2expression("{alpha}^2^")
  expect_true(is.language(result))
  expect_match(deparse(result), "alpha")
})

test_that("mixed formatting produces valid expression", {
  result <- markdown2expression("R^2^ = *0.99*")
  expect_true(is.language(result))
})

# --- Vectorize ---
test_that("vectorize returns list of expressions", {
  result <- markdown2expression(c("**bold**", "*italic*"), vectorize = TRUE)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, is.language)))
})

# --- Usable in plot ---
test_that("result works in plot without error", {
  expect_no_error({
    expr <- markdown2expression("R^2^ = 0.99")
    plot(1, main = expr)
  })
})