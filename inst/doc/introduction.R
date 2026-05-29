## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(digits = 4)

## -----------------------------------------------------------------------------
library(eulerr)

# Input in the form of a named numeric vector
fit1 <- euler(c(
  "A" = 25,
  "B" = 5,
  "C" = 5,
  "A&B" = 5,
  "A&C" = 5,
  "B&C" = 3,
  "A&B&C" = 3
))

# Input as a matrix of logicals
set.seed(1)
mat <- cbind(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c(TRUE, FALSE), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
)
fit2 <- euler(mat)

## -----------------------------------------------------------------------------
fit2

## -----------------------------------------------------------------------------
# Cleveland dot plot of the residuals
dotchart(resid(fit2))

## -----------------------------------------------------------------------------
coef(fit2)

## -----------------------------------------------------------------------------
wilkinson2012 <- c(
  A = 4,
  B = 6,
  C = 3,
  D = 2,
  E = 7,
  F = 3,
  "A&B" = 2,
  "A&F" = 2,
  "B&C" = 2,
  "B&D" = 1,
  "B&F" = 2,
  "C&D" = 1,
  "D&E" = 1,
  "E&F" = 1,
  "A&B&F" = 1,
  "B&C&D" = 1
)
fit3 <- euler(wilkinson2012, shape = "ellipse")
plot(fit3)

## -----------------------------------------------------------------------------
plot(fit2)

# Switch to squares for the same set combination, customizing borders,
# labels, and quantities. The `shape` argument also accepts `"ellipse"`
# and `"rectangle"`.
plot(
  euler(mat, shape = "square"),
  quantities = TRUE,
  fill = "transparent",
  lty = 1:3,
  labels = list(font = 4)
)

