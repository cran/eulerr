## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
combos <- c(
  "agc" = 9,
  "camk" = 17,
  "cmgc" = 16,
  "tk" = 16,
  "tkl" = 23,
  "agc&camk" = 1,
  "camk&tk" = 1,
  "tk&tkl" = 1,
  "camk&cmgc&tkl" = 1,
  "camk&tk&tkl" = 2,
  "agc&camk&tk&tkl" = 1,
  "camk&cmgc&tk&tkl" = 3,
  "agc&camk&cmgc&tk&tkl" = 1
)

## -----------------------------------------------------------------------------
library(eulerr)

fit <- euler(combos)
plot(fit)

## -----------------------------------------------------------------------------
losses <- c(
  "sum_squared",
  "sum_absolute",
  "sum_absolute_region_error",
  "max_squared",
  "max_absolute",
  "diag_error"
)

for (loss in losses) {
  fit <- euler(combos, loss = loss)
  print(plot(fit, main = loss))
}

