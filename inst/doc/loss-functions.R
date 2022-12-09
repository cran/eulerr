## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
combos <- c(
  "agc"                  = 9,
  "camk"                 = 17,
  "cmgc"                 = 16,
  "tk"                   = 16,
  "tkl"                  = 23,
  "agc&camk"             = 1,
  "camk&tk"              = 1,
  "tk&tkl"               = 1,
  "camk&cmgc&tkl"        = 1,
  "camk&tk&tkl"          = 2,
  "agc&camk&tk&tkl"      = 1,
  "camk&cmgc&tk&tkl"     = 3,
  "agc&camk&cmgc&tk&tkl" = 1
)

## ----fig.cap = "An Euler diagram fit to the combination given earlier on, showing that only 1-by-1 intersections are present. This fit uses the default loss function, the sum of squared errors."----
library(eulerr)

fit <- euler(combos)
plot(fit)

## ----fig.cap = "Euler diagrams fit to the combination above, using different loss function", fig.show = "hold", fig.width = 3, fig.height = 3----
losses <- c("square", "abs", "region")
aggregators <- c("sum", "max")

for (loss in losses) {
  for (aggregator in aggregators) {
    fit <- euler(combos, loss = loss, loss_aggregator = aggregator)
    print(plot(fit, main = paste(aggregator, loss, sep = ", ")))
  }
}

