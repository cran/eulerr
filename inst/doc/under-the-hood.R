## ----setup, include = FALSE----------------------------------------------
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
#knitr::knit_theme$set("default")

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 2.1,
  fig.height = 2.1,
  crop = TRUE,
  fig.align = "center",
  dev = "pdf",
  dev.args = list(family = "sans", pointsize = 8)
)
library(lattice)
lattice.options(default.theme = list(fontsize = list(text = 8, points = 4)))

## ------------------------------------------------------------------------
matrix(sample(c(TRUE, FALSE), 12, replace = TRUE),
       ncol = 3,
       dimnames = list(NULL, c("A", "B", "C")))

## ------------------------------------------------------------------------
matrix(c(TRUE, FALSE, FALSE,
         TRUE, TRUE, FALSE,
         FALSE, FALSE, TRUE),
       ncol = 3,
       dimnames = list(NULL, c("A", "B", "C")))

## ------------------------------------------------------------------------
as.table(apply(Titanic, 2:4, sum))

## ------------------------------------------------------------------------
list(A = c("x", "xy", "xyz"),
     B = c("xy"),
     C = c("x", "xyz"))

## ------------------------------------------------------------------------
library(eulerr)
eulerr:::bit_indexr(3)

## ------------------------------------------------------------------------
r1 <- 0.7 #radius of set 1
r2 <- 0.9 #radius of set 2
overlap <- 1 #area of overlap

stats::optimize(eulerr:::discdisc, #computes the squared loss
                interval = c(abs(r1 - r2), sum(r1, r2)),
                r1 = r1,
                r2 = r2,
                overlap = overlap)

# minimum is our required distance

## ------------------------------------------------------------------------
combo <- c("A" = 1, "B" = 1, "C" = 1,
           "A&B" = 0.5, "A&C" = 0.5, "C&B" = 0.5)

fit1 <- euler(combo)
fit1

## ----circle-plot, echo = FALSE-------------------------------------------
plot(fit1, counts = TRUE)

## ------------------------------------------------------------------------
fit2 <- euler(combo, shape = "ellipse")
fit2

## ----ellipse-plot, echo = FALSE------------------------------------------
plot(fit2, counts = TRUE)

## ----legend-plot, eval = FALSE-------------------------------------------
#  plot(euler(c(A = 2, B = 3, "A&B" = 1)), auto.key = TRUE)

## ----legend-dev, ref.label = "legend-plot", echo = FALSE-----------------
plot(euler(c(A = 2, B = 3, "A&B" = 1)), auto.key = TRUE)

## ----counts-plot, eval = FALSE-------------------------------------------
#  plot(euler(c(A = 2, B = 3, "A&B" = 1)), counts = TRUE)

## ----counts-dev, ref.label = "counts-plot", echo = FALSE-----------------
plot(euler(c(A = 2, B = 3, "A&B" = 1)), counts = TRUE)

## ------------------------------------------------------------------------
n <- 500
seqn <- seq(0, n, 1)
theta <- seqn*pi*(3 - sqrt(5))
rad <- sqrt(seqn/n)
x <- rad*cos(theta)
y <- rad*sin(theta)

## ----echo = FALSE--------------------------------------------------------
xyplot(y ~ x, asp = 1, pch = 16, cex = 0.5, axes = FALSE, xlab = NULL,
       col = 1,
       ylab = NULL, scales = list(draw = FALSE),
       par.settings = list(axis.line = list(col = "transparent")))

