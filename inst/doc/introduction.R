## ----setup, include = FALSE----------------------------------------------
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
knitr::knit_hooks$set(document = function(x) {
  sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)
})

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 2.3,
  fig.height = 2.3,
  crop = TRUE,
  size = "small",
  fig.align = "center",
  dev = "pdf",
  dev.args = list(family = "sans", pointsize = 8)
)
library(lattice)
lattice.options(default.theme = list(fontsize = list(text = 8, points = 4)))


## ----input---------------------------------------------------------------
library(eulerr)
options(digits = 4)

# Input in the form of a named numeric vector
fit1 <- euler(c("A" = 25, "B" = 5, "C" = 5,
                "A&B" = 5, "A&C" = 5, "B&C" = 3,
                "A&B&C" = 3))

# Input as a matrix of logicals
set.seed(1)
mat <- cbind(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c(TRUE, FALSE), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
)
fit2 <- euler(mat)

## ------------------------------------------------------------------------
fit2

## ----residual-plot, eval = FALSE-----------------------------------------
#  # Cleveland dot plot of the residuals
#  lattice::dotplot(resid(fit2), xlab = "",
#                   panel = function(...) {
#                     panel.abline(v = 0, lty = 2)
#                     panel.dotplot(...)
#                   })

## ----residual-dev, ref.label='residual-plot', echo = FALSE, fig.width = 3.5----
# Cleveland dot plot of the residuals
lattice::dotplot(resid(fit2), xlab = "",
                 panel = function(...) {
                   panel.abline(v = 0, lty = 2)
                   panel.dotplot(...)
                 })

## ----coefs---------------------------------------------------------------
coef(fit2)

## ------------------------------------------------------------------------
wilkinson2012 <-  c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 3,
                    "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
                    "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
                    "A&B&F" = 1, "B&C&D" = 1)
fit3 <- euler(wilkinson2012, shape = "ellipse")

## ----wilkinson2012, echo = FALSE, fig.width = 3.4, fig.height = 2.5------
plot(fit3)

## ----eulerr-plot, eval = FALSE-------------------------------------------
#  plot(fit2)

## ----eulerr-dev, ref.label = "eulerr-plot", echo = FALSE-----------------
plot(fit2)

## ----custom-plot, eval = FALSE-------------------------------------------
#  # Remove fills, vary border type, and switch fontface.
#  plot(fit2, fill = "transparent", lty = 1:3, fontface = 4)

## ----custom-plot, ref.label = "custom-plot", echo = FALSE----------------
# Remove fills, vary border type, and switch fontface.
plot(fit2, fill = "transparent", lty = 1:3, fontface = 4)

