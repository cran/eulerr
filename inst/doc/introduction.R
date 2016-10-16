## ----venneuler_failure---------------------------------------------------
venn_fit <- venneuler::venneuler(c(A = 75, B = 50, "A&B" = 0))
par(mar = c(0, 0, 0, 0))
plot(venn_fit)

## ----input---------------------------------------------------------------
library(eulerr)

# Input in the form of a named numeric vector
fit1 <- eulerr(c("A" = 25, "B" = 5, "C" = 5,
                 "A&B" = 5, "A&C" = 5, "B&C" = 3,
                 "A&B&C" = 3))

# Input as a matrix of logicals
set.seed(1)
mat <-
  cbind(
  A = sample(c(TRUE, TRUE, FALSE), size = 50, replace = TRUE),
  B = sample(c(TRUE, FALSE), size = 50, replace = TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), size = 50, replace = TRUE)
  )
fit2 <- eulerr(mat)

## ----fit1_print----------------------------------------------------------
fit2

## ----residual_plot-------------------------------------------------------
resid(fit2)

# Cleveland dot plot of the residuals
graphics::dotchart(resid(fit2))
abline(v = 0, lty = 3)

## ----stress--------------------------------------------------------------
fit2$stress

## ----eulerr_plot, fig.show='hold'----------------------------------------
par(mar = c(0, 0, 0, 0))
plot(fit2)

# Change fill colors, border type (remove) and fontface.
plot(fit2,
     polygon_args = list(col = c("dodgerblue4", "darkgoldenrod1", "cornsilk4"),
                         border = "transparent"),
     text_args = list(font = 8))

