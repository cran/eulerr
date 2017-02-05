## ----venneuler_failure, fig.height=5, fig.width=5, fig.cap='venneuler plot with unwanted overlap.', dev.args=list(type='cairo-png')----
venn_fit <- venneuler::venneuler(c(A = 75, B = 50, "A&B" = 0))
par(mar = c(0, 0, 0, 0))
plot(venn_fit)

## ----input---------------------------------------------------------------
library(eulerr)

# Input in the form of a named numeric vector
fit1 <- euler(c("A" = 25, "B" = 5, "C" = 5,
                "A&B" = 5, "A&C" = 5, "B&C" = 3,
                "A&B&C" = 3))

# Input as a matrix of logicals
set.seed(1)
mat <- cbind(
  A = sample(c(TRUE, TRUE, FALSE), size = 50, replace = TRUE),
  B = sample(c(TRUE, FALSE), size = 50, replace = TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), size = 50, replace = TRUE)
)
fit2 <- euler(mat)

## ----fit1_print----------------------------------------------------------
fit2

## ----residual_plot, fig.cap='Residuals for the eulerr fit.'--------------
# Cleveland dot plot of the residuals
dotchart(resid(fit2))
abline(v = 0, lty = 3)

## ----coefs---------------------------------------------------------------
coef(fit2)

## ----eulerr_plot, fig.width=4, fig.height=4, fig.show='hold', fig.cap='eulerr plots can be modified in every possible way.', dev.args=list(type='cairo-png')----
plot(fit2)

# Remove fills, vary border type, and switch fontface.
plot(fit2, fill = "transparent", lty = 1:3, fontface = 4)

