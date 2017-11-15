### R code from vignette source 'introduction.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: setup
###################################################
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
knitr::knit_hooks$set(document = function(x) {
  sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)
})

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 2.1,
  fig.height = 2.1,
  crop = TRUE,
  size = "small",
  fig.align = "center",
  dev = "pdf",
  dev.args = list(family = "sans", pointsize = 8),
  cache = TRUE,
  fig.path = "figure/graphics-",
  cache.path = "cache/graphics-",
  autodep = TRUE
)
library(lattice)
lattice.options(default.theme = list(fontsize = list(text = 8, points = 4)))



###################################################
### code chunk number 2: input
###################################################
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


###################################################
### code chunk number 3: introduction.Rnw:259-260
###################################################
fit2


###################################################
### code chunk number 4: residual-plot (eval = FALSE)
###################################################
## # Cleveland dot plot of the residuals
## lattice::dotplot(resid(fit2), xlab = "",
##                  panel = function(...) {
##                    panel.abline(v = 0, lty = 2)
##                    panel.dotplot(...)
##                  })


###################################################
### code chunk number 5: residual-dev
###################################################



###################################################
### code chunk number 6: coefs
###################################################
coef(fit2)


###################################################
### code chunk number 7: introduction.Rnw:312-317
###################################################
wilkinson2012 <-  c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 3,
                    "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
                    "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
                    "A&B&F" = 1, "B&C&D" = 1)
fit3 <- euler(wilkinson2012, shape = "ellipse")


###################################################
### code chunk number 8: wilkinson2012
###################################################
plot(fit3)


###################################################
### code chunk number 9: eulerr-plot (eval = FALSE)
###################################################
## plot(fit2)


###################################################
### code chunk number 10: eulerr-dev
###################################################



###################################################
### code chunk number 11: custom-plot (eval = FALSE)
###################################################
## # Remove fills, vary border type, and switch fontface.
## plot(fit2, fill = "transparent", lty = 1:3, fontface = 4)


###################################################
### code chunk number 12: custom-plot
###################################################



