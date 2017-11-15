### R code from vignette source 'under-the-hood.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: setup
###################################################
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
  dev.args = list(family = "sans", pointsize = 8),
  cache = TRUE,
  fig.path = "figure/graphics-",
  cache.path = "cache/graphics-"
)
library(lattice)
lattice.options(default.theme = list(fontsize = list(text = 8, points = 4)))


###################################################
### code chunk number 2: under-the-hood.Rnw:214-217
###################################################
matrix(sample(c(TRUE, FALSE), 12, replace = TRUE),
       ncol = 3,
       dimnames = list(NULL, c("A", "B", "C")))


###################################################
### code chunk number 3: under-the-hood.Rnw:222-227
###################################################
matrix(c(TRUE, FALSE, FALSE,
         TRUE, TRUE, FALSE,
         FALSE, FALSE, TRUE),
       ncol = 3,
       dimnames = list(NULL, c("A", "B", "C")))


###################################################
### code chunk number 4: under-the-hood.Rnw:232-233
###################################################
as.table(apply(Titanic, 2:4, sum))


###################################################
### code chunk number 5: under-the-hood.Rnw:237-240
###################################################
list(A = c("x", "xy", "xyz"),
     B = c("xy"),
     C = c("x", "xyz"))


###################################################
### code chunk number 6: under-the-hood.Rnw:256-258
###################################################
library(eulerr)
eulerr:::bit_indexr(3)


###################################################
### code chunk number 7: under-the-hood.Rnw:287-298
###################################################
r1 <- 0.7 #radius of set 1
r2 <- 0.9 #radius of set 2
overlap <- 1 #area of overlap

stats::optimize(eulerr:::discdisc, #computes the squared loss
                interval = c(abs(r1 - r2), sum(r1, r2)),
                r1 = r1,
                r2 = r2,
                overlap = overlap)

# minimum is our required distance


###################################################
### code chunk number 8: under-the-hood.Rnw:523-528
###################################################
combo <- c("A" = 1, "B" = 1, "C" = 1,
           "A&B" = 0.5, "A&C" = 0.5, "C&B" = 0.5)

fit1 <- euler(combo)
fit1


###################################################
### code chunk number 9: circle-plot
###################################################
plot(fit1, counts = TRUE)


###################################################
### code chunk number 10: under-the-hood.Rnw:542-544
###################################################
fit2 <- euler(combo, shape = "ellipse")
fit2


###################################################
### code chunk number 11: ellipse-plot
###################################################
plot(fit2, counts = TRUE)


###################################################
### code chunk number 12: legend-plot (eval = FALSE)
###################################################
## plot(euler(c(A = 2, B = 3, "A&B" = 1)), auto.key = TRUE)


###################################################
### code chunk number 13: legend-dev
###################################################



###################################################
### code chunk number 14: counts-plot (eval = FALSE)
###################################################
## plot(euler(c(A = 2, B = 3, "A&B" = 1)), counts = TRUE)


###################################################
### code chunk number 15: counts-dev
###################################################



###################################################
### code chunk number 16: under-the-hood.Rnw:615-621
###################################################
n <- 500
seqn <- seq(0, n, 1)
theta <- seqn*pi*(3 - sqrt(5))
rad <- sqrt(seqn/n)
x <- rad*cos(theta)
y <- rad*sin(theta)


###################################################
### code chunk number 17: under-the-hood.Rnw:625-629
###################################################
xyplot(y ~ x, asp = 1, pch = 16, cex = 0.5, axes = FALSE, xlab = NULL,
       col = 1,
       ylab = NULL, scales = list(draw = FALSE),
       par.settings = list(axis.line = list(col = "transparent")))


