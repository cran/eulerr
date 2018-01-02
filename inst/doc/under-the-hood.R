## ----setup, include = FALSE----------------------------------------------
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
knitr::knit_hooks$set(
  crop = knitr::hook_pdfcrop,
  document = function(x) {
    sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)
  }
)

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 2.1,
  fig.height = 2.1,
  crop = TRUE,
  fig.align = "center",
  dev = "pdf",
  dev.args = list(family = "sans", pointsize = 8)
)

library(grid)
library(RConics)
library(eulerr)
library(lattice)

lattice.options(default.theme = list(fontsize = list(text = 8, points = 6)))

pal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
         "#0072B2", "#D55E00", "#CC79A7")

ellipse_arc <- function(saxes = c(1, 1),
                        loc = c(0, 0),
                        theta = 0,
                        n = 200,
                        rng = c(0, 2*pi)) {
  b <- min(saxes[1], saxes[2])
  a <- max(saxes[1], saxes[2])
  d2 <- (a - b) * (a + b)
  if (length(rng) == 1)
    phi <- rng - theta
  else
    phi <- seq(rng[1], rng[2], len = n) - theta
  sp <- sin(phi)
  cp <- cos(phi)
  r <- a * b/sqrt((saxes[2] * cp)^2 + (saxes[1] * sp)^2)
  P <- matrix(nrow = n, ncol = 2)
  P[, 1] <- r * cp
  P[, 2] <- r * sp

  if (theta != 0) {
    P <- P %*% matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
                      byrow = TRUE, nrow = 2, ncol = 2)
  }
  P <- P + matrix(loc[1:2], nrow = nrow(P), ncol = 2, byrow = TRUE)
  P
}

set.seed(1)

## ----circleOverlap, fig.width=2.2, fig.height = 2.4----------------------
c0 <- ellipseToConicMatrix(c(1, 1), c(0, 0), 0)
c1 <- ellipseToConicMatrix(c(0.7, 0.7), c(1.2, 0), 0)
pp <- intersectConicConic(c0, c1)

theta0 <- atan2(pp[2, 1], pp[1, 1])
theta1 <- atan2(pp[2, 2], pp[1, 2])

phi0 <- atan2(pp[2, 1], pp[1, 1] - 1.2)
phi1 <- atan2(pp[2, 2], pp[1, 2] - 1.2)

seg <- rbind(ellipse_arc(c(0.7, 0.7), c(1.2, 0), 0, n = 50, c(-pi, phi0)),
             ellipse_arc(c(1, 1), c(0, 0), 0, n = 100, c(theta0, theta1)),
             ellipse_arc(c(0.7, 0.7), c(1.2, 0), 0, n = 50, c(phi1, pi)))

ospot <- c(mean(seg[, 1]), mean(seg[, 2]))

xyplot(1~1, xlim = c(-1.1, 2), ylim = c(-1.5, 1.1), asp = "iso",
       xlab = NULL, ylab = NULL,
       scales = list(draw = FALSE),
       par.settings = list(axis.line = list(col = "transparent")),
       panel = function() {
         panel.polygon(seg, col = "steelblue1", alpha = 0.5,
                       border = "transparent")
         grid::grid.circle(0, 0, r = 1, default.units = "native",
                           gp = gpar(fill = "transparent"))
         grid::grid.circle(1.2, 0, r = 0.7, default.units = "native",
                           gp = gpar(fill = "transparent"))
         pBrackets::grid.brackets(1.2, -1.2, 0, -1.2, h = 0.05)
         pBrackets::grid.brackets(-1, 0, 0, 0, h = 0.05)
         pBrackets::grid.brackets(1.2, 0, 1.9, 0, h = 0.05)
         panel.lines(c(0, 0), c(0, -1.2), lty = 2, col = "grey65")
         panel.lines(c(1.2, 1.2), c(0, -1.2), lty = 2, col = "grey65")
         panel.text(0.6, unit(-1.3, "native"),
                    labels = expression(italic(d[ij])), pos = 1)
         panel.text(ospot[1], ospot[2], labels = expression(italic(O[ij])))
         panel.text(-0.5, 0.1, labels = expression(italic(r[i])), pos = 3,
                    default.units = "native")
         panel.text(1.55, 0.1, labels = expression(italic(r[j])), pos = 3,
                    default.units = "native")
         panel.points(c(0, 1.2), c(0, 0), pch = 21, col = 1, cex = 1,
                      fill = "white")
       })

## ----ellipse, fig.env="marginfigure", fig.cap="A rotated ellipse with semimajor axis $a$, semiminor axis $b$, rotation $\\phi$, and center $h,k$."----
h <- 0.7
k <- 0.5
a <- 1
b <- 0.6
phi <- pi/5
n <- 200
ellipse <- RConics::ellipse(c(a, b), c(h, k), phi,  n = n)

xyplot(1 ~ 1, xlim = c(-0.3, 1.7), ylim = c(-0.3, 1.3), asp = "iso",
       xlab = NULL, ylab = NULL,
       par.settings = list(axis.line = list(col = "transparent")),
       scales = list(draw = FALSE),
       panel = function(x, y, ...) {
         # grid
         panel.refline(h = 0)
         panel.refline(v = 0)

         # rotation
         arc <- ellipse_arc(c(0.2, 0.2), c(h, k), rng = c(0, phi))
         theta <- ellipse_arc(c(a, b), c(h, k), phi, n = 1)

         panel.lines(x = c(h, ellipse[1, 1]), y = c(k, ellipse[1, 2]), lty = 2,
                     col = "grey50")
         panel.lines(x = c(h, theta[1, 1]), y = c(k, theta[1, 2]), lty = 2,
                     col = "grey50")
         panel.lines(arc, col = "grey50")
         panel.text(x = arc[nrow(arc)/2, 1] + 0.06, y = arc[nrow(arc)/2, 2] + 0.02,
                    labels = expression(italic(phi)))

         # semiaxes
         pBrackets::grid.brackets(h, k, ellipse[n/2, 1], ellipse[n/2, 2],
                                  col = "grey50")
         pBrackets::grid.brackets(h, k, ellipse[n/4, 1], ellipse[n/4, 2],
                                  h = 0.04, col = "grey50")

         panel.text(0.37, 0.12, labels = "a", font = 3)
         panel.text(0.43, 0.68, labels = "b", font = 3)

         # center
         panel.text(x = h, y = k + 0.025, pos = 3, label = c("h,k"), font = 3)
         panel.points(x = h, y = k, pch = 21, fill = "white", col = "black")

         # ellipse
         panel.lines(ellipse, col = 1)
       })

## ----intersect-1---------------------------------------------------------
C1 <- ellipseToConicMatrix(c(8, 2), c(0, 0), -pi/3)
C2 <- ellipseToConicMatrix(c(5, 2), c(1, -2), pi/5)

ellipses <- data.frame(rbind(ellipse(c(8, 2), c(0, 0), -pi/3),
                             ellipse(c(5, 2), c(1, -2), pi/5)))
colnames(ellipses) <- c("x", "y")
ellipses$fac <- rep(c("A", "B"), each = 201)
pp <- intersectConicConic(C1, C2)
x <- pp[1, ]
y <- pp[2, ]
a <- double(6)
b <- double(6)
k <- 1
for (i in 1:3) {
  for (j in (i + 1):4) {
    b[k] <- (y[j] - y[i])/(x[j] - x[i])
    a[k] <- b[k]*(-x[i]) + y[i]
    k <- k + 1
  }
}
a <- a[c(1, 6, 3, 4, 2, 5)]
b <- b[c(1, 6, 3, 4, 2, 5)]

xyplot(
  y ~ x, data = ellipses, type = "l", groups = fac, asp = 1,
  xlab = NULL, ylab = NULL,
  scales = list(draw = FALSE, axes = FALSE),
  par.settings = list(axis.line = list(col = "transparent")),
  panel = function(x, y, ...) {
    panel.abline(a = a[3], b = b[3], col = "#FEFEFE")
    panel.abline(a = a[4], b = b[4], col = "#FEFEFE")
    panel.xyplot(x, y, ..., col = 1)
})

## ----intersect-2---------------------------------------------------------
xyplot(y ~ x, data = ellipses, type = "l", groups = fac, asp = 1,
  xlab = NULL, ylab = NULL,
  scales = list(draw = FALSE, axes = FALSE),
  par.settings = list(axis.line = list(col = "transparent")),
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ..., col = 1)
    for (i in 1:length(a)) {
      panel.abline(a = a[i], b = b[i], col = pal[ceiling(i/2) + 1])
    }
})

## ----intersect-3---------------------------------------------------------
xyplot(
  y ~ x, data = ellipses, type = "l", groups = fac, asp = 1,
  xlab = NULL, ylab = NULL,
  scales = list(draw = FALSE, axes = FALSE),
  par.settings = list(axis.line = list(col = "transparent")),
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ..., col = c("transparent", "black"))
    panel.abline(a = a[3], b = b[3], col = pal[3])
    panel.abline(a = a[4], b = b[4], col = pal[3])
    panel.points(t(pp[1:2, ]), col = 1, pch = 21, fill = "white")
})

## ----polyarea, fig.height = 4.2, fig.width = 4.2-------------------------
x <- c(0, -0.3, 0.2)
y <- c(0, 0.1, 0.3)
ra <- a <- c(0.3, 0.5, 0.4)
rb <- b <- c(0.3, 0.3, 0.6)
phi <- c(-pi/6, 2, -2)

ee <- data.frame(x, y, ra, rb, phi)

tx <- atan2(-b*tan(phi), a)
ty <- atan2(b*tan(pi/2L - phi), a)

xlim <- range(x + a*cos(tx)*cos(phi) - b*sin(tx)*sin(phi),
              x + a*cos(tx + pi)*cos(phi) - b*sin(tx + pi)*sin(phi))
ylim <- range(y + b*sin(ty)*cos(phi) + a*cos(ty)*sin(phi),
              y + b*sin(ty + pi)*cos(phi) + a*cos(ty + pi)*sin(phi))

pp <- matrix(NA, ncol = 4, nrow = 0)

for (i in 1:2) {
  for (j in (i + 1):3) {
    e1 <- ellipseToConicMatrix(c(ra[i], rb[i]), c(x[i], y[i]), phi[i])
    e2 <- ellipseToConicMatrix(c(ra[j], rb[j]), c(x[j], y[j]), phi[j])
    pp <- rbind(pp, cbind(t(intersectConicConic(e1, e2)[1:2, ]), i, j))
  }
}

sel <- logical(nrow(pp))
for (k in 1:nrow(pp)) {
  in_which <- ((pp[k, 1] - x)*cos(phi) + (pp[k, 2] - y)*sin(phi))^2/ra^2 +
    ((pp[k, 1] - x)*sin(phi) - (pp[k, 2] - y)*cos(phi))^2/rb^2 <= 1 + 0.1
  sel[k] <- all(in_which)
}

pp <- pp[sel, ]
mid <- cbind(mean(pp[, 1]), mean(pp[, 2]))
seglines <- matrix(NA, ncol = 2, nrow = 0)

ang <- atan2(pp[, 1] - mid[1], pp[, 2] - mid[2])
ord <- order(ang)

pp <- pp[ord, ]

j <- nrow(pp)
for (i in 1:nrow(pp)) {
  k <- intersect(pp[i, 3:4], pp[j, 3:4])
  start <- atan2(pp[j, 2] - y[k], pp[j, 1] - x[k])
  stop <- atan2(pp[i, 2] - y[k], pp[i, 1] - x[k])
  arc <- ellipse_arc(c(a[k], b[k]), c(x[k], y[k]), theta = phi[k],
                     rng = c(start, stop))
  seglines <- rbind(seglines, arc)
  j <- i
}

xyplot(
  y ~ x, data = ee, asp = "iso",
  xlim = extendrange(xlim, f = 0.01), ylim = extendrange(ylim, f = 0.01),
  scales = list(draw = FALSE), xlab = NULL, ylab = NULL,
  par.settings = list(axis.line = list(col = "transparent")),
  panel = function(x, y, ...) {
    panel.euler.ellipses(x, y, ra, rb, phi, ...)
    panel.polygon(seglines, col = "slategray2")
    panel.polygon(pp[, 1:2], col = "grey90")
    panel.points(mid, pch = 4, col = 1, cex = 1.5)
    panel.points(pp[, 1:2, drop = FALSE], col = 1, pch = 21, fill = "white", cex = 1.5)
  }
)

## ----ellipsesegment------------------------------------------------------
ellipse <- ellipse(c(1, 0.6), c(0, 0), 0)
i0 <- 15
i1 <- 45

tri <- rbind(cbind(0, 0),
             ellipse[i0, , drop = FALSE],
             ellipse[i1, , drop = FALSE])

xyplot(1~1, xlim = c(-1.2, 1.2), ylim = c(-0.8, 0.8), asp = "iso",
       xlab = NULL, ylab = NULL,
       par.settings = list(axis.line = list(col = "transparent")),
       scales = list(draw = FALSE),
       panel = function(x, y, ...) {
         panel.grid(x = 0, y = 0, h = 1,v = 1)
         panel.lines(ellipse, col = 1)
         panel.polygon(ellipse[i0:i1, ], col = "steelblue1", alpha = 0.5)
         panel.polygon(tri, col = "grey95")
         panel.points(ellipse[c(i0, i1), ], col = 1, pch = 21, fill = "white")
         panel.text(ellipse[c(i0, i1), ],
                    adj = c(-0.5, -0.5),
                    labels = c(expression(italic(theta[0])),
                               expression(italic(theta[1]))))
                    pBrackets::grid.brackets(1, 0, 0, 0, h = 0.05,
                                  type = 1)
         pBrackets::grid.brackets(0, 0, 0, 0.6, h = 0.04)
         panel.text(0.5, unit(-0.05, "npc"), labels = "a", font = 3, pos = 1)
         panel.text(-0.05, unit(0.3, "native"), labels = "b", font = 3, pos = 2)
       })

## ----origin-regress, fig.env="marginfigure", fig.cap="Optimizing via stress is analogous to least-squares linear regression through the origin. $\\omega$ is the set of unique quantities in the input~(\\cref{def:omega}) and $A$ the respective areas in the diagram.",fig.width=2.4,fig.height=2.4----
fit <- euler(c(X = 3, Y = 1, Z = 2,
               "X&Y" = 2, "X&Z" = 1.2, "Y&Z" = 1.5,
               "X&Y&Z" = 1))
d <- data.frame(A = fitted(fit), omega = fit$original.values)
xyplot(A ~ omega, d, groups = as.factor(1:7), asp = "iso",
       xlim = c(-0.1, 3.9), ylim = c(-0.1, 3.9),
       xlab = expression(omega),
       panel = function(x, y, ...) {
         panel.abline(a = 0, b = sum(d$A*d$omega)/(sum(d$omega^2)),
                       rotate = TRUE, at = 0.8, pos = 1,
                       col = "grey60")
         panel.xyplot(x, y, col = trellis.par.get("superpose.symbol")$col, ...)
         panel.text(x, y, labels = names(resid(fit)),
                    col = trellis.par.get("superpose.symbol")$col,
                    pos = c(3, 3, 3, 2, 3, 4, 2))
       })

## ----vogel, fig.show = "hold"--------------------------------------------
h <- c(0.2, 0.6)
k <- c(0.4, 0.4)
a <- c(0.4, .5)
b <- c(0.3, 0.6)
phi <- c(2.5, -1)
n <- 250
seqn <- seq(1, n, 1)
theta <- seqn*pi*(3 - sqrt(5))
rad <- sqrt(seqn/n)
x <- rad*cos(theta)
y <- rad*sin(theta)
p <- rbind(x, y, 1)

# Scale, rotate and translate to match the ellipse
p0 <- translation(c(h[1], k[1])) %*%
   rotation(-phi[1]) %*%  scaling(c(a[1], b[1])) %*% p

inside <- ((p0[1, ] - h[2])*cos(phi[2]) + (p0[2, ] - k[2])*sin(phi[2]))^2/a[2]^2 +
  ((p0[1, ] - h[2])*sin(phi[2]) - (p0[2, ] - k[2])*cos(phi[2]))^2/b[2]^2 <= 1

# Set up limits
tx <- atan2(-b*tan(phi), a)
ty <- atan2(b*tan(pi/2L - phi), a)

xlim <- range(h + a*cos(tx)*cos(phi) - b*sin(tx)*sin(phi),
              h + a*cos(tx + pi)*cos(phi) - b*sin(tx + pi)*sin(phi))
ylim <- range(k + b*sin(ty)*cos(phi) + a*cos(ty)*sin(phi),
              k + b*sin(ty + pi)*cos(phi) + a*cos(ty + pi)*sin(phi))

xyplot(
  x ~ y, aspect = "iso", pch = 20, xlab = "", ylab = "",
  col = 1,
  xlim = extendrange(xlim, f = 0.01),
  ylim = extendrange(ylim, f = 0.01),
  par.settings = list(axis.line = list(col = "transparent")),
  scales = list(draw = FALSE),
  panel = function(x, y, ...) {
    panel.points(t(p0[1:2, !inside]), pch = 19, col = "grey80", cex = 0.5)
    panel.points(t(p0[1:2, inside]), pch = 19, col = "steelblue2", cex = 0.5)
    panel.points(t(p0[1:2, !inside][, 14]), pch = 19, col = 1, cex = 0.5)
    panel.polygon(ellipse(c(a[1], b[1]), c(h[1], k[1]), phi[1]))
    panel.polygon(ellipse(c(a[2], b[2]), c(h[2], k[2]), phi[2]))
  })

centers <- eulerr:::locate_centers(h, k, a, b, phi, 1:2)

xyplot(
  x ~ y, aspect = "iso", pch = 19, xlab = "", ylab = "",
  col = 1,
  xlim = extendrange(xlim, f = 0.01),
  ylim = extendrange(ylim, f = 0.01),
  par.settings = list(axis.line = list(col = "transparent")),
  scales = list(draw = FALSE),
  panel = function(x, y, ...) {
    panel.arrows(p0[1, !inside][14], p0[2, !inside][14],
                 centers[1, 1], centers[2, 1],
                 length = 0.05, col = "grey70")
    panel.points(t(p0[1:2, !inside][, 14]), pch = 19, col = 1, cex = 0.5)
    panel.polygon(ellipse(c(a[1], b[1]), c(h[1], k[1]), phi[1]))
    panel.polygon(ellipse(c(a[2], b[2]), c(h[2], k[2]), phi[2]))
  })

xyplot(
  x ~ y, aspect = "iso", pch = 19, xlab = "", ylab = "",
  col = 1,
  xlim = extendrange(xlim, f = 0.01),
  ylim = extendrange(ylim, f = 0.01),
  par.settings = list(axis.line = list(col = "transparent")),
  scales = list(draw = FALSE),
  panel = function(x, y, ...) {
    panel.text(t(centers[1:2, 1]), labels = "42")
    panel.polygon(ellipse(c(a[1], b[1]), c(h[1], k[1]), phi[1]))
    panel.polygon(ellipse(c(a[2], b[2]), c(h[2], k[2]), phi[2]))
  })

#grid.arrange(p1, p2, p3, ncol = 1)

## ----colorexamle, fig.width = 4.2, fig.height = 3------------------------
set.seed(2)
con <- c(A = 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1, H = 1,
         "A&B" = 0.2, "B&C" = 0.2, "C&D" = 0.2, "D&E" = 0.2, "E&F" = 0.2,
         "F&G" = 0.2, "G&H" = 0.2)
plot(euler(con), labels = as.character(1:8), auto.key = F)

