## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(eulerr)

## -----------------------------------------------------------------------------
set.seed(1)
# A chain of four sets with long names and very small overlaps — the
# overlap regions are too narrow to fit their quantity labels inside,
# so all three strategies push the overlap labels to the exterior.
fit <- euler(c(
  Animalia = 8,
  Plantae = 7,
  Fungi = 6,
  Bacteria = 5,
  "Animalia&Plantae" = 0.05,
  "Plantae&Fungi" = 0.05,
  "Fungi&Bacteria" = 0.05,
  "Animalia&Bacteria" = 0.05
))

## -----------------------------------------------------------------------------
plot(
  fit,
  quantities = TRUE,
  labels = list(placement = "raycast"),
  main = "raycast"
)

## -----------------------------------------------------------------------------
plot(
  fit,
  quantities = TRUE,
  labels = list(placement = "force_directed"),
  main = "force-directed"
)

## -----------------------------------------------------------------------------
plot(
  fit,
  quantities = TRUE,
  labels = list(placement = "elbow"),
  main = "elbow"
)

## -----------------------------------------------------------------------------
plot(
  fit,
  quantities = TRUE,
  labels = list(
    placement = "elbow",
    elbow = list(min_gap = 1)
  )
)

## -----------------------------------------------------------------------------
set.seed(2)
con <- c(
  A = 1,
  B = 1,
  C = 1,
  D = 1,
  E = 1,
  F = 1,
  G = 1,
  H = 1,
  "A&B" = 0.2,
  "B&C" = 0.2,
  "C&D" = 0.2,
  "D&E" = 0.2,
  "E&F" = 0.2,
  "F&G" = 0.2,
  "G&H" = 0.2
)
plot(euler(con), labels = as.character(1:8))

## -----------------------------------------------------------------------------
p1 <- plot(euler(c(A = 1, B = 8, "A&B" = 1)), main = "First")
p2 <- plot(euler(c(A = 1, C = 1, "A&C" = 1)), main = "Second")

p1 | p2

## -----------------------------------------------------------------------------
p3 <- plot(euler(c(X = 3, Y = 2, "X&Y" = 1)), main = "Third")

(p1 | p2) / p3

## -----------------------------------------------------------------------------
# eulerr_options(composition = list(spacing = grid::unit(2, "lines")))
# p1 | p2

