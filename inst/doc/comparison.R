## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(eulerr)

# The benchmark numbers are precomputed by `data-raw/benchmarks.R` (which needs
# the competitor packages and is not part of the package build) and stored in
# `benchmark_results.rds`. This vignette only reads and displays them, so it
# builds with eulerr, knitr, and lattice alone.
results_path <- "benchmark_results.rds"
have_results <- file.exists(results_path)
if (have_results) {
  results <- readRDS(results_path)
}

## -----------------------------------------------------------------------------
# Reshape a long accuracy/timing slice to a dataset-by-package matrix, dropping
# competitor columns that were never run (all NA) so the scaffolded state (only
# eulerr available) still produces a clean table.
to_matrix <- function(df, value_col) {
  m <- tapply(
    df[[value_col]],
    list(df$dataset, df$package),
    FUN = function(x) x[1]
  )
  m <- m[, colSums(!is.na(m)) > 0, drop = FALSE]
  # Order datasets as first seen, eulerr columns first.
  m <- m[unique(df$dataset), , drop = FALSE]
  pkg_order <- order(!grepl("^eulerr", colnames(m)), colnames(m))
  m[, pkg_order, drop = FALSE]
}

fmt <- function(x) {
  ifelse(is.na(x), "---", formatC(x, format = "g", digits = 2))
}

## -----------------------------------------------------------------------------
if (!have_results) {
  cat(
    "> **Benchmark results are not available.** Run",
    "`Rscript data-raw/benchmarks.R` to generate `benchmark_results.rds`,",
    "then rebuild this vignette.\n"
  )
}

## -----------------------------------------------------------------------------
run <- results$meta$competitors_run
if (length(run) == 0) {
  cat(
    "> The stored results were generated **without any competitor package",
    "installed**, so only eulerr's own numbers are shown below. Install",
    "venneuler, BioVenn, and nVennR and re-run `data-raw/benchmarks.R` for the",
    "full comparison.\n"
  )
} else {
  cat(
    "> Competitors benchmarked:",
    paste(run, collapse = ", "),
    paste0("(generated ", results$meta$generated, ").\n")
  )
}

## -----------------------------------------------------------------------------
acc <- results$accuracy
for (cmp in unique(acc$comparison)) {
  sub <- acc[acc$comparison == cmp, ]
  metric <- sub$metric[1]
  m <- to_matrix(sub, "value")
  cat("\n**", cmp, "** --- lower ", metric, " is better.\n\n", sep = "")
  print(knitr::kable(
    apply(m, 2, fmt),
    align = "r",
    caption = NULL
  ))
  cat("\n")
}

## -----------------------------------------------------------------------------
library(lattice)

acc <- results$accuracy
acc$package <- factor(acc$package, levels = unique(acc$package))

lattice::barchart(
  value ~ dataset | comparison,
  groups = package,
  data = acc,
  scales = list(x = list(rot = 45), y = list(relation = "free")),
  layout = c(3, 1),
  ylab = "fit error",
  auto.key = list(columns = 1, space = "right"),
  par.settings = lattice::simpleTheme(col = eulerr_options()$fills$fill(8))
)

## -----------------------------------------------------------------------------
tim <- results$timing
# Average eulerr-circle / competitor timings across datasets for a compact view.
m <- to_matrix(tim, "time_ms")
cat("Median fit time (ms), by dataset and package:\n\n")
knitr::kable(
  apply(m, 2, function(x) formatC(x, format = "f", digits = 1)),
  align = "r"
)

## -----------------------------------------------------------------------------
tim <- results$timing
tim$package <- factor(tim$package, levels = unique(tim$package))

lattice::barchart(
  time_ms ~ dataset,
  groups = package,
  data = tim[!duplicated(tim[c("dataset", "package")]), ],
  scales = list(x = list(rot = 45), y = list(log = 10)),
  ylab = "median time (ms)",
  auto.key = list(columns = 1, space = "right"),
  par.settings = lattice::simpleTheme(col = eulerr_options()$fills$fill(8))
)

## -----------------------------------------------------------------------------
qual <- data.frame(
  Package = c(
    "eulerr",
    "venneuler",
    "nVennR",
    "BioVenn",
    "vennplot",
    "VennDiagram"
  ),
  Shapes = c(
    "circle, ellipse, rectangle, square",
    "circle",
    "irregular polygon",
    "circle",
    "circle (2D), sphere (3D)",
    "circle"
  ),
  `Max sets` = c("many", "many", "many", "2--3", "2--3", "4 (2 scaled)"),
  Proportional = c(
    "approximate",
    "approximate",
    "quasi",
    "accurate (2--3)",
    "approximate",
    "2 sets only"
  ),
  Input = c(
    "vectors, data frames, matrices, tables, lists",
    "named vector, data frame",
    "lists",
    "ID lists",
    "counts, lists",
    "counts"
  ),
  `Fit reported` = c(
    "stress, diagError, regionError",
    "stress",
    "none",
    "none",
    "none",
    "none"
  ),
  `Key dependency` = c(
    "none (Rust)",
    "rJava / Java",
    "C++",
    "none",
    "Rcpp, rgl",
    "none"
  ),
  Source = c(
    "CRAN",
    "CRAN",
    "GitHub",
    "CRAN",
    "CRAN (dormant)",
    "CRAN"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

knitr::kable(qual, align = "l")

## -----------------------------------------------------------------------------
excl <- data.frame(
  Package = c(
    "venn, ggVennDiagram, ggvenn, RVenn, gplots",
    "UpSetR",
    "colorfulVennPlot",
    "VennMaster",
    "Vennerable"
  ),
  Reason = c(
    "Draw fixed, schematic shapes; quantity shown via labels or color, not area",
    "Not a Venn/Euler diagram at all (UpSet matrix/bar charts)",
    "Archived from CRAN; only a 2-set helper, never a general fitter",
    "Area-proportional, but a standalone Java application --- not an R package",
    "Area-weighted, but hosted on R-Forge/GitHub, not CRAN or Bioconductor"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

knitr::kable(excl, align = "l")

