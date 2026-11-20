## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(eulerr)

# Run benchmarks separately so rendering does not need competitor packages.
results_path <- "benchmark_results.rds"
if (!file.exists(results_path)) {
  stop("Run Rscript data-raw/benchmarks.R from the package root first.")
}
results <- readRDS(results_path)
missing_competitors <- setdiff(
  c("venneuler", "nVennR", "BioVenn"),
  results$meta$competitors_run
)
if (length(missing_competitors) > 0L) {
  stop(
    "Benchmark results are missing competitors: ",
    paste(missing_competitors, collapse = ", "),
    ". Run Rscript data-raw/benchmarks.R from the package root first."
  )
}
case_labels <- setNames(results$meta$cases$label, results$meta$cases$dataset)

## -----------------------------------------------------------------------------
run <- results$meta$competitors_run
cat(
  "> Competitors benchmarked:",
  paste(run, collapse = ", "),
  paste0("(generated ", results$meta$generated, ").\n")
)

## -----------------------------------------------------------------------------
library(lattice)

acc <- results$accuracy
packages <- unique(acc$package)
colors <- setNames(eulerr_options()$fills$fill(length(packages)), packages)

for (cmp in unique(acc$comparison)) {
  sub <- acc[acc$comparison == cmp, ]
  sub$dataset <- factor(
    sub$dataset,
    levels = rev(unique(sub$dataset)),
    labels = case_labels[rev(unique(sub$dataset))]
  )
  sub$package <- factor(sub$package, levels = unique(sub$package))
  print(lattice::barchart(
    dataset ~ value,
    groups = package,
    data = sub,
    origin = 0,
    main = cmp,
    xlab = sub$metric[1],
    ylab = NULL,
    auto.key = list(columns = 3, space = "bottom"),
    par.settings = lattice::simpleTheme(col = colors[levels(sub$package)])
  ))
}

## -----------------------------------------------------------------------------
tim <- results$timing
tim$dataset <- factor(
  tim$dataset,
  levels = rev(unique(tim$dataset)),
  labels = case_labels[rev(unique(tim$dataset))]
)
tim$package <- factor(tim$package, levels = unique(tim$package))

lattice::barchart(
  dataset ~ time_ms,
  groups = package,
  data = tim[!duplicated(tim[c("dataset", "package")]), ],
  scales = list(x = list(log = 10)),
  xlab = "median time (ms)",
  ylab = NULL,
  auto.key = list(columns = 3, space = "bottom"),
  par.settings = lattice::simpleTheme(col = colors[levels(tim$package)])
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
    "Area-proportional, but a standalone Java application and not an R package",
    "Area-weighted, but hosted on R-Forge/GitHub, not CRAN or Bioconductor"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

knitr::kable(excl, align = "l")

