test_that("normal plotting works without errors", {
  tmp <- tempfile()
  png(tmp)

  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  expect_silent(plot(f1,
                     fills = list(fill = c("black", "blue"),
                                  alpha = 0.3),
                     labels = list(font = 4)))

  expect_silent(plot(f1))
  expect_silent(plot(f1, legend = TRUE, quantities = TRUE))
  expect_silent(plot(f1, legend = FALSE, quantities = TRUE))
  expect_silent(plot(f1, legend = TRUE, labels = FALSE))
  expect_silent(plot(f1, labels = c("asdf", "qwer")))
  expect_silent(plot(f1, main = "Hello"))
  expect_silent(plot(f1, expression = "phi[1]"))
  expect_silent(plot(f1, edges = c("white", "blue")))
  expect_silent(plot(f1, quantities = list(type = "percent")))
  expect_silent(plot(f1, quantities = list(type = "counts")))
  expect_silent(plot(f1, quantities = list(type = c("percent", "counts"))))
  expect_silent(plot(f1, quantities = list(type = c("counts", "percent"))))

  expect_error(plot(f1, quantities = list(type = c("asdf"))))

  grid <- expand.grid(labels = c(TRUE, FALSE),
                      quantities = c(TRUE, FALSE),
                      legend = c(TRUE, FALSE),
                      fills = c(TRUE, FALSE),
                      edges = c(TRUE, FALSE))

  for (i in seq_len(nrow(grid))) {
    expect_silent(plot(f1,
                       labels = grid$labels[!!i],
                       quantities = grid$quantities[!!i],
                       legend = grid$legend[!!i],
                       fills = grid$fills[!!i],
                       edges = grid$edges[!!i]))
  }

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:3], by = list(Gender))
  f3 <- euler(dat, by = list(Gender, Nation))

  expect_silent(plot(f2,
                     edges = list(fill = "transparent",
                                  lty = c(1, 2),
                                  lwd = c(1, 2))))

  grid <- expand.grid(side = c(NA, "right", "left", "top", "bottom"),
                      main = c("Title", FALSE),
                      stringsAsFactors = FALSE)

  for (i in seq_len(nrow(grid))) {
    if (is.na(grid$side[i])) {
      legend <- FALSE
    } else {
      legend <- list(side = grid$side[i])
    }

    expect_silent(plot(f1, legend = legend, main = grid$main[!!i]))
    expect_silent(plot(f2, legend = legend, main = grid$main[!!i]))
    expect_silent(plot(f3, legend = legend, main = grid$main[!!i]))
  }

  f4 <- euler(c(A = 1))
  expect_silent(plot(f4))

  dev.off()
  unlink(tmp)
})

test_that("plotting zero-fits works", {
  tmp <- tempfile()
  png(tmp)

  s <- c(a = 0, b = 0)
  expect_is(plot(euler(s)), "gTree")
  dev.off()
  unlink(tmp)
})

test_that("error_plot functions normally", {
  tmp <- tempfile()
  png(tmp)

  f <- euler(organisms)

  expect_silent(error_plot(f))
  expect_silent(error_plot(f,
                           pal = grDevices::colorRampPalette(c("red", "blue"))))
  expect_silent(error_plot(f, quantities = FALSE))
  expect_silent(error_plot(f, edges = FALSE))

  dev.off()
  unlink(tmp)
})

test_that("plots with euler lists works", {
  tmp <- tempfile()
  png(tmp)

  f1 <- euler(fruits[, 1:5], by = age)
  f2 <- euler(fruits[, 1:5], by = list(age, sex))

  expect_silent(plot(f1, legend = TRUE, strips = FALSE))
  expect_silent(plot(f2, strips = list(cex = 2, fontface = "bold")))
  expect_silent(plot(f1))

  dev.off()
  unlink(tmp)
})
