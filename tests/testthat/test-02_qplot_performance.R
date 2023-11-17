
# Specification Errors ----------------------------------------------------

test_that("specification errors",{
  expect_error(
    qplot_performance()
  )
})


# Default Run -------------------------------------------------------------

test_that("Default Runs",{

  withr::local_dir(tempdir())

  #withr:::local_envvar(
  #  QPLOT_TEST = TRUE)

  labels <- qplot::labels1

  expect_no_error(
    p1 <- qplot_performance(data=qplot::perf1, labels=labels)
  )

  vdiffr::expect_doppelganger("qplot_performance_1", p1)

  expect_warning(
    p2 <- qplot_performance(data=qplot::perf1)
  )
  vdiffr::expect_doppelganger("qplot_performance_2", p2)

  expect_no_error(
    p3 <- qplot_performance(data=qplot::perf1, labels=labels, order=qplot::coefs1)
  )
  vdiffr::expect_doppelganger("qplot_performance_3", p3)

  expect_no_error(
    p4 <- qplot_performance(data=qplot::perf1, labels=labels, order=qplot::coefs1,
                            legend=FALSE)
  )
  vdiffr::expect_doppelganger("qplot_performance_4", p4)

  expect_no_error(
    p5 <- qplot_performance(data=qplot::perf1, labels=labels, order=qplot::coefs1,
                            decimals=2)
  )
  vdiffr::expect_doppelganger("qplot_performance_5", p5)

  expect_no_error(
    p6 <- qplot_performance(data=qplot::perf1, labels=labels, output="test6")
  )

  expect_true(file.exists("test6.png"))
})

# Skeleton ----------------------------------------------------------------

test_that("skeleton returns correct text",{

  #withr:::local_envvar(
  #  QPLOT_TEST = TRUE)

  labels <- qplot::labels1

  test1 <- capture.output(qplot_performance(data=qplot::perf1, labels=labels,
                                            output="./output/test1.png",
                                            skeleton=TRUE))

  expect_equal(test1[1], "performance_plot <- data %>%")
  expect_equal(test1[3], "  ggplot2::geom_bar(position=\"stack\", stat=\"identity\", width=.8) +")
  expect_equal(test1[13], "  ggplot2::theme_minimal()  +")
  expect_equal(test1[24], "grDevices::png(\"./output/test1.png\"," )

  test2 <- capture.output(qplot_performance(data=qplot::perf1, labels=labels,
                                            skeleton=TRUE))
  expect_equal(test2[24], "grDevices::png(OUTPUT_PATH_WITH_PNG_EXTENSION,")

})


test_that("skeleton returns dataset",{

  withr:::local_envvar(
    QPLOT_TEST = TRUE)

  labels <- qplot::labels1

  test3 <- qplot_performance(data=qplot::perf1, labels=labels,
                                            output="./output/test1.png",
                                            skeleton=TRUE, data_out=TRUE)

  expect_equal(names(test3), c("data", "order"))
  expect_equal(test3$data$percent,
               c(0.199, 0.287, 0.514, 0.095, 0.145, 0.760, 0.060, 0.121, 0.818,
                 0.091, 0.175, 0.734, 0.078, 0.133, 0.788, 0.088, 0.170, 0.741,
                 0.169, 0.386, 0.445, 0.206, 0.387, 0.407), tolerance=.1)

})





