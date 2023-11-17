
# Specification Errors ----------------------------------------------------

test_that("specification errors",{
  expect_error(
    qplot_cor()
  )

  expect_error(
    test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=.,
                names=c("Age", "Gender", "Race", "Insurance", "Income", "Education",
                        "nope")),
  )
})


# Default Run -------------------------------------------------------------

test_that("Default Run - Visual Inspection",{

  expect_no_error(
    p1 <- qplot::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=.)
    )

    vdiffr::expect_doppelganger("qplot_cor_1", p1)
})


test_that("Typical Run - File Inspection",{

  withr::local_dir(tempdir())

  expect_no_error(qplot::test1 %>%
                  dplyr::select(q1:q6) %>%
                  qplot_cor(data=., output="test1.jpg"))

  expect_true(file.exists("test1.xlsx"))
  expect_true(file.exists("test1.jpg"))

  test1 <- readxl::read_xlsx("test1.xlsx", sheet=1)
  expect_equal(names(test1), c("variable", "q1", "q2", "q3", "q4", "q5", "q6"))
  expect_equal(test1$q1, c(1.00, .035, -.168, .144, -.001, .044), tolerance=.01)

  test2 <- readxl::read_xlsx(file.path(tempdir(), "test1.xlsx"), sheet=2)
  expect_equal(test2$q3, c(.212, 1, 0, .715, .198, .706), tolerance=.01)

})


# Use of First Option -----------------------------------------------------

test_that("Use of First Option - Visual Inspection",{

  expect_no_error(
    p2 <- qplot::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=., first="q5")
  )

  vdiffr::expect_doppelganger("qplot_cor_2", p2)

  expect_error(
    p3 <- qplot::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=., first="q8"))

  expect_warning(
    p3 <- qplot::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=., first="q5", sort=TRUE))
})

test_that("Use of First Option - File Inspection",{

  withr::local_dir(tempdir())

  expect_no_error(
    qplot::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=., first="q5", output="test2.jpg")
  )

  expect_true(file.exists("test2.xlsx"))
  expect_true(file.exists("test2.jpg"))

  test1 <- readxl::read_xlsx("test2.xlsx", sheet=1)
  expect_equal(names(test1), c("variable", "q5", "q1", "q2", "q3", "q4", "q6"))
  expect_equal(test1$q5, c(1.00, 0.00, -0.15, -0.09, -0.37, .33), tolerance=.03)

  test2 <- readxl::read_xlsx(file.path(tempdir(), "test2.xlsx"), sheet=2)
  expect_equal(test2$q3, c(1.00, 0.21, 1.00, 0.00, 0.72, 0.71), tolerance=.03)

  expect_no_error(
    qplot::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=., first="q5", output="test3")
  )

  expect_true(file.exists("test3.xlsx"))
  expect_true(file.exists("test3.jpg"))

})

# Supplying Options -------------------------------------------------------

test_that("Suppying colors",{

  expect_silent(
    p3 <- qpack::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=.,
                colors=c("#ffaba8", "#ff7269", "#ff2424",
                         "#ffffff",
                         "#1a9635", "#62af6c", "#99c69f"
                         ))
  )

  vdiffr::expect_doppelganger("qplot_cor_3", p3)

})

test_that("Suppying names",{

  expect_silent(
    p4 <- qpack::test1 %>%
      dplyr::select(q1:q6) %>%
      qplot_cor(data=.,
                names=c("Age", "Gender", "Race", "Insurance", "Income", "Education"))
  )

  vdiffr::expect_doppelganger("qplot_cor_4", p4)

})


# Skeleton ----------------------------------------------------------------

test_that("skeleton returns correct text",{

  dat1 <- qplot::test1 %>%
    dplyr::select(q1:q6)

  test1 <- capture.output(qplot_cor(data=dat1, output="./output/test1.jpg", skeleton=TRUE))
  expect_equal(test1[1], "corrs <- psych::corr.test(qplot_data)")
  expect_equal(test1[3], "grDevices::jpeg(\"./output/test1.jpg\", width=5000, height=5000, res=300, pointsize=16)")
  expect_equal(test1[13], "                   order=\"original\",")
  expect_equal(test1[19], "                   p.mat=corrs$p,")

  test2 <- capture.output(qplot_cor(data=dat1, skeleton=TRUE))
  expect_equal(test2[1], "corrs <- psych::corr.test(qplot_data)")
  expect_equal(test2[3], "grDevices::jpeg(OUTPUT_PATH_WITH_JPG_EXTENSION, width=5000, height=5000, res=300, pointsize=16)")
  expect_equal(test2[13], "                   order=\"original\",")
  expect_equal(test2[19], "                   p.mat=corrs$p,")

})

test_that("skeleton returns dataset",{

  withr:::local_envvar(
    QPLOT_TEST = TRUE)

  dat1 <- qplot::test1 %>%
    dplyr::select(q1:q6)

  test3 <- qplot_cor(data=dat1, skeleton=TRUE, data_out=TRUE)
  expect_equal(names(test3), c("q1", "q2", "q3", "q4", "q5", "q6"))

})
