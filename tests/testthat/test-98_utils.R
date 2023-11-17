
# version -----------------------------------------------------------------


test_that("version returns current version", {
  expect_equal(version(), packageVersion("qplot"))
})


# refresh -----------------------------------------------------------------

test_that("update works correctly using QPLOT_TEST option", {
  withr:::local_envvar(
    QPLOT_TEST = TRUE,
    .local_envir = parent.frame())

  expect_equal(qplot:::refresh(), "update_pack")
  expect_equal(qplot:::refresh(dev=TRUE), "update_dev")

})


# process_coefs -----------------------------------------------------------

test_that("process_coefs correctly creates a process coefficient file", {

  test1 <- process_coefs(DATA=qplot::coefs1, LABELS=NULL, DECIMALS=1, SORT=TRUE)

  expect_equal(names(test1$order),
               c("services_products", "registration_process", "online_account",
                 "billing", "smart_tech", "customer_service", "price"))

  test2 <- process_coefs(DATA=qplot::coefs1, LABELS=NULL, DECIMALS=1, SORT=FALSE)

  expect_equal(names(test2$order),
               c("billing", "customer_service", "online_account", "price",
                 "registration_process", "services_products", "smart_tech"))

  test3 <- process_coefs(DATA=qplot::coefs1, LABELS=qplot::labels1, DECIMALS=1, SORT=TRUE)

  expect_equal(names(test3$order),
               c("Energy Services Products", "Registration Process", "Online Account",
                 "Billing", "Smart Tech", "Customer Service", "Price" ))

  test4 <- process_coefs(DATA=qplot::coefs1, LABELS=qplot::labels1, DECIMALS=1, SORT=FALSE)

  expect_equal(names(test4$order),
               c("Billing", "Customer Service", "Energy Services Products",
                 "Online Account", "Price", "Registration Process", "Smart Tech" ))

})
