#' @title test1 is an example dataset containing labelled variables for testing
#'
#' @description Dataset containing a few variables with labelled data, as well
#' as three problem variables for detecting issues with labelled data.
#'
#' @format A data frame of 200 observations and 11 variables:
#' \describe{
#' \item{response_id}{Respondent ID Number}
#' \item{q1}{Age Category}
#' \item{q2}{Gender}
#' \item{q3}{Race}
#' \item{q4}{Type of Insurance}
#' \item{q5}{Income}
#' \item{q6}{Education}
#' \item{region}{Region}
#' \item{prob1}{Problem - No Variable Label}
#' \item{prob2}{Problem - Character Data}
#' \item{prob3}{Problem - No Value labels}
#' }
"test1"

#' @title \code{coefs1}
#'
#' @description Dataset containing example regression outputs from a key driver
#' analysis.
#'
#' @format A data frame of 14 rows and 6 variables:
#' \describe{
#' \item{term}{coefficient name}
#' \item{estimate}{regression estimate}
#' \item{p.value}{p value of regression estimate}
#' \item{rsq}{model r square}
#' \item{p.mod}{model p value}
#' \item{check}{direction check}
#' }
"coefs1"

#' @title \code{labels1}
#'
#' @description Dataset containing example labels used for outputs in key driver
#' analysis.
#'
#' @format A data frame of 8 rows and 2 variables:
#' \describe{
#' \item{var}{coefficient name}
#' \item{label}{label to use in place of var in output}
#' }
"labels1"

#' @title \code{perf1}
#'
#' @description Dataset containing example performance data from a key driver
#' analysis.
#'
#' @format A data frame of 8 rows and 4 variables:
#' \describe{
#' \item{Variable}{variable name that matches label var and term prefix}
#' \item{Negative}{percent negative performance, max = 1 for 100%}
#' \item{Neutral}{percent neutral performance, max = 1 for 100%}
#' \item{Positive}{percent positive performance, max = 1 for 100%}
#' }
"perf1"
