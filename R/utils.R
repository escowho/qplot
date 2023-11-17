globalVariables(c(".", ":=", "packageVersion", "term", "estimate", "var", "hilo",
                  "label", "Variable", "label", "perf", "percent", "bar_label",
                  "png"))

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Convenience Function to Pull Version
#'
#' @keywords internal

version <- function(){
  packageVersion("qplot")
}


#' @title Refreshes github install
#' @description Convenience function to perform a remotes::install_github for qplot.
#' @param dev A logical to indicate if the development version of qplot should be
#' installed.  Will only work if qplot is the only package listed in the pack_list.
#' Default: FALSE.
#' @return Nothing, updates package(s).
#' @keywords internal
#' @examples
#' \dontrun{
#' qplot:::refresh()
#' qplot:::refresh(dev=TRUE)
#' }
#' @importFrom remotes install_github

refresh <- function(dev=FALSE){

  # Function ----------------------------------------------------------------

  if (dev==TRUE){
    if (Sys.getenv("QPLOT_TEST")==TRUE){
      return("update_dev")
    } else {
      invisible(remotes::install_github("mshefferq/qplot", ref="dev"))
    }

 } else {

    if (Sys.getenv("QPLOT_TEST")==TRUE){
      return("update_pack")
    } else {
      invisible(remotes::install_github("mshefferq/qplot"))
    }
  }

}


#' Syntax for processing coefficients captured by broom::tidy for use with
#' other key driver related functions
#'
#' @keywords internal
#' @importFrom dplyr filter select mutate case_when left_join group_by summarize arrange
#' @importFrom stringr str_detect str_replace
#' @importFrom tidyr pivot_wider

process_coefs <- function(DATA, LABELS, DECIMALS, SORT){

  # Checks ------------------------------------------------------------------

  #Internal function only, checks handled by calling function

  # Function ----------------------------------------------------------------

  DATA <- DATA %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::select(term, estimate) %>%
    dplyr::mutate(hilo = dplyr::case_when(stringr::str_detect(term, "_HI") ~ "Perform Well",
                                          stringr::str_detect(term, "_LO") ~ "Perform Poorly"),
                  var = term,
                  var = stringr::str_replace(var, "_HI", ""),
                  var = stringr::str_replace(var, "_LO", ""),
                  range = abs(estimate)) %>%
    dplyr::mutate(bar_label = sprintf(paste0("%.", DECIMALS, "f"), round(estimate, 1)),
                  bar_just = ifelse(estimate <0, 1.5, -.5)) %>%
    dplyr::mutate(label_high = ifelse(hilo=="Perform Well", round(estimate, 1), 0),
                  label_low = ifelse(hilo=="Perform Poorly", round(estimate, 1), 0))

  if (is.null(LABELS)){
    DATA <- dplyr::mutate(DATA, label=var)
  } else {
    DATA <- dplyr::left_join(DATA, LABELS, by="var")
  }

  if(SORT==TRUE){
    order <- DATA %>%
      dplyr::group_by(label) %>%
      dplyr::summarize(range=sum(range)) %>%
      dplyr::arrange(range, label) %>%
      tidyr::pivot_wider(names_from=label, values_from=range)
  } else {
    order <- DATA %>%
      dplyr::group_by(label) %>%
      dplyr::summarize(range=sum(range)) %>%
      tidyr::pivot_wider(names_from=label, values_from=range)
  }

  return(list(data=DATA, order=order))
}
