#' @title Wrapper for corrplot that generates a reasonably consistent corrplot
#' @description Generates a corrplot that tends to look good in most situations.
#' The corrplot will be exported as a jpg if the output parameter is specified.
#' The underlying correlation table and p values is also automatically exported
#' with the same name but with an .xlsx extension.
#' Note that the on-screen version is not optimally designed and that this function
#' is optimized for the outputted jpg version.  Possible to provide a list of names
#' to replace variable names (the default action).  Can also specify colors but
#' there is a standardized set of colors as default from red to white to green for
#' correlation color coding.  Option to sort based on overall correlation size
#' (happens by default).  Can identify the name of a variable that show first in
#' the output, which is particularly useful for dependent variables like NPS score
#' (but this will disengage any other sorting).
#' @param data Name of the dataframe upon which to build the corrplot.  All variables
#' will be included.  Required.
#' @param names Vector of quoted strings to be used in place of the variable names
#' in the resulting plot.  Must be the same number of names as variables in the
#' data. Optional.
#' @param first The name of a variable to appear first, or left-most, on the corrplot.
#' Use of the first parameter disables sorting.  Be sure to change the order of
#' any supplied names when using the first option. Optional.
#' @param colors Vector of quoted hexadecimal color codes to be used for coloring
#' the correlations.  Default is to go from dark green to light green for positive
#' correlations, white for no correlation, and light red to dark red for negative
#' correlations. Optional.
#' @param output Character string containing the complete path and file name of
#' a jpg file of the resulting corrplot.  If not provided, the plot will be shown
#' on the screen instead but may not be optimized for screen viewing.  The underlying
#' correlation table and p values is also automatically exported with the same
#' name but with an .xlsx extension. Optional.
#' @param sort A logical value to indicate if the output should be ordered by
#' strength of correlation.  If set to TRUE, this sets the parameter order = c("hclust")
#' in the corrplot function. Default: FALSE
#' @param skeleton A logical value to indicate if the syntax should be printed to
#' the console.  This is useful if the default selections need tweaking so the
#' user can see the raw syntax that can be used in place of the function.
#' Default: FALSE
#' @param data_out A logical value to indicate if the process data file should be
#' returned when skeleton=TRUE.  Default: FALSE
#' @param ps Point size setting for grDevices::jpeg.  May need to reduce if lots
#' of attributes are included.  Default=16.
#' @param ... Pass additional parameters to corrplot.  Optional.
#' @return Outputs a jpg file of the corrplot if output path specified, otherwise
#' generates plot on screen.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  test1 %>%
#'  dplyr::select(q1:q6) %>%
#'    qplot_cor(., names=c("Age", "Gender", "Race", "Insurance", "Income", "Education"))
#'
#'  test1 %>%
#'    dplyr::select(q1:q6) %>%
#'    qplot_cor(data=., first="q5", sort=FALSE)
#'  }
#' }
#' @export
#' @importFrom rlang enquo quo_name f_text
#' @importFrom dplyr relocate mutate_if
#' @importFrom fs path_ext path_ext_remove
#' @importFrom psych corr.test
#' @importFrom corrplot corrplot
#' @importFrom tibble rownames_to_column
#' @importFrom qpack set_colnames
#' @importFrom grDevices dev.off jpeg
#' @importFrom cli cli_abort cli_warn

qplot_cor <- function(data=NULL, first=NULL, names=NULL, colors=NULL,
                      output=NULL, sort=FALSE, skeleton=FALSE, data_out=FALSE,
                      ps=16, ...){

    # Checks ------------------------------------------------------------------

    if (is.null(data) == TRUE){
      cli::cli_abort("Data must be specified.")
    }

    if (is.null(first) == FALSE){
      FIRST <- rlang::enquo(first)

      if (! rlang::quo_name(FIRST) %in% colnames(data)){
        cli::cli_abort("Firt parameter variable {rlang::f_text(FIRST)} not found in the data.")
      }

      data <- data %>%
        dplyr::relocate({{first}})
    }

    if (is.null(names) == FALSE){
      if (ncol(data) != length(names)){
        cli::cli_abort("Number of names specified not equal to number of columns in data. # of cols in the data: {ncol(data)}, length of name vector: {length(names)}")
      } else {
        data <- data %>%
          qpack::set_colnames(names)
      }
    }

    if (is.null(colors) == TRUE){
      colors <- c("#ff2424", "#ff7269", "#ffaba8", "#fde1e1",
                  "#ffffff",
                  "#d0dcd2", "#99c69f", "#62af6c", "#1a9635")
    }

    if (is.null(output) == FALSE){
      if(fs::path_ext(output) %in% c("jpg", "jpeg")){
        file_plot <- output
        file_data <- paste0(fs::path_ext_remove(output), ".xlsx")
      } else {
        file_plot <- paste0(output, ".jpg")
        file_data <- paste0(output, ".xlsx")
      }
    }

    if(sort==TRUE & is.null(first)==FALSE){
      cli::cli_warn("Can't sort output since the first option is being used.")
      order_entry <- "original"
    } else if(sort==TRUE & is.null(first==TRUE)){
      order_entry <- "hclust"
    } else {
      order_entry <- "original"
    }

  # Skeleton ----------------------------------------------------------------

  if (skeleton==TRUE){

    if (Sys.getenv("QPLOT_TEST") != TRUE){

      cat("corrs <- psych::corr.test(qplot_data)\n")
      cat("\n")

      if (is.null(output)==FALSE){
        cat(paste0("grDevices::jpeg(\"", as.character(file_plot), "\", width=5000, height=5000, res=300, pointsize=16)\n"))
      } else {
        cat("grDevices::jpeg(OUTPUT_PATH_WITH_JPG_EXTENSION, width=5000, height=5000, res=300, pointsize=16)\n")
      }

      cat("corrplot::corrplot(corr=corrs$r,\n")
      cat("                   #skeleton outputs default colors (shown below)\n")
      cat("                   col=c(\"#ff2424\", \"#ff7269\", \"#ffaba8\", \"#fde1e1\",\n")
      cat("                         \"#ffffff\",\n")
      cat("                         \"#d0dcd2\", \"#99c69f\", \"#62af6c\", \"#1a9635\"),\n")
      cat("                   method=\"color\",\n")
      cat("                   type=\"lower\",\n")
      cat("                   diag=FALSE,\n")
      cat("                   outline=TRUE,\n")
      cat(paste0("                   order=\"", as.character(order_entry), "\",\n"))
      cat("                   tl.col=\"black\",\n")
      cat("                   tl.srt=45,\n")
      cat("                   addCoef.col=\"black\",\n")
      cat("                   number.cex=1.3,\n")
      cat("                   number.digits=2,\n")
      cat("                   p.mat=corrs$p,\n")
      cat("                   insig=\"pch\",\n")
      cat("                   pch=\"X\",\n")
      cat("                   pch.col=\"black\",\n")
      cat("                   pch.cex=2)\n")
      cat("invisible(grDevices::dev.off())\n")
      cat("\n")

    }

    if (data_out==TRUE){
      return(data)
    }


  } else if (skeleton==FALSE) {

    # Function ----------------------------------------------------------------

    corrs <- psych::corr.test(data)

    # Screen Version ----------------------------------------------------------

    if(is.null(output)){
      corrplot::corrplot(corr=corrs$r,
                         col=colors,
                         method="color",
                         type="lower",
                         diag=FALSE,
                         outline=TRUE,
                         order=order_entry,
                         tl.col="black",
                         tl.srt=45,
                         addCoef.col="black",
                         number.cex=.5,
                         number.digits=2,
                         p.mat=corrs$p,
                         insig="pch",
                         pch="X",
                         pch.col="black",
                         pch.cex=1.5,
                         ...)
    } else {

      # Output Version ----------------------------------------------------------

      grDevices::jpeg(file_plot, width=5000, height=5000, res=300, pointsize=ps)
      corrplot::corrplot(corr=corrs$r,
                         col=colors,
                         method="color",
                         type="lower",
                         diag=FALSE,
                         outline=TRUE,
                         order=order_entry,
                         tl.col="black",
                         tl.srt=45,
                         addCoef.col="black",
                         number.cex=1.3,
                         number.digits=2,
                         p.mat=corrs$p,
                         insig="pch",
                         pch="X",
                         pch.col="black",
                         pch.cex=2,
                         ...)
      invisible(grDevices::dev.off())
    }

  if (is.null(output) == FALSE){
    cr <- corrs$r %>%
      data.frame() %>%
      tibble::rownames_to_column(., "variable")
    qpack::write_xlsx(cr, file_data, sheet="cors", overfile=TRUE)

    cb <- corrs$p %>%
      data.frame() %>%
      tibble::rownames_to_column(., "variable") %>%
      dplyr::mutate_if(is.numeric, ~round(., digits=4))
    qpack::write_xlsx(cb, file_data, sheet="p_vals")
  }

  }

}
