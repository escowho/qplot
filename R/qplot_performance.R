#' @title qplot_performance
#' @description Generates a performance graph showing Positive, Neutral, and Negative
#' proportions.  Output for Key Driver Analyses.
#' @param data Dataframe name that contains the performance data.  Expects 4 columns
#' named Variable, Negative, Neutral, and Positive.  Variable is the name of each
#' attribute in truncated form, similar to a coefficient name.  Examine \code{qplot::perf1}
#' as an example dataframe for this parameter.  Required.
#' @param labels Dataframe that contains the label names to be used in the graphic
#' output on the plot rather than the default variable names that may be found in
#' the dataset.  Expects 2 columns named var and label.  Examine \code{qplot::labels1}
#' as an example dataframe for this parameter.  Optional.
#' @param order Dataframe that contains the tidied output from a modeling process
#' like regression that contains the coefficients, estimates, p values, etc. Expects
#' columns named term and estimate, although such files often contain other columns.
#' See \code{qplot::coefs1} for an example dataframe for this parameter. The file will
#' be processed to put the coefficients in decreasing order of magnitude.  The
#' term prefixes should match the var column (from labels) and Variable (from data).
#' The Performance Graph will also be sorted according to the same decreasing
#' order of magnitude as the estimates. Optional.
#' @param output Character string containing the complete path and file name of
#' a png file of the resulting performance plot.  If not provided, the plot will
#' be shown on the screen instead but may not be optimized for screen viewing.
#' Optional.
#' @param legend A logical value indicating if a legend containing the Labels of
#' Positive, Neutral, and Negative along with their corresponding colors should
#' be displayed in the bottom right-hand corner of the plot. Default: TRUE
#' @param decimals Numeric value indicating the number of decimals to use for
#' rounding the displayed numbers in the graph. Default: 1
#' @param skeleton A logical value to indicate if the syntax should be printed to
#' the console.  This is useful if the default selections need tweaking so the
#' user can see the raw syntax that can be used in place of the function.
#' Default: FALSE
#' @param data_out A logical value to indicate if the process data file should be
#' returned when skeleton=TRUE.  Default: FALSE
#' @param ps Point size setting for grDevices::jpeg.  May need to reduce if lots
#' of attributes are included.  Default=10.
#' @return Outputs a png file of the performance plot if output path specified,
#' otherwise generates plot on screen.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  qplot_performance(data=qplot::perf1, labels=qplot::labels1,
#'                    output="./output/test1.png")
#'  }
#' }
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom fs path_ext
#' @importFrom dplyr mutate left_join filter select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggplot2 ggplot aes geom_bar geom_col scale_x_discrete coord_flip scale_fill_manual geom_text position_stack labs theme_minimal theme element_blank unit margin element_text
qplot_performance <- function(data=NULL, labels=NULL, order=NULL, output=NULL,
                              legend=TRUE, decimals=1, skeleton=FALSE, data_out=FALSE, ps=10){

  # Checks ------------------------------------------------------------------

  if (is.null(data)){
    cli::cli_abort("Either coefs must be specified or else skeleton=TRUE.")
  }

  if (is.null(labels)){
    cli::cli_warn("No label file is specified; using variable names instead")
  }

  # Filenames ---------------------------------------------------------------

  if (is.null(output) == FALSE){
    if(fs::path_ext(output) %in% c("png")){
      plot_path <- output
    } else {
      plot_path <- paste0(output, ".png")
    }
  }

  # Prep Data ---------------------------------------------------------------

  if (is.null(labels)){
    data <- dplyr::mutate(data, label=Variable)
  } else {
    data <- dplyr::left_join(data, labels, by=c("Variable"="var"))
  }

  data <- data %>%
    dplyr::filter(!is.na(label)) %>%
    dplyr::select(-Variable) %>%
    tidyr::pivot_longer(-label, names_to="perf", values_to="percent") %>%
    dplyr::mutate(perf = factor(perf, levels=c("Positive", "Neutral", "Negative"))) %>%
    dplyr::mutate(bar_label = sprintf(paste0("%.", 1, "f"), round(percent, 3)*100))

  if (is.data.frame(order)==TRUE){

    order <- process_coefs(DATA=order, LABELS=labels, DECIMALS=decimals, SORT=TRUE)$order

  } else {

    order <- data %>%
      dplyr::filter(perf=="Positive") %>%
      dplyr::select(label, percent) %>%
      tidyr::pivot_wider(names_from=label, values_from=percent)

  }

  data <- data %>%
    dplyr::filter(label %in% names(order))

  # Performance Plot --------------------------------------------------------

  if(skeleton==TRUE){
    if (Sys.getenv("QPLOT_TEST") != TRUE){
      cat("performance_plot <- data %>%\n")
      cat("  ggplot2::ggplot(ggplot2::aes(x=label, y=percent, fill=perf)) +\n")
      cat("  ggplot2::geom_bar(position=\"stack\", stat=\"identity\", width=.8) +\n")
      cat("  ggplot2::geom_col(ggplot2::aes(fill=perf)) +\n")
      cat("  ggplot2::scale_x_discrete(limits = names(order)) +\n")
      cat("  ggplot2::coord_flip() +\n")
      cat("  ggplot2::scale_fill_manual(values=c(\"Negative\"= \"#FF0000\",\n")
      cat("                                      \"Neutral\" = \"#BFBFBF\",\n")
      cat("                                      \"Positive\" = \"#008000\")) +\n")
      cat("  ggplot2::geom_text(ggplot2::aes(label=bar_label, fontface=\"bold\"), size=3, color=\"black\",\n")
      cat("                     position=ggplot2::position_stack(vjust=.5), check_overlap=TRUE) +\n")
      cat("  ggplot2::labs(title=NULL, y=NULL, x=NULL) +\n")
      cat("  ggplot2::theme_minimal()  +\n")
      cat("  ggplot2::theme(\n")
      cat("    legend.title = ggplot2::element_blank(),\n")
      cat("    legend.key.size = ggplot2::unit(4, \"mm\"),\n")
      cat("    plot.title = ggplot2::element_blank(),\n")
      cat("    panel.background = ggplot2::element_blank(),\n")
      cat("    panel.grid = ggplot2::element_blank(),\n")
      cat("    axis.text.x = ggplot2::element_blank(),\n")
      cat("    plot.margin = ggplot2::margin(1,20,1,1, \"mm\")) +\n")

      if (legend == FALSE){
        cat("    ggplot2::theme(legend.position = \"none\") +\n")
      } else {
        cat("    ggplot2::theme(legend.position = c(1.07, .10)) +\n")
      }

      cat("  ggplot2::theme(axis.text.y=ggplot2::element_text(size=10, face=\"bold\", color=\"black\"))\n")
      if (is.null(output)==FALSE){
        cat(paste0("grDevices::png(\"", as.character(plot_path), "\",\n"))
        cat("               width=1920, height=1080, res=300, pointsize=10, antialias=\"cleartype\")\n")
      } else {
        cat("grDevices::png(OUTPUT_PATH_WITH_PNG_EXTENSION,\n")
        cat("               width=1920, height=1080, res=300, pointsize=10, antialias=\"cleartype\")\n")
      }

      cat("print(performance_plot)\n")
      cat("invisible(dev.off())\n")
      cat("\n")
    }

    if (data_out==TRUE){
      return(list(data=data, order=order))
    }

  } else if (skeleton==FALSE){

    performance_plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(x=label, y=percent, fill=perf)) +
      ggplot2::geom_bar(position="stack", stat="identity", width=.8) +
      ggplot2::geom_col(ggplot2::aes(fill=perf)) +
      ggplot2::scale_x_discrete(limits = names(order)) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values=c("Negative"= "#FF0000", "Neutral" = "#BFBFBF", "Positive" = "#008000")) +
      ggplot2::geom_text(ggplot2::aes(label=bar_label, fontface="bold"), size=3, color="black",
                         position=ggplot2::position_stack(vjust=.5), check_overlap=TRUE) +
      ggplot2::labs(title=NULL, y=NULL, x=NULL) +
      ggplot2::theme_minimal()  +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(4, "mm"),
        plot.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(1,20,1,1, "mm"))

    #Display on screen
    if (is.null(output)){

      if (legend == FALSE){
        performance_plot <- performance_plot +
          ggplot2::theme(legend.position = "none")
      } else {
        performance_plot <- performance_plot +
          ggplot2::theme(legend.position = c(1.04, .09))
      }

      performance_plot <- performance_plot +
        ggplot2::theme(axis.text.y=ggplot2::element_text(size=12, face="bold", color="black"))
      print(performance_plot)

      #Saved as png
    } else {

      if (legend == FALSE){
        performance_plot <- performance_plot +
          ggplot2::theme(legend.position = "none")
      } else {
        performance_plot <- performance_plot +
          ggplot2::theme(legend.position = c(1.07, .10))
      }

      performance_plot <- performance_plot +
        ggplot2::theme(axis.text.y=ggplot2::element_text(size=10, face="bold", color="black"))
      png(plot_path, width=1920, height=1080, res=300, pointsize=ps, antialias="cleartype")

      #if (Sys.getenv("QPLOT_TEST") == TRUE){
      #  suppressWarnings(print(performance_plot))
      #} else {
        print(performance_plot)
      #}
      invisible(dev.off())
    }

  }

}
