#' @title qplot_nps
#' @description Generates a barchart of the average NPS score for every value of a
#' second variable, likely a key driver prospect.
#' @param data Dataframe name that contains the data to be plotted.  Required.
#' @param nps The name of the NPS variable in the dataframe, without quotes. Required.
#' @param var The name of the variable plotting variable whose values will form
#' the groups across which the NPS average will be calculated.
#' @param var_text Character string to be used in the plot in place of the name
#' of the var variable.  If not provided, the variable name will be used.  Optional.
#' @param level_labels Character vector of labels to be used along the horizontal access,
#' one for each value of var.  Do not include an NA label as this will automatically
#' be created.  If not labels are provided, the values themselves are used.  Optional.
#' @param output output Character string containing the complete path and file name of
#' a png file of the resulting performance plot.  If not provided, the plot will
#' be shown on the screen instead but may not be optimized for screen viewing.
#' Optional.
#' @param legend A logical value indicating if a legend containing the Labels of
#' Positive, Neutral, and Negative along with their corresponding colors should
#' be displayed in the bottom right-hand corner of the plot. Default: TRUE
#' @param rm.na A logical value indicating if missing values should be removed
#' from var or not.  Default: FALSE
#' @param sizes A logical value indicating if the frequency and percentage of each
#' category of var along the horizontal access is shown with the level labels on
#' the plot.  Default: FALSE
#' @param tukey A logical value indicating if an ANOVA and Tukey Tests for contrasts
#' should be performed
#' @param skeleton A logical value to indicate if the syntax should be printed to
#' the console.  This is useful if the default selections need tweaking so the
#' user can see the raw syntax that can be used in place of the function.
#' Default: FALSE
#' @param data_out Logical indicating if the plot data should be returned.  If
#' tukey parameter is set to TRUE, then it will output a list containing both the
#' plot data and the tukey data.
#' @param ps Point size setting for grDevices::jpeg.  May need to reduce if lots
#' of attributes are included.  Default=8.
#' @return Outputs a png file of the nps plot if output path specified,
#' otherwise generates plot on screen.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  qplot::nps1 %>%
#'  dplyr::select(-response_id) %>%
#'    qplot_nps(data=., var=price, nps=nps, var_text="Price", rm.na=TRUE,
#'              level_labels=c("Very Unsatisifed", "Unsatisfied", "Neither", "Satisified", "Very Satisfied"))
#'  }
#' }
#' @export
#' @importFrom rlang enquo quo_name f_text
#' @importFrom cli cli_abort
#' @importFrom fs path_ext
#' @importFrom dplyr filter mutate case_when select group_by summarize n relocate
#' @importFrom broom tidy
#' @importFrom scales comma percent
#' @importFrom ggplot2 ggplot ylim geom_bar geom_text position_dodge scale_fill_gradientn theme_minimal geom_hline theme element_line element_text labs

qplot_nps <- function(data, var, nps, var_text=NULL, level_labels=NULL, output=NULL,
                      legend=TRUE, rm.na = FALSE, sizes=FALSE, tukey=FALSE,
                      skeleton = FALSE, data_out=FALSE, ps=8){

  VAR <- rlang::enquo(var)
  NPS <- rlang::enquo(nps)

  # Checks ------------------------------------------------------------------

  if (missing(data)){
    cli::cli_abort("Data must be speciffied.")
  }

  if (missing(var)){
    cli::cli_abort("Variable must be specified.")
  }

  if (missing(nps)){
    cli::cli_abort("NPS variable must be specified.")
  }

  if (! rlang::quo_name(NPS) %in% colnames(data)){
    cli::cli_abort("Variabe {nps} not found in data.")
  }

  if (! rlang::quo_name(VAR) %in% colnames(data)){
    cli::cli_abort("Variabe {var} not found in data.")
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

  nps_colors <- c("#FF0000","#FF4F4F", "#FF7B7B", "#FFA7A7",
                  "#FFD3D3", "#FFFFFF", "#D1EAD6",
                  "#A3D5AE", "#75C085", "#47AB5D", "#008000")

  if (is.null(var_text)){
    var_text <- rlang::f_text(VAR)
  }

  if (rm.na==TRUE){
    data <- data %>%
      dplyr::filter(!is.na(!!VAR))
  }

  if (tukey==TRUE){
    tdata <- nps1 %>%
      dplyr::mutate(NPS = nps,
                    VAR = dplyr::case_when(is.na(price) ~ "NA",
                                           TRUE ~ as.character(price)))

    tukey_out <- aov(data=tdata, NPS ~ VAR)
    tukey_out1 <- broom::tidy(tukey_out) %>%
      mutate(contrast=NA_character_,
             estimate=NA,
             adj.p.value=NA)

    tukey_out2 <- TukeyHSD(tukey_out) %>%
      broom::tidy() %>%
      dplyr::select(contrast, estimate, adj.p.value)

    tukey_out <- dplyr::bind_rows(tukey_out1, tukey_out2)
  }

  plot_data <- data %>%
    dplyr::select(!!NPS, !!VAR) %>%
    dplyr::group_by(!!VAR) %>%
    dplyr::summarize(nps=mean(!!NPS, na.rm=TRUE),
                     n=dplyr::n()) %>%
    dplyr::mutate(number = 1:dplyr::n(),
                  nps = round(nps, 1),
                  percent = round(n / sum(n), 3),
                  vjust = ifelse(nps < 0, 2, -2),
                  n = scales::comma(n),
                  percent = scales::percent(percent)) %>%
    dplyr::relocate(number) %>%
    dplyr::mutate(label = as.character(!!VAR),
                  label = ifelse(is.na(label), "NA", label),
                  label = paste0(label)); #"**"

  if (!is.null(level_labels)){
    #coding label this way to account for NA at the end of the list, if present
    for (i in 1:length(level_labels)){
      plot_data$label[i] <- paste0(level_labels[i]); #**"
    }
  }

  if (sizes==TRUE){
    plot_data <- plot_data %>%
      dplyr::mutate(percent = paste0("(", percent, ")")) %>%
      dplyr::mutate(label = paste(label, n, percent, sep='\n')); #\n\n
  }

  # NPS Graph ---------------------------------------------------------------

  if (skeleton==TRUE){

    if (Sys.getenv("QPLOT_TEST") != TRUE){
      cat("nps_plot <- data %>%\n")
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

      cat("nps_plot <- plot_data %>%\n")
      cat("  ggplot2::ggplot(., aes(x=factor(x=number, labels = label), y=nps, fill=nps)) +\n")
      cat("  ggplot2::ylim(-120, 120) +\n")
      cat("  ggplot2::geom_bar(stat='identity', linewidth=.5, linetype=\"solid\", color=\"black\") +\n")
      cat("  ggplot2::geom_text(aes(label=nps), size=5, color=\"black\",\n")
      cat("                     position=ggplot2::position_dodge(width=1),\n")
      cat("                     vjust=plot_data$vjust,\n")
      cat("                     fontface='bold') +\n")
      cat("  ggplot2::scale_fill_gradientn(name=paste('NPS',\"\n\"),\n")
      cat("                                colours = c(\"#FF0000\",\"#FF4F4F\", \"#FF7B7B\", \"#FFA7A7\",\n")
      cat("                                            \"#FFD3D3\", \"#FFFFFF\", \"#D1EAD6\",\n")
      cat("                                            \"#A3D5AE\", \"#75C085\", \"#47AB5D\", \"#008000\"),\n")
      cat("                                limits=c(-100, 100),\n")
      cat("                                breaks = c(-100,-80,-60,-40,-20,0,20,40,60,80,100)) +\n")
      cat("  ggplot2::theme_minimal() +\n")
      cat("  ggplot2::geom_hline(yintercept = 0, color = \"black\") +\n")
      cat("  ggplot2::theme(panel.grid = element_blank(),\n")
      cat("                 plot.background = element_blank(),\n")
      cat("                 axis.line.y = ggplot2::element_line(linewidth = 0.5, linetype = \"solid\", colour = \"black\"),\n")
      cat("                 axis.text = ggplot2::element_text(size=10,colour='black'),\n")
      cat("                 legend.text.align=1) +\n")
      cat("  ggplot2::labs(title=var_text,y=\"NPS\",x=NULL)\n")

      if (legend == FALSE){
        cat("nps_plot <- nps_plot +\n")
        cat("  ggplot2::theme(legend.position = \"none\")\n")
      } else {
        cat("nps_plot <- nps_plot +\n")
        cat(". ggplot2::theme(legend.position = c(1.1, .15),\n")
        cat("                   plot.margin = margin(1,20,1,1, \"mm\"))\n")
      }

      cat("nps_plot <- nps_plot +\n")
      cat("  ggplot2::theme(legend.text = element_text(size=6),\n")
      cat("                 plot.title=element_text(size=14, face=\"bold\", hjust=.5),\n")
      cat("                 axis.text.y=element_text(size=8, color=\"black\"))\n")

      if (sizes==TRUE){
        cat("nps_plot <- nps_plot +\n")
        cat("    ggplot2::theme(axis.text.x=element_text(size=8, color=\"black\"))\n")
      } else {
        cat("nps_plot <- nps_plot +\n")
        cat("    ggplot2::theme(axis.text.x=element_text(size=8, face=\"bold\", color=\"black\")\n")
      }

      cat("png(plot_path, width=1920, height=1080, res=300, pointsize=8)\n")
      cat("print(nps_plot)\n")
      cat("invisible(dev.off())\n")
    }

    if (data_out==TRUE & tukey==TRUE){
      return(list(data=plot_data, tukey=tukey_out))
    } else if (data_out==TRUE & tukey==FALSE){
      return(plot_data)
    } else if (data_out==FALSE & tukey==TRUE){
      return(tukey_out)
    }

  } else {

    nps_plot <- plot_data %>%
      ggplot2::ggplot(., aes(x=factor(x=number, labels = label), y=nps, fill=nps)) +
      ggplot2::ylim(-120, 120)+
      ggplot2::geom_bar(stat='identity', linewidth=.5, linetype="solid", color="black") +
      ggplot2::geom_text(aes(label=nps),
                         size=5,
                         color="black",
                         position=ggplot2::position_dodge(width=1),
                         vjust=plot_data$vjust,
                         fontface='bold') +
      ggplot2::scale_fill_gradientn(name=paste('NPS',"\n"),
                                    colours = nps_colors,
                                    limits=c(-100, 100),
                                    breaks = c(-100,-80,-60,-40,-20,0,20,40,60,80,100))+
      ggplot2::theme_minimal() +
      ggplot2::geom_hline(yintercept = 0, color = "black") +
      ggplot2::theme(panel.grid = element_blank(),
                     plot.background = element_blank(),
                     axis.line.y = ggplot2::element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
                     axis.text = ggplot2::element_text(size=10,colour='black'),
                     legend.text.align=1) +
      ggplot2::labs(title=var_text,y="NPS",x=NULL)

    #Display on screen
    if (is.null(output)){

      if (legend == FALSE){
        nps_plot <- nps_plot +
          ggplot2::theme(legend.position = "none")
      } else {
        nps_plot <- nps_plot +
          ggplot2::theme(legend.position = c(1.05, .15),
                         plot.margin = margin(1,20,1,1, "mm"))
      }

      nps_plot <- nps_plot +
        ggplot2::theme(legend.text = element_text(size=8),
                       plot.title=element_text(size=16, face="bold", hjust=.5),
                       axis.text.y=element_text(size=12, color="black"))

      if (sizes==TRUE){
        nps_plot <- nps_plot +
          ggplot2::theme(axis.text.x=element_text(size=10, color="black"))
      } else {
        nps_plot <- nps_plot +
          ggplot2::theme(axis.text.x=element_text(size=10, face="bold", color="black"))
      }

      print(nps_plot)

      #Saved as png
    } else {

      if (legend == FALSE){
        nps_plot <- nps_plot +
          ggplot2::theme(legend.position = "none")
      } else {
        nps_plot <- nps_plot +
          ggplot2::theme(legend.position = c(1.1, .15),
                         plot.margin = margin(1,20,1,1, "mm"))
      }

      nps_plot <- nps_plot +
        ggplot2::theme(legend.text = element_text(size=6),
                       plot.title=element_text(size=14, face="bold", hjust=.5),
                       axis.text.y=element_text(size=8, color="black"))

      if (sizes==TRUE){
        nps_plot <- nps_plot +
          ggplot2::theme(axis.text.x=element_text(size=8, color="black"))
      } else {
        nps_plot <- nps_plot +
          ggplot2::theme(axis.text.x=element_text(size=8, face="bold", color="black"))
      }

      png(plot_path, width=1920, height=1080, res=300, pointsize=8)
      print(nps_plot)
      invisible(dev.off())
    }
  }

  if (tukey==TRUE){
    return(tukey_out)
  }
}
