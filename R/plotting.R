#' Custom Theme
#'
#' @param font_size
#' @param font_family
#' @param line_size
#' @param rel_small
#' @param rel_tiny
#' @param rel_large
#' @param color
#'
#' @return
#' @export
#' @importFrom ggtext element_markdown
#'
custom_theme <-  function (font_size = 14, font_family = "", line_size = 0.5,
                           rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                           color = "grey85", map_void = 1, border_size = 2) {
  half_line <- font_size/2
  small_size <- rel_small * font_size
mapping_arg <- if(map_void == 1){

} else if(map_void == 2){
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = 2))
} else if(map_void == 3){
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
}  else if(map_void == 4){
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = border_size))
}

  theme_bw(base_size = font_size, base_family = font_family) %+replace%
    theme(line = element_line(color = "black",
                              size = line_size,
                              linetype = 1,
                              lineend = "butt"),
                              rect = element_rect(fill = NA,
                              color = NA, size = line_size, linetype = 1),
                              text = element_text(family = font_family,
                              face = "plain", color = "black", size = font_size,
                              hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                              margin = margin(), debug = FALSE), axis.line = element_line(color = "black",
                              size = line_size, lineend = "square"), axis.line.x = NULL,
                              axis.line.y = NULL, axis.text = element_text(color = "black",
                                                       size = small_size),
          axis.text.x =  element_markdown(margin = margin(t = small_size/4), vjust = 1),
          axis.ticks = element_line(color = "black",size = line_size),
          axis.ticks.length = unit(half_line/2,"pt"),
          axis.title.x = element_markdown(margin = margin(t = half_line/2),vjust = 1),
          axis.text.x.top = element_markdown(margin = margin(b = small_size/4), vjust = 0),
          axis.text.y = element_markdown(margin = margin(r = small_size/4), hjust = 1),
          axis.text.y.right = element_markdown(margin = margin(l = small_size/4), hjust = 0),
          axis.title.x.top = element_markdown(margin = margin(b = half_line/2),vjust = 0),
          axis.title.y = element_markdown(angle = 90,margin = margin(r = half_line/2), vjust = 1),
          axis.title.y.right = element_markdown(angle = -90, margin = margin(l = half_line/2),
                                                vjust = 0),
          legend.background = element_blank(),
          legend.spacing = unit(font_size, "pt"), legend.spacing.x = NULL,
          legend.spacing.y = NULL, legend.margin = margin(0,
                                                          0, 0, 0), legend.key = element_blank(), legend.key.size = unit(1.1 *
                                                                                                                           font_size, "pt"), legend.key.height = NULL,
          legend.key.width = NULL, legend.text = element_text(size = rel(rel_small)),
          legend.text.align = NULL, legend.title = element_markdown(hjust = 0),
          legend.title.align = NULL, legend.position = "right",
          legend.direction = NULL, legend.justification = c("left",
                                                            "center"), legend.box = NULL, legend.box.margin = margin(0,
                                                                                                                     0, 0, 0), legend.box.background = element_blank(),
          legend.box.spacing = unit(font_size, "pt"),
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid = element_line(color = color, size = line_size), panel.grid.major = NULL,
          panel.grid.minor = NULL,
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = NULL,
          panel.grid.minor.x = NULL,
          panel.grid.minor.y = NULL,
          panel.spacing = unit(half_line,
                               "pt"), panel.spacing.x = NULL, panel.spacing.y = NULL,
          panel.ontop = FALSE, strip.background = element_rect(fill = "grey80"),
          strip.text = element_text(size = rel(rel_small),
                                    margin = margin(half_line/2, half_line/2, half_line/2,
                                                    half_line/2)), strip.text.x = NULL, strip.text.y = element_text(angle = -90),
          strip.placement = "inside", strip.placement.x = NULL,
          strip.placement.y = NULL, strip.switch.pad.grid = unit(half_line/2,
                                                                 "pt"), strip.switch.pad.wrap = unit(half_line/2,
                                                                                                     "pt"), plot.background = element_blank(),
          plot.title = element_textbox(face = "bold", size = rel(rel_large),
                                        hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.subtitle = element_text(size = rel(rel_small),hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.caption = element_markdown(size = rel(rel_tiny),
                                          hjust = 1, vjust = 1, margin = margin(t = half_line)),
          plot.tag = element_text(face = "bold", hjust = 0,
                                  vjust = 0.7),
                                  plot.tag.position = c(0, 1),
                                  plot.margin = margin(half_line,half_line, half_line, half_line), complete = TRUE) +
    mapping_arg


}

#' USGS Style Plot
#'
#' @param family character of font face family
#'
#' @return A ggplot whisker and box plot diagram.
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#'
#' master_flow <- readxl::read_xlsx('C:/Users/joshualerickson/Box/2500WatershedAirMgmt/EUR/stream/+Master Flow Data.xlsx')
#' cleaning_update <- discharge_cleaning(master_flow)
#'
#' edna <- cleaning_update %>%
#'  filter(Stream == 'Edna') %>%
#'  group_by(year) %>%
#'  add_count() %>%
#'  ungroup() %>%
#'  filter(month %in% c(4,5,6,7,8,9)) %>%
#'   mutate(season = if_else(month %in% c(4,5,6), 'Runoff (April-June)', 'Lowflow (July-Aug)'),
#'          season = factor(season, levels = c('Runoff (April-June)', 'Lowflow (July-Aug)')))
#'
#'  bx <- ggplot(data = edna,
#'   aes(x = year, y = TSS, group = year)) +
#'   stat_boxplot(geom ='errorbar', width = 0.6) +
#'   geom_boxplot(width = 0.6, fill = "lightgrey") +
#'   stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
#'   expand_limits(y = 0) +
#'   theme_USGS_box() +
#'   scale_y_continuous(sec.axis = dup_axis(label = NULL,
#'                                          name = NULL),
#'                      expand = expansion(mult = c(0, 0)),
#'                      breaks = pretty(c(0,70), n = 5),
#'                      limits = c(0,70)) +
#'   labs(x = 'Year', y = 'Total Suspended Sediment (TSS) mg/l',
#'        title = 'Figure 1: Edna Creek Longitudinal TSS Monitoring',
#'        subtitle = paste0(edna %>%
#'                            pull(year) %>%
#'                            min(na.rm = T), ' - ', edna %>%
#'                            pull(year) %>%
#'                            max(na.rm = T))
#'   ) +
#'   facet_wrap(~season)
#'
#' bx
#'
#'
#' legend_plot <- ggplot_box_legend()
#'
#'
#' cowplot::plot_grid(bx,
#'                    legend_plot,
#'                    nrow = 1,
#'                    rel_widths = c(1, .5))
#'
#' }
#'
ggplot_box_legend <- function(family = "serif"){

  # Create data to use in the boxplot legend:
  set.seed(100)

  sample_df <- data.frame(parameter = "test",
                          values = sample(500))

  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350

  # Function to calculate important values:
  ggplot2_boxplot <- function(x){

    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))

    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")

    IQR <- diff(quartiles[c(1,3)])

    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])

    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]

    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }

  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)

  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))

  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey") +
    geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    geom_text(aes(x = 1.17, y = 950,
                  label = "Number of values"),
              fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["75th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]),
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Outside value"),
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.9),
                  y =  ggplot_output[["lower_dots"]],
                  label = "-Value is >1.5 times and"),
              vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")


  ggplot2::update_geom_defaults("text",
                       list(size = 2,
                            family = "serif"))
  return(explain_plot)

}

#' USGS Theme
#'
#' @param base_family character
#' @param ... Arguments to pass on to theme_bw().
#'
#' @return A ggplot theme layer.
#' @export

theme_USGS_box <- function(base_family = "serif", ...){
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12, face="bold"),
      plot.title = element_text(color = "black", size = 14, face = "bold.italic"),
      axis.ticks.length = unit(-0.05, "in"),
      axis.text.y = element_text(size = 10,margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.text.x = element_text(size = 10,margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.ticks.x = element_blank(),
      aspect.ratio = 1,
      legend.background = element_rect(color = "black", fill = "white")
    )
}

#' Stat Summary Helper
#'
#' @param x numeric
#' @param y numeric
#'
#' @return A data.frame
#' @export
#'

n_fun <- function(x, y){
  return(data.frame(y = 0.95*y,
                    label = length(x)))
}


