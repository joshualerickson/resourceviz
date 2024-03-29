% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{ggplot_box_legend}
\alias{ggplot_box_legend}
\title{USGS Style Plot}
\usage{
ggplot_box_legend(family = "serif")
}
\arguments{
\item{family}{character of font face family}
}
\value{
A ggplot whisker and box plot diagram.
}
\description{
USGS Style Plot
}
\examples{
\dontrun{

master_flow <- readxl::read_xlsx('C:/Users/joshualerickson/Box/2500WatershedAirMgmt/EUR/stream/+Master Flow Data.xlsx')
cleaning_update <- discharge_cleaning(master_flow)

edna <- cleaning_update \%>\%
 filter(Stream == 'Edna') \%>\%
 group_by(year) \%>\%
 add_count() \%>\%
 ungroup() \%>\%
 filter(month \%in\% c(4,5,6,7,8,9)) \%>\%
  mutate(season = if_else(month \%in\% c(4,5,6), 'Runoff (April-June)', 'Lowflow (July-Aug)'),
         season = factor(season, levels = c('Runoff (April-June)', 'Lowflow (July-Aug)')))

 bx <- ggplot(data = edna,
  aes(x = year, y = TSS, group = year)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6, fill = "lightgrey") +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  expand_limits(y = 0) +
  theme_USGS_box() +
  scale_y_continuous(sec.axis = dup_axis(label = NULL,
                                         name = NULL),
                     expand = expansion(mult = c(0, 0)),
                     breaks = pretty(c(0,70), n = 5),
                     limits = c(0,70)) +
  labs(x = 'Year', y = 'Total Suspended Sediment (TSS) mg/l',
       title = 'Figure 1: Edna Creek Longitudinal TSS Monitoring',
       subtitle = paste0(edna \%>\%
                           pull(year) \%>\%
                           min(na.rm = T), ' - ', edna \%>\%
                           pull(year) \%>\%
                           max(na.rm = T))
  ) +
  facet_wrap(~season)

bx


legend_plot <- ggplot_box_legend()


cowplot::plot_grid(bx,
                   legend_plot,
                   nrow = 1,
                   rel_widths = c(1, .5))

}

}
