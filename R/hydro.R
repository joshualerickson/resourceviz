

#' Cleaning Data Frame
#'
#' @description This function helps clean the data from the districts
#' master flow data as well as perform some interpolation for discharge.
#' @param data A data.frame from `get_master_flow()`.
#' @param wy_month A numeric vector for water year month.
#'
#' @return A data.frame
#' @export
#' @importFrom dplyr across mutate group_by select left_join ungroup if_else filter pull
#' @importFrom tidyr nest unnest
#' @importFrom purrr map pluck safely keep
#' @examples
#' \dontrun{
#' master_flow <- get_master_flow()
#' cleaning_update <- discharge_cleaning(master_flow)
#'
#' }
discharge_cleaning <- function(data, wy_month = 10){

  master_flow <- data %>%
    mutate(year = lubridate::year(Date),
           month = lubridate::month(Date),
           day = lubridate::day(Date)) %>%
    mutate(across(c('Q', 'TSS'), as.numeric)) %>%
    mutate(Stream = dplyr::if_else(Stream %in% c('Young 303', 'Young 303 Bridge'), 'Young 303', Stream))

  linear_results <- master_flow %>%
    group_by(Stream, year) %>%
    nest() %>%
    mutate(model = map(data, safely(~lm(log10(.$Q)~log10(.$GH), data = .)))) %>%
    purrr::keep(~length(.) != 0) %>%
    mutate(result = map(model, ~.x[['result']]),
           result_null = map(result, ~!is.null(.))) %>%
    dplyr::filter(result_null == TRUE) %>%
    mutate(intercept = as.numeric(map(result, ~pluck(.$coefficients[1]))),
           beta = as.numeric(map(result, ~pluck(.$coefficients[2])))) %>%
    dplyr::select(Stream,year, intercept, beta) %>% unnest()

  master_flow_update <- master_flow %>%
    left_join(linear_results, by = c('Stream', 'year')) %>%
    group_by(Stream, year) %>%
    mutate(Q_flag = if_else(is.na(Q), 'Not Measured', 'Measured'),
           Q = as.numeric(if_else(is.na(Q), 10^((log10(GH)*beta)+intercept), Q))) %>%
    ungroup %>% mutate(type = 'Observed Q',
                       dt = lubridate::as_datetime(as.character(Date, format="%Y-%m-%d 12:00:00")))

  clean_up(master_flow_update, wy_month = wy_month)

}


#' Water Year Stats
#' @description This function uses the results of the \link[resourceviz]{discharge_cleaning} object to
#' generate mean, maximum, median, standard deviation and some normalization methods (drainage
#' area, scaled by log and standard deviation) per water year.
#' @param procDV A previously created \link[resourceviz]{discharge_cleaning}.
#' @return A \code{tibble} filtered by water year with added meta-data.
#' @export
#'
#' @examples \dontrun{
#'
#' library(resourceviz)
#'
#' master_flow <- get_master_flow()
#' cleaning_update <- discharge_cleaning(master_flow)
#'
#' wy_stats <- get_wy_stats(cleaning_update)
#' }
#'
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup everything row_number n
#' @importFrom stringr str_c str_remove_all
#' @importFrom stats median sd
#'
get_wy_stats <- function(procDV) {

  proc_raw <- procDV %>%
    pad_zero_for_logging()

  cols <- c('Q', 'TSS')

  proc_raw %>%
  group_by(Stream, wy) %>%
  summarise(across(dplyr::any_of(cols),
                   list(
                     max = ~max(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     mean = ~mean(.x, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     stdev = ~sd(.x, na.rm = TRUE),
                     coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),
                     max_sdnorm = ~log(max(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                     min_sdnorm = ~log(min(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                     mean_sdnorm = ~log(mean(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                     med_sdnorm = ~log(median(.x, na.rm = TRUE))/sd(log(.x), na.rm = TRUE),
                     sd_norm = ~sd(log(.x), na.rm = TRUE))))  %>%
  ungroup() %>%
  dplyr::left_join(procDV %>%
                     dplyr::select(
                       Stream,
                       wy,
                       obs_per_wy) %>%
                     dplyr::group_by(Stream,wy) %>%
                     dplyr::slice(n = 1) %>%
                     dplyr::ungroup(), by = c('Stream', 'wy')) %>%
  dplyr::group_by(Stream) %>%
  dplyr::add_count(name = 'wy_count') %>%
  dplyr::ungroup() %>%
  dplyr::relocate(Stream, obs_per_wy, wy_count, dplyr::everything())
}

#' Get HUC12 Info
#'
#' @param path character; typically the T:Drive
#' @param sf A sf object.
#' @param ... additional arguments
#' @return A data.frame
#' @export
#' @note This function assumes that there is a gdb with a layer name 'ProposedAction'. If not
#' (missing) then it defaults to the sf object of the units.

get_huc12_information <- function(path = NULL, sf = NULL, ...){


# get units for project within the T: drive
if(!is.null(sf)){pa <- sf %>% sf::st_cast('MULTIPOLYGON')}

if(!is.null(path)){

  if(stringr::str_detect(path, '.gdb')) {

  pa <- sf::st_read(path, layer = 'ProposedAction', quiet = T) %>%
  sf::st_cast('MULTIPOLYGON')

  } else {

  pa <- sf::st_read(path, quiet = T) %>%
      sf::st_cast('MULTIPOLYGON')

  }

  }

stopifnot(inherits(pa, 'sf'))

pa <- pa[!sf::st_is_empty(pa),]

## Get HUC 12's that intersect the units
get_centroid <- sf::st_centroid(pa)

# huc12s <- nhdplusTools::get_huc12(sf::st_as_sfc(sf::st_bbox(pa)))
vars <- c('max', 'mean', 'median', 'min', 'stdDev')

huc12s <- districth12 %>%
          sf::st_transform(sf::st_crs(pa)) %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(vars), ~.*0.0393701))

huc12_intersect <- sf::st_intersects(huc12s, get_centroid)
huc12_intersect_logic <- lengths(huc12_intersect) > 0
huc12s <- huc12s[huc12_intersect_logic,]

huc12s <- huc12s %>% dplyr::bind_rows()

huc12s <- huc12s %>% dplyr::distinct(geometry, .keep_all = T)

huc12s_df <- huc12s %>% sf::st_drop_geometry()

hucs_prop <- huc12s %>%
  sf::st_intersection(pa) %>%
  dplyr::mutate(unit_area = as.numeric(units::set_units(st_area(.), 'acres'))) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(sum_of_units = sum(unit_area),
                   proportion = 100*(sum_of_units/areaacres)) %>%
  dplyr::slice(n = 1) %>%
  dplyr::left_join(huc12s_df %>% dplyr::select(dplyr::all_of(vars), name), by = 'name' )

huc12s_df <- huc12s_df %>%
  dplyr::select(huc12, areaacres, name, dplyr::all_of(vars)) %>%
  dplyr::left_join(hucs_prop) %>%
  dplyr::arrange(proportion)

}

#' Finalize Q Data
#' @description This function will combine all the previously
#'  created lists (`wl_sites_daily` and `wl_sites_hourly`) and perform
#'  the log10(Q)~log10(height) regression parameters to daily and hourly data. It will
#'  then add to the original `data` for a collated final dataset.
#'
#'
#' @param data Previously created data.frame using \link{get_master_flow}.
#' @param wl_sites_daily Previously created list with daily hobo data.
#' @param wl_sites_hourly Previously created list with hourly hobo data.
#' @param year A numeric vector determining what year to regress the `wl_sites_*` on.
#' @param wy_month A numeric vector determining the water year month.
#' @return A data.frame
#' @export
#'
#' @examples
finalize_q_data <- function(data, wl_sites_daily, wl_sites_hourly, year, wy_month = 10) {

  mf_mod <- data %>%
  filter(year %in% {{year}},
         !is.na(GH),
         !is.na(Q),
         Q > 0,
         GH > 0,
         Stream %in% names(wl_sites_daily)) %>%
  group_by(Stream) %>%
  nest() %>%
  mutate(model = map(data, safely(~lm(log10(.$Q)~log10(.$GH), data = .)))) %>%
  purrr::keep(~length(.) != 0) %>%
  mutate(result = map(model, ~.x[['result']]),
         result_null = map(result, ~!is.null(.))) %>%
  dplyr::filter(result_null == TRUE) %>%
  mutate(intercept = map(result, ~pluck(.$coefficients[1])),
         beta = map(result, ~pluck(.$coefficients[2]))) %>%
  dplyr::select(Stream,intercept, beta) %>% unnest() %>% ungroup()

mf_mod_final_daily <- map(wl_sites_daily, ~merge(., mf_mod, by = c('Stream')))

mod_results_final_daily <- map(mf_mod_final_daily, ~.x %>%
                                 mutate(Q = 10^((log10(.$GH)*.$beta)+.$intercept),
                                                                  type = 'Daily Q',
                                        year = lubridate::year(dt),
                                        month = lubridate::month(dt),
                                        day = lubridate::day(dt))) %>% bind_rows() %>%
                               clean_up(wy_month = wy_month)


mf_mod_final_hourly <- map(wl_sites, ~merge(., mf_mod, by = 'Stream'))

mod_results_final_hourly <- map(mf_mod_final_hourly, ~.x %>%
                                  mutate(Q = 10^((log10(.$GH)*.$beta)+.$intercept),
                                         type = 'Hourly Q',
                                         year = lubridate::year(dt),
                                         month = lubridate::month(dt),
                                         day = lubridate::day(dt))) %>% bind_rows() %>%
                                clean_up(wy_month = wy_month)

final_data <- list(final_daily = mod_results_final_daily,final_hourly = mod_results_final_hourly)

}

#' Getting Treatment Proportions
#'
#' @param path character; typically the T:Drive
#' @param sf An sf object.
#' @param ... Additional arguments.
#' @return A data.frame
#' @export
#'
get_treatment_proportion <- function(path = NULL,sf = NULL, ...) {

  if(missing(path) && !is.null(sf)){pa <- sf %>% sf::st_cast('MULTIPOLYGON')}


  if(!is.null(path)){

    if(stringr::str_detect(path, '.gdb')) {

      pa <- sf::st_read(path, layer = 'ProposedAction', quiet = T) %>%
        sf::st_cast('MULTIPOLYGON')

    } else {

      pa <- sf::st_read(path, quiet = T) %>%
        sf::st_cast('MULTIPOLYGON')

    }

  }

  stopifnot(inherits(pa, 'sf'))

  pa <- pa[!sf::st_is_empty(pa),]

  possible_treatment_names <- c('Treatment', 'treatment', 'activity', 'Activity')
  possible_col_names <- c('unit', 'Unit', 'Units', 'units', 'acres', 'Acres')


  df <- pa %>%
    dplyr::mutate(acres_unit = as.numeric(units::set_units(sf::st_area(.), 'acres'))) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::any_of(c(possible_treatment_names, possible_col_names, 'acres_unit')))

  group_name <- possible_treatment_names[match(colnames(df), possible_treatment_names, nomatch = 0)]

  df %>%
    dplyr::group_by(!!rlang::sym(group_name)) %>%
    dplyr::summarise(acres = sum(acres_unit),
                     proportion = 100*(sum(acres_unit)/sum(df$acres_unit))) %>%
    dplyr::mutate(across(dplyr::any_of(group_name), ~stringr::str_to_title(.))) %>%
    dplyr::rename_with(.cols = dplyr::any_of(group_name), ~stringr::str_to_title(.))  %>%
    dplyr::arrange(proportion)



}


#' Get TSS Boxplot
#'
#' @param data A data.frame with Stream, year, month and TSS cols.
#' @param stream A character vector of stream names
#' @param remove_outliers logical. Whether to remove outliers or not.
#' @param ... Arguments to pass to appedix functions.
#' @return A ggplot.
#' @export
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes stat_boxplot geom_boxplot stat_summary scale_y_continuous
#' @importFrom rlang .data
#'

get_tss_boxplot <- function(data, stream,remove_outliers = FALSE, ...) {

# master_flow <- get_master_flow()
#
# cleaning_update <- discharge_cleaning(master_flow)

site <- data %>%
  filter(Stream == dplyr::all_of(stream)) %>%
  filter(month %in% c(4,5,6,7,8,9)) %>%
  mutate(season = dplyr::if_else(month %in% c(4,5,6), 'Runoff (April-June)', 'Lowflow (July-Aug)'),
         season = factor(season, levels = c('Runoff (April-June)', 'Lowflow (July-Aug)')))

if(length(stream) == 1){
bx <- ggplot(data = site,
             aes(x = year, y = TSS, group = year)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6, fill = "lightgrey",
               outlier.shape = ifelse(remove_outliers, NA, 20)) +
  stat_summary(fun.data = ~n_fun(., site %>% dplyr::slice_max(TSS) %>% pull(TSS)),
               geom = "text",
               hjust = 0.5) +
  expand_limits(y = 0)  +
  scale_y_continuous(sec.axis = dup_axis(label = NULL,
                                         name = NULL),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = 'Year', y = 'Total Suspended Sediment (TSS) mg/l',
       title = paste0('Figure 1: ', stream,' Creek Longitudinal TSS Monitoring'),
       subtitle = paste0(site %>%
                           pull(year) %>%
                           min(na.rm = T), ' - ',
                           site %>%
                           pull(year) %>%
                           max(na.rm = T))
  ) +
  custom_theme()+
  facet_wrap(~season)


legend_plot <- ggplot_box_legend()


cowplot::plot_grid(bx,
                   legend_plot,
                   nrow = 1,
                   rel_widths = c(1, .5))


} else {

  bx <- ggplot(data = site,
               aes(x = year, y = TSS, group = year)) +
    stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot(width = 0.6, fill = "lightgrey",
                 outlier.alpha = ifelse(remove_outliers, 0, 1)) +
    stat_summary(fun.data = ~n_fun(., site %>% dplyr::slice_max(TSS) %>% dplyr::pull(TSS)),
                 geom = "text", hjust = 0.5) +
    expand_limits(y = 0)  +
    scale_y_continuous(sec.axis = dup_axis(label = NULL,
                                           name = NULL),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = 'Year', y = 'Total Suspended Sediment (TSS) mg/l') +
    custom_theme()+
    facet_wrap(Stream~season,...)

  bx

}

}

#' Get TSS Timeseries
#'
#' @param stream A character vector of stream names
#' @param year A numeric vector of years, e.g. 2012:2015, 2015, etc.
#' @param ... Arguments to pass to appedix functions.
#' @return A ggplot.
#'
#' @export
#'
get_tss_timeseries <- function(stream, year, ...){

  master_flow <- get_master_flow()

  cleaning_update <- discharge_cleaning(master_flow)

  if(missing(year)){

  site <- cleaning_update %>%
    filter(Stream == dplyr::all_of(stream))

  } else {

    site <- cleaning_update %>%
      filter(Stream == dplyr::all_of(stream),
             year %in% {{year}})

  }

  bx <- ggplot(data = site,
               aes(x = Date, y = TSS, group = year)) +
    expand_limits(y = 0)  +
    geom_point(size = 0.25) +
    geom_line(size = .5) +
    scale_y_continuous(sec.axis = dup_axis(label = NULL,
                                           name = NULL),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = 'Year', y = 'Total Suspended Sediment (TSS) mg/l') +
    custom_theme()+
    facet_wrap(dplyr::vars(Stream),...)

  bx

}

#' Get Flow Data
#'
#' @return A data.frame of local discharge and TSS data
#' @export
#' @noRd
get_master_flow <- function() {

  master_flow <- readxl::read_xlsx('C:/Users/joshualerickson/Box/2500WatershedAirMgmt/EUR/stream/+Master Flow Data.xlsx')

}


#'Water Year These functions are hi-jacked from smwrBase package.
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the water year.
#' @noRd
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'result in corresponding missing values in the output.
#' @param wy_month A numeric indicating the month the water year begins.
#' @param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#' @return An ordered factor or numeric vector corresponding to the water year.
#' @note The water year is defined as the period from October 1 to September 30.
#'The water year is designated by the calendar year in which it ends. Thus, the
#'year ending September 30, 1999, is the "1999 water year."
#' @seealso
#Flip for production/manual
#'\code{\link[lubridate]{year}}
#\code{year} (in lubridate package)

waterYear <- function(x, wy_month = 10, numeric=FALSE) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated verion
  ##    2010Feb17 DLLorenz Added option to return numerics
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < as.integer(wy_month), 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}

#' Add Counts
#' @description Adds counts of observation per water and month
#'
#' @param data A daily value df
#'
#' @return counts within df
#' @noRd
add_date_counts <- function(data) {

  dplyr::group_by(data, Stream, wy) %>%
    filter(!is.na(Q)) %>%
    dplyr::add_count(name = 'obs_per_wy') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Stream, wy, month) %>%
    dplyr::add_count(name = 'obs_per_month') %>%
    dplyr::ungroup()
}


#' water year to months
#' @description Change wy_month to doy.
#' @param wy_month A numeric
#' @param leap Logical
#' @return A numeric value
#' @noRd
month_to_doy <- function(wy_month, leap = FALSE) {


  ifelse(isTRUE(leap),
         dplyr::case_when(wy_month == 1 ~ 1,
                          wy_month == 2 ~ 32,
                          wy_month == 3 ~ 61,
                          wy_month == 4 ~ 92,
                          wy_month == 5 ~ 122,
                          wy_month == 6 ~ 153,
                          wy_month == 7 ~ 183,
                          wy_month == 8 ~ 214,
                          wy_month == 9 ~ 245,
                          wy_month == 10 ~ 275,
                          wy_month == 11 ~ 306,
                          wy_month == 12 ~ 336,
                          TRUE ~ NA_real_)
         ,
         dplyr::case_when(wy_month == 1 ~ 1,
                          wy_month == 2 ~ 32,
                          wy_month == 3 ~ 60,
                          wy_month == 4 ~ 91,
                          wy_month == 5 ~ 122,
                          wy_month == 6 ~ 152,
                          wy_month == 7 ~ 182,
                          wy_month == 8 ~ 213,
                          wy_month == 9 ~ 244,
                          wy_month == 10 ~ 274,
                          wy_month == 11 ~ 305,
                          wy_month == 12 ~ 335,
                          TRUE ~ NA_real_)
  )

}

#' Prepping for loggin
#'
#' @param data data.frame
#'
#' @return values are padded if zero for parameter of interest
#' @noRd
#'
pad_zero_for_logging <- function(data){

  param_names <- c("Q","TSS")

  cols_to_update <- names(data[which(names(data) %in% param_names)])

  data %>%
    mutate(across(dplyr::all_of(cols_to_update), ~ifelse(.x <= 0 , .x + 0.000001, .x)))
}


#' Clean-Up helper
#'
#' @param data data.frame
#' @param wy_month A numeric vector determining the wy month.
#' @return A data.frame
#' @noRd
#'
clean_up <- function(data, wy_month = 10) {


  leap_years <- seq(1832,3048, 4)

    data %>%
    mutate(Date = lubridate::as_date(Date),
           doy=lubridate::yday(Date),
           wy_doy = ifelse(!(year %in% leap_years),ifelse(doy >= month_to_doy(wy_month, leap = F),
                                                          doy-month_to_doy(wy_month, leap = F)+1,
                                                          (365-month_to_doy(wy_month, leap = F)+1+doy)),
                           ifelse(doy >= month_to_doy(wy_month, leap = T),
                                  doy-month_to_doy(wy_month, leap = T)+1,
                                  (366-month_to_doy(wy_month, leap = T)+1+doy))),
           month_day = str_c(month, day, sep = "-"),
           wy = waterYear(Date, wy_month, TRUE),
           month_abb = factor(month.abb[month], levels = month.abb),
           month_day = str_c(month, day, sep = "-")) %>%
    add_date_counts()
}
