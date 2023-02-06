#' Get Soils Appendix
#'
#' @param path A file path to the polygons.
#' @param save_path A file path to save the .docx file.
#' @param ... If no path, then ... can be an sf object.
#' @return A rendered HTML report
#' @export
#' @importFrom utils browseURL
#'
get_soils_appendix <- function(path, save_path, ...){

  rmarkdown::render(system.file('app/www', 'snotel_stats_static.Rmd', package = 'hydroapps'),
                    output_format = rmarkdown::html_document())
  browseURL(system.file('app/www', 'snotel_stats_static.html', package = 'hydroapps'))
}


#' Get Hydro Appendix
#'
#' @param save_path A file path to save the .docx file.
#' @param eval_plot Render a TSS plot.
#' @param eval_huc_table Render a HUC table.
#' @param eval_prop_table Render a proportion table.
#' @param ... Arguments to pass to \link{get_huc12_information} and \link{get_treatment_proportion}.
#' @return A rendered HTML report.
#' @export
#'
#'

get_hydro_appendix <- function(save_path,
                               eval_plot = TRUE,
                               eval_huc_table = TRUE,
                               eval_prop_table = TRUE,
                               eval_roads_table = TRUE,
                               ...){


  rmarkdown::render(system.file('app/www', 'hydro_appendix.Rmd', package = 'resourceviz'),
                    output_format = rmarkdown::word_document())

  if(missing(save_path)){save_path <- getwd()}

  file.rename(from = system.file('app/www', 'hydro_appendix.docx', package = 'resourceviz'),to = paste0(save_path, '/hydro_appendix.docx'))
}


#' Generate gt Hydro Table
#'
#' @param data A previously created \link{get_huc12_information} object.
#' @return A gt table.
#' @export
#'

hydro_huc_table <- function(data) {
  vars <- c('max', 'mean', 'median', 'min', 'stdDev')

  data %>%
  gt::gt() %>%
    gt::fmt_number(columns = c(dplyr::all_of(vars), areaacres, proportion, sum_of_units), decimals = 0) %>%
    gt::sub_small_vals(small_pattern = "<{x}") %>%
    #gt::fmt_number(columns = dplyr::all_of(vars), pattern = "{x}''") %>%
    gt::cols_label(
      huc12 = 'HUC 12',
      areaacres = "Area (acres)",

      name = "HUC Name",
      max = 'Max',
      mean = 'Mean',
      median = 'Median',
      min = 'Min',
      stdDev = 'Standard Deviation',
      sum_of_units = 'Unit (acres)',
      proportion = "Proportion (%)"
    )  %>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::cols_align(align = 'left') %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#e5e5e5")
      ),
      locations = gt::cells_body(
        columns = dplyr::everything(), # not needed if coloring all columns
        rows = c(which(1:nrow(data) %% 2==1)))
    ) %>%
    gt::sub_missing()%>%
    gt::tab_header(
      title = "Hydrological Unit Code (HUC) Proportions"
    ) %>%
    gt::tab_footnote(locations = gt::cells_column_labels(dplyr::all_of(vars)),
                     footnote = 'Annual precipitation (in.) from 1991-2020, PRISM.') %>%
    gt::opt_align_table_header(align = 'left')
}


#' Get Treatment Table
#'
#' @param data A previously created \link{get_treatment_propotion} object.
#'
#' @return
#' @export
#' @importFrom gt gt fmt_number sub_small_vals cols_label tab_style cell_text cells_row_groups
#' cols_align cell_fill cells_body sub_missing tab_header opt_align_table_header
#'
#' @examples
treatment_table <- function(data) {

  data %>%
    gt()%>%
    fmt_number(columns = c(proportion, acres), decimals = 0) %>%
    sub_small_vals(small_pattern = "<{x}") %>%
    cols_label(
      acres = 'Area (acres)',
      proportion = "Proportion (%)"
    )  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups()
    ) %>%
    cols_align(align = 'left') %>%
    tab_style(
      style = list(
        cell_fill(color = "#e5e5e5")
      ),
      locations = cells_body(
        columns = dplyr::everything(), # not needed if coloring all columns
        rows = c(which(1:nrow(data) %% 2==1)))
    ) %>%
    sub_missing()%>%
    tab_header(
      title = "Vegetation Treatment Proportion"
    ) %>%
    opt_align_table_header(align = 'left')

}

#' Get Roads Table
#'
#' @param data A previously created \link{get_huc12_information} object.
#' @return
#' @export
#' @importFrom gt gt fmt_number sub_small_vals cols_label tab_style cell_text cells_row_groups
#' cols_align cell_fill cells_body sub_missing tab_header opt_align_table_header
#'
#' @examples
roads_table <- function(data) {

  data <- districth12 %>% dplyr::filter(huc12 %in% data$huc12)

  data <- sf::st_intersection(INFRA, data)

  data <- data %>%
          dplyr::mutate(rd_length = as.numeric(units::set_units(sf::st_length(.), 'mi'))) %>%
          sf::st_drop_geometry() %>%
          dplyr::mutate(Jurisdiction = dplyr::if_else(JURISDICTI == 'FS - FOREST SERVICE','FS - FOREST SERVICE', 'Other', missing = 'Other'),
                        sq_miles = areaacres*0.0015625) %>%
          dplyr::group_by(name, Jurisdiction) %>%
          dplyr::summarise(length_of_roads = sum(rd_length, na.rm = TRUE),
                           density_of_roads = length_of_roads/sq_miles,
                           sq_miles = sq_miles) %>%
          dplyr::slice(n = 1)

  data %>%
    dplyr::ungroup() %>%
    gt()%>%
    fmt_number(columns = c(length_of_roads), decimals = 0) %>%
    sub_small_vals(small_pattern = "<{x}") %>%
    cols_label(
      length_of_roads = 'Road Length (mi)',
      density_of_roads = "Road Density (mi/mi^2)"
    )  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups()
    ) %>%
    cols_align(align = 'left') %>%
    tab_style(
      style = list(
        cell_fill(color = "#e5e5e5")
      ),
      locations = cells_body(
        columns = dplyr::everything(), # not needed if coloring all columns
        rows = c(which(1:nrow(data) %% 2==1)))
    ) %>%
    sub_missing()%>%
    tab_header(
      title = "Road Treatment"
    ) %>%
    opt_align_table_header(align = 'left')

}
