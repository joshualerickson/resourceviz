
#' Read-in HOBO files
#' @description This function takes in a previously created csv file or multiple csv files within
#' the HOBOware software.
#' @param hobo_csv Either a folder path or path to csv.
#' @param pressure_unit A \code{character} of input pressure unit
#' @return a tibble with HOBO attributes: logger_sn, dt, date, time,abs_pres, temp_f
#' @export
#'
read_hobo_csv <- function (hobo_csv = NULL, pressure_unit) {


  if(isTRUE(grep("\\.csv$", hobo_csv)>0)){

    hobo_files <- basename(hobo_csv)

    hobo_csv <- paste0(dirname(hobo_csv), '/')

  } else {

    hobo_files <- list.files(hobo_csv, pattern = '*.csv')

  }



  # Informed by https://github.com/RyanLab/microclimloggers
  # Extract first two lines using an encoding that removes BOM characters
  # at start of file, if present

  hobo_list <- list()

  for(i in hobo_files){

    con <- file(paste0(hobo_csv, i), encoding="UTF-8")
    header <- readLines(con=con, n=2)
    close(con)


    #Split second header line containing column names, logger serial number, and time zone
    header_bits <- unlist(strsplit(header[2], '",\\"'))

    # Extract serial numbers
    SNs <-  stringr::str_extract(header_bits, '(?<=S\\/N:\\s)[0-9]+')
    SN <- unique(SNs[!is.na(SNs)])
    if (length(SN) > 1) stop("multiple serial numbers in file header")

    # Extract timezone
    tz <- stringr::str_extract(header_bits[grep("Date Time", header_bits)], "GMT[+-][0-9][0-9]") %>%
      ifelse(substr(., 5, 5) == 0, sub("0", "", .), .)
    tz <- paste0("Etc/", tz)

    # Read data, parse timestamp, and process relevant data
    hobo <- utils::read.csv(paste0(hobo_csv, i), skip=2, header=FALSE, stringsAsFactors = FALSE, na.strings = "") %>%
      # Parse timestamp
      dplyr::mutate(dt = lubridate::mdy_hms(.[[2]], tz = tz),
             date = format(dt, "%m/%d/%Y"),
             time = format(dt, "%H:%M"),
             abs_pres = .[[3]],
             temp_f = .[[4]],
             logger_sn = SN) %>%
      dplyr::select(logger_sn, dt, date, time,abs_pres, temp_f)

    hobo <- hobo[complete.cases(hobo), ]

    class(hobo) <- c("hobo", "data.frame")
    attr(hobo, "tzone") <- tz

    hobo_list <- append(hobo_list, list(hobo))


  }

  final_hobo <- dplyr::bind_rows(hobo_list)

  final_hobo <- final_hobo %>%
    mutate(abs_pres = dplyr::case_when(
      pressure_unit == 'psi' ~ abs_pres*51.7149*0.0393701,
      pressure_unit == 'mm Hg' ~ abs_pres*0.0393701,
      pressure_unit == 'kPa' ~ abs_pres*7.50062*0.0393701,
      pressure_unit == 'Pa' ~ abs_pres*0.00750062*0.0393701,
      pressure_unit == 'mbar' ~ abs_pres*0.750062*0.0393701,
      pressure_unit == 'mm In' ~ abs_pres,
      TRUE ~ NA_real_
    )) %>%
    dplyr::as_tibble()

}


#' Convert to Inches
#'
#' @param baro_df A data.frame with column abs_pres and dt.
#' @param water_df A data.frame with column abs_pres and dt.
#' @param baro_elev Elevation in feet at the barometer site.
#' @param hobo_elev Elevation in feet at the gage site.
#' @param gh A numeric vector indicating the gage height for a particular day(s).
#' @param gh_date A character vector for the date of `add_gh`, e.g. '2022-05-14 11:00:00'.
#' @param var_select A character vector indicating what variables to use with `mice::mice()` imputation.
#' @param method A character indicating what method to use in `mice::mice()`, 'lasso.select.norm' (default).
#' @note The `gh` and `gh_date` parameters will perform differently if the
#' vector length 1, e.g. one gh_date and gh. It will just subtract or add to
#' the height at that date going forward; however, if greater than 1 then `gh` and `gh_date`
#' will be used to try and 'calibrate'
#' the height of the transducer to a 'observed' gage height `height_imp` using `mice::mice(method = 'lasso.norm')`.
#' @return
#' @export
#'
#' @examples
water_level_conversion <- function(baro_df,
                                   water_df,
                                   baro_elev,
                                   hobo_elev,
                                   gage_height = NULL,
                                   gh_date = NULL,
                                   var_select = c('abs_pres_water',
                                                  'abs_pres_baro',
                                                  'GH',
                                                  'gage_height'),
                                   method = 'lasso.select.norm') {

  conversion_df <- water_df %>%
                  left_join(
                    baro_df,
                    by = 'dt',
                    suffix = c('_water', '_baro')
                  )

  mod <- mgcv::gam(psia~feet, data = psi_comp)

  baro_diff <- abs(predict(mod, data.frame(feet = baro_elev)) - predict(mod, data.frame(feet = hobo_elev)))

  add_to_hobo_abs <- as.numeric(baro_diff*51.7149*0.0393701)

  conversion_df <- conversion_df %>%
    mutate(logic = baro_elev >= hobo_elev,
           GH = dplyr::if_else(logic,
                            abs_pres_water-abs_pres_baro-add_to_hobo_abs,
                            abs_pres_water-abs_pres_baro+add_to_hobo_abs))

  if(!is.null(gage_height) && !is.null(gh_date)){

    if(length(gage_height) > 1) {
  conversion_df <- conversion_df %>% dplyr::left_join(na.omit(data.frame(gage_height, dt = gh_date)))

  conv_imp <- conversion_df %>% dplyr::select(var_select)
  conv_imp <- conv_imp %>% mutate(dplyr::across(is.character, ~as.factor(.x)))
  conversion_df <- conversion_df %>% mutate(
    GH_imp = complete(mice::mice(conv_imp,
                                     method = method,
                                     remove.collinear = FALSE,
                                     grouped=TRUE))$gage_height
  )

    } else {

      date_height <- conversion_df[conversion_df$dt %in% gh_date,]$GH

      if(date_height > gage_height) {

        conversion_df <- conversion_df %>%
                         mutate(GH = GH-(date_height-gage_height))
      } else {


        conversion_df <- conversion_df %>%
                         mutate(GH = GH+(gage_height-date_height))

      }

    }

  }

  conversion_df %>% dplyr::mutate(Date = lubridate::as_date(dt),
                                  dt = lubridate::as_datetime(dt))

}


#' Convert Date and Time
#'
#' @param data A data.frame with dt variable
#' @param unit How to parse the time, e.g. '2 hour', '1 day'.
#' @return
#' @export
#'
#' @examples
convert_to_timeframe <- function(data, unit) {

  data %>% dplyr::mutate(
      Date = lubridate::floor_date(
      dt,
      unit =  unit
    )
  ) %>%
    dplyr::group_by(Stream,Date) %>%
    dplyr::summarise(
                     GH = mean(
      GH_imp,
      na.rm = T
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dt = lubridate::as_datetime(as.character(Date, format="%Y-%m-%d 12:00:00")))
}

