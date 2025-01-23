#' Title Read a CSV file and return a tibble.
#'
#' This function reads a CSV file and returns its contents as a tibble.
#' It checks if the file exists before attempting to read it.
#'
#' @param filename A csv file that may or may not exist to import.
#'
#' @returns A tibble containing the data from the CSV file.
#' If the csv does not exist, an error message will be shown.
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @source extdate/accident_year.csv.bz2
#'
#' @export
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Title Generate a filename for accident data
#'
#'This function generates a filename for accident data based
#'on the given year. The filename follows the format
#'"accident_<year>.csv.bz2"
#'
#' @param year An integer representing the year for which
#' the filename is to be generated.
#'
#' @returns A character string representing the filename.
#'
#' @source extdate/accident_year.csv.bz2
#'
#' @export
#'
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Title Read accident data for multiple years
#'
#' This function reads accident data files
#' for multiple years and returns a list of tibbles.
#' Each tibble contains the month and year columns.
#' If a file for a particular year does not exist,
#' a warning is issued and `NULL` is returned for that year.
#'
#' @param years A vector of integers representing
#' the years for which the data files are to be read.
#'
#' @returns A list of tibbles, each containing
#' the accident data for a specific year.
#' If a file for a year does not exist,
#' `NULL` is returned for that year.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
#'
#' @examples
#'      fars_read_years(1999)
#'      fars_read_years(as.list(1999, 2000, 2001))
#'      fars_read_years(1999:2016)
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Title Summarize accident data by year and month.
#'
#' This function reads accident data for multiple years, combines the data,
#' and summarizes the number of accidents by year and month.
#'
#'#' @param years A vector of integers representing the years
#'for which the data files are to be read and summarized.
#'
#' @returns A tibble summarizing the number of accidents by year and month
#' @importFrom  dplyr bind_rows
#' @importFrom  dplyr group_by
#' @importFrom  dplyr summarize
#' @importFrom  tidyr spread
#'
#' @export
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2015))
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Title Plot accidents on a state map for a given year.
#'
#' This function plots the locations of accidents on
#' a state map for a specified year.
#' It reads the accident data, filters it by the given state number,
#' and plots the longitude and latitude of the accidents.
#'
#' @param state.num An integer representing the state number.
#' @param year An integer representing the
#' year for which the accident data is to be plotted.
#'
#' @returns A plot of the accident locations on a state map.
#' If there are no accidents to plot,
#' a message is displayed and `NULL` is returned invisibly.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
#' @examples
#'   # Example usage:
#'   fars_map_state(1, 2013)
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
