#' @title Read .csv file
#'
#' This is a function that, by default, reads .csv file.
#' If the file does not exist in the repository, the function
#' will stop. If the file exists, the function will return the
#' Data Frame Tbl of the .csv file.
#'
#' @param filename A character string giving the name of the .csv file
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return return the .csv file in the format of Data Frame Tbl
#'
#' @examples
#' fars_read(filename = "accident_2013.csv")
#' fars_read("accident_2013.csv")
#'
#'Sys.sleep(6)
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(file.path(filename)))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(file.path(filename), progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title Return String the name of the .csv.bz2 file
#'
#' This is a function return the .csv.bz2 file name with the
#' given year.
#'
#' @param year Integer indicating the year, this is contained in the name of .csv.bz2 file
#'
#' @return return the .csv.bz2 file name with the given year
#'
#' @examples
#' make_filename(year = 2015)
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}

#' @title Parse column month and column year from the data set specified by year
#'
#' This is a function returning the month and year column from the specified
#' data set. The data set is specified by year, the year the data set
#' is for. This function can also take a list or a vector of years.
#'
#' @param years An integer year, a list of integer years and a vector of integer
#' years
#'
#' @importFrom dplyr mutate select
#'
#' @return the column year and column month parsed from the data frame correspo-
#' nding to the \code{years} passed into the function
#'
#' @examples
#' fars_read_years(list(2015, 2013))
#' fars_read_years(c(2015, 2013, 2014))
#' fars_read_years(2013)
#'
#' @export
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

#' @title The count of records for the month of the specified year
#'
#' This is a function returning the data frame of monthly record counts for the
#' specified year/years. The first column of the returned data frame is
#' the month, the second column is the record counts of the corresponding month
#' for the passed year/years.
#'
#' @param years An integer year, a list of integer years and a vector of integer
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @return a data frame: the first column of the returned data frame is the
#' month. The rest of the column of the monthly counts for the passed year/years
#'
#' @examples
#' fars_summarize_years(list(2013, 2014))
#' fars_summarize_years(c(2013, 2014))
#' fars_summarize_years(2013)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title The map with the state ploted on the map
#'
#' This is a function of creating map, the map plots the location of the record on the mao for the specif-
#' ied state and year.
#'
#' @param state.num An integer representing the index of the state
#' @param year An interger representing the specified year
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return a map, it plots the location of the record on the map for the specified state
#' and year
#'
#' @examples
#' fars_map_state(state.num = 4, year = 2014)
#' fars_map_state(5, 2013)
#'
#' @export
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
}# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}
