#################################
###### 'indexanalysis' package v1.0
######
#################################

## Global variables
utils::globalVariables(names = c(
                                "abbreviation.period",
                                 "Component",
                                 "Date",
                                 "Divisor",
                                 "Key",
                                 "Market.Cap",
                                "Period",
                                 "Price",
                                "price.col",
                                 "Price.Type",
                                 "prod_Share.Price",
                                 "Shares",
                                "share.col",
                                 "Symbol",
                                 "Weights",
                                 "."
                                 ),
                       package = "indexanalysis")

#' @title xts_finance_to_tibble
#'
#' @description  Download financial xts data and coerce to tibble
#'
#' @param symbol A character vector containing the Symbols of a component,
#' e.g. 'AAPL' for Apple
#' @param from_date Start of the time period
#' @param to_date End of the time period
#'
#' @return A tibble containing the time series associated to these symbols
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr rename_with
#' @importFrom tibble as_tibble add_column
#' @importFrom quantmod getSymbols
#' @export
xts_finance_to_tibble <- function(symbol, from_date, to_date){
  prices_xts <- quantmod::getSymbols(Symbols = symbol,
                                     from = from_date, to = to_date,
                                     auto.assign = F)
  prices_tib <- tibble::as_tibble(prices_xts)

  prices_tib_ts <- prices_tib %>%
    tibble::add_column(Date = zoo::index(prices_xts), .before = 1 ) %>%
    tibble::add_column(Key = symbol, .after = "Date")%>%
    dplyr::rename_with(.fn = stringr::str_remove, pattern = glue("{symbol}.") )

}




#' @title select_partial_colnames
#' @description Select columns of a table using partial names,
#' e.g. select "AAPL.Close" from "Close"
#'
#' @param data a table
#' @param column_to_keep a vector of partial column names
#'
#' @return a vector containing the index of columns
#' to select based on partial names
#' @export
select_partial_colnames <- function(data, column_to_keep){

  column_names <- colnames(data)
  print(column_names)

  ind_bool <- sapply(column_names,
                     function(x) {any(sapply(column_to_keep,
                                             stringr::str_detect,
                                             string = x)) })

  index_col <- which(ind_bool)

}


#' @title remove_component_column
#' @description Rename a column by removing the component pattern
#'
#' @param table the table to rename
#' @param remove a vector of length containing the pattern
#'
#' @return the table with new column names
#'
#' @importFrom magrittr %>%
#' @export
remove_component_column <- function(table, remove = c()){

  if(length(remove) > 1) stop("Can only remove one pattern at a time")

  renamed_table <- table %>%
    dplyr::rename_with(stringr::str_remove, pattern = remove)

}




#' @title add_component_column
#' @description  Rename a column by adding a prefix
#' Antonym of "remove_component_column"
#'
#' @param table a table to rename
#' @param add a prefix containing the component e.g. 'AAPL' for Apple
#' @param exclude a vector of column names to exclude
#'
#' @importFrom magrittr %>%
#'
#' @return table with modified columns
#' @importFrom magrittr %>%
#' @export
add_component_column <- function(table, add = c(), exclude=c()){

  match_col <- function(x) {!(colnames(x) %in% exclude)}
  rename_col <- match_col(table)
  table %>% dplyr::rename_with(.fn = ~paste0(add, ".", .),
                        .cols = which(rename_col) )



}




#' @title checkpoint
#' @description Create a checkpoint with the data and
#' can load the data from the previous checkpoint
#'
#' Use: Saving a copy before testing code
#' @param data A variable to save
#' @param load A boolean indicating whether to save, or load the data
#'
#' @importFrom magrittr %>%
#' @return Assign to Global environment
#'
#' @export
checkpoint <- function(data, load = FALSE){
  name_data <- deparse(substitute(data))

  if(!load){eval(parse(text =
                         glue::glue("save.{name_data} <<- {name_data}")
  ))
  }else{
    eval(parse(text =
                 glue::glue("{name_data} <<- save.{name_data}")
    ))

  }

}




#' @title tib_to_tsib
#' @description Coerce tibble to tsibble for time series
#'
#' @param tibble_ts a tibble containing a time series
#' @param key_ts a character vector containing the identifying keys for the different time series
#' @param index_ts a character vector containing the time indexes
#' @param keep_price a vector character of price names to select
#' @param wide Is the table in wide format? Default: TRUE. If FALSE, will pivot the table
#'
#' @importFrom magrittr %>%
#'
#' @return a tsibble, a.k.a. "time series tibble"
#' @export
tib_to_tsib <- function(tibble_ts, key_ts = c(), index_ts = c(),
                        keep_price = c("All", "Open", "High", "Low", "Volume", "Adjusted", "Close"),
                        wide = TRUE){

  if(length(key_ts) == 0) stop("Argument 'key_ts' missing with no values")
  if(length(index_ts) == 0) stop("Argument 'index_ts' missing with no values")
  if(length(keep_price) == 0) stop("Argument 'keep_col' missing with no values")

  # cat("Keys are in: ", key_ts, "\n")

  keep_price <- match.arg(keep_price)

  # Prepare a vector of names for column selection, keeping always the Date
  keep_price_all = c("Open", "High", "Low", "Volume", "Adjusted", "Close")
  keep_price <- c(index_ts,
                  if(!wide){key_ts},
                  if(keep_price == "All"){keep_price_all}else{keep_price} )


  # Convert and select columns
  ## If wide
  if(wide){
    price_tsib <- tibble_ts %>%
      dplyr::select(!Key) %>%
      tsibble::as_tsibble(index = index_ts )  %>%
      dplyr::select(tidyselect::contains(keep_price))
  }else{
    # if long: not(wide)
    price_tsib <- tibble_ts %>%
      tsibble::as_tsibble(key = tidyselect::all_of(key_ts),
                 index = index_ts )  %>%
      dplyr::select(tidyselect::contains(keep_price))

  }
}




#' @title merge_data_parameters
#' @description Calculate index price from individual component values
#'
#' @param time_series A table containing the time series of prices for each component
#' @param to_merge_with Table containing index parameters (e.g. number of shares)
#' @param by_col Which column to merge with
#' @param is_long A boolean indicating the table is in long or wide format.
#' If wide, will convert the table to long format
#' @importFrom magrittr %>%
#'
#' @return A new table containing index values per unit of time (e.g. 'day')
#' @export
merge_data_parameters <- function(time_series,
                                  to_merge_with, by_col = NULL,
                                  is_long = TRUE){
  assertthat::assert_that(assertthat::not_empty(by_col), msg = "Provide a column to merge on")


  ## Add component parameters: e.g. shares and divisor
  time_series <- time_series %>%
    dplyr::inner_join(y=to_merge_with, by = by_col) %>%
    dplyr::select(-any_of(c("Component"))) %>% ## add any_of
    dplyr::arrange(Date, Symbol)


}





#' @title Deprecated calc_index
#' @description Calculate index price from individual component values
#'
#' @param time_series A table containing the time series of prices for each component
#' and their index parameters
#' @param price.type A string indicating which price to use
#' @param rename A boolean indicating if column "Index" should be renamed to "Index.{Price.Type}"
#' e.g., "Index.Close"
#'
#' @importFrom assertthat assert_that see_if
#' @importFrom magrittr %>%
#'
#' @return A new table containing index values per unit of time (e.g. 'day')
#'
#' @export
calc_index_ <- function(time_series,
                       price.type = c("Close", "Open", "High", "Low", "Volume", "Adjusted"),
                       rename = c(FALSE, TRUE)){
  # Check arguments
  ## Set value to default and check that value is a possible value
  price.type <- match.arg(price.type)



  is_price_present <- any(stringr::str_detect(time_series$Price.Type, pattern = price.type))
  assertthat::assert_that(is_price_present == TRUE, msg = "Price type not present in the dataset")
  assertthat::see_if(assertthat::is.flag(rename), msg = "'rename' should be a boolean 'TRUE' or 'FALSE'")
  ####


  # Calculate index by day
  index <- time_series %>%
    dplyr::filter(Price.Type ==  price.type)%>%
    dplyr::mutate(prod_Share.Price = Shares * Price)%>%
    dplyr::group_by(Date)%>%
    dplyr::summarise(Index = sum(prod_Share.Price)/Divisor[1])%>%
    { if(rename){  dplyr::rename_with(., ~ str_c(., price.type, sep="."), .cols = tidyselect::contains("Index")) }else . }


  ## Use mean because the divisor is always the same for all components of an index
  ## and only change when a component is added or remove
  ## to compensate differences in prices




}

#' @title calc_index
#' @description Calculate index price from individual component values
#'
#' @param time_series A table containing the time series of prices for each component
#' and their index parameters
#' @param price.type A string indicating which price to use
#' @param rename A boolean indicating if column "Index" should be renamed to "Index.{Price.Type}"
#' e.g., "Index.Close"
#' @param share.col Name of column (unquoted) containing the shares (default: Shares)
#' @param share.col Name of column (unquoted) containing the prices (default: Price)

#'
#' @importFrom assertthat assert_that see_if
#' @importFrom magrittr %>%
#'
#' @return A new table containing index values per unit of time (e.g. 'day')
#'
#' @export
calc_index <- function(time_series, share.col = Shares, price.col = Price,
                       price.type = c("Close", "Open", "High", "Low", "Volume", "Adjusted"),
                       rename = c(FALSE, TRUE)){
  # Check arguments
  ## Set value to default and check that value is a possible value
  price.type <- match.arg(price.type)



  is_price_present <- any(stringr::str_detect(time_series$Price.Type, pattern = price.type))
  assertthat::assert_that(is_price_present == TRUE, msg = "Price type not present in the dataset")
  assertthat::see_if(assertthat::is.flag(rename), msg = "'rename' should be a boolean 'TRUE' or 'FALSE'")
  ####


  # Calculate index by day
  index <- time_series %>%
    dplyr::filter(Price.Type ==  price.type)%>%
    dplyr::mutate(prod_Share.Price = {{share.col}} * {{price.col}})%>%
    dplyr::group_by(Date)%>%
    dplyr::summarise(Index = sum(prod_Share.Price)/Divisor[1])%>%
    { if(rename){  dplyr::rename_with(., ~ str_c(., price.type, sep="."), .cols = tidyselect::contains("Index")) }else . }


  ## Use mean because the divisor is always the same for all components of an index
  ## and only change when a component is added or remove
  ## to compensate differences in prices


}



#' @title calc_several_index
#' @description Compute index values for one or more price types (e.g., Opening price, High prices...)
#' Expand function calc_index() to several price types
#' @param timeseries Time series of prices in long format
#' @param price.type For which price to compute the index
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr full_join filter
#'
#' @return A timeseries of index prices
#' @export
calc_several_index <- function(timeseries,
                               price.type = c("Close", "Open", "High", "Low", "Volume","Adjusted")){
  run = 1
  for(price in price.type){
    ts <- timeseries %>%
      dplyr::filter(`Price.Type` == price)

    index_ts <- indexanalysis::calc_index(time_series = ts,
                           price.type = price, rename = TRUE)

    if(run < 2){index_ts_all <- index_ts}else{
      index_ts_all <- dplyr::full_join(index_ts_all, index_ts, by = "Date", keep = FALSE)

    }
    run = run + 1
  }

  return(index_ts_all)
}



#' @title calc_deFacto_weights
#' @description Calculate deFactoWeights from individual component values and index parameters
#'
#' @param time_series A table containing the time series of prices for each component
#' and their index parameters
#' @param price.type A string indicating which price to use
#' @param rename A boolean indicating if column "Weights" should be renamed to "Weights.{Price.Type}"
#' e.g., "Weights.Close"
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_c
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate group_by rename_with
#' @importFrom tidyselect contains
#'
#' @return A new table containing de-facto weights  per unit of time (e.g. 'day')
#' @export
calc_deFacto_weights <- function(time_series,
                                 price.type = c("Close", "Open", "High", "Low", "Volume", "Adjusted"),
                                 rename = FALSE ){


  ## Set value to default and check that value is a possible value
  price.type <- match.arg(price.type)


  is_price_present <- any(stringr::str_detect(time_series$Price.Type, pattern = price.type))
  assertthat::assert_that(is_price_present == TRUE, msg = "Price type not present in the dataset")
  ####


  # Calculate Relative market value
  index <- time_series %>%
    dplyr::filter(Price.Type == price.type)%>%
    dplyr::mutate(., Market.Cap = Shares * Price)%>%
    dplyr::group_by(., Date)%>%
    dplyr::mutate(Weights = Market.Cap/sum(Market.Cap))%>%
    { if(rename){  dplyr::rename_with(., ~ stringr::str_c(., price.type, sep="."), .cols = tidyselect::contains("Weights")) }else . }

}

#' @title compute_across
#' @description Apply a function across all values provided
#' @param ... objects to be concatenated
#' @param .fun a function to apply
#'
#' @return A single value
#' @export
compute_across <- function(..., .fun){
  all_data <- c(...)

  .fun(all_data)
}




#' @title extract_price
#' @description Extract price for first, last, and middle day of the period
#'
#' @param date A period of time, matching price
#' @param price A time series of price, matching date
#' @param .fun A function to apply for extracting a date
#' 'min' returns the first day, 'max' returns the last day
#' @param message A boolean specifying whether to display a summary or not
#' @param return.date A boolean specifying whether to extract the date instead
#'
#' @importFrom glue glue
#'
#' @return A single value: a price corresponding to a specific date (or a date if 'date' is TRUE)
#' @export
extract_price <- function(date, price, .fun, message = FALSE, return.date = FALSE){
  extracted.date = .fun(as.Date(date))

  fun_name <-  as.character(substitute(.fun))

  if(fun_name == "min"){
    day_position <- "first"
  }else if(fun_name == "max") {
    day_position <- "last"

  } else if(fun_name == "median"){
    day_position <- "middle"

  } else {
    day_position <- "a"

  }

  if(return.date != TRUE){
    index <- which(as.Date(date) == extracted.date)
    my_price <- price[index]
    if(message){
      cat(glue::glue("Price '{my_price}' on day '{extracted.date}' ({day_position} day of period)\n"))
      cat("\n")
    }
    return(my_price)}else{
      if(message){
        cat(glue::glue("Extracted date: '{extracted.date}' ({day_position} day of period)\n"))
        cat("\n")
      }
      return(extracted.date)
    }


}




#' @title pivot_prices_wider
#' @description Take a long table of financial data and pivot the table by reuniting Component and Price types if needed
#' @family pivot_prices_longer
#'
#' @param data A long table
#' @param nameS A vector of names for the column or columns containing column identifiers
#' @param valueS A vector of names containing the variable(s)
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider unite
#' @importFrom tidyselect all_of
#' @importFrom rlang enquo eval_tidy
#' @importFrom glue glue
#'
#' @return A tibble in wider format
#' @export
pivot_prices_wider <- function(data, nameS, valueS){

  if(length(nameS) < 2 ){
    data <- data %>%
      tibble::as_tibble(.)%>%
      tidyr::pivot_wider(names_from = all_of(nameS),
                         values_from = all_of(valueS),
                         # names_sep = "."
                         names_glue = glue::glue("{[nameS]}.{.value}",
                                                 .open = "[", .close = "]")
      )

  }else{
    # Reunite component and price.type
    data <- data %>%
      tibble::as_tibble(.)%>%
      tidyr::unite(col = "Component.Price", tidyselect::all_of(nameS), sep = ".")%>%
      tidyr::pivot_wider(names_from = tidyselect::all_of("Component.Price"),
                         values_from =  tidyselect::all_of({{valueS}}) )

  }

  # %>%
  #     dplyr::arrange(where(~ lubridate::is.Date(.x) ))

}


#' @title pivot_prices_longer
#' @description Take a wide table of financial data and pivot the table
#' @family pivot_prices_wider
#'
#' @param data A wide table
#' @param symbols Symbols of financial components
#' @param separate A boolean, if TRUE, separate the component from the price type (e.g. 'AAPL' and 'Close' price)
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer separate
#' @importFrom tidyselect contains any_of
#'
#' @return A tibble in wider format
#' @export
pivot_prices_longer <- function(data, symbols, separate = FALSE){
  data <- data %>%
    tibble::as_tibble(.)%>%
    tidyr::pivot_longer(cols = tidyselect::contains(symbols),
                        names_to = "Component.Symbol",
                        values_to = "Price" )


  if(separate){
    data <- data %>% tidyr::separate(col = "Component.Symbol",
                                     into = c("Symbol", "Price.Type"),
                                     sep = "\\.(?=[A-Z]{1}[a-z])")


  }

  return(data)
}



#' @title periodify
#' @description Extract time components from a time series and add it to the table containing the time series.
#'
#'
#' @details
#' This helper function can be used in combination with a grouping and an aggregation function to calculate weekly, monthly, or yearly values
#'
#' @param timeseries a timeseries table containing a variable for time
#' @param date_col name of the column containing dates
#' @param period a vector of characters indicating which time components to extract (pick one or more of the
#' following options: day, week, month, year
#' @param create_group If TRUE, combines extracted periods for future grouping by period
#'
#' @importFrom lubridate isoweek year month day
#' @importFrom purrr map2_dfc
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate bind_cols relocate
#' @importFrom purrr map2_dfc
#' @importFrom glue glue
#' @importFrom tidyr unite
#' @importFrom tidyselect any_of
#'
#' @return the timeseries table with new columns for the extracted time components
#' @export
periodify <- function(timeseries, date_col, period = c("day", "week", "month", "year"), create_group = FALSE){

  assertthat::assert_that(all(period %in% c("day", "week", "month", "year") ), msg = "Possible periods are: day, week, month, or year")


  # initialize a list to store functions
  period_function <- list()

  period_grouping_col <- character(1)

  # store function for extracting year component
  if("year" %in% period){
    period_function$year <- lubridate::year

    period_grouping_col <- glue::glue("{period_grouping_col}y")

  }

  # store function for extracting month component
  if("month" %in% period){
    period_function$month <- lubridate::month
    period_grouping_col <- glue::glue("{period_grouping_col}m")


  }

  # store function for extracting week component
  if("week" %in% period){
    period_function$week <- lubridate::isoweek
    period_grouping_col <- glue::glue("{period_grouping_col}w")


  }

  # store function for extracting day component
  if("day" %in% period){
    period_function$day <- lubridate::day
    period_grouping_col <- glue::glue("{period_grouping_col}d")

  }



  # col_sym = rlang::sym(period_grouping_col)



  name_functions <- names(period_function)

  periodized <- purrr::map2_dfc(.x = period_function, .y = name_functions,  ~ timeseries %>%
                                  tibble::as_tibble(.)%>%
                                  dplyr::transmute("{.y}" := .x(Date)) ) %>%
    dplyr::bind_cols(timeseries, .) %>%
    dplyr::relocate(any_of(c("year", "month", "week", "day")), .after = {{date_col}} )

  if(all(length(period) > 1 & create_group)){
    periodized <- tidyr::unite(periodized, col = "Period", # !! col_sym # to name the column by the period, e.g., "ymw"
                          tidyselect::any_of(period),
                          sep = "-",
                          remove = FALSE) %>%
      dplyr::mutate(abbreviation.period = period_grouping_col) %>%
      dplyr::relocate(abbreviation.period, .after = Period)


    }


}
