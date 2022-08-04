# Functions that process strings

#' For debugging purposes
# box::use(box/modify_strings)
# box::reload(modify_strings)
# box::unload(modify_strings)


#' Description: Sample function
#' 
#' @param my_parameter_1
#' @param my_parameter_2
#' 
#' @return return the sum of `my_parameter_1` and `my_parameter_2` 
#' 
#' @examples Code showing examples 
#' 
#' Export the function through box::use function
#' @export
add <- function(my_parameter_1, my_parameter_2){
  my_parameter_1 + my_parameter_2
  
}

#' Display data from two tables
#' 
#' @param table1 DATAFRAME, first dataset to compare
#' @param table2 DATAFRAME, second dataset to compare
#' @param range INTEGER, vector of integers indicating which rows to display
#' @param stop LOGICAL, stop execution, default 'FALSE'
#' 
#' @return NULL, does not return objects but print table
#' 
#' @export
display_comparison <- function(table1, table2, range, stop = FALSE){
  cat(paste("Display table 1 : ", deparse(substitute(table1)), "\n", collapse = "") )
  print(table1[range, ])
  cat("--------\n")
  cat("Display table 2: ",deparse(substitute(table2)) , "\n")
  print(table2[range, ])
  
  on.exit(cat("Exiting..."), add = TRUE)
  if(stop){stop()}
}

#' Locate all strings that match a pattern in a table
#' 
#' @param dataset CHARACTER, DATASET, a data of strings
#' @param pattern CHARACTER, a character vector containing regular expressions
#' 
#' @return LOGICAL, DATASET, return a logical dataset of the same length of 
#' `dataset` with TRUE for cells matching the pattern
#' 
#' Export the function through box::use function
#' @export
locate_pattern <- function(dataset, pattern){
  # Call dependencies
  old_opts <- options() # preserve current preferences for after we exist the function
  options(box.path = c("./scripts", 
                       "../scripts",
                       "./scripts/box",
                       "../scripts/box") )
  on.exit(options(old_opts))
  
  cat("it works\n")
  stop()
  box::use(utils[capture.output],
           stringr[str_detect])
  
  
  ## Load module
  box::use(box/modify_strings[display_comparison])
  # need to go to parent directory to be able to call folder/mod
  
  pat_sequence = 1
  
  # Initiate a dataset of same dimension
  location <- matrix(data = FALSE, 
                     nrow =  dim(dataset)[1], 
                     ncol =  dim(dataset)[2])
  location <- as.data.frame(location)
  colnames(location) <- colnames(dataset)
  
  
  
  # Search each pattern across the dataframe
  while(pat_sequence <= length(pattern) ){
    capture.output( # stop printing
      
      temp_location <-  apply(dataset, 2, FUN = str_detect, pattern = "[^\\p{L} ^_]")
      
    )
    
    
    display_comparison(location, temp_location, range = 15, stop = T)
    
    
  }
  
  print("Test")
 
  return(location)
  
  
}

#' Subset and combine all characters into a vector
#' 
#' @param table_ori CHARACTER, DATASET, the original dataset where to 
#' extract vectors from using `bool_location` 
#' @param bool_location LOGICAL, DATASET, a boolean dataset of the same dimension 
#' of the original dataset indicated where to extract each string from
#'  
#' @param y A number.
#' @return a vector `selected_character` of characters
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
extract_character <- function(table_ori, bool_location){
  
  
}

#' Test box
#' 
#' @param x
#' @param y
#' 
#' @export
sum_x_y <- function(x, y){
  x * y
  
}