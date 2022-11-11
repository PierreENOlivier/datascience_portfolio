# Functions that process strings

#' For debugging purposes
# box::use(box/refresh_box[refresh])
# refresh("box/modify_strings")
# box::use(box/modify_strings)


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
  TOO_BIG_RANGE <- length(range) > 15
  if(TOO_BIG_RANGE){range <- range[1:15]}
  cat(paste("Display table 1 : ", deparse(substitute(table1)), "\n", collapse = "") )
  print(table1[range, ])
  if(TOO_BIG_RANGE){cat("Range cut at 15 rows for display\n")}
  cat("--------\n")
  cat("Display table 2: ",deparse(substitute(table2)) , "\n")
  print(table2[range, ])
  
  on.exit(cat("Exiting...\n"), add = TRUE)
  if(stop){stop()}
}

#' Locate all strings that match a pattern in a table
#' Return
#' 
#' @param dataset CHARACTER, DATASET, a data of strings
#' @param pattern CHARACTER, a character vector containing regular expressions
#' 
#' @return LIST, the list contains: 
#' - a logical DATASET of the same length of `dataset` with TRUE for cells matching the pattern
#' - a logical VECTOR containing all the match
#' 
#' Export the function through box::use function
#' @export
locate_pattern <- function(dataset, pattern_to_search){
  # Call dependencies
  old_opts <- options() # preserve current preferences for after we exist the function
  options(box.path = c("./scripts", 
                       "../scripts",
                       "./scripts/box",
                       "../scripts/box") )
  on.exit(options(old_opts))
  
  box::use(utils,
           stringr[str_detect])
  
  # Check input for errors
  if(!(class(dataset) %in% c("data.frame", "matrix", "array")) ){
    stop("Parameter 'dataset' is not a data.frame, matrix, or array.")
  }
  
  if(!(class(pattern_to_search) %in% c("character")) ){
    stop("Parameter 'pattern_to_search' is not a character vector.")
  }
  
  
  # ## Load module
  # box::use(box/modify_strings[display_comparison])
  # # need to go to parent directory to be able to call folder/mod or set it up in options(box.path =c())
  
  # Initiliaze vector to return
  vector_pattern <- c()
  #Initialize flag
  pat_sequence = 1

  
  # Search each pattern across the dataframe
  while(pat_sequence <= length(pattern_to_search) ){
    
    # store location of match
    utils$capture.output( # stop printing
      
      temp_location <-  apply(dataset, 2, FUN = str_detect, pattern = pattern_to_search[pat_sequence])
      
    )
    
    # locate the rows with a pattern
    has_pattern <- apply(temp_location, 1, sum) > 0
    has_pattern_ind <- which(has_pattern)
    
    cat(sum(has_pattern), "rows contain the pattern:", pattern_to_search, "\n")
    
    # Extract pattern
    ## Load module
    box::use(box/modify_strings[extract_character])
    vector_pattern_temp <- extract_character(dataset, temp_location)
    
    vector_pattern <- c(vector_pattern, vector_pattern_temp)

    # move to next pattern
    pat_sequence = pat_sequence + 1  
  }
  

  vector_pattern <- vector_pattern[!is.na(vector_pattern)]
  vector_pattern <- unique(vector_pattern)
  return(vector_pattern)
  
  
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
  selected_character <- c()
  
  ind <- ncol(table_ori)
  for(i in 1:ind){
    cat("Column: ", i, "\n", sep="")
    temp_pattern <- table_ori[bool_location[ , i], i]
    
    
    temp_pattern <- unique(temp_pattern)
    selected_character <- c(selected_character, temp_pattern)
    selected_character <- selected_character[!is.na(selected_character)]
    # print(selected_character)
    
  }
  
  return(selected_character)
  
}


#' Convert vowels with accents to vowels without
#' 
#' @param x a character vector
#' @param old a string containing characters to replace
#' @param new a string containing the characters to convert to
#' N.B. `old` and `new` need to match in length and order
#' 
#' @return a character vector that has been latinized
#' 
#' @export
latinize <- function(x, old = NULL, new = NULL){
  box::use(stringr)
  
  if(is.null(old) | is.null(new) ){
   
    old = c("äåâàáëêèéïîíìöôóòuüûùúyÿýñÄÅÂÀÁËÊÈÉÏÎÍÌÖÔÓÒUÜÛÙÚYŸÝÑ")
      
    new = c("aaaaaeeeeiiiioooouuuuuyyynAAAAAEEEEIIIIOOOOUUUUUYYYN")
    
  }
  
  if(!(stringr$str_count(old) == stringr$str_count(new)) ){stop( paste("Pattern vector 'old' and replacement vector 'new' 
                                                              should contain the same number of characters\n") ) }
  
  converted_vector <- chartr(x = x, old = old, new = new)
  
  return(converted_vector)
  
}

#' Replace sets of patterns with their counterpart
#' 
#' @param x a character vector containing strings
#' @param vector_pattern a vector containing patterns
#' @param vector_replacement a vector containing replacements
#' N.B. both vectors need to be of the same length
#' 
#' @return VECTOR, a modified version of x
#' 
#' @export
replace_pattern <- function(x, vector_pattern, vector_replacement){
  # Identify class dataset
  data_class <- class(x)
  
  # Check that replacements match in length
  box::use(stringr)
  if(!(length(vector_pattern) == length(vector_replacement)) ){stop( paste("Pattern vector and replacement vector 
                                                              should be of same length\n") ) }
  flag <- length(vector_pattern)
  ind <- 1
  while(ind <= flag){ # Cycle through patterns
    cat("Replacing", paste("'", vector_pattern[ind], "'", sep=""),
        "with", paste("'", vector_replacement[ind], "'", sep=""), sep =" ", "\n")
    
    # Replace patterns across columns
    x <- apply(x, 2, stringr$str_replace, pattern = vector_pattern[ind], replacement = vector_replacement[ind])  
    
    ind = ind + 1
  }
  
  if( !(data_class %in% c("matrix", "array") ) ){
    x <- as.data.frame(x)
    
  }else{return(x)}
  
  
}

#' Extract two or more patterns from a string and merge them together
#' @param x CHARACTER, VECTOR vector from where to extract the patterns from
#' 
#' @param patterns CHARACTER VECTOR: vector containing the ordered list of all patterns to use for extraction
#' 
#' @param sep CHARACTER, a character string to separate the terms
#' 
#' @param ending CHARACTER, a character string containing the last pattern to search
#' 
#' @return CHARACTER, VECTOR: vector with concatenated extracts
#' 
#' @export
extract_and_merge <- function(x, patterns, sep = "", ending = ""){
  
  box::use(stringr[str_extract], 
           tidyverse, 
           magrittr[`%>%`])
  # Initialize vector
  merged <- c()
  

  flag <- length(patterns)
  ind <- 1
  while(ind <= flag){
    # extract pattern
    extracted_pattern <- x %>% str_extract(., patterns[ind])
    
    # merge patterns
    ## if round 1, sep = ""
    if(ind < 2 & length(patterns) > 1){
      # if we are on the first round but there is only one pattern to extract
      merged <- paste(merged, 
                      ifelse(is.na(extracted_pattern), "", extracted_pattern),
                      sep="")}else if(
        ind < length(patterns)){
      # if we are on another round but the last
      merged <- paste(merged, ifelse(is.na(extracted_pattern), "", extracted_pattern)
                        , sep=sep)
      }else{
        # if we are on the last round
      merged <- paste(merged, ifelse(is.na(extracted_pattern), "", extracted_pattern)
                        , ending,  sep="")
        
      }
  ind = ind + 1  
    
  }

 
return(merged)
  
  ## need a if condition to extract _SP$ and _SPP$ completely
  
}

#' Test extract
#' 
#' @param x vector of strings
#' @param pattern pattern to extract
#' @param return if TRUE, then print a sample instead
#' 
#' @return return a character vector of extracts
#' 
#' @export
test_extract <- function(x, pattern, return = FALSE){
  # Load dependencies
  box::use(stringr[str_extract], 
           tidyverse, 
           magrittr[`%>%`],
           utils[head])
  
  extracts <- x %>% str_extract(., pattern)
  
  rand_sample <- sample(extracts, 10)
  
  if(return){return(extracts)}else(
    print(rand_sample)
    
  )
  
  
}

#' Extract pattern using normal regular expression, or complex regular expression
#' using PERL language
#' 
#' @param x vector of strings
#' @param pattern pattern to extract
#' @param PERL if TRUE, switch to PERL syntax
#' 
#' 
#' @return return a character vector of extracts
#' 
#' @export
extract_switch <- function(x, pattern, PERL = FALSE){
  box::use(stringr[str_extract, perl], 
           tidyverse, 
           magrittr[`%>%`])
  
  
  if(!PERL){
    extracts <- x %>% str_extract(., pattern)
    
    
  }else{
    temp <- regmatches(pairwise_list$PREDATOR, regexpr(pattern = "(?(?=_G_)_G_[A-Za-z]{1,3}|_[A-Za-z]{1,3})",
                                                           text = pairwise_list$PREDATOR, perl = TRUE),
                           invert = NA) 
      
    temp <- sapply(temp, '[', 2)
    pw_temp <- cbind(pairwise_list, 'EXTRACT' = temp)
    
    temp <- regmatches(pairwise_list$PREY, regexpr(pattern = "(?(?=_G_)_G_[A-Za-z]{1,3}|_[A-Za-z]{1,3})",
                                                       text = pairwise_list$PREY, perl = TRUE),
                       invert = NA) 
    temp2 <- sapply(temp, '[', 2)
    pw_temp <- cbind(pw_temp, 'EXTRACT2' = temp2)
    
    
    
  }
  
temp <- 
    
  return(extracts)
  
}
