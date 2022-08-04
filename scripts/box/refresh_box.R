#' Box module debugging helper function

#' Reload and refresh
#' 
#' @param module_namespace CHARACTER, string specify where to
#'  look for the module in the form of "folder/module" where module is 
#'  an rscript "module.R"
#' 
#' @return NONE, perform actions
#' 
#' @export
refresh <- function(module_path){
  
  # Indicate possible search paths
  old_opts <- options() # preserve current preferences for after we exist the function
  options(box.path = c("./scripts", 
                       "../scripts",
                       "./scripts/box",
                       "../scripts/box") )
  on.exit(options(old_opts))
  
  box::use(box/refresh_box[detach_module])
  
  
  cat("Detach exported functions\n")
  cat("-----------\n")
  detach_module(module_path)
  
  eval(parse(text = paste(
    "box::use(my_module =", module_path, ")"
  )))
  
  cat("Reloading '", module_path ,"' " , "......", sep = "")
  box::reload(my_module)
  cat("Reloaded!\n")
  box::unload(my_module)
  cat("Unloaded.\n")
  

}

#' Detach exported functions
#' 
#' @param pattern CHARACTER, string contain the path to a module function
#' Default: 'mod:' will remove all exported functions
#' 
#' @export
detach_module <- function(module_path = "mod:"){
  
  path <- grep(search(), pattern= module_path, value = T )
  

  while(length(path) > 0){
    cat("Detaching: ", path[1] , "\n")
    detach(path[1], character.only = T)
    
    path <- path[-1]
  }
  
  cat("All functions have been detached.\n\n")
  
  
}
