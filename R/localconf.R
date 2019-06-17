#' Encrypt configuration details in project root
#'
#' @param ...
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
encrypt_conf <- function(..., file_name = ".conf.rds") {

  conf_list <- list(...)

  conf_crypt <- safer::encrypt_object(conf_list, key = rstudioapi::askForPassword())

  saveRDS(conf_crypt, file_name)

}

#' Register configuration details as environment variables
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
register_conf <- function(file_name = ".conf.rds", verbose = TRUE) {

  if(!file.exists(file_name)) stop("no .conf file detected, please use encrypt_conf first")

  conf_list <- readRDS(file_name)

  conf_list <- safer::decrypt_object(conf_list, key = rstudioapi::askForPassword())

  do.call(Sys.setenv, conf_list)

  if(verbose) {

    msg <- paste0(c("Registered the following items:", names(conf_list)), collapse = "\n\t-")
    cat(msg)

  }


}

#' Retrieve configuration setting from the environment
#'
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
retrieve_conf <- function(var_name) {

  res <- Sys.getenv(var_name)

  res

}
