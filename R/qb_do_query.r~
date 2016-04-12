#' GET Request for tablular data within quickbase app. 
#' 
#' This function sends a request to quickbase to return a response with the app data. The response in returned as html which is then parsed and returned
#' as a html page which then can be parsed using qb_parse
#' @param URL A url for Quickbase company domain
#' @param qb_ticket_id A valid quickbase authentication ticket
#' @param app_token A valid quickbase app token
#' @keywords db_do_query
#' @export

qb_do_query <- function(URL, db_id, qb_ticket_id, app_token) {
  db <- "/db/"
  db_id <- paste0(db_id, "?")
  api_action <- "a=API_DoQuery"
  qb_ticket_id <- paste0("&ticket=", qb_ticket_id)
  app_token <- paste0("&apptoken=", app_token)
  pg1 <- GET(
  paste0(URL,
          db,
          db_id,
          api_action,
          qb_ticket_id,
          api_token)
  )
  pg1
}

#' Parses a valid quickbase html do_query response page into a dataframe
#' 
#' This function takes a page returned by qb_do_query and parses the contents to an R dataframe
#' @param html_qb_db A valid quickbase response page return from qb_do_query
#' @keywords qb_parse
#' @export

qb_parse <- function(html_qb_db) {
  content1 <- 
    content(html_qb_db, "text")
  qb_parsed_ls <- xmlToList(content1)
  
  my_records <-
    names(qb_parsed_ls) == "record"
  qb_parsed_ls  <- qb_parsed_ls[my_records]
  
  qb_df <- 
    as.data.frame(do.call(rbind, 
                          lapply(qb_parsed_ls , rbind)))
  
  qb_df[] <- 
    apply(qb_df, FUN = as.character, MARGIN = 2)
  qb_df
}