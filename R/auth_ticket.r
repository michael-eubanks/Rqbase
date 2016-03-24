#' A Function for creating URL handle object to retrieve a quickbase ticket id
#' 
#' This Function passes a character string to the httr::handle function to return a list
#' @param URL A url for Quickbase company domain
#' @param username Quickbase username's email address
#' @param password Username's password
#' @param hours Length of time authentication ticket remains valid. Defaults to 24
#' @keywords auth_ticket_handle
#' @export

auth_ticket_handle <- function(URL, username, password, hours = 24)  {
  db <- "/db/"
  api_action <- "a=API_Authenticate"
  main <- "main?"
  username <- paste0("&username=", username)
  password <- paste0("&password=", password)
  hours = paste0("&hours=", hours)
  handle(paste0(URL, db, main, api_action, username, password))
}


#' A Function for getting Quickbase Authentication Tickets
#' 
#' This Function uses GET to return a response from quickbase with the Quickbase Ticket ID located within the contents of the html response
#' @param ticket_handle A handle object returned by the auth_ticket_handle() function
#' @keywords auth_ticket
#' @export

auth_ticket <- function(ticket_handle) {
  pg <-
    GET(handle = ticket_handle)
  pg_content <-
    content(pg, as = "text")
  qb_ticket_id <- 
    xmlToList(pg_content)$ticket
  return(qb_ticket_id)
}