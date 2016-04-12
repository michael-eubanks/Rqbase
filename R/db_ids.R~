#' GET Quickbase db/app/table names and unique IDs
#' 
#' This function sends a request to quickbase to return a response with the db names and ids
#' which the user has access to. The response in returned as html which is then parsed and returned
#' as a dataframe.
#' @param URL A url for Quickbase company domain
#' @param qb_ticket_id A valid quickbase authentication ticket
#' @keywords db_ids
#' @export

db_ids <- function(URL, qb_ticket_id)  {
  
  db <- "/db/"
  api_action <- "a=API_GrantedDBs"
  main <- "main?"
  qb_ticket_id <- paste0("&ticket=", qb_ticket_id)
  parent_ancestors <- "&excludeparents=1&includeancestors=1"
  get_dbs_url <- paste0(URL, db, main,
                    api_action, 
                    qb_ticket_id,
                    parent_ancestors)
  pg1 <-
    GET(get_dbs_url)
  content1 <-
    content(pg1, "text")
  
  ls_dbs <- 
    xmlToList(content1)$databases
  
  ls_dbs <- 
    lapply(ls_dbs, function(x) {
      dfs <- as.data.frame(x[1:2])
      dfs
    })
  
  ls_dbs <- 
    do.call(rbind, 
            ls_dbs)
  return(ls_dbs)
  
            }