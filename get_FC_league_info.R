library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(lubridate)



options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

#### DELETE AFTER TESTING ########
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")
#Sys.setenv(MFL_CLIENT = "")

search_draft_year = "2025"
find_leagues = "TRUE"
polite = "FALSE"
search_string="zzz #FCEliminator"
total_picks_in_draft = 288
user ="COMMISH"

#GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("MFL_PWD"))



cli::cli_alert_success("Running under {user}")


if ( user == "COMMISH") {
  
  
  
  #GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
  mfl_client <- Sys.getenv(c("COMMISH_CLIENT"))
  mfl_user_id <- Sys.getenv(c("MFL_COMMISH_USER_ID"))
  mfl_pass <- Sys.getenv(c("COMMISH_PWD"))
  cli::cli_alert("Client ID: {mfl_client}")
  
}

mfl_leagues <- mfl_getendpoint(mfl_connect(search_draft_year),"leagueSearch", user_agent=mfl_client, SEARCH=search_string, user_name=mfl_user_id, password = mfl_pass) |>
  purrr::pluck("content","leagues","league") |>
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  select( league_name = name, league_id = id,league_home = homeURL) 



get_mfl_franchises <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(2)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, password = mfl_pass)
  franchises<- ff_franchises(conn)
  
    
  } 


get_mfl_draft_starts <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(2)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, password = mfl_pass)
  calendar <-  mfl_getendpoint(conn,endpoint = "calendar", W="YTD")|>
    
    purrr::pluck("content","calendar") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)
}
  

cli::cli_alert("Starting franchises pull")
cli::cli_alert(now())
mfl_franchises <- mfl_leagues |>
  mutate(franchises = map(league_id, possibly(get_mfl_franchises, otherwise = tibble()))) |>
  unnest(franchises)
cli::cli_alert("Ending franchise pull")
cli::cli_alert(now())

cli::cli_alert("Starting drafts pull")
cli::cli_alert(now())
mfl_draft_times <- mfl_leagues |>
  mutate(draft_times = map(league_id, possibly(get_mfl_draft_starts, otherwise = tibble()))) |>
  unnest(draft_times)
cli::cli_alert("Ending drafts pull")
cli::cli_alert(now())



mfl_draft_times <- mfl_draft_times |>
  filter(type %in% c( "DRAFT_START", "AUCTION_START", "WAIVER_BBID"))|>
  mutate(start_time = as_datetime(as.numeric(start_time)))
         


# To summarize franchises
# by league id (franchise 1 linked, total franchises, franchises linked, draft start)

league_summary <- mfl_franchises |>
  group_by(league_name, league_id, league_home)|>
  summarise(
            celeb_linked = max(ifelse(franchise_id == "0001" & !(is.na(username)),1,0)),
            total_franchises = n(),
            franchises_linked = sum(ifelse(!(is.na(username)),1,0))
  )|>
  left_join(mfl_draft_times|> select(league_id,type, start_time))|>
  mutate(ready_to_go = ifelse(celeb_linked == 1 & franchises_linked > 1, "Yes", "No"),
         days_until_draft = as.numeric(difftime(start_time, today(), units = "days"))
         )|>
  arrange(desc(ready_to_go), desc(days_until_draft))


write_csv(league_summary, "league_summary.csv")



pb_upload("league_summary.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded to Git")








