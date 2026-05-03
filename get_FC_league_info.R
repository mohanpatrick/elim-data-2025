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

search_draft_year = "2026"
find_leagues = "TRUE"
polite = "FALSE"
search_string="zzz #FCEliminator"
total_picks_in_draft = 288
user ="COMMISH"

#GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("MFL_PWD"))

VALID_EMAIL_REGEX <- "^[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}$"


cli::cli_alert_success("Running under {user}")


if ( user == "COMMISH") {
  
 
  
  #GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
  mfl_client <- Sys.getenv(c("COMMISH_CLIENT"))
  mfl_user_id <- Sys.getenv(c("MFL_COMMISH_USER_ID"))
  mfl_pass <- Sys.getenv(c("COMMISH_PWD"))
  cli::cli_alert("Client ID: {mfl_client}")
  
}



#conn <- mfl_connect(search_draft_year, "11104", user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, password = mfl_pass)



mfl_leagues <- mfl_getendpoint(mfl_connect(search_draft_year),"leagueSearch", user_agent=mfl_client, SEARCH=search_string, user_name=mfl_user_id, password = mfl_pass) |>
  purrr::pluck("content","leagues","league") |>
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  select( league_name = name, league_id = id,league_home = homeURL) 

#mfl_leagues <- mfl_leagues|>slice_sample(n=10)

get_mfl_franchises <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(2)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, 
  password = mfl_pass)
  cookie_value <- conn$auth_cookie$options$cookie
  cli::cli_alert("{cookie_value}")
  franchises<- ff_franchises(conn)
  
    
  } 


get_mfl_draft_starts <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(2)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, password = mfl_pass)
  cookie_value <- conn$auth_cookie$options$cookie
  cli::cli_alert("{cookie_value}")
  calendar <-  mfl_getendpoint(conn,endpoint = "calendar", W="YTD")|>
    
    purrr::pluck("content","calendar") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)
}
  

obscure_email <- function(email) {
  parts <- strsplit(email, "@")[[1]]
  local <- parts[1]
  domain <- parts[2]
  
  visible_chars <- max(1, floor(nchar(local) * 0.3))
  obscured_local <- paste0(
    substr(local, 1, visible_chars),
    strrep("*", nchar(local) - visible_chars)
  )
  
  paste0(obscured_local, "@", domain)
}

cli::cli_alert("Starting franchises pull")
cli::cli_alert(now())
mfl_franchises <- mfl_leagues |>
  mutate(franchises = map(league_id, possibly(get_mfl_franchises, otherwise = tibble()))) |>
  unnest(franchises)|>
  mutate(
      league_home = str_replace(league_home, "https//", "https://"),
     is_linked =ifelse(!is.na(username) & trimws(username) != "",1,0)
      )
cli::cli_alert("Ending franchise pull")
cli::cli_alert(now())

cli::cli_alert("Starting drafts pull")
cli::cli_alert(now())
mfl_draft_times <- mfl_leagues |>
  mutate(draft_times = map(league_id, possibly(get_mfl_draft_starts, otherwise = tibble()))) |>
  unnest(draft_times)
cli::cli_alert("Ending drafts pull")
cli::cli_alert(now())

# ── Email validation flag ──────────────────────────────────────────────────────
# Regex covers the most common structural failures without being overly strict.
# Flagged rows should be reviewed manually — this catches obvious invalids only.


# Quick summary to paste into your console during prep

mfl_franchise_list <- mfl_franchises |>
  select(league_name,league_id, league_home, franchise_id, franchise_name, email, is_linked)|>
  mutate(league_home = str_replace(league_home, "https//", "https://"))|>
  mutate(
    email_flag = case_when(
      is.na(email) | str_trim(email) == ""
      ~ "missing",
      !str_detect(str_trim(email), VALID_EMAIL_REGEX)
      ~ "invalid_format",
      str_detect(str_trim(email), "\\s")
      ~ "contains_spaces",
      str_length(email) > 254
      ~ "too_long",
      TRUE                              ~ "ok"
    )
  )


league_progress <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/league_progress.csv")|>
  mutate(league_id = as.character(league_id))




mfl_draft_times <- mfl_draft_times |>
  filter(type %in% c( "DRAFT_START", "AUCTION_START", "WAIVER_BBID"))|>
  mutate(start_time = as_datetime(as.numeric(start_time)))
         

# Filter franchises down to a list with just franchises 1-3, then pivot wider
multiples <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/fc_multiple_leagues.csv")|>
  mutate(league_id = as.character(league_id))




# To summarize franchises
# by league id (franchise 1 linked, total franchises, franchises linked, draft start)



league_summary <- mfl_franchises |>
  group_by(league_name, league_id, league_home)|>
  summarise(
    celeb_1_linked = max(ifelse(franchise_id == "0001" & !is.na(username) & trimws(username) != "",1,0)),
    celeb_2_linked = max(ifelse(franchise_id == "0002" & !is.na(username) & trimws(username) != "",1,0)),
    celeb_3_linked = max(ifelse(franchise_id == "0003" & !is.na(username) & trimws(username) != "",1,0)),
    franchise_1_email = max(ifelse(franchise_id == "0001" & !(is.na(email)),email,"")),
    total_franchises = n(),
    franchises_linked = sum(!is.na(username) & trimws(username) != "")
  )|>
  left_join(multiples |> select(league_id, celeb_count))|>
  left_join(mfl_draft_times|> select(league_id,type, start_time))|>
 
      mutate(
        celeb_count = ifelse(is.na(celeb_count),1,celeb_count),
      ready_to_go = case_when(
        celeb_count == 1 & celeb_1_linked ==1 & franchises_linked > 1 ~ "Y",
        celeb_count == 2 & celeb_1_linked + celeb_2_linked ==2 & franchises_linked > 2 ~ "Y",
        celeb_count == 3 & celeb_1_linked + celeb_2_linked + celeb_3_linked==3 & franchises_linked > 2 ~ "Y",
        .default = "N"
        
        
      ),
      days_until_draft = round(as.numeric(difftime(start_time, today(), units = "days")), 2)
  )|>
  arrange(desc(ready_to_go), days_until_draft) 

email_sum <- mfl_franchise_list |>
  group_by(league_id)|>
  summarise(suspect_emails = sum(ifelse(email_flag != "ok",1,0)))


league_summary <- league_summary |>
  left_join(email_sum, by=c("league_id" = "league_id"))

# when celeb count is 1 and 2 franchises are linked then "Y"
# when celeb count is 2 and franchises 1, 2  are linked  and franchises_linked gt 2 then then "Y"
# hen celeb count is 2 and franchises 1, 2  are linked  and franchises_linked gt 2 then then "Y"


league_summary <- league_summary |>
  left_join(league_progress)|>
  filter(is.na(last_pick))


#cleanup emails

mfl_franchise_list <- mfl_franchise_list|>
  mutate(email = if_else(
    !is.na(email),
    map_chr(email, obscure_email),
    NA_character_
  ))

league_summary <- league_summary|>
  mutate(franchise_1_email = if_else(
    !is.na(franchise_1_email),
    map_chr(franchise_1_email, obscure_email),
    NA_character_
  ))

write_csv(mfl_franchise_list, "franchise_list.csv")


pb_upload("franchise_list.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded to Git")


write_csv(league_summary, "league_summary.csv")



pb_upload("league_summary.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded to Git")


update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")

pb_upload("timestamp.txt",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded to Git")






