library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(tidyverse)
library(dplyr)

options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)



search_draft_year = "2024"
find_leagues = "TRUE"


mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("MFL_PWD"))

ssb2 <- ff_connect(platform = "mfl", league_id = "10042", season = search_draft_year, user_agent = "MFLRCLIENT")
player_list <- mfl_players(conn=ssb2)
player_list <- player_list |>
  select(player_id, draft_year) #|>
#mutate(player_id = as.integer(player_id))


# Save last run ADP before we begin


if(find_leagues == "TRUE") {
  # This appears to work for Safeleagues SF/TE: SafeLeagues % (SF/TE). Filter then for dynasty
  leagues <- mfl_getendpoint(mfl_connect(search_draft_year),"leagueSearch",SEARCH="SafeLeagues % (SF/TE)") |>
    pluck("content","leagues","league") |>
    rbindlist(use.names=TRUE) |>
    filter(str_detect(name,"Dynasty")) |>
    select( league_name = name, league_id = id) |>
    mutate(clean_league_id = str_sub(league_id, -5) ) |>
    select(league_name, league_id = clean_league_id)
  
  fwrite(leagues,"mfl_league_ids.csv",quote = TRUE)
}
# Start from here when live



get_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  Sys.sleep(1)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = mfl_client, user_name=mfl_user_id, password = mfl_pass, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_draft(conn)
}

# For testing

#leagues <- leagues |>
 # slice_sample(n=100)

cli::cli_alert("Starting draft pull")
cli::cli_alert(now())
drafts <- leagues |>
  mutate(drafts = map(league_id, possibly(get_draft, otherwise = tibble()))) |>
  unnest(drafts)

cli::cli_alert("Ending draft pull")
cli::cli_alert(now())


update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")

drafts <- drafts |>
  left_join(player_list, b =c("player_id" = "player_id") ) # |>

warnings <- dplyr::last_dplyr_warnings(n=20)


# Put after any filtering
draft_sum <- drafts |>
  group_by (league_id)|>
  summarise(
    num_rounds = max(round),
    num_picks = max(overall),
    start_date = min(timestamp),
    non_rookie_n = sum(ifelse(draft_year == search_draft_year,1,0))
  )

league_names <- drafts |>
  select(league_id, league_name)|>
  distinct()



draft_sum <- draft_sum |>
  left_join (league_names, c=("league_id" = "league_id"))


# Need to add player id/year and filter out earlier draft years






# get rid of best ball

write_csv(drafts, "mfl_dynasty_drafts.csv")
write_csv(draft_sum, "draft_sum.csv")



pb_upload("mfl_dynasty_drafts.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully draft picks uploaded to Git")


pb_upload("draft_sum.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully adp metadata uploaded to Git")

