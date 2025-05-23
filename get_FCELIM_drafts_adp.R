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
user ="DEFAULT"

#GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("MFL_PWD"))






if ( user == "COMMISH") {
  


#GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
mfl_client <- Sys.getenv(c("COMMISH_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_COMMISH_USER_ID"))
mfl_pass <- Sys.getenv(c("COMMISH_PWD"))
cli::cli_alert("Client ID: {mfl_client}")

}





# Hmmm, can we use the number of picks to filter out weird ones? Before we filter?


pb_download("draft_picks_mfl.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully draft picks uploaded to Git")

pb_upload("draft_picks_mfl.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-archive")
cli::cli_alert_success("Successfully uploaded last run to archive")


# Find leagues and get rid of those with the no no strings
mfl_leagues <- mfl_getendpoint(mfl_connect(search_draft_year),"leagueSearch", user_agent=mfl_client, SEARCH=search_string, user_name=mfl_user_id, password = mfl_pass) |>
  purrr::pluck("content","leagues","league") |>
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  select( league_name = name, league_id = id,league_home = homeURL) |>
  mutate(league_home = str_replace(league_home, "https//", "https://"))
#write_csv(mfl_leagues, "mfl_leagues.csv")

leagues_to_exclude <- mfl_leagues |>
  filter(str_detect(toupper(league_name),"ROOKIES ONLY|NON SCORING|WITH TRADING|IDP ONLY|BBID|TEMPLATE|RBS ONLY"))

  leagues_ids_to_exclude <- pull(leagues_to_exclude, league_id)




get_mfl_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(3)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, password = mfl_pass)
  draft<- ff_draft(conn)
  if("timestamp" %in% names(draft)){
    return(draft)

  } 
  #else {

 #   cli::cli_alert_success("{league_id} returns nothing")
 #   return(NULL)
 # }
}




# Check to see we are in polite mode and if so

if ( polite == "TRUE") {
cli::cli_alert_success("Getting prior completed league ids")
prior_completed_leagues <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/completed_leagues.csv", col_names = c("league_id"))

completed_count <- nrow(prior_completed_leagues)
cli::cli_alert("Found {completed_count} leagues to exclude")

# Get rid of leagues that are completed and that we already have picks for. This is to keep the MFL calls to a minimum
mfl_leagues <- mfl_leagues |>
  anti_join(prior_completed_leagues)

run_league_count <- nrow(mfl_leagues)
cli::cli_alert("Running with {run_league_count} leagues")

}






fwrite(mfl_leagues,"mfl_league_ids.csv",quote = TRUE)

#test<-get_mfl_draft(57692)

# FOR TESTING

#mfl_leagues <- mfl_leagues |>
 # slice_sample(n=15)

cli::cli_alert("Starting draft pull")
cli::cli_alert(now())
mfl_drafts <- mfl_leagues |>
  mutate(drafts = map(league_id, possibly(get_mfl_draft, otherwise = tibble()))) |>
  unnest(drafts)
cli::cli_alert("Ending draft pull")
cli::cli_alert(now())

warnings <- dplyr::last_dplyr_warnings(n=20)


if (nrow(mfl_drafts) <1) {
  cli::cli_alert("Drafts not started or data pull error. Exiting")

  write_csv(mfl_drafts,"draft_picks_mfl_bad_file.csv")
  pb_upload("draft_picks_mfl_bad_file.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-mfl")
  stop()

}

#sf_test <-get_mfl_draft(67246)

# Add interval between picks, note this dies without picks so adding the if
# ADD filter for not NA timestamps as a condition
# Ts doesn't work with no data

mfl_drafts <- mfl_drafts |>
  mutate(
    player_id = as.character(player_id)
  ) |>
  group_by(league_id) |> # Note removed division here
  mutate(
    timestamp = as.POSIXct(timestamp, origin= "1970-01-01"),
    time_to_pick_int = interval(lag(timestamp), timestamp),
    time_to_pick = seconds(time_to_pick_int)
  )

made_picks <- mfl_drafts |>
  filter (!is.na(player_name))

made_pick_count <- nrow(made_picks)





# Write this before filtering to incude next picks for Power User
write_csv(mfl_drafts,"draft_picks_mfl.csv")
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")






pb_upload("draft_picks_mfl.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully draft picks uploaded to Git")








pb_upload("mfl_league_ids.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded league ids to Git")
cli::cli_alert_success("Moving on to ADP")


# For ADP...we have drafts, so really just need to nrow() and exit if none, then polite mode
# ADD filter for not NA timestamps as a condition

  cli::cli_alert_success("{made_pick_count} picks found calculating ADP")

  # Then we we have some draft picks, but perhaps not many. ADP app will filter lt 5 currently
  #Exclude weird leagues like rookie only. 
  # Update filtering placement to allow us to get weird league pick count for other reasons

  all_picks <- mfl_drafts|>
    mutate(league_id = as.character(league_id))|>
    #filter(!(league_id %in% leagues_ids_to_exclude))|>
    filter(!is.na(timestamp))|>
    distinct()

league_progress <- all_picks |>
  group_by(league_id)|>
  summarise(last_pick = max(overall))


all_picks <- all_picks|>
  filter(!(league_id %in% leagues_ids_to_exclude))

  if(polite == "TRUE") {
    # Then we we need to look for completed and merge
    cli::cli_alert_success("And we are in polite mode so merging with completed")
    pick_cols = names(mfl_drafts)
    completed_draft_picks <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/completed_mfl_drafts.csv", col_names = pick_cols)|>
      filter(!(league_id %in% leagues_to_exclude))


    # Combine and filter out picks that haven't been made yet
    all_picks <- union(completed_draft_picks, mfl_drafts)|>
      mutate(league_id = as.character(league_id))|>
      filter(!(league_id %in% leagues_to_exclude))|>
      filter(!is.na(timestamp))|>
      distinct()


  }


# Write out CSV of made picks

write_csv(all_picks, "all_picks.csv")

  draft_sum <- all_picks |>
    group_by (league_id)|>
    summarise(
      num_rounds = max(round),
      num_picks = max(overall),
      start_date = min(timestamp)

    )


  adp_metadata <- draft_sum |>
    summarise(
      num_leagues = n(),
      avg_curr_pick = mean(num_picks),
      last_start_ts = max(start_date),
      as_ofdate = Sys.Date(),
      farthest_pick = max(num_picks),
      total_picks = sum(num_picks)
    )
  write_csv(adp_metadata, "adp_metadata.csv")



  adp <- all_picks |>

    group_by(league_id,pos) |>
    mutate(pos_rank = rank(overall)) |>
    group_by(player_id, player_name, pos, team) |>
    summarise(
      n = n(),
      overall_avg = mean(overall, na.rm = TRUE) |> round(2),
      overall_sd = sd(overall, na.rm = TRUE) |> round(2),
      pos_avg = mean(pos_rank, na.rm = TRUE) |> round(2),
      pos_sd = sd(pos_rank, na.rm = TRUE) |> round(2),
      overall_min = min(overall, na.rm = TRUE),
      overall_max = max(overall, na.rm = TRUE),
      pos_min = min(pos_rank, na.rm = TRUE),
      pos_max = max(pos_rank, na.rm = TRUE)
    ) |>
    ungroup() |>
    arrange(overall_avg,-n)



  adp <- adp |>
    #  left_join(adp_last_ovr|> select(player_id, last_overall_avg)) |>
    filter(n>4)
# Once we start having enough data, re-enable to filter out weird picks
 # adp <- adp |>
    #  left_join(adp_last_ovr|> select(player_id, last_overall_avg)) |>
  #  filter(n>4)

  write_csv(adp, "adp_mfl.csv")

  cli::cli_alert_success("Successfully calculated ADP!")
  # Still need to upload metadata and adp

  write_csv(leagues_to_exclude, "excluded_leagues.csv")
  
  pb_upload("excluded_leagues.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-mfl")
  cli::cli_alert_success("Successfully uploaded excluded to Git")
  
  
  write_csv(league_progress, "league_progress.csv")
  
  pb_upload("league_progress.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-mfl")
  cli::cli_alert_success("Successfully uploaded league progress to Git")
  
  
  
  
  
  

  pb_upload("adp_mfl.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-mfl")
  cli::cli_alert_success("Successfully adp uploaded to Git")

  pb_upload("all_picks.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-mfl")
  cli::cli_alert_success("Successfully made picks uploaded to Git")



  pb_upload("adp_metadata.csv",
            repo = "mohanpatrick/elim-data-2025",
            tag = "data-mfl")
  cli::cli_alert_success("Successfully uploaded adp metadata to Git")




  current_picks <- mfl_drafts |>
    filter(!is.na(player_name)) |>
    group_by(league_id) |>
    summarize(
      last_pick = max(overall),
      last_pick_made_ts = max(timestamp)
    ) |>
    mutate(otc = last_pick +1)

  # Append franchise info
  #current_picks <- current_picks |>
  #  mutate(league_id = as.character(league_id))

  current_picks <- current_picks |>
    left_join(mfl_drafts |> select (league_id, league_name, league_home)|>unique(), by=c("league_id" = "league_id"))

  last_updated <- mfl_drafts |>
    ungroup()|>
    filter(!is.na(player_name)) |>
    summarise(max_ts = max(timestamp))



  max_pick_made <- with_tz(last_updated[[1]], tz = "America/New_York" )

  max_pick_made <- paste0(format(max_pick_made, '%Y-%m-%d %I:%S %p'))



  # Determine everyone's next pick on a league by league basis
  all_next_picks <- mfl_drafts |>
    filter(is.na(player_name)) |>
    #filter(!grepl("DND", league_name)) |>
    group_by(league_id, franchise_id, franchise_name) |>
    #mutate(league_id = as.character(league_id)) |>
    summarise(
      next_pick = min(overall)
    )

  all_league_summary <- current_picks |>
    left_join(all_next_picks) |>
    mutate(pick_delta= next_pick - otc) |>
    ungroup() |>
    arrange(pick_delta)




  all_league_summary <- all_league_summary |>
    # left_join(draft_sum) |>
    mutate(league_name = gsub("zzz #FCEliminator 2025","", league_name),
           league_home = str_replace(league_home, "https//", "https://"),
           last_pick_ts_est = with_tz(last_pick_made_ts, "America/New_York"),
           time_since = as.period(interval(last_pick_ts_est,now(tzone="America/New_York"))),
           time_since_fmt = format(time_since, '%h')
    )|>
    arrange(last_pick_ts_est)





write_csv(all_league_summary, "otc_file.csv")

pb_upload("otc_file.csv",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded otc metadata to Git")

#mfl_conn <- mfl_connect(season = 2024, league_id = 43231, user_name = mfl_user_id, password = mfl_pass , user_agent = mfl_client)
#draft <- get_mfl_draft(43231)
