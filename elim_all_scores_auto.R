
library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)



options(dplyr.summarise.inform = FALSE,
piggyback.verbose = FALSE)
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("MFL_PWD"))







GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))




cli::cli_alert("Client ID: {mfl_client}")

# Get franchise info for missing leagues
franchise_list <- tribble(
  ~league_id, 
  "33121", 
  "39863", 
  "59150",
  "65052",
  "15472",
  "10144",
  "57215"
)


get_franchises <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now sleeping for 2 seconds")
  Sys.sleep(3)
  
  mfl_user_id = "android16"
  mfl_pass = "R1BRX70x"
  search_draft_year = "2025"
  
  mfl_conn <- mfl_connect(search_draft_year, user_agent = "MFLRCLIENT", user_name=mfl_user_id, password = mfl_pass, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60, league_id = league_id)
  
  franchises <-   ff_franchises(mfl_conn)
  
}





# Define function to get results for each league and parse them
get_results <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now sleeping for 2 seconds")
  Sys.sleep(3)
  
  mfl_user_id = "android16"
  mfl_pass = "R1BRX70x"
  search_draft_year = "2025"
  
  mfl_conn <- mfl_connect(search_draft_year, user_agent = "MFLRCLIENT", user_name=mfl_user_id, password = mfl_pass, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60, league_id = league_id)
  
  
  weekly_scoring <-  mfl_getendpoint(mfl_conn,endpoint = "weeklyResults", league_id = league_id, W= "YTD")
  
  weekly_scoring <- weekly_scoring |>
    purrr::pluck("content","weeklyResults", "franchise") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    select(id, score)|>
    mutate(franchise_id = id,
           league_id = league_id
    )
  
  
  
}



get_results_full <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now sleeping for 2 seconds")
  Sys.sleep(3)
  
  mfl_user_id = "android16"
  mfl_pass = "R1BRX70x"
  search_draft_year = "2025"
  
  mfl_conn <- mfl_connect(search_draft_year, user_agent = "MFLRCLIENT", user_name=mfl_user_id, password = mfl_pass, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60, league_id = league_id)
  
  
  weekly_scoring <-  mfl_getendpoint(mfl_conn,endpoint = "weeklyResults", league_id = league_id, W= "YTD")
  
  weekly_scoring <- weekly_scoring |>
    purrr::pluck("content", "allWeeklyResults", "weeklyResults") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    unnest_longer(franchise)|>
    unnest_wider(franchise)|>
    select(id, score, week)|>
    mutate(franchise_id = id,
           league_id = league_id
    )
  
  
  
}






# Read all leagues from draft file created this summer
elim_leagues <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/mfl_league_ids.csv")


# Pass in league ids to results function and iterate over it

#elim_leagues <- elim_leagues|>
#  slice_head(n=10)

all_results <- elim_leagues |>
  mutate(results = map(league_id, possibly(get_results_full, otherwise = tibble(), quiet=FALSE))) |>
  unnest(results, names_sep = "_")

# So logically we have to:
# 1) summarize total points so far -i.e overall leaderboard. Can't do this yet without franchise info
# 2) Identify and separate out current week data and feed into the usual
# 3) Determine how many leagues people are alive in, merge that back to overall leaderboard
# and filter out teams with none


all_results |> group_by(results_week)|>

  summarise(total = sum(as.numeric(results_score)))


#41474 still throwing an error 44072

#League ID: 42093
#→ Now sleeping for 2 seconds
#Error: Can't select columns that don't exist.
#League ID: 46397
#→ Now sleeping for 2 seconds
#Error: Can't select columns that don't exist.
#✖ Column `score` doesn't exist.

current_week <- all_results |> group_by(results_week)|>
  filter(!(is.na(results_score)))|>
  summarise(total = sum(as.numeric(results_score)))|>
  select(results_week)|>
  arrange(desc(results_week))|>
  slice_head(n=1)


#current_week <- 5

leaderboard <- all_results|>
filter(!is.na(results_score))|>
  select (league_id, results_id, results_score)|>
  group_by(league_id, results_id)|>
  mutate(results_score = as.numeric(results_score))|>
  summarise(total_points = sum(results_score),
            avg_points = mean(results_score))|>
  arrange(desc(total_points))



# Determine lowest score in each
league_min_scores <- all_results|>
  filter(results_week == current_week)|>
  #select(-franchise_id, -franchise_name,-league_url)|>
  mutate(results_score = as.numeric(results_score))|>
  filter(results_score >0)|>
  group_by(league_id, league_name)|>
  summarise(league_min_score = min(results_score),
            num_teams = n())


# order the results by league, finish
all_results_ordered <- all_results|>
  filter(results_week == current_week)|>
  #select(-franchise_id, -franchise_name,-league_url)|>
  mutate(results_score = as.numeric(results_score))|>
  filter(results_score >0)|>
  group_by(league_id, league_name)|>
  arrange(league_id,desc(results_score))|>
  mutate(
    finish = row_number(desc(results_score)),
    next_highest_score = lag(results_score, n=1),
    next_lowest_score = lead(results_score, n=1),
    num_teams = n_distinct(results_id),
    next_to_last = ifelse(finish == num_teams-1,1,0),
    score_delta_higher = ifelse(!is.na(next_highest_score),next_highest_score - results_score,999 ),
    score_delta_lower  = ifelse(!is.na(next_lowest_score),results_score - next_lowest_score,999 )
    
  )|>
  distinct()

# Enrich with low score
all_results_ordered <- all_results_ordered |>
  left_join(league_min_scores |>select(league_id, league_min_score, num_teams), by=c("league_id" = "league_id"))|>
  mutate(is_eliminated = ifelse(results_score == league_min_score,1,0))


#Get franchises from draft picks file
#all_franchises <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/completed_drafts.csv")|>
#  mutate(league_id = as.double(league_id))|>
 # select(league_id, franchise_id, franchise_name)|>
 # distinct()

all_franchises <- read_csv("https://github.com/mohanpatrick/elim-data-2025/releases/download/data-mfl/draft_picks_mfl.csv")|>
mutate(league_id = as.double(league_id))|>
select(league_id, franchise_id, franchise_name)|>
distinct()

#######END DON"T RUN
# Add franchises to ordered results and write out results file
all_results_ordered <- all_results_ordered|>
  left_join(all_franchises |>select(league_id,franchise_id, franchise_name), by=c("league_id"="league_id","results_franchise_id" = "franchise_id"))|>
  select(league_id, league_name, league_home,results_franchise_id, franchise_name, finish, results_score, next_highest_score, next_lowest_score, is_eliminated,next_to_last,score_delta_lower)|>
  distinct()


leader_names <- all_results_ordered |>
  filter(is_eliminated == 0)|>
  select(league_id, league_name, results_franchise_id, franchise_name)|>
  mutate(franchise_name = str_replace(franchise_name, "@@", "@")) 

leaderboard <- leaderboard |>
  left_join(leader_names, by=c("league_id" = "league_id", "results_id" = "results_franchise_id"))|>
  filter(!is.na(league_name))

all_results_ordered <- all_results_ordered |>
  left_join(leaderboard |> select(league_id, results_id, total_points), by=c("league_id" = "league_id", "results_franchise_id"= "results_id"))



leaderboard_alive <- leader_names |>
  filter(!is.na(franchise_name))|>
  group_by(franchise_name)|>
  count()|>
  arrange(desc(n))

leader_names |>
  filter(is.na(franchise_name))


# Objects n


write_csv(all_results_ordered, "all_results_ordered.csv")
write_csv(leaderboard, "leaderboard.csv")
write_csv(leaderboard_alive, "leaderboard_alive.csv")
#write_csv(all_franchises, "all_elim_franchises.csv")
#pb_upload("all_elim_franchises.csv",repo = "mohanpatrick/elim-data-2024",tag = "data-mfl")

pb_upload("all_results_ordered.csv",repo = "mohanpatrick/elim-data-2024",tag = "data-mfl")
pb_upload("leaderboard.csv",repo = "mohanpatrick/elim-data-2024",tag = "data-mfl")
pb_upload("leaderboard_alive.csv",repo = "mohanpatrick/elim-data-2024",tag = "data-mfl")
cli::cli_alert_success("Successfully draft picks uploaded to Git")


