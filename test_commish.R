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

search_draft_year = "2024"
find_leagues = "TRUE"
polite = "FALSE"
search_string="zzz #FCEliminator"
total_picks_in_draft = 288
user ="MAIN"
# Exclude blind bid leagues
leagues_to_exclude = c(19123,33163,39863,52021,64792,58866,10144,15472,33121,42972,44072,57215,59150,65052,69507, 41474)
leagues_to_exclude_adp = c(15099,28530,29122,29276,37484,45539,50996,69507,70181,70715)
#GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("BOT_BATCH"))


if ( user == "COMMISH") {
  
  
  
  #GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
  mfl_client <- Sys.getenv(c("COMMISH_CLIENT"))
   #mfl_client = Sys.getenv("COMMISH_CLIENT")
  mfl_user_id = "patmflcommish"
  #mfl_user_id <- Sys.getenv(c("MFL_COMMISH_USER_ID"))
  mfl_bot <- Sys.getenv("COMMISH_PWD")

  cli::cli_alert("Client ID: {mfl_client}")
  
}

writeLines(mfl_client, "output_c.txt")
writeLines(mfl_user_id, "output_u.txt")
writeLines(mfl_bot, "output_p.txt")




pb_upload("output_c.txt",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded adp metadata to Git")

pb_upload("output_u.txt",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded adp metadata to Git")



pb_upload("output_p.txt",
          repo = "mohanpatrick/elim-data-2025",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded adp metadata to Git")




#cookie <- ssb2025[["auth_cookie"]][["options"]][["cookie"]]

#print(cookie)


#cli::cli_alert("Client ID: {cookie}")
