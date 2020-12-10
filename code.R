## Packages ##

library(tidyverse)
library(readxl)
library(humaniformat)

## FEC Data Wrangling ##

fec_url <- "https://github.com/lgorak/finalproject/raw/main/rawdata/FEC_24month_2018.xlsx"
destfile <- "ConCand7_2018_24m.xlsx"
curl::curl_download(fec_url, destfile)
fec_raw <- read_excel(destfile)

colnames(fec_raw) <- as.character(fec_raw[4, ])
fec_raw <- fec_raw[-c(1:4), ]
fec_raw <- fec_raw %>% select(-c(`Coverage End Date`, `State`, `District`))

fec_data <-
  fec_raw %>% rename(
    "candidate" = Candidate,
    "can_party" = Party,
    "receipts" = Receipts,
    "ico" = `Incumbent/\r\nChallenger/Open`,
    "idv_contr" = `Contributions \r\nfrom Individuals`,
    "pac_contr" = `Contributions\r\nfrom PACs and\r\nOther Committees`,
    "candidate_contr" = `Contributions and\r\nLoans from \r\n the Candidate`,
    "total_spent" = Disbursements,
    "onhand" = `Cash On Hand`,
    "debts" = Debts) # Rename columns

fec_data <-
  fec_data %>% mutate(district = as.numeric(district),
                        receipts = as.numeric(receipts),
                        idv_contr = as.numeric(idv_contr),
                        pac_contr = as.numeric(pac_contr), 
                        candidate_contr = as.numeric(candidate_contr), 
                        total_spent = as.numeric(total_spent), 
                        onhand = as.numeric(onhand),
                        debts = as.numeric(debts)) # Make numbered columns numeric

fec_data <-
  fec_data %>% mutate(
    can_party = recode(
      can_party,
      `Republican Party` = "republican",
      `Democratic Party` = "democrat",
      `Independent` = "independent", 
      `Unknown` = "NA",
      `Unaffiliated` = "NA",
      `No Party Affiliation` = "NA",
      `Green Party` = "green",
      `Libertarian Party` = "libertarian",
      `Democratic-Farm-Labor` = "democrat",
      .default = "other")) # Recode party names. Small third parties coded as "other."

fec_data <-
  fec_data %>% mutate(ico = recode(
    ico,
    `Challenger` = "challenger",
    `Incumbent` = "incumbent",
    `Open` = "open", 
    .default = "NA"
  )) # Recode ico for sake of tidyness

fec_data <- fec_data %>% na_if("NA") # Make sure anything NA is NA. 

## Election Data Wrangling ##

election_raw <- read_csv("https://raw.githubusercontent.com/lgorak/finalproject/main/rawdata/MITLab_Electionresults_1976-2018.csv")
election_raw <- election_raw %>% filter(year == "2018")
election_raw <- election_raw %>% filter(writein == "FALSE")
election_2018 <-
  election_raw %>% select("state" = state_po,
                          district,
                          candidate,
                          "votes" = totalvotes)

## Election Winner Data ##

win_raw <- read_csv("https://raw.githubusercontent.com/lgorak/finalproject/main/rawdata/CookPR_Electionwinner_2-18.csv")
win_2018 <-
  win_raw %>% select("district" = Dist,
                     "win_can" = `2018 Winner`,
                     "dem_votes" = `Dem Votes`,
                     "rep_votes" = `GOP Votes`,
                     "dem_perc" = `Dem %`,
                     "rep_perc" = `GOP %`,
                     "dem_margin" = `Dem Margin`) # Select rows I want

win_2018 <- win_2018 %>% mutate("rep_margin" = dem_margin * -1) # New column: margin of win by GOP

win_2018 <- win_2018 %>% separate(win_can, c("win_can", "win_party"), " \\(") 
win_2018 <-
  win_2018 %>% mutate(win_party = recode(win_party, `R)` = "republican", `D)` = "democrat")) # New column: winning party

win_2018 <- win_2018 %>% separate(district, c("state", "district"), "-")
win_2018 <- win_2018 %>% mutate(district = recode(district, `AL` = "0", .default = district)) #Separate district columns and fix At-Large problem
win_2018 <- win_2018 %>% mutate(district = as.numeric(district))

## District Data Wrangling ##

context_raw <- read_csv("https://raw.githubusercontent.com/lgorak/finalproject/main/rawdata/FiveThirtyEight_Electioncontext_2018.txt")
context_raw <- context_raw %>% filter(maptype == "current")
context_2018 <-
  context_raw %>% select(
    state,
    district,
    "dist_population" = population,
    "dist_population_18_over" = population_18_over,
    "dist_cookPVI" = PVI,
    "dist_white" = `Non-Hispanic White`,
    "dist_africanamerican" = `African-American`,
    "dist_latino" = `Hispanic/Latino`,
    "dist_asian" = `Asian`,
    "dist_nativeamerican" = `Native American`,
    "dist_pacificislander" = `Pacific Islander`,
    "dist_other" = `Other`,
    "dist_race_category" = race_category)
context_2018 <- context_2018 %>% mutate(district = as.numeric(district))

## Parse names in all data ##

fec_data_fixnames <- fec_data %>% separate(candidate, c("lastname", "firstname"), "\\, ")
fec_data_fixnames <- fec_data_fixnames %>% separate(firstname, c("firstname", "extra"), "\\ ")
fec_data_fixnames <- fec_data_fixnames %>% separate(lastname, c("lastname", "extra"), "\\ ")
fec_data_fixnames <- fec_data_fixnames %>% mutate(candidate = paste(firstname, lastname))
fec_data_fixnames$candidate <- fec_data_fixnames$candidate %>% str_replace_all("[[:punct:]]", "")
fec_data_fixnames <- fec_data_fixnames %>% select(-c(lastname, firstname, extra))

candidatenames <- election_2018$candidate %>% parse_names()
election_2018_fixnames <- full_join(election_2018, candidatenames, by = c("candidate" = "full_name"))
election_2018_fixnames <- election_2018_fixnames %>% mutate(candidate = paste(toupper(first_name), toupper(last_name)))
election_2018_fixnames$candidate <- election_2018_fixnames$candidate %>% str_replace_all("[[:punct:]]", "")
election_2018_fixnames <- election_2018_fixnames %>% select(-c(salutation, first_name, middle_name, last_name, suffix))

win_2018_fixnames <- win_2018 %>% separate(win_can, c("firstname", "lastname"), "\\ ")
win_2018_fixnames <- win_2018_fixnames %>% separate(firstname, c("firstname", "extra"), "\\ ")
win_2018_fixnames <- win_2018_fixnames %>% separate(lastname, c("lastname", "extra"), "\\ ")
win_2018_fixnames <- win_2018_fixnames %>% mutate(win_can = paste(toupper(firstname), toupper(lastname)))
win_2018_fixnames$win_can <- win_2018_fixnames$win_can %>% str_replace_all("[[:punct:]]", "")
win_2018_fixnames <- win_2018_fixnames %>% select(-c(lastname, firstname, extra))

## Combine Data ##

context_win_2018 <- win_2018_fixnames %>% full_join(context_2018, by = c("state", "district")) #Combine winner data with context data using congressional district
votes_win_context <- context_win_2018 %>% left_join(election_2018_fixnames, by = c("state", "district"))
votes_win_context <- votes_win_context %>% distinct(.keep_all = TRUE)
votes_win_context_FEC <- votes_win_context %>% left_join(fec_data_fixnames, by = "candidate")

## Combine and Clean Everything ##

votes_win_context_FEC <- votes_win_context_FEC %>% mutate(winner = (win_can == candidate))
votes_win_context_FEC$winner <- votes_win_context_FEC$winner %>% as.character()
votes_win_context_FEC <- votes_win_context_FEC %>% mutate(winner = recode_factor(winner, "TRUE" = 1, "FALSE" = 0))

colnames(votes_win_context_FEC)
colorder <- c(22, 1, 2, 33, 25, 26, 23, 24, 27, 28, 29, 30, 31, 32, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)

House_2018 <- votes_win_context_FEC[, colorder]
House_2018

colnames(House_2018)


# write_csv(House_2018, "finalproject")

