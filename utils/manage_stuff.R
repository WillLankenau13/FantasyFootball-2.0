library("ggplot2")
library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library("modelr")
library("leaps")
library("ggrepel")
library("nflfastR")
library("nflreadr")
library("lpSolve")
library("jsonlite")



###Years
#Set Years
Past_Year_d <- 2025
This_Year_d <- 2026

# Years_Dataframe <- data.frame(Past_Year = Past_Year_d,
#                               This_Year = This_Year_d)

#Create DF
# write_csv(Years_Dataframe, "~/R Stuff/FantasyFootball 2.0/Years_Dataframe.csv")

###Names
player_names_func <- function(df){
  
  if("player" %in% colnames(df)){
  df$player <- str_replace_all(df$player, "\\.", "")
  df$player <- str_replace_all(df$player, "[^[:alnum:]]", " ")
  df$player <- str_replace_all(df$player, "\\s+", " ")
  df$player <- str_replace_all(df$player, " IV", " ")
  df$player <- str_replace_all(df$player, " III", " ")
  df$player <- str_replace_all(df$player, " II", " ")
  df$player <- str_replace_all(df$player, " Jr", " ")
  df$player <- str_replace_all(df$player, " Sr", " ")
  df$player <- trimws(df$player)
  
  }
  
  if("pos" %in% colnames(df)){
    df <- df %>% 
      mutate(pos = ifelse(player == "Taysom Hill", "TE", pos),
             pos = ifelse(player == "Lawrence Cager", "TE", pos),
             pos = ifelse(player == "Anthony Firkser", "TE", pos),
             pos = ifelse(player == "Andrew Beck", "TE", pos),
             pos = ifelse(player == "Juwan Johnson", "TE", pos),
             pos = ifelse(player == "Jody Fortson", "TE", pos),
             pos = ifelse(player == "Tanner Conner", "TE", pos),
             pos = ifelse(player == "Feleipe Franks", "TE", pos),
             pos = ifelse(player == "Chris Myarick", "TE", pos),
             pos = ifelse(player == "Giovanni Ricci", "TE", pos),
             pos = ifelse(player == "Colson Yankoff", "TE", pos),
             pos = ifelse(player == "Darren Waller", "TE", pos),
             pos = ifelse(player == "Scott Matlock", "RB", pos),
             pos = ifelse(player == "Keith Smith", "RB", pos),
             pos = ifelse(player == "Ty Montgomery", "WR", pos),
             pos = ifelse(player == "Jacob Harris", "WR", pos),
             pos = ifelse(player == "Velus Jones", "WR", pos),
             pos = ifelse(player == "Tyreik McAllister", "WR", pos),
             pos = ifelse(player == "Justin Shorter", "WR", pos),
             pos = ifelse(player == "Jack Westover", "WR", pos),
             pos = ifelse(player == "Jeff Driskel", "QB", pos),
             pos = ifelse(pos == "HB", "RB", pos),
             pos = ifelse(pos == "FB", "RB", pos))
  }
  
  if("team" %in% colnames(df)){
    df$team[df$team == "GNB"] <- "GB"
    df$team[df$team == "JAX"] <- "JAC"
    df$team[df$team == "KAN"] <- "KC"
    df$team[df$team == "LVR"] <- "LV"
    df$team[df$team == "NWE"] <- "NE"
    df$team[df$team == "NOR"] <- "NO"
    df$team[df$team == "SFO"] <- "SF"
    df$team[df$team == "TAM"] <- "TB"
    df$team[df$team == "LA"] <- "LAR"
  }
  
  if("opp" %in% colnames(df)){
    df$opp[df$opp == "GNB"] <- "GB"
    df$opp[df$opp == "JAX"] <- "JAC"
    df$opp[df$opp == "KAN"] <- "KC"
    df$opp[df$opp == "LVR"] <- "LV"
    df$opp[df$opp == "NWE"] <- "NE"
    df$opp[df$opp == "NOR"] <- "NO"
    df$opp[df$opp == "SFO"] <- "SF"
    df$opp[df$opp == "TAM"] <- "TB"
    df$opp[df$opp == "LA"] <- "LAR"
  }
  
  df[df == "DJ Moore"] <- "D J Moore"
  df[df == "DJ Chark"] <- "D J Chark"
  df[df == "DK Metcalf"] <- "D K Metcalf"
  df[df == "PJ Walker"] <- "P J Walker"
  df[df == "AJ McCarron"] <- "A J McCarron"
  df[df == "AJ Dillon"] <- "A J Dillon"
  df[df == "AJ Henning"] <- "A J Henning"
  df[df == "CJ Marable"] <- "C J Marable"
  df[df == "JJ Howland"] <- "J J Howland"
  df[df == "TJ Sheffield"] <- "T J Sheffield"
  df[df == "KJ Hamler"] <- "K J Hamler"
  df[df == "CJ Stroud"] <- "C J Stroud"
  
  df[df == "Eli Mitchell"] <- "Elijah Mitchell"
  df[df == "Gabe Davis"] <- "Gabriel Davis"
  df[df == "Mitch Trubisky"] <- "Mitchell Trubisky"
  df[df == "Josh Palmer"] <- "Joshua Palmer"
  df[df == "Ken Walker"] <- "Kenneth Walker"
  df[df == "Chigoziem Okonkwo"] <- "Chig Okonkwo"
  df[df == "Scotty Miller"] <- "Scott Miller"
  df[df == "Andrew Ogletree"] <- "Drew Ogletree"
  df[df == "Dee Eskridge"] <- "D Wayne Eskridge"
  df[df == "Mitch Tinsley"] <- "Mitchell Tinsley"
  df[df == "Phillip Walker"] <- "P J Walker"
  df[df == "Robby Anderson"] <- "Robbie Anderson"
  df[df == "Jeffery Wilson"] <- "Jeff Wilson"
  df[df == "Mike Woods"] <- "Michael Woods"
  df[df == "Rod Williams"] <- "Rodney Williams"
  df[df == "Nate Carter"] <- "Nathan Carter"
  
  df[df == "Deonte Harris"] <- "Deonte Harty"
  df[df == "Robbie Chosen"] <- "Robbie Anderson"
  df[df == "Hollywood Brown"] <- "Marquise Brown"
  df[df == "Bam Knight"] <- "Zonovan Knight"
  
  df[df == "DeMario Douglas"] <- "Demario Douglas"
  df[df == "JaMycal Hasty"] <- "Jamycal Hasty"
  df[df == "Grant DuBose"] <- "Grant Dubose"
  df[df == "JaQuan Hardy"] <- "Jaquan Hardy"
  df[df == "ZaQuandre White"] <- "Zaquandre White"
  
  df[df == "Audric Estimé"] <- "Audric Estime"
  
  df[df == "David Sills V"] <- "David Sills"
  

  df[df == "Washington Football Team"] <- "Washington Commanders"
  
  return(df)
}


