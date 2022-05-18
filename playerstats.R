# load libraries
library(tidyverse)
library(rvest)
library(janitor)

# function to scrape hoop-math.com team tables
scrape_hoop_math_team_tables <- function(team, years, type = c("OffTable", "RebTable", "TransOTable", "DefTable", "TransDTable")) {
  if (years[1] < 2012) {
    stop("hoop-math.com only has years from 2012 onward")
  }
  nodes <- paste0("#", type, "1 p")
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    message(paste0("scraping year: ", year))
    # navigate to page
    session <- try(read_html(paste0("https://hoop-math.com/", team, year, ".php")))
    #session <- try(read_html("https://hoop-math.com/Louisiana2013.php"))
    if (class(session)[1] == "try-error") {
      message(paste0("no data for: ", team, " during ", year))
      return(NULL)
    }
    # scrape table
    table_list <- session %>% 
      html_nodes(nodes) %>% 
      html_text() %>% 
      str_split(., "word")
    # reshape table
    df <- reshape_table(list_table = table_list, type = type, year = year) %>% 
      mutate(team = team)
    
    if (!exists("total_df")) {
      total_df <- df
    } else {
      total_df <- bind_rows(total_df, df)
    }
  }
  return(total_df)
}
# function to scrape hoop-math.com leaderboard tables
scrape_hoop_math_leader_tables <- function(years, type = c("OffDefLeader", "TransLeader"), off_def = c("Offense", "Defense")) {
  if (years[1] < 2012) {
    stop("hoop-math.com only has years from 2012 onward")
  }
  # navigate to correct page
  if (type == "TransLeader" & off_def == "Offense") {
    page <- "to"
  } else if (type == "TransLeader" & off_def == "Defense") {
    page <- "td"
  } else if (type == "OffDefLeader" & off_def == "Offense") {
    page <- "o"
  } else if (type == "OffDefLeader" & off_def == "Defense") {
    page <- "d"
  }
  for (i in 1:length(years)) {
    # get year
    year <- years[i]
    message(paste0("scraping year: ", year))
    # navigate to page
    session <- read_html(paste0("https://hoop-math.com/leader_", page, year, ".php"))
    # scrape table
    list_table <- session %>% 
      html_nodes(".sortable p") %>% 
      html_text() %>% 
      str_split(., "word")
    # reshape table
    df <- reshape_table(list_table = list_table, type = type, year = year) %>% 
      mutate(type = off_def)
    
    if (!exists("total_df")) {
      total_df <- df
    } else {
      total_df <- bind_rows(total_df, df)
    }
  }
  return(total_df)
}
# helper function to reshape lists of words into dataframes
reshape_table <- function(list_table, type, year) {
  extra <- F
  if (type == "OffTable") {
    variables <- 15L
    first_stat <- "fga"
    last_stat <- "ft_percent"
    extra = T
  } else if (type == "RebTable") {
    variables <- 8L
    first_stat <- "percent_of_total_at_rim_shots_coming_on_putbacks"
    last_stat <- "putback_fg_percent_2pt_jumpers"
    extra = T
  } else if (type %in% c("TransOTable", "TransDTable")) {
    variables <- 9L
    first_stat <- "percent_of_initial_attempts"
    last_stat <- "fg_percent_3pt"
  } else if (type == "DefTable") {
    variables <- 5L
    first_stat <- "percent_of_shots"
    last_stat <- "unblocked_fg_percent"
  } else if (type == "TransLeader") {
    variables <- 17L
    first_stat <- "percent_of_initial_fga_in_transition"
    last_stat <- "e_fg_percent_steal_11_30_s"
  } else if (type == "OffDefLeader") {
    variables <- 14L
    first_stat <- "e_fg_percent"
    last_stat <- "non_transtion_e_fg_percent"
  }
  
  df <- reshape2::melt(list_table) %>% 
    as_tibble() %>% 
    select(-L1) %>% 
    mutate(value = ifelse(value == "eFG% -- Score, 11-35", "eFG% -- Score, 11-35 s", value),
           value = ifelse(value == "eFG% -- Score, 11-30", "eFG% -- Score, 11-30 s", value),
           value = str_replace_all(value, "11-35 s", "11-30 s"),
           value = ifelse(value == "North Carolina St.", "NC State", value),
           group = (row_number() - 1) %/% variables) %>%
    group_by(group) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    arrange(row, group) %>%
    pivot_wider(names_from = "row",
                values_from = "value") %>% 
    select(-1) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    mutate(across(c(all_of(first_stat):all_of(last_stat)), ~ parse_number(., na = c("", "NA", "-", "---"))),
           across(c(all_of(first_stat):all_of(last_stat)), ~ ./100),
           season = paste0(as.numeric(year) - 1, "-", str_extract(year, "[012][0-9]$"))) 
  
  if (extra == T) {
    df <- df %>% 
      mutate(name = str_remove(name, " Jr.,")) %>% 
      separate(name, into = c("last", "first"), sep = ", ", fill = "left") %>% 
      mutate(name = ifelse(first == "Total", first, paste0(first, " ", last)),
             name = str_remove(name, " NA")) %>%
      select(name, everything(), -c(first, last))
  }
  if (type == "OffTable") {
    df <- df %>% 
      mutate(fga = 100 * fga,
             ts_percent = 100 * ts_percent)
  }
  if (type == "RebTable") {
    df <- df %>% 
      mutate(putbacks = as.numeric(putbacks)) %>% 
      filter(putbacks > 0)
  }
  return(df)
}
# function to combine hoop-math leaderboards for given years
combine_hoop_math_leaders <- function(years) {
  # scrape offensive leaderboard
  off <- scrape_hoop_math_leader_tables(years = years, 
                                        type = "OffDefLeader", 
                                        off_def = "Offense")
  # scrape defensive leaderboard
  def <- scrape_hoop_math_leader_tables(years = years, 
                                        type = "OffDefLeader", 
                                        off_def = "Defense")
  # scrape offensive transition leaderboard
  trans_o <- scrape_hoop_math_leader_tables(years = years, 
                                            type = "TransLeader", 
                                            off_def = "Offense") %>% 
    rename_with(cols = contains("_opp_score_"), ~ str_replace(., "_opp_score_", "_score_"))
  
  # scrape defensive transition leaderboard
  trans_d <- scrape_hoop_math_leader_tables(years = years, 
                                            type = "TransLeader", 
                                            off_def = "Defense")
  # combine leaderboards
  hoop_math <- off %>% 
    select(-type) %>% 
    left_join(def %>% select(-type), 
              by = c("team", "season"),
              suffix = c("_off", "_def")) %>% 
    select(team, season, everything()) %>% 
    left_join(trans_o %>% 
                select(-type) %>% 
                left_join(trans_d %>% select(-type), 
                          by = c("team", "season"),
                          suffix = c("_off", "_def")) %>% 
                select(team, season, everything()),
              by = c("team", "season")) %>% 
    select(-c(percent_of_total_fga_in_transition_off, percent_of_total_fga_in_transition_def,
              e_fg_percent_transition_off, e_fg_percent_transition_def,
              e_fg_percent_non_transition_off, e_fg_percent_non_transition_def))
  return(hoop_math)
}

# load packages
library(tidyverse)
library(broom)
library(ggiraph)
library(rvest)
library(modelr)

# load data
Duke201415teamstats <- read_csv("data/Duke201415teamstats.csv")
ShotChart <- read_csv("data/shot_chart_NN_SVM.csv")

# change from wide to long
long <- subset %>%
  dplyr::select(game_number, opponent, PPS, ePPS_NN, ePPS_SVM, ePPS, shot_making_NN, shot_making_SVM, shot_making) %>%
  gather(type, value, -game_number, -opponent, -shot_making_NN, -shot_making_SVM, -shot_making)

subset <- subset %>%
  mutate(pt_diff = pts - opp_pts)

# add ePPS and shot making
ShotChart <- ShotChart %>%
  mutate(EPS_NN = value * NN_probability,
         EPS_SVM = value * SVM_probability)

ePPS_NN <- ShotChart %>%
  dplyr::group_by(game) %>%
  dplyr::summarise(ePPS = mean(EPS_NN)) %>%
  dplyr::select(ePPS) %>%
  pull()

ePPS_SVM <- ShotChart %>%
  dplyr::group_by(game) %>%
  dplyr::summarise(ePPS = mean(EPS_SVM)) %>%
  dplyr::select(ePPS) %>%
  pull()

ePPS = (ePPS_NN + ePPS_SVM)/2

Duke201415teamstats <- Duke201415teamstats %>%
  mutate(PPS = (pts - ft) /fga)

subset <- Duke201415teamstats %>%
  filter(game_number %in% c(1:7, 9, 11:13, 16, 18, 22, 23, 26, 27, 29, 30, 32:35)) %>%
  dplyr::mutate(shot_making_NN = PPS - ePPS_NN,
                shot_making_SVM = PPS - ePPS_SVM,
                shot_making = PPS - ePPS)

subset <- subset %>%
  dplyr::mutate(ePPS_NN = ePPS_NN,
                ePPS_SVM = ePPS_SVM,
                ePPS = ePPS)

ShotChart <- ShotChart %>%
  dplyr::mutate(ePPS_NN = NN_probability * value,
                ePPS_SVM = SVM_probability * value,
                ePPS = (ePPS_NN +ePPS_SVM)/2)

# function to scrape gameIDs
get_game_ids <- function(Year) {
  url <- paste("https://www.espn.com/mens-college-basketball/team/schedule/_/id/150/season/", Year, sep = "")
  y <- read_html(url) %>%
    html_nodes(".ml4 a") %>%
    html_attr("href") %>%
    substr(57, 65)
  return(y)
}

gameIDs201415 <- get_game_ids("2015")
NCAATourn <- gameIDs201415[1:6]
gameIDs201415 <- gameIDs201415[-c(1:6)]
gameIDs201415[c(34:39)] <- NCAATourn
gameIDs201415 <- gameIDs201415[c(1:7, 9, 11:13, 16, 18, 22, 23, 26, 27, 29, 30, 32:35)]

long$gameID <- paste(gameIDs201415)
subset$gameID <- paste(gameIDs201415)

long$tooltip <- paste(long$type, ": ", round(long$value, 2), sep = "")
long$onclick <- sprintf("window.open(\"%s%s\")",
                        "https://www.espn.com/mens-college-basketball/game?gameId=", as.character(long$gameID))

game_by_game <- long %>%
  filter(type %in% c("ePPS", "PPS")) %>%
  ggplot(aes(x = reorder(opponent, game_number), y = value, group = type, 
             color = type, tooltip = tooltip, onclick = onclick)) + 
  geom_line() + 
  geom_point_interactive(aes(data_id = value), size = 2) +
  geom_label(aes(x = reorder(opponent, game_number), y = 0.75, label = round(shot_making, 1), 
                 fill = shot_making), color = "black", size = 2.5, label.size = 0.1, label.r = unit(0.1, "lines"), label.padding = unit(0.1, "lines")) +
  geom_text(aes(x = 3, y = 0.8, label = "Shot-Making:"), size = 3, inherit.aes = F) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", labels = c(low = "Worse: -0.3", mid = "Expected: 0", high = "Better: 0.3"), breaks = c(-0.2, 0, 0.2), limits = c(-0.3, 0.3)) +
  scale_color_manual(values = c("#001A57", "grey", "light blue", "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Duke's Game-by-Game Shooting", subtitle = "2014-15 Season",
       x = "Opponent", y = "Points Per Shot", color = "Type", fill = "Shot-Making")

ggiraph(code = {print(game_by_game)})

subset$tooltip <- paste("vs. ", subset$opponent, ": ", subset$pts, " - ", subset$opp_pts, sep = "")
subset$onclick <- sprintf("window.open(\"%s%s\")",
                          "https://www.espn.com/mens-college-basketball/game?gameId=", as.character(subset$gameID))

model <- lm(pts ~ shot_making, data = subset)
r2 <- paste("R-squared: ", round(100 * glance(model)$r.squared, 4), "%", sep = "") 
p_val <- paste("P-value: ", round(glance(model)$p.value, 10), sep = "")

shot_making_pt_diff <- subset %>%
  ggplot(aes(x = shot_making, y = pt_diff, color = result, tooltip = tooltip, onclick = onclick)) + 
  geom_point_interactive(aes(data_id = shot_making), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(x = -0.11, y = 50, label = "Worse Than Expected Shot-Making:"), size = 3.5, inherit.aes = F) +
  geom_text(aes(x = 0.11, y = 50, label = "Better Than Expected Shot-Making:"), size = 3.5, inherit.aes = F) +
  geom_text(aes(x = 0.25, y = 38, label = r2), size = 2.5, inherit.aes = F) +
  geom_text(aes(x = 0.25, y = 35, label = p_val), size = 2.5, inherit.aes = F) +
  scale_color_manual(values = c("red", "#001A57")) +
  geom_smooth(aes(x = shot_making, y = pt_diff), method = "lm", se = F, inherit.aes = F) + 
  labs(title = "Better shot-making leads to larger margins of victory", 
       subtitle = "Duke 2014-15 Season",
       x = "Shot-Making Index", y = "Point Differential", color = "Result")

ggiraph(code = {print(shot_making_pt_diff)}, width_svg = 8, height_svg = 7)

subset$tooltip_ast <- paste("vs. ", subset$opponent, ": ", subset$pts, " - ", subset$opp_pts,
                            ", Asts: ", subset$ast, sep = "")

model2 <- lm(shot_making ~ ast, data = subset)
r22 <- paste("R-squared: ", round(100 * glance(model2)$r.squared, 4), "%", sep = "") 
p_val2 <- paste("P-value: ", round(glance(model2)$p.value, 10), sep = "")

ast_shot_making <- subset %>%
  ggplot(aes(x = ast, y = shot_making, color = result, tooltip = tooltip_ast, onclick = onclick)) +
  geom_point_interactive(aes(data_id = shot_making), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(subset$ast), linetype = "dashed") +
  geom_text(aes(x = 25, y = -0.05, label = "Worse Than Expected Shot-Making:"), size = 3, inherit.aes = F) +
  geom_text(aes(x = 25, y = 0.05, label = "Better Than Expected Shot-Making:"), size = 3, inherit.aes = F) +
  geom_text(aes(x = mean(subset$ast) + 2, y = -0.2, label = paste("Mean: ", as.character(round(mean(subset$ast), 1)), " asts", sep = "")), size = 3, inherit.aes = F) +
  geom_text(aes(x = 26, y = 0.26, label = r22), size = 2.5, inherit.aes = F) +
  geom_text(aes(x = 26, y = 0.24, label = p_val2), size = 2.5, inherit.aes = F) +
  scale_color_manual(values = c("red", "#001A57")) +
  geom_smooth(aes(x = ast, y = shot_making), method = "lm", se = F, inherit.aes = F) + 
  labs(title = "More assists lead to better shot-making", subtitle = "Duke 2014-15 Season", 
       x = "Assists", y = "Shot-Making Index", color = "Result")

ggiraph(code = {print(ast_shot_making)})


