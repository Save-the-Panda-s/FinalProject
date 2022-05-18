#' Information for Division I college basketball teams
#'
#' A dataset containing the team ID's, various team names, colors, and logos
#' of Division I college basketball teams
#'
#' @format A data frame with 359 rows and 24 variables:
#' \describe{
#'   \item{espn_id}{ESPN unique team ID}
#'   \item{game_zone_id}{GameZone unique team ID}
#'   \item{team_name}{A common team name}
#'   \item{game_zone}{GameZone team name}
#'   \item{espn}{ESPN team name}
#'   \item{ncaa}{NCAA team name}
#'   \item{espn_pbp}{ESPN team name in Play-by-Play data}
#'   \item{warren_nolan}{Warren Nolan team name}
#'   \item{trank}{Barttorvik Trank team name}
#'   \item{name_247}{247 Sport team name}
#'   \item{sref_link}{sports-reference.com team link}
#'   \item{sref_name}{sports-reference.com team name}
#'   \item{espn_pbp}{NCAA conference}
#'   \item{color_espn}{ESPN primary color}
#'   \item{alt_color_espn}{ESPN alternate color}
#'   \item{team_logo_espn}{ESPN logo url}
#'   \item{alt_team_logo_espn}{ESPN alternate logo url}
#'   \item{primary_color}{Primary color}
#'   \item{secondary_color}{Secondary color}
#'   \item{tertiary_color}{Tertiary color}
#'   \item{color_4}{Color 4}
#'   \item{color_5}{Color 5}
#'   \item{color_6}{Color 6}
#'   \item{logo_url}{Wikimedia logo url}
#' }
"mbb_team_info"

#' An object for shot chart plotting
#'
#' A list which contains a base court for basketball shot location plotting
#' for the old college three point line (20.75 feet all around)
#'
#' @format A list with 9 items
"base_court_old"

#' An object for shot chart plotting
#'
#' A list which contains a base court for basketball shot location plotting
#'
#' @format A list with 9 items
"base_court"
#' @param game_id unique GameZone GameID
#' @export
#'
#' @examples
#' \dontrun{
#'  gamezone_mbb_boxscore(game_id = 2316023)
#' }
#'
gamezone_mbb_boxscore <- function(game_id) {
  # some error checks
  if (is.na(game_id) || is.null(game_id)) {
    usethis::ui_oops("GameID is missing...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }
  
  if (length(game_id) != 1) {
    usethis::ui_oops("Passed in multiple GameID's. This function returns the box score of one game at a time...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }
  
  usethis::ui_todo(paste0("Scraping box score for GameID: ", game_id))
  
  # formulate url
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/events/"
  append <- "/boxscore"
  url <- paste0(base_url, game_id, append)
  
  # extract json
  json <- try(jsonlite::fromJSON(url),
              silent = T)
  
  if ("try-error" %in% class(json) || length(json[["Boxscore"]]) == 0) {
    usethis::ui_oops("GameZone does not have box score data for this game...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }
  
  # extract box score
  box <- json[["Boxscore"]]
  
  # extract date
  date <- json[["DateTime"]]
  date <- as.Date(unlist(stringr::str_split(date, " \\| "))[2],
                  format = "%B %d, %Y")
  
  # extract season
  season <- json[["Season"]]
  season <- paste0(season, "-", as.numeric(stringr::str_sub(season, start = 3)) + 1)
  
  # function to extract home and away team
  extract_home_away <- function(data, home = T) {
    data %>%
      purrr::map_df(., `[`) %>%
      dplyr::select(-c(dplyr::any_of("Record"))) %>%
      janitor::clean_names() %>%
      dplyr::distinct(.data$id, .keep_all = T) %>%
      dplyr::transmute(team_abbr = .data$abbr,
                       team_id = .data$id,
                       team = .data$location,
                       nickname = .data$name,
                       score = .data$total,
                       location = ifelse(home == T, "home", "away"))
  }
  
  # extract team information
  teams <- dplyr::bind_rows(extract_home_away(json[["Home"]], home = T),
                            extract_home_away(json[["Away"]], home = F))
  
  unnested <- box %>%
    tidyr::unnest(.data$Players)
  
  boxscore <- unnested[["Player"]] %>%
    dplyr::bind_cols(unnested %>%
                       dplyr::select(-.data$Player)) %>%
    tidyr::unnest(.data$Stats) %>%
    tidyr::pivot_wider(names_from = .data$Abbr,
                       values_from = .data$Game) %>%
    janitor::clean_names() %>%
    tidyr::separate(.data$or_tr, into = c("o_reb", "tot_reb"),
                    sep = "-", convert = T, remove = T) %>%
    dplyr::left_join(teams,
                     by = "team_id") %>%
    dplyr::transmute(season = season, date = date,
                     .data$team_id, .data$team, .data$location,
                     .data$score, player_id = .data$id,
                     jersey_number = as.numeric(.data$uniform),
                     position = .data$pos, starter = .data$is_starter,
                     name = paste0(.data$first_name, " ", .data$last_name),
                     dplyr::across(c(.data$min, .data$pts), as.numeric),
                     .data$fg_fga, .data$ft_fta, .data$x3p_3pa, .data$o_reb, .data$tot_reb,
                     dplyr::across(.data$ast:.data$pf, as.numeric))
  
  message <- paste0("Completed box score collection for GameID: ", game_id, "\n",
                    teams$team[teams$location == "away"], " @ ",
                    teams$team[teams$location == "home"])
  usethis::ui_done(message)
  
  return(boxscore)
}

@param team team name
#' @param season season (of the form "2020-21")
#' @export
#'
#' @examples
#' \dontrun{
#'  gamezone_mbb_team_stats(team = "Duke", season = "2018-19")
#' }
#'
gamezone_mbb_team_stats <- function(team, season = "2020-21") {
  # find year
  year <- stringr::str_sub(season, end = 4)
  
  # find team id
  team_id <- gamezoneR::mbb_team_info %>%
    tidyr::pivot_longer(cols = c(.data$team_name:.data$sref_name),
                        names_to = "organization",
                        values_to = "team_name") %>%
    dplyr::distinct(.data$team_name, .keep_all = T) %>%
    dplyr::select(.data$conference, .data$organization,
                  .data$game_zone_id, .data$team_name,
                  dplyr::everything()) %>%
    dplyr::filter(.data$team_name == team) %>%
    dplyr::pull(.data$game_zone_id)
  
  # error checking
  if (rlang::is_empty(team_id)) {
    usethis::ui_oops("Team name is not found in dictionary...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }
  
  message <- paste0("Scraping ", season, " season summary statistics for: ", team)
  usethis::ui_info(message)
  
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/team/"
  append <- "/seasonstats/"
  url <- paste0(base_url, team_id, append, year)
  
  json <- try(jsonlite::fromJSON(url, flatten = T),
              silent = T)
  
  if ("try-error" %in% class(json)) {
    usethis::ui_oops(paste0("No ", season, " season statistics available for: ", team))
    usethis::ui_info("Returning NULL")
    return(NULL)
  }
  
  team_stats <- json[["TeamStats"]] %>%
    dplyr::select(-.data$Name) %>%
    dplyr::mutate(Game = as.numeric(.data$Game)) %>%
    tidyr::pivot_wider(names_from = .data$Abbr,
                       values_from = .data$Game) %>%
    janitor::clean_names() %>%
    dplyr::mutate(season = season,
                  team_id = team_id) %>%
    dplyr::select(.data$season, .data$team_id, dplyr::everything()) %>%
    dplyr::as_tibble()
  
  return(team_stats)
}

#' Get GameZone player statistical summary for a given player
#' @author Jack Lichtenstein
#' @param player_id playerID found in a play-by-play or box score dataframe returned by
#' \code{gamezone_mbb_pbp} or \code{gamezone_mbb_boxscore}, respectively
#' @export
#'
#' @examples
#' \dontrun{
#'  gamezone_mbb_player_stats(player_id = 1061640)
#' }
#'
gamezone_mbb_player_stats <- function(player_id) {
  # construct urls
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/player/"
  append <- "/seasonstats"
  url1 <- paste0(base_url, player_id)
  url2 <- paste0(base_url, player_id, append)
  
  json1 <- try(jsonlite::fromJSON(url1, flatten = T),
               silent = T)
  
  json2 <- try(jsonlite::fromJSON(url2, flatten = T),
               silent = T)
  
  if ("try-error" %in% class(json1) || "try-error" %in% class(json2)) {
    usethis::ui_oops(paste0("No ", season, " season statistics available for: ", player_id))
    usethis::ui_info("Returning NULL")
    return(NULL)
  }
  
  # extract player info
  player_info <- json1 %>%
    purrr::map_df(., `[`) %>%
    janitor::clean_names() %>%
    dplyr::transmute(player_id = id,
                     name = paste0(first_name, " ", last_name),
                     jersey_number = as.numeric(uniform),
                     position, height, weight = as.numeric(weight), position,
                     date_of_birth = as.Date(date_of_birth, format = "%m/%d/%Y"),
                     hometown, team_id, team_name)
  
  # extract player season summary stats
  player_stats <- json2[["Stats"]] %>%
    dplyr::select(-.data$Name) %>%
    dplyr::mutate(Game = as.numeric(.data$Game),
                  Abbr = stringr::str_remove(Abbr, "Stat_")) %>%
    tidyr::pivot_wider(names_from = .data$Abbr,
                       values_from = .data$Game) %>%
    janitor::clean_names() %>%
    dplyr::bind_cols(player_info) %>%
    dplyr::select(dplyr::any_of(c(
      "player_id", "name", "jersey_num")),
      dplyr::everything()) %>%
    dplyr::as_tibble()
  
  return(player_stats)
}

# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")

# Initialize first available season in data repository
first_season <- "2008-09"

# Find most recent college basketball season
most_recent_season <- function() {
  end <- ifelse(as.numeric(substr(Sys.Date(), 6, 7)) >= 11, # if greater or equal to November
                as.numeric(substr(Sys.Date(), 1, 4)) + 1,
                as.numeric(substr(Sys.Date(), 1, 4)))
  
  season <- paste0(end - 1, "-", substr(end, 3, 4))
  return(season)
}

# Find available college basketball seasons in data repository
available_seasons <- function() {
  last_season <- most_recent_season()
  
  seasons <- as.numeric(substr(first_season, 1, 4)):as.numeric(substr(last_season, 1, 4))
  
  return(paste0(seasons, "-", substr(seasons + 1, 3, 4)))
}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes

BuildType: Package
PackageUseDevtools: Yes
PackageInstallArgs: --no-multiarch --with-keep.source