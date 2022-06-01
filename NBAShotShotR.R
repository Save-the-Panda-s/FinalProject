packages = c("shiny", "ggplot2", "hexbin", "dplyr", "httr", "jsonlite")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("ballr", "bobbypaul")

fetch_shots_by_player_id_and_season = function(player_id, season, season_type = "Regular Season") {
  req(player_id, season, season_type)
  
  request = GET(
    "https://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      Season = season,
      SeasonType = season_type,
      PlayerPosition = "",
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )
  
  stop_for_status(request)
  
  data = content(request)
  
  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))
  
  if (length(raw_shots_data) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }
  
  shots = as_tibble(shots)
  names(shots) = col_names
  
  shots = mutate(shots,
                 loc_x = -as.numeric(as.character(loc_x)) / 10,
                 loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                 shot_distance = as.numeric(as.character(shot_distance)),
                 shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                 shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                 shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                 shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                 game_date = as.Date(game_date, format = "%Y%m%d")
  )
  
  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = as_tibble(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
                           fga = as.numeric(as.character(fga)),
                           fgm = as.numeric(as.character(fgm)),
                           fg_pct = as.numeric(as.character(fg_pct)),
                           shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )
  
  return(list(player = shots, league_averages = league_averages))
}

default_shots = fetch_shots_by_player_id_and_season(
  default_player$person_id,
  default_season,
  default_season_type
)
fetch_shots_by_player_id = function(player_id, bigquery_project_id) {
  fetch_shots("player_id", player_id, bigquery_project_id)
}

fetch_shots_by_team_id = function(team_id, bigquery_project_id) {
  fetch_shots("team_id", team_id, bigquery_project_id)
}

fetch_shots = function(column_name, column_value, bigquery_project_id) {
  req(column_name, column_value, bigquery_project_id)
  
  if (!(column_name %in% c("player_id", "team_id"))) {
    stop("invalid column_name")
  }
  
  shots_sql = paste0("
    SELECT
      tournament,
      tournament_type,
      period,
      elapsed_time_sec,
      team_basket,
      timestamp,
      event_coord_x,
      event_coord_y,
      event_type,
      type,
      shot_made,
      shot_type,
      shot_subtype,
      three_point_shot,
      points_scored
    FROM [bigquery-public-data:ncaa_basketball.mbb_pbp_sr]
    WHERE type = 'fieldgoal'
      AND event_coord_x IS NOT NULL
      AND event_coord_y IS NOT NULL
      AND ", column_name, " = '", column_value, "'
  ")
  
  shots_raw = query_exec(shots_sql, bigquery_project_id) %>%
    as_data_frame()
  
  shots = mutate(shots_raw,
                 # convert all shots to same side of court
                 event_coord_x = case_when(
                   team_basket == "right" ~ (94 * 12) - event_coord_x,
                   TRUE ~ event_coord_x
                 ),
                 event_coord_y = case_when(
                   team_basket == "right" ~ (50 * 12) - event_coord_y,
                   TRUE ~ event_coord_y
                 ),
                 
                 # convert NCAA (x, y) coordinates to BallR (x, y) coordinates
                 loc_x = 25 - (event_coord_y / 12),
                 loc_y = (event_coord_x / 12),
                 
                 # approximate zones as provided by NBA Stats API
                 shot_distance = sqrt(loc_x ^ 2 + (loc_y - hoop_center_y) ^ 2),
                 shot_zone_range = case_when(
                   shot_distance < 8 ~ "Less Than 8 ft.",
                   shot_distance < 16 ~ "8-16 ft.",
                   shot_distance < 24 ~ "16-24 ft.",
                   TRUE ~ "24+ ft."
                 ),
                 shot_angle = acos(loc_x / shot_distance) * 180 / pi,
                 shot_zone_area = case_when(
                   shot_angle < 36 ~ "Right Side(R)",
                   shot_angle < 72 ~ "Right Side Center(RC)",
                   shot_angle < 108 ~ "Center(C)",
                   shot_angle < 144 ~ "Left Side Center(LC)",
                   TRUE ~ "Left Side(L)"
                 ),
                 shot_zone_basic = case_when(
                   shot_distance > 40 ~ "Backcourt",
                   shot_distance < 4 ~ "Restricted Area",
                   loc_x > (-outer_key_width / 2) & loc_x < (outer_key_width / 2) & loc_y < key_height ~ "In The Paint (Non-RA)",
                   three_point_shot & shot_angle < 36 ~ "Right Corner 3",
                   three_point_shot & shot_angle > 144 ~ "Left Corner 3",
                   three_point_shot ~ "Above the Break 3",
                   TRUE ~ "Mid-Range"
                 ),
                 shot_made_numeric = as.numeric(shot_made),
                 shot_made_flag = factor(shot_made, levels = c(TRUE, FALSE), labels = c("made", "missed")),
                 shot_value = ifelse(three_point_shot, 3, 2),
                 year = case_when(
                   month(timestamp) <= 4 ~ year(timestamp) - 1,
                   TRUE ~ year(timestamp)
                 ),
                 season = paste(year, substr(year + 1, 3, 4), sep = "-"),
                 game_date = as.Date(timestamp)
  )
  
  return(shots)
}
generate_heatmap_chart = function(shots, base_court, court_theme = court_themes$dark) {
  base_court +
    stat_density_2d(
      data = shots,
      aes(x = loc_x, y = loc_y, fill = stat(density / max(density))),
      geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
    ) +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    scale_fill_viridis_c(
      "Shot Frequency    ",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("lower", "higher"),
      option = "inferno",
      guide = guide_colorbar(barwidth = 15)
    ) +
    theme(legend.text = element_text(size = rel(0.6)))
}
# from ggplot2 hexbin.R: https://github.com/hadley/ggplot2/blob/master/R/hexbin.R
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value)
    )
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    summarize(attempts = n()) %>%
    ungroup() %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    data_frame(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

calculate_hexbins_from_shots = function(shots, binwidths = c(1, 1), min_radius_factor = 0.6, fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) {
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = group_by(shots, shot_zone_range, shot_zone_area)
  
  zone_stats = grouped_shots %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(shot_made_numeric),
      zone_points_scored = sum(shot_made_numeric * shot_value),
      zone_points_per_shot = mean(shot_made_numeric * shot_value)
    )
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("shot_zone_area", "shot_zone_range")
  
  hex_data = hex_data %>%
    inner_join(zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(hex_data = hex_data, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
}

generate_hex_chart = function(hex_data, base_court, court_theme = court_themes$dark, metric = sym(bounded_fg_diff), alpha_range = c(0.85, 0.98)) {
  if (length(hex_data) == 0) {
    return(base_court)
  }
  
  if (metric == "bounded_fg_pct") {
    fill_limit = hex_data$fg_pct_limits
    fill_label = "FG%"
    label_formatter = percent_formatter
  } else if (metric == "bounded_points_per_shot") {
    fill_limit = hex_data$pps_limits
    fill_label = "Points Per Shot"
    label_formatter = points_formatter
  } else {
    stop("invalid metric")
  }
  
  base_court +
    geom_polygon(
      data = hex_data$hex_data,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id,
        fill = !!metric,
        alpha = hex_attempts
      ),
      size = court_theme$hex_border_size,
      color = court_theme$hex_border_color
    ) +
    scale_fill_viridis_c(
      paste0(fill_label, "   "),
      limit = fill_limit,
      labels = label_formatter,
      guide = guide_colorbar(barwidth = 15)
    ) +
    scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    theme(legend.text = element_text(size = rel(0.6)))
}

default_player_name = "Trae Young"

fetch_all_players_and_teams = function(bigquery_project_id) {
  players_sql = "
    SELECT
      player_id,
      player_full_name,
      team_market,
      team_name,
      team_id,
      team_alias,
      MAX(timestamp) AS max_timestamp
    FROM [bigquery-public-data:ncaa_basketball.mbb_pbp_sr]
    WHERE type = 'fieldgoal'
      AND event_coord_x IS NOT NULL
      AND event_coord_y IS NOT NULL
    GROUP BY player_id, player_full_name, team_market, team_name, team_id, team_alias
  "
  
  players_raw = query_exec(players_sql, bigquery_project_id) %>%
    as_tibble() %>%
    mutate(team_full_name = paste(team_market, team_name))
  
  players_with_unique_names = players_raw %>%
    group_by(player_full_name) %>%
    summarize(
      n = n_distinct(player_id),
      team = paste0(team_full_name, collapse = ", "),
      player_id = first(player_id)
    ) %>%
    ungroup() %>%
    filter(n == 1) %>%
    select(player_id, player_full_name, team)
  
  players_with_non_unique_names = players_raw %>%
    filter(!(player_id %in% players_with_unique_names$player_id)) %>%
    transmute(
      player_id = player_id,
      player_full_name = paste0(player_full_name, " (", team_market, ")"),
      team = team_full_name
    )
  
  players = bind_rows(players_with_unique_names, players_with_non_unique_names) %>%
    group_by(player_id) %>%
    arrange(player_full_name) %>%
    summarize(
      name = first(player_full_name),
      team = first(team)
    ) %>%
    ungroup() %>%
    mutate(lower_name = tolower(name)) %>%
    arrange(lower_name)
  
  teams = players_raw %>%
    group_by(team_id, team_full_name) %>%
    summarize(max_ts = max(max_timestamp)) %>%
    arrange(team_id, desc(max_ts)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(team_id, name = team_full_name) %>%
    mutate(lower_name = tolower(name)) %>%
    arrange(lower_name)
  
  return(list(players = players, teams = teams))
}
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 12
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 20.75
three_point_side_radius = 20.75
three_point_side_height = hoop_center_y

plot_court = function(court_theme = court_themes$dark) {
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  court_points <<- court_points
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}
generate_scatter_chart = function(shots, base_court, court_theme = court_themes$dark, alpha = 0.8, size = 2.5) {
  base_court +
    geom_point(
      data = shots,
      aes(x = loc_x, y = loc_y, color = shot_made_flag),
      alpha = alpha, size = size
    ) +
    scale_color_manual(
      "",
      values = c(made = court_theme$made, missed = court_theme$missed)
    )
}
library(shiny)

shinyServer(function(input, output, session) {
  bigquery_project_id = reactive({
    input$bigquery_project_id
  })
  
  output$bigquery_notice = renderUI({
    if (bigquery_project_id() == "") {
      h2("Enter your BigQuery project info to get started")
    }
  })
  
  players_and_teams = reactive({
    req(bigquery_project_id())
    
    withProgress({
      fetch_all_players_and_teams(bigquery_project_id())
    }, message = "Fetching players data...")
  })
  
  players = reactive({
    req(players_and_teams())
    players_and_teams()$players
  })
  
  teams = reactive({
    req(players_and_teams())
    players_and_teams()$teams
  })
  
  output$shots_select_input = renderUI({
    req(input$view_shots_by, players(), teams())
    
    if (input$view_shots_by == "Player") {
      input_id = "player_name"
      label = "Player"
      choices = c("Enter a player..." = "", players()$name)
      selected = ifelse(is.null(input$player_name), default_player_name, input$player_name)
    } else if (input$view_shots_by == "Team") {
      input_id = "team_name"
      label = "Team"
      choices = c("Enter a team..." = "", teams()$name)
      selected = ifelse(is.null(input$team_name), "", input$team_name)
    }
    
    selectizeInput(inputId = input_id,
                   label = label,
                   choices = choices,
                   selected = selected,
                   options = list(
                     selectOnTab = TRUE,
                     maxOptions = 20000,
                     onDropdownOpen = I("function() { this.clear('silent'); }")
                   ))
  })
  
  current_player = reactive({
    req(bigquery_project_id(), input$player_name, input$view_shots_by == "Player")
    filter(players(), lower_name == tolower(input$player_name))
  })
  
  current_team = reactive({
    req(bigquery_project_id(), input$team_name, input$view_shots_by == "Team")
    filter(teams(), lower_name == tolower(input$team_name))
  })
  
  current_selection = reactive({
    req(bigquery_project_id(), input$view_shots_by)
    
    if (input$view_shots_by == "Player") {
      req(current_player())
      current_player()
    } else if (input$view_shots_by == "Team") {
      req(current_team())
      current_team()
    }
  })
  
  current_selection_seasons = reactive({
    req(shots())
    shots()$season %>% unique() %>% sort()
  })
  
  court_theme = reactive({
    req(input$court_theme)
    court_themes[[tolower(input$court_theme)]]
  })
  
  court_plot = reactive({
    req(court_theme())
    plot_court(court_theme = court_theme())
  })
  
  current_seasons = reactive({
    if (is.null(input$season_filter)) {
      current_selection_seasons()
    } else {
      input$season_filter
    }
  })
  
  update_season_input = observe({
    updateSelectInput(session,
                      "season_filter",
                      choices = rev(current_selection_seasons()),
                      selected = NULL)
  })
  
  shots = reactive({
    req(bigquery_project_id(), input$view_shots_by)
    
    if (input$view_shots_by == "Player") {
      req(current_player())
      fetch_shots_by_player_id(current_player()$player_id, bigquery_project_id())
    } else if (input$view_shots_by == "Team") {
      req(current_team())
      fetch_shots_by_team_id(current_team()$team_id, bigquery_project_id())
    }
  })
  
  filtered_shots = reactive({
    req(input$shot_result_filter, shots())
    
    filter(shots(),
           input$shot_result_filter == "all" | shot_made_flag == input$shot_result_filter,
           shot_zone_basic != "Backcourt",
           is.null(input$shot_zone_basic_filter) | shot_zone_basic %in% input$shot_zone_basic_filter,
           is.null(input$shot_zone_angle_filter) | shot_zone_area %in% input$shot_zone_angle_filter,
           is.null(input$shot_distance_filter) | shot_zone_range %in% input$shot_distance_filter,
           is.null(input$season_filter) | season %in% input$season_filter,
           is.na(input$date_range[1]) | game_date >= input$date_range[1],
           is.na(input$date_range[2]) | game_date <= input$date_range[2]
    )
  })
  
  hexbin_data = reactive({
    req(filtered_shots(), shots(), hexbinwidths(), input$hex_radius)
    
    calculate_hexbins_from_shots(filtered_shots(),
                                 binwidths = hexbinwidths(),
                                 min_radius_factor = input$hex_radius)
  })
  
  output$hexbinwidth_slider = renderUI({
    req(input$chart_type == "Hexagonal")
    
    sliderInput("hexbinwidth",
                "Hexagon Size (feet)",
                min = 0.5,
                max = 4,
                value = 1.5,
                step = 0.25)
  })
  
  hexbinwidths = reactive({
    req(input$hexbinwidth)
    rep(input$hexbinwidth, 2)
  })
  
  output$hex_radius_slider = renderUI({
    req(input$chart_type == "Hexagonal")
    
    sliderInput("hex_radius",
                "Min Hexagon Size Adjustment",
                min = 0,
                max = 1,
                value = 0.4,
                step = 0.05)
  })
  
  alpha_range = reactive({
    req(input$chart_type == "Hexagonal", input$hex_radius)
    max_alpha = 0.98
    min_alpha = max_alpha - 0.25 * input$hex_radius
    c(min_alpha, max_alpha)
  })
  
  output$hex_metric_buttons = renderUI({
    req(input$chart_type == "Hexagonal")
    
    selectInput("hex_metric",
                "Hexagon Colors",
                choices = c("FG%" = "bounded_fg_pct",
                            "Points Per Shot" = "bounded_points_per_shot"),
                selected = "bounded_fg_pct",
                selectize = FALSE)
  })
  
  output$scatter_size_slider = renderUI({
    req(input$chart_type == "Scatter")
    
    sliderInput("scatter_size",
                "Dot size",
                min = 1,
                max = 10,
                value = 4,
                step = 0.5)
  })
  
  output$scatter_alpha_slider = renderUI({
    req(input$chart_type == "Scatter")
    
    sliderInput("scatter_alpha",
                "Opacity",
                min = 0.01,
                max = 1,
                value = 0.7,
                step = 0.01)
  })
  
  shot_chart = reactive({
    req(filtered_shots(), input$chart_type, court_plot())
    
    filters_applied()
    
    if (input$chart_type == "Hexagonal") {
      req(input$hex_metric, alpha_range())
      
      generate_hex_chart(
        hex_data = hexbin_data(),
        base_court = court_plot(),
        court_theme = court_theme(),
        metric = sym(input$hex_metric),
        alpha_range = alpha_range()
      )
    } else if (input$chart_type == "Scatter") {
      req(input$scatter_alpha, input$scatter_size)
      
      generate_scatter_chart(
        filtered_shots(),
        base_court = court_plot(),
        court_theme = court_theme(),
        alpha = input$scatter_alpha,
        size = input$scatter_size
      )
    } else if (input$chart_type == "Heat Map") {
      generate_heatmap_chart(
        filtered_shots(),
        base_court = court_plot(),
        court_theme = court_theme()
      )
    } else {
      stop("invalid chart type")
    }
  })
  
  output$shot_chart_css = renderUI({
    req(court_theme())
    tags$style(paste0(
      ".shot-chart-container {",
      "background-color: ", court_theme()$court, "; ",
      "color: ", court_theme()$text,
      "}"
    ))
  })
  
  output$chart_header = renderText({
    req(current_selection())
    current_selection()$name
  })
  
  output$chart_header_info = renderText({
    req(shots())
    paste(current_seasons(), collapse = ", ")
  })
  
  output$chart_header_player_team = renderText({
    req(shots(), current_player(), input$view_shots_by == "Player")
    current_player()$team
  })
  
  output$shot_chart_footer = renderUI({
    req(shot_chart())
    
    tags$div(
      "Data via Sportradar on Google BigQuery",
      tags$br(),
      "toddwschneider.com/ballr"
    )
  })
  
  output$download_link = renderUI({
    req(shot_chart(), current_selection())
    
    filename_parts = c(
      current_selection()$name,
      "Shot Chart",
      input$chart_type
    )
    fname = paste0(gsub("_", "-", gsub(" ", "-", tolower(filename_parts))), collapse = "-")
    
    tags$a("Download Chart",
           href = "#",
           class = "download-shot-chart",
           "data-filename" = paste0(fname, ".png"))
  })
  
  output$court = renderPlot({
    req(shot_chart())
    withProgress({
      shot_chart()
    }, message = "Calculating...")
  }, height = 600, width = 800, bg = "transparent")
  
  filters_applied = reactive({
    req(filtered_shots())
    filters = list()
    
    if (!is.null(input$shot_zone_basic_filter)) {
      filters[["Zone"]] = paste("Zone:", paste(input$shot_zone_basic_filter, collapse = ", "))
    }
    
    if (!is.null(input$shot_zone_angle_filter)) {
      filters[["Angle"]] = paste("Angle:", paste(input$shot_zone_angle_filter, collapse = ", "))
    }
    
    if (!is.null(input$shot_distance_filter)) {
      filters[["Distance"]] = paste("Distance:", paste(input$shot_distance_filter, collapse = ", "))
    }
    
    if (input$shot_result_filter != "all") {
      filters[["Result"]] = paste("Result:", input$shot_result_filter)
    }
    
    if (!is.na(input$date_range[1]) | !is.na(input$date_range[2])) {
      dates = format(input$date_range, "%m/%d/%y")
      dates[is.na(dates)] = ""
      
      filters[["Dates"]] = paste("Dates:", paste(dates, collapse = "–"))
    }
    
    filters
  })
  
  output$shot_filters_applied = renderUI({
    req(length(filters_applied()) > 0)
    
    div(class = "shot-filters",
        tags$h5("Shot Filters Applied"),
        lapply(filters_applied(), function(text) {
          div(text)
        })
    )
  })
  
  output$summary_stats_header = renderText({
    req(current_selection())
    paste(current_selection()$name, "Summary Stats")
  })
  
  output$summary_stats = renderUI({
    req(filtered_shots(), shots())
    req(nrow(filtered_shots()) > 0)
    
    player_zone = filtered_shots() %>%
      group_by(shot_zone_basic) %>%
      summarize(fgm = sum(shot_made_numeric),
                fga = n(),
                pct = mean(shot_made_numeric),
                pct_as_text = fraction_to_percent_format(pct),
                points_per_shot = mean(shot_value * shot_made_numeric)) %>%
      arrange(desc(fga), desc(fgm)) %>%
      ungroup()
    
    # NCAA data does not include league averages
    merged = player_zone
    
    overall = summarize(merged,
                        total_fgm = sum(fgm),
                        total_fga = sum(fga),
                        pct = total_fgm / total_fga,
                        pct_as_text = fraction_to_percent_format(pct),
                        points_per_shot = sum(points_per_shot * fga) / sum(fga)
    )
    
    html = list(div(class = "row headers",
                    span(class = "col-xs-4 col-md-3 zone-label", "Zone"),
                    span(class = "col-xs-2 numeric", "FGM"),
                    span(class = "col-xs-2 numeric", "FGA"),
                    span(class = "col-xs-2 numeric", "FG%"),
                    span(class = "col-xs-2 numeric", "Pts/Shot")
    ))
    
    for (i in 1:nrow(merged)) {
      html[[i + 2]] = div(class = paste("row", ifelse(i %% 2 == 0, "even", "odd")),
                          span(class = "col-xs-4 col-md-3 zone-label", merged$shot_zone_basic[i]),
                          span(class = "col-xs-2 numeric", merged$fgm[i]),
                          span(class = "col-xs-2 numeric", merged$fga[i]),
                          span(class = "col-xs-2 numeric", merged$pct_as_text[i]),
                          span(class = "col-xs-2 numeric", round(merged$points_per_shot[i], 2))
      )
    }
    
    html[[length(html) + 1]] = div(class = "row overall",
                                   span(class = "col-xs-4 col-md-3 zone-label", "Overall"),
                                   span(class = "col-xs-2 numeric", overall$total_fgm),
                                   span(class = "col-xs-2 numeric", overall$total_fga),
                                   span(class = "col-xs-2 numeric", overall$pct_as_text),
                                   span(class = "col-xs-2 numeric", round(overall$points_per_shot, 2))
    )
    
    html
  })
})