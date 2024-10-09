generateRegionPlot <- function(region_data, selected_years, region_name, selected_covers) {
  cover_colors <- c(
    "CCA" = "#e27c7c",
    "HARD CORALS" = "#a86464",
    "MACROALGAE" = "#6d4b4b",
    "RAMICRUSTA SPP." = "#503f3f",
    "SOFT CORALS" = "#333333",
    "SPONGES" = "#3c4e4b",
    "TURF ALGAE" = "#466964",
    "OTHER" = "#599e94"
  )
  
  region_data <- region_data %>%
    filter(cover_group %in% selected_covers & YEAR %in% selected_years)
  
  region_label <- region_name
  
  plot <- ggplot(region_data, aes(x = YEAR, y = avCvr, color = cover_group)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Cover (%)") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    annotate("text", x = max(region_data$YEAR), y = max(region_data$avCvr), label = region_label, hjust = 1, vjust = 1)
  
  generate_label <- function(cover_group, region_name, year) {
    case_when(
      cover_group == "HARD CORALS" & region_name == "Dry Tortugas" & (year %in% c(2014, 2016, 2018)) ~ "a",
      cover_group == "HARD CORALS" & region_name == "Dry Tortugas" & (year %in% c(2021, 2022)) ~ "b",
      cover_group == "MACROALGAE" & region_name == "Dry Tortugas" & (year %in% c(2014, 2016)) ~ "a,c",
      cover_group == "MACROALGAE" & region_name == "Dry Tortugas" & year == 2018 ~ "b",
      cover_group == "MACROALGAE" & region_name == "Dry Tortugas" & year == 2021 ~ "a",
      cover_group == "MACROALGAE" & region_name == "Dry Tortugas" & year == 2022 ~ "c",
      cover_group == "HARD CORALS" & region_name == "Florida Keys" & (year %in% c(2014, 2016)) ~ "a",
      cover_group == "HARD CORALS" & region_name == "Florida Keys" & year == 2018 ~ "b",
      cover_group == "HARD CORALS" & region_name == "Florida Keys" & year == 2021 ~ "*",
      cover_group == "HARD CORALS" & region_name == "Florida Keys" & year == 2022 ~ "c",
      cover_group == "MACROALGAE" & region_name == "Florida Keys" ~ " ",
      cover_group == "HARD CORALS" & region_name == "Southeast Florida" & (year %in% c(2014, 2016)) ~ "a",
      cover_group == "HARD CORALS" & region_name == "Southeast Florida" & year == 2018 ~ "b",
      cover_group == "HARD CORALS" & region_name == "Southeast Florida" & (year %in% c(2021, 2022)) ~ "b",
      cover_group == "MACROALGAE" & region_name == "Southeast Florida" & year == 2014 ~ "a",
      cover_group == "MACROALGAE" & region_name == "Southeast Florida" & year == 2016 ~ "b",
      cover_group == "MACROALGAE" & region_name == "Southeast Florida" & year == 2018 ~ "b",
      cover_group == "MACROALGAE" & region_name == "Southeast Florida" & year == 2021 ~ "c",
      cover_group == "MACROALGAE" & region_name == "Southeast Florida" & year == 2022 ~ "d",
      TRUE ~ " "
    )
  }
  
  labels <- mapply(generate_label, region_data$cover_group, rep(region_name, nrow(region_data)), region_data$YEAR)
  
  plot <- plot + 
    geom_text(data = region_data, aes(x = YEAR, y = avCvr, label = labels), hjust = -0.5)
  
  plot <- plot +
    geom_errorbar(aes(ymin = avCvr - SE, ymax = avCvr + SE), width = .1)
  
  return(plot)
}
