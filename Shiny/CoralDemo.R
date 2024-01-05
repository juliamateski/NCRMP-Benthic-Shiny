
generateCoralPlot <- function(region, selected_year, weightedmeans, om_spp, s_spp, rm_spp, species_list) {
  selected_year <- as.double(selected_year)
  
  a <- weightedmeans %>% 
    dplyr::select(REGION, YEAR, SPECIES_CD, avDen, CV, SE) %>% 
    dplyr::rename("SE_avDen" = SE)
  
  b <- om_spp %>% 
    dplyr::mutate(SPECIES_CD = SPECIES_NAME) %>% 
    dplyr::select(REGION, YEAR, SPECIES_CD, avMort, SE) %>% 
    dplyr::rename("avMortOld" = avMort,
                  "SE_avMortOld" = SE)
  c <- s_spp %>% 
    dplyr::mutate(SPECIES_CD = SPECIES_NAME) %>% 
    dplyr::select(REGION, YEAR, SPECIES_CD, avMaxdiam, SE_maxdiam)
  
  d <- rm_spp %>% 
    dplyr::mutate(SPECIES_CD = SPECIES_NAME) %>% 
    dplyr::select(REGION, YEAR, SPECIES_CD, avMort, SE) %>% 
    dplyr::rename("avMortRec" = avMort,
                  "SE_avMortRec" = SE)
  
  newdat <- a %>% 
    dplyr::left_join(., b) %>% 
    dplyr::left_join(., c) %>% 
    dplyr::left_join(., d) %>%
    arrange(desc(avDen)) %>%
    dplyr::filter(CV < 1) %>%
    mutate(REGION = region)
  
  
  if (!is.null(species_list) && !is.null(selected_year)) {
    selected_year <- as.numeric(selected_year)
    newdat <- newdat %>%
      dplyr::filter(SPECIES_CD %in% species_list & YEAR == selected_year)
  }
  
  print(newdat)
  
  spp_levels <- newdat %>%
    dplyr::arrange(avDen)
  
  spp_levels <- spp_levels$SPECIES_CD
  
  newdat <- newdat %>%
    dplyr::mutate(SPECIES_CD = factor(SPECIES_CD, levels = spp_levels))
  
  
  dens_plot <- ggplot(data = newdat, aes(x = avDen, y = SPECIES_CD)) +
    geom_point(stat = "identity", size = 3.75) +
    geom_errorbar(aes(xmin = avDen - SE_avDen, xmax = avDen + SE_avDen, y = SPECIES_CD), width = 0, color = "black") +
    labs(x = expression(Density ~ (corals/m^{2})), y = "") +
    theme_light() +
    theme_Publication(base_size = 20) +
    theme(axis.text.y = element_text(face = "italic"))
  
  om_plot <- ggplot(data = newdat, aes(x = avMortOld, y = SPECIES_CD)) +
    geom_point(stat = "identity", size = 3.75) +
    geom_errorbar(aes(xmin = avMortOld - SE_avMortOld, xmax = avMortOld + SE_avMortOld, y = SPECIES_CD), width = 0, color = "black") +
    labs(x = expression(Old ~ mortality ~ ("%")), y = "") +
    theme_light() +
    theme_Publication(base_size = 20) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  
  rm_plot <- ggplot(data = newdat, aes(x = avMortRec, y = SPECIES_CD)) +
    geom_point(stat = "identity", size = 3.75) +
    geom_errorbar(aes(xmin = avMortRec - SE_avMortRec, xmax = avMortRec + SE_avMortRec, y = SPECIES_CD), width = 0, color = "black") +
    labs(x = expression(Recent ~ mortality ~ ("%")), y = "") +
    theme_light() +
    theme_Publication(base_size = 20) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  
  size_plot <- ggplot(data = newdat, aes(x = avMaxdiam, y = SPECIES_CD)) +
    geom_point(stat = "identity", size = 3.75) +
    geom_errorbar(aes(xmin = avMaxdiam - SE_maxdiam, xmax = avMaxdiam + SE_maxdiam, y = SPECIES_CD), width = 0, color = "black") +
    labs(x = expression(Maximum ~ diameter ~ (cm)), y = "") +
    theme_light() +
    theme_Publication(base_size = 20) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  
  
  combined_plot <- egg::ggarrange(dens_plot, om_plot, rm_plot, size_plot, ncol = 4)  # You can adjust ncol as needed
  
  # Print or save the combined plot
  print(combined_plot)
  
  
  # 
  # dens_plot <- plot_ly(data = newdat, x = ~avDen, y = ~SPECIES_CD, type = "scatter", mode = "markers") %>%
  #   #add_errorbars(x = ~avDen, y = ~SPECIES_CD, xmin = ~avDen - SE_avDen, xmax = ~avDen + SE_avDen) %>%
  #   layout(xaxis = list(title = expression(Density ~ (corals/m^{2})), yaxis = list(title = "")))
  # 
  # om_plot <- plot_ly(data = newdat, x = ~avMortOld, y = ~SPECIES_CD, type = "scatter", mode = "markers") %>%
  #   #add_errorbars(x = ~avMortOld, y = ~SPECIES_CD, xmin = ~avMortOld - SE_avMortOld, xmax = ~avMortOld + SE_avMortOld) %>%
  #   layout(xaxis = list(title = expression(Old ~ mortality ~ ("%")), yaxis = list(title = "")))
  # 
  # rm_plot <- plot_ly(data = newdat, x = ~avMortRec, y = ~SPECIES_CD, type = "scatter", mode = "markers") %>%
  #   #add_errorbars(x = ~avMortRec, y = ~SPECIES_CD, xmin = ~avMortRec - SE_avMortRec, xmax = ~avMortRec + SE_avMortRec) %>%
  #   layout(xaxis = list(title = expression(Recent ~ mortality ~ ("%")), yaxis = list(title = "")))
  # 
  # 
  # size_plot <- plot_ly(data = newdat, x = ~avMaxdiam, y = ~SPECIES_CD, type = "scatter", mode = "markers") %>%
  #   #add_errorbars(x = ~avMaxdiam, y = ~SPECIES_CD, xmin = ~avMaxdiam - SE_maxdiam, xmax = ~avMaxdiam + SE_maxdiam) %>%
  #   layout(xaxis = list(title = expression(Maximum ~ diameter ~ (cm)), yaxis = list(title = "")))
  # 
  #   subplot(dens_plot, om_plot, rm_plot, size_plot, nrows = 2)  
  
  return(NULL)
  
}