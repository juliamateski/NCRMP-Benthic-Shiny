plot_bleaching_prev <- function(region_data, region_name, species_select, health_year) {

  filtered_data <- region_data %>%
    filter(species_name %in% species_select) %>%
    filter(YEAR == health_year)
  
 
  ggplot(filtered_data, aes(x = reorder(species_name, avBlePrev), 
                            y = avBlePrev)) +
    geom_bar(stat = "identity",
             fill = "deepskyblue4") + 
    geom_errorbar(aes(ymin = avBlePrev, ymax = avBlePrev + SE_B),
                  width = 0.1) +
    coord_flip() + 
    guides(fill = "none") +
    labs(y = "Bleaching Prevalence (%)", 
         x = "", 
         title = region_name) + 
    scale_y_continuous(expand = c(0, 0)) +
    theme_Publication(base_size = 20) +
    theme(axis.text.y = element_text(face = "italic"))
}
