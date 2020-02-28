# Plotting ----------------------------------------------------------------
# preview annual precipitation for all sites
weather_on_site_formated %>%
  group_by(siteid, year = year(date)) %>%
  summarise(rain = sum(precip_on_site, na.rm = TRUE),
            days_count = n()) %>%
  filter(days_count > 300) %>%
  # mutate(year = as.factor(year)) %>%
  ggplot(aes(x=year, y=rain)) +
  geom_point(aes(color=siteid), size = 2) +
  geom_line(aes(color=siteid), alpha = 0.5, linetype = 'dotted') +
  scale_x_continuous(breaks = 2005:2018) +
  labs(title = "Annual Precipitation",
       subtitle = "based on calendar years with more than 300 records",
       x = NULL, y = "Precipitation, mm", col = 'Research Site') +
  theme_light()
ggsave('Output/Figs/Annual_Precipitation.png', width = 12, height = 8)



