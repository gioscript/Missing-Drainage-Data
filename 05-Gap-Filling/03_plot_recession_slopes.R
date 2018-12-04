# show distribution of the slopes

# general plot
recession_slope_plot <-
  recession_slope_dist %>%
  mutate(id = paste(siteid, plotid)) %>%
  ggplot(aes(x = id, y = slope)) +
  geom_boxplot() +
  geom_point(data = . %>% filter(slope < -15), col = "red") +
  facet_grid(season ~ .) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# function to calculate sample size
give.n <- function(x){return(c(y = 5, label = length(x)))}

# recession slopes for each plot  
recession_slope_plot +
  stat_summary(fun.data = give.n, geom = "text", 
               position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(-55, 10))
ggsave(filename = "Output/Figs/recession_slope_distribution.png", width = 12, height = 8)

# zoomed in recession slopes with arithmetic mean slope for each plot-season
recession_slope_plot +  
  # add a line for mean
  stat_summary(fun.y = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.8, linetype = "solid", colour = "deeppink2", 
               show.legend = FALSE) +
  stat_summary(fun.data = function(x){return(c(y = 2, label = length(x)))}, 
               geom = "text", 
               position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(-13, 3))
ggsave(filename = "Output/Figs/recession_slope_distribution_zoomed.png", width = 12, height = 8)


# zoomed in recession slopes with arithmetic mean slope for each plot-season
recession_slope_plot +  
  stat_summary(fun.y = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.8, linetype = "solid", colour = "deeppink2", 
               show.legend = FALSE) +
  # add trimmed mean (10% from each side)
  stat_summary(fun.y = function(x){mean(x, trim = 0.1)}, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.8, linetype = "solid", colour = "deepskyblue3", 
               show.legend = FALSE) +
  # add geometric mean
  # stat_summary(fun.y = function(x){-psych::geometric.mean(-x, na.rm = T)}, geom = "errorbar", 
  #              aes(ymax = ..y.., ymin = ..y..),
  #              width = 0.75, size = 0.8, linetype = "solid", colour = "darkviolet", 
  #              show.legend = FALSE) +
  stat_summary(fun.data = function(x){return(c(y = 1, label = length(x)))}, 
               geom = "text", 
               position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(-4, 1.5))
ggsave(filename = "Output/Figs/recession_slope_distribution_zoomed_with_additional_means.png", width = 12, height = 8)
