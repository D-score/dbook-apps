library(trelliscopejs)
library(ggplot2)
library(gapminder)

theme_set(theme_light())
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_blank(),
             rect = element_rect(fill = "transparent"))

qplot(year, lifeExp, data = subset(gapminder, continent == "Europe")) +
  facet_trelliscope(~ country + continent,
                    path = "docs/transparent")
