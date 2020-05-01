library(trelliscopejs)
library(ggplot2)
library(dplyr)
library(tidyr)
theme_set(theme_light())

# example using mpg data
mpg %>%
  group_by(class) %>%
  nest() %>%
  mutate(panel = map_plot(data,
                          ~ ggplot(data = .x, aes(displ, hwy)) +
                            geom_point() +
                            xlab("Engine displacement, litres") +
                            ylab("Highway miles per gallon") +
                            xlim(1, 7) + ylim(10, 60))) %>%
  trelliscope(name = "MPG default panel order",
              path = "docs/mpg-default")

# my_order <- c("subcompact", "compact", "midsize",
#               "minivan", "pickup", "suv", "2seater")

