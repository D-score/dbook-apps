library(trelliscopejs)
library(ggplot2)
library(dplyr)
library(tidyr)

my_order <- c("subcompact", "compact", "midsize",
              "minivan", "pickup", "suv", "2seater")

gg <- mpg %>%
  mutate(newclass = factor(class, levels = my_order),
         seq = as.integer(newclass)) %>%
  group_by(seq) %>%
  nest() %>%
  mutate(panel = map_plot(data,
                          ~ ggplot(data = .x, aes(displ, hwy)) +
                            geom_point() +
                            xlab("Engine displacement, litres") +
                            ylab("Highway miles per gallon") +
                            xlim(1, 7) + ylim(10, 60) +
                            theme_light())) %>%
  trelliscope(name = "MPG custom panel order",
              path = "docs/mpg-custom")


