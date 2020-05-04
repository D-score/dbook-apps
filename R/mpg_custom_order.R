library(trelliscopejs)
library(ggplot2)
library(dplyr)
library(tidyr)

my_order <- c("subcompact", "compact", "midsize",
              "minivan", "pickup", "suv", "2seater")

# old solution that does not display label
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

# Ryan's solution: https://github.com/hafen/trelliscopejs/issues/25
mpg %>%
  mutate(class = factor(class, levels = my_order),
         class_index = as.integer(class)) %>%
  group_by(class, class_index) %>%
  nest() %>%
  mutate(panel = map_plot(data,
                          ~ ggplot(data = .x, aes(displ, hwy)) +
                            geom_point() +
                            xlab("Engine displacement, litres") +
                            ylab("Highway miles per gallon") +
                            xlim(1, 7) + ylim(10, 60) +
                            theme_light())) %>%
  trelliscope(name = "MPG custom panel order",
              state = list(sort = list(sort_spec("class_index"))),
              nrow = 1, ncol = 1,
              path = "docs/mpg-custom")

