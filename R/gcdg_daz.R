library(trelliscopejs)
library(ggplot2)
library(dmetric)
library(dplyr)

theme_set(theme_light())
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_blank(),
             rect = element_rect(fill = "transparent") # all rectangles
)

model <- dmetric::model_lean
data <- model$dscore %>%
  select(a, daz, cohort)

#get gcdg references as backdrop
reference <- dscore::get_reference(population = "gcdg") %>%
  mutate(month = .data$age * 12) %>%
  select("month", .data$SDM2:.data$SDP2) %>%
  filter(.data$month <= 60) %>%
  tidyr::pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
color <- "lightblue"

ggplot(data,
       aes(x = a * 12, y = daz, group = cohort, color = cohort)) +
  scale_colour_manual(
    values = get_palette("study", "gsed_palettes"), na.value = "grey") +
  scale_x_continuous(
    "Age (in months)",
    limits = c(-3, 63),
    breaks = seq(0, 60, 12),
    expand = c(0, 0)) +
  scale_y_continuous(
    paste0("DAZ"),
    breaks = seq(-3, 3, 1),
    limits = c(-3.5, 3.5)) +
  geom_point(
    size = 0.7,
    shape = 1) +
  annotate("rect", xmin = -3, xmax = 63, ymin = -2, ymax = +2,
           fill = "grey", alpha = 0.1) +
  facet_trelliscope(
    vars(cohort),
    name = "DAZ by age per cohort",
    ncol = 1,
    nrow = 1,
    data = data,
    path = "docs/gcdgdaz") +
  theme(legend.position = "none")
