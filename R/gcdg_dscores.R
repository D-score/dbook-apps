library(trelliscopejs)
library(ggplot2)
library(dmetric)
library(dplyr)

theme_set(theme_light())
model <- dmetric::model_lean
data <- model$dscore %>%
  select(a, d, cohort)

#get gcdg references as backdrop
reference <- dscore::get_reference(population = "gcdg") %>%
  mutate(month = .data$age * 12) %>%
  select("month", .data$SDM2:.data$SDP2) %>%
  filter(.data$month <= 60) %>%
  tidyr::pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
color <- "lightblue"

ggplot(data,
       aes(x = a * 12, y = d , group = cohort, color = cohort)) +
  scale_colour_manual(
    values = get_palette("study", "gsed_palettes"), na.value = "grey") +
  scale_x_continuous(
    "Age (in months)",
    limits = c(0, 60),
    breaks = seq(0, 60, 12)) +
  scale_y_continuous(
    paste0("D-score"),
    breaks = seq(0, 80, 20),
    limits = c(0, 80)) +
  geom_line(
    mapping = aes(x = month, y = d, group = centile),
    data = reference,
    colour = color) +
  geom_point(
    size = 0.7,
    shape = 1) +
  facet_trelliscope(
    vars(cohort),
    name = "D-score by age per cohort",
    ncol = 1,
    nrow = 1,
    data = data,
    path = "docs/gcdgdscores") +
  theme(legend.position = "none")
