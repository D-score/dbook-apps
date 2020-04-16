library("dmetric", warn.conflicts = FALSE)
library("dplyr")
set.seed(123)

m_0   <- load_model("1339_0",   model_dir = "models_2020", project = "Jamaica")
m_11  <- load_model("1339_11",  model_dir = "models_2020", project = "Jamaica")
m_33  <- load_model("1339_33",  model_dir = "models_2020", project = "Jamaica")
m_184 <- load_model("1339_184", model_dir = "models_2020", project = "Jamaica")

mod <- bind_rows(
  m_0$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_000"),
  m_11$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_011"),
  m_33$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_033"),
  m_184$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_184")) %>%
  mutate(month = a * 12) %>%
  select(-a) %>%
  sample_frac(size = 0.25)



# load 25% sample of four models with 1339 items
# mod <- readRDS(file = "data/model_1339_data.rds")
library(trelliscopejs)
library(ggplot2)
theme_set(theme_light())

# get the dutch references as backdrop
reference <- dscore::get_reference(population = "dutch") %>%
  mutate(month = .data$age * 12) %>%
  select("month", .data$SDM2:.data$SDP2) %>%
  filter(.data$month <= 30) %>%
  tidyr::pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
color <- "grey"

# create the trelliscope display
ggplot(
  aes(month, d, group = cohort, colour = cohort),
  data = subset(mod, model = "1339_000")) +
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
    vars(model, cohort),
    ncol = 2,
    nrow = 2,
    data = subset(mod, model = "1339_000"),
    path = "docs/models1339") +
  theme(legend.position = "none")

