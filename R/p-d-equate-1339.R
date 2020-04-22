library("dmetric")
library("trelliscopejs")
library("ggplot2")
library("dplyr")
library("tidyr")
library("gtools")

theme_set(theme_light())

m_000 <- load_model("1339_0",   model_dir = "models_2020", project = "Jamaica")
m_011 <- load_model("1339_11",  model_dir = "models_2020", project = "Jamaica")
m_033 <- load_model("1339_33",  model_dir = "models_2020", project = "Jamaica")
m_184 <- load_model("1339_184", model_dir = "models_2020", project = "Jamaica")

# data is identical for all models
d_184 <- load_data ("1339_184", model_dir = "models_2020", project = "Jamaica")

# extact and sort equates nicely
equates <- m_184$fit$equate
ms <- mixedsort(names(equates))
equates <- equates[ms]
equate_000 <- data.frame(
  equate = rep(names(equates), sapply(equates, length)),
  item = unlist(equates),
  model = "1339_000",
  row.names = NULL,
  stringsAsFactors = FALSE
)

equate_011 <- mutate(equate_000, model = "1339_011")
equate_033 <- mutate(equate_000, model = "1339_033")
equate_184 <- mutate(equate_000, model = "1339_184")

# nest equate data frame by equate
by_equate_000 <- equate_000 %>%
  group_by(model, equate) %>%
  nest()
by_equate_011 <- equate_011 %>%
  group_by(model, equate) %>%
  nest()
by_equate_033 <- equate_033 %>%
  group_by(model, equate) %>%
  nest()
by_equate_184 <- equate_184 %>%
  group_by(model, equate) %>%
  nest()

# hack to convince plot_p_d_equate() to plot inactive equates
m_000$fit$equate <- equates
m_011$fit$equate <- equates
m_033$fit$equate <- equates
m_184$fit$equate <- equates

# add in a plot column with map_plot
by_equate_000 <- by_equate_000 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_000,
                                       equates = .data$equate,
                                       show_rug = FALSE)))
by_equate_011 <- by_equate_011 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_011,
                                       equates = .data$equate,
                                       show_rug = FALSE)))
by_equate_033 <- by_equate_033 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_033,
                                       equates = .data$equate,
                                       show_rug = FALSE)))
by_equate_184 <- by_equate_184 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_184,
                                       equates = .data$equate,
                                       show_rug = FALSE)))
by_equate <- bind_rows(by_equate_000,
                       by_equate_011,
                       by_equate_033,
                       by_equate_184)

# remove seven problem equates that have missing plots
keep <- sapply(by_equate$panel, length) > 0
keep2 <- matrix(keep, ncol = 4)
keep <- rep(apply(keep2, 1, all), 4)
by_equate <- by_equate[keep, ]

# create the app
by_equate %>%
  trelliscope(name = "Percent pass by D-score, four models",
              height = 500,
              width = 1000,
              nrow = 2,
              ncol = 2,
              panel_col = "panel",
              path = "docs/p-d-equate-1339")

# FIXME: internal sorting by trelliscope gobbles up the mixedsort()
# FIXME: curves are lost for many equates. Need to check.
