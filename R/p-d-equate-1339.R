if (packageVersion("dmetric") < "0.51.3") stop("Update dmetric")

library("dmetric")
library("trelliscopejs")
library("ggplot2")
library("dplyr")
library("tidyr")
library("gtools")

theme_set(theme_light())
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_blank(),
             rect = element_rect(fill = "transparent") # all rectangles
)

m_000 <- load_model("1339_0",   model_dir = "models_2020", project = "Jamaica")
m_011 <- load_model("1339_11",  model_dir = "models_2020", project = "Jamaica")
m_033 <- load_model("1339_33",  model_dir = "models_2020", project = "Jamaica")
m_184 <- load_model("1339_184", model_dir = "models_2020", project = "Jamaica")

# data is identical for all models
d_184 <- load_data ("1339_184", model_dir = "models_2020", project = "Jamaica")

# extact and sort equates nicely
equatenames <- gtools::mixedsort(names(m_184$fit$equate))
equatelist <- m_184$fit$equate[equatenames]

equate_000 <- data.frame(
  equate = rep(names(equatelist), sapply(equatelist, length)),
  item = unlist(equatelist),
  model = "1339_000",
  row.names = NULL,
  stringsAsFactors = FALSE
)

equate_011 <- mutate(equate_000, model = "1339_011")
equate_033 <- mutate(equate_000, model = "1339_033")
equate_184 <- mutate(equate_000, model = "1339_184")

# nest equate data frame by equate
by_equate_000 <- equate_000 %>%
  mutate(equate = factor(equate, levels = unique(equate)),
         equate_index = as.integer(equate)) %>%
  group_by(equate, equate_index, model) %>%
  nest()
by_equate_011 <- equate_011 %>%
  mutate(equate = factor(equate, levels = unique(equate)),
         equate_index = as.integer(equate)) %>%
  group_by(equate, equate_index, model) %>%
  nest()
by_equate_033 <- equate_033 %>%
  mutate(equate = factor(equate, levels = unique(equate)),
         equate_index = as.integer(equate)) %>%
  group_by(equate, equate_index, model) %>%
  nest()
by_equate_184 <- equate_184 %>%
  mutate(equate = factor(equate, levels = unique(equate)),
         equate_index = as.integer(equate)) %>%
  group_by(equate, equate_index, model) %>%
  nest()

# add in a plot column with map_plot
by_equate_000 <- by_equate_000 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_000,
                                       equates = as.character(.data$equate),
                                       show_rug = FALSE)))
by_equate_011 <- by_equate_011 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_011,
                                       equates = as.character(.data$equate),
                                       show_rug = FALSE)))
by_equate_033 <- by_equate_033 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_033,
                                       equates = as.character(.data$equate),
                                       show_rug = FALSE)))
by_equate_184 <- by_equate_184 %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_d_equate(data = d_184,
                                       model = m_184,
                                       equates = as.character(.data$equate),
                                       show_rug = FALSE)))

by_equate <- bind_rows(by_equate_000,
                       by_equate_011,
                       by_equate_033,
                       by_equate_184)

# remove problem equates that have missing plots
keep <- sapply(by_equate$panel, length) > 0
keep2 <- matrix(keep, ncol = 4)
keep <- rep(apply(keep2, 1, all), 4)
by_equate <- by_equate[keep, ]

# create the app
by_equate %>%
  ungroup() %>%
  trelliscope(name = "Percent pass by D-score, four models",
              height = 500,
              width = 1000,
              nrow = 2,
              ncol = 2,
              panel_col = "panel",
              state = list(sort = list(sort_spec("equate_index"),
                                       sort_spec("model"))),
              path = "docs/p-d-equate-1339")
