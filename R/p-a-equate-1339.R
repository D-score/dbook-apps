library("dmetric")
library("trelliscopejs")
library("ggplot2")
library("dplyr")
library("tidyr")
library("gtools")

theme_set(theme_light())

m_184 <- load_model("1339_184", model_dir = "models_2020", project = "Jamaica")
d_184 <- load_data("1339_184", model_dir = "models_2020", project = "Jamaica")

# extact and sort equates nicely
equates <- m_184$fit$equate
ms <- mixedsort(names(equates))
equates <- equates[ms]
equatedf <- data.frame(
  equate = rep(names(equates), sapply(equates, length)),
  item = unlist(equates),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# nest equate data frame by equate
by_equate <- equatedf %>%
  group_by(equate) %>%
  nest()

# add in a plot column with map_plot
m_184$active_equates <- NULL
by_equate <- by_equate %>%
  mutate(
    panel = map_plot(data,
                     ~ plot_p_a_equate(data = d_184,
                                       model = m_184,
                                       equates = .data$equate)))

# remove two problem equates that have no plots
by_equate <- by_equate[sapply(by_equate$panel, is.list), ]

# create the app
by_equate %>%
  trelliscope(name = "Percent pass by age for all equate groups",
              height = 500,
              width = 1000,
              nrow = 1,
              ncol = 1,
              path = "docs/p-a-equate-1339")

# FIXME: internal sorting by trelliscope gobbles up the mixedsort()
# FIXME: two groups (EXP5, EXP6) are lost (single items)
