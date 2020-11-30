#make the GCDG references
library(gseddata)
library(ggplot2)
library(gtools)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(tidyr)


theme_set(theme_light())
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_blank(),
             rect = element_rect(fill = "transparent") # all rectangles
)

dscores <- dmetric::model_lean$dscore %>%
  mutate(agemos = a * 12, track = ifelse(daz < (-2), 1, 0)) %>%
  filter(agemos <= 60 & daz > -5) %>%
  select(agemos, d, daz, track)


p1d <- dscores
p1d$step <- "Step 1: Age for D-score"
p1d$score <- p1d$d
p1d$track <- 0
p2d <- dscores
p2d$step <- "Step 2: Median trend"
p2d$score <- p2d$d
p2d$track <- 0
p3d <- dscores
p3d$step <- "Step 3: Centile distribution"
p3d$score <- p3d$d
p3d$track <- 0
p4d <- dscores
p4d$step <- "Step 4: Standardized scores (DAZ)"
p4d$score <- p4d$daz
p4d$track <- 0
p5d <- dscores
p5d$step <- "Step 5: Standard deviation lines"
p5d$score <- p5d$daz
p5d$track <- 0
p6d <- dscores
p6d$step <- "Step 6: Marked off-track observations (red)"
p6d$score <- p6d$daz

pd <- bind_rows(p1d,
                p2d,
                p3d,
                p4d,
                p5d,
                p6d)

pd$d <- pd$daz <- NULL

#p1set <- c(step = 1, ylab = "D-score", xmin = 0, xmax = 60, ymin = 0, ymax = 80)
#p2set <- c(step = 2, ylab = "D-score", xmin = 0, xmax = 60, ymin = 0, ymax = 80)
#p3set <- c(step = 3, ylab = "D-score", xmin = 0, xmax = 60, ymin = 0, ymax = 80)
#p4set <- c(step = 4, ylab = "DAZ", xmin = 0, xmax = 60, ymin = -4, ymax = 4)
##p5set <- c(step = 5, ylab = "DAZ", xmin = 0, xmax = 60, ymin = -4, ymax = 4)
#p6set <- c(step = 6, ylab = "DAZ", xmin = 0, xmax = 60, ymin = -4, ymax = 4)

#pset <- bind_rows(p1set, p2set, p3set, p4set, p5set, p6set)

gcdg_reference <- dscore::get_reference(population = "gcdg")

references <- gcdg_reference %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)


p1r <- references
p1r$step <- "Step 1: Age for D-score"
p1r$d <- NA
p2r <- references
p2r$step <- "Step 2: Median trend"
p2r$d <- ifelse(p2r$centile == "SD0", p2r$d, NA)
p3r <- references
p3r$step <- "Step 3: Centile distribution"
p4r <- references
p4r$d <- 0
p4r$step <- "Step 4: Standardized scores (DAZ)"
p5r <- references
p5r$step <- "Step 5: Standard deviation lines"
p5r$d <- ifelse(p5r$centile == "SDM2", -2, p5r$d)
p5r$d <- ifelse(p5r$centile == "SDM1", -1, p5r$d)
p5r$d <- ifelse(p5r$centile == "SD0", 0, p5r$d)
p5r$d <- ifelse(p5r$centile == "SDP1", 1, p5r$d)
p5r$d <- ifelse(p5r$centile == "SDP2", 2, p5r$d)
p6r <- p5r
p6r$step <- "Step 6: Marked off-track observations (red)"


pr <- bind_rows(p1r,p2r,p3r,p4r,p5r,p6r)

pdata <- bind_rows(pd, pr)


ggplot(data = pdata)+
  geom_point(aes(agemos, y = score, group = track, color = factor(track)))+
  xlab("Age (months)") + ylab("D-score")+
  geom_line(aes(x = month, y = d, group = centile), size = 1)+
  scale_colour_manual(values = c("grey", "red")) +
#  facet_wrap(.~step, scales = "free_y")
  facet_trelliscope(
    vars(step),
    ncol = 1,
    nrow = 1,
    width = 800,
    height = 800,
    data = pdata,
    path = "docs/gcdgreferences") +
    theme(legend.position = "none")



