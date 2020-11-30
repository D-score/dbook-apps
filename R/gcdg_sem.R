theme_set(theme_light())
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_blank(),
             rect = element_rect(fill = "transparent") # all rectangles
)

model <- dmetric::model_lean
data_input <- dmetric::gcdg_lean


sem_daz <- dmetric::model_lean$dscore %>%
  mutate(agemos = a *12,
         low_d1 = d - sem,
         high_d1 = d +sem,
         low_daz1 = dscore::daz(low_d1, x = a),
         high_daz1 = dscore::daz(high_d1, x = a),
         agecat = cut(agemos, breaks = 0:max(agemos, na.rm=TRUE)),
         agenum = as.numeric(agecat))

sem_daz_a <- sem_daz %>%
#  mutate(agecat = cut(agemos, breaks = 0:max(agemos, na.rm=TRUE)),
#         agenum = as.numeric(agecat))%>%
  group_by(agenum, cohort) %>%
  summarize (
             mean_a = mean(a, na.rm=TRUE), #voor omzetting naar daz
             mean_d = mean(d, na.rm = TRUE),
             var_betw = var(d, na.rm = TRUE),
             var_with = sum(sem^2, na.rm = TRUE),
             n =  sum(!is.na(sem))
  ) %>% ungroup() %>%
  mutate(sem_pool = sqrt((var_with + var_betw)/ (n-1)),
         low_d = mean_d - sem_pool,
         high_d = mean_d + sem_pool,
         mean_daz = dscore::daz(mean_d, x = mean_a),
         low_daz = dscore::daz(low_d, x = mean_a),
         high_daz = dscore::daz(high_d, x = mean_a))

sem_daz <- sem_daz %>% left_join(sem_daz_a, by = c("cohort","agenum"))


ggplot(data = sem_daz, aes(x = agemos, y = daz, group = cohort))+
  geom_point(aes(x = agemos, y = daz, group = cohort), color = "lightgrey")+
  geom_point(aes(x = agenum, y = mean_daz, group = cohort))+
  geom_errorbar(aes(x = agenum, ymin = low_daz, ymax = high_daz))+
  xlab("Age (months)")+ ylab("daz (sem)")+
  ylim(c(-2,2))+xlim(c(0,60))+
  facet_trelliscope(
    vars(cohort),
    ncol = 1,
    nrow = 1,
    width = 800,
    height = 800,
    data = sem_daz,
    path = "docs/gcdgsem") +
  theme(legend.position = "none")


