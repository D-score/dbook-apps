library(shiny)
library(ggraph)
library(igraph)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtools)
library(dmetric)
library(ddata)
library(gseddata)
library(dscore)

plot_map_domain <- function(domain, equates){
  theme_set(theme_light())
  theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA),
               legend.key = element_blank(),
               rect = element_rect(fill = "transparent") # all rectangles
  )

  if(domain == "Fine Motor") {domain_short <- "FM"}
  if(domain == "Gross Motor") {domain_short <- "GM"}
  if(domain == "Cognitive") {domain_short <- "COG"}
  if(domain == "Expressive") {domain_short <- "EXP"}
  if(domain == "Receptive") {domain_short <- "REC"}


  indicator <- which(grepl(domain_short, names(equates)))
  eqinst1 <- equates[indicator]

  eqlist <- eqinst1
  itnames <- unlist(eqlist)
  inst_grps <- data.frame(from = substr(itnames, 1,3), to = itnames)
  inst_grps <- inst_grps[order(inst_grps$from),]
  inst_top <- data.frame(from = "origin", to = unique(inst_grps$from))
  hierarchy <- rbind(inst_top, inst_grps)

  eqsub <- vector()
  for(i in names(eqlist)){
    eqdf1 <- data.frame(eq = i, item = eqlist[[i]])
    eqsub <- rbind(eqsub, eqdf1)
  }

  #eqsub <- eqsub[order(as.character(eqsub[,"to"])),]
  #hierarchy <- rbind(eqtop, eqsub)
  vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) )
  vertices$group  <-  hierarchy$from[ match(vertices$name, hierarchy$to) ]
  vertices$eq <- eqsub[match(vertices$name, eqsub$item), "eq"]


  #make df of connections
  eqcon <- vector()
  for(i in names(eqlist)){
    eqcon1 <- combn(eqlist[[i]], m=2)
    eqcon1 <- data.frame(from = eqcon1[1,], to = eqcon1[2,])
    eqcon <- rbind(eqcon, eqcon1)
  }


  #Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
  #calculate the ANGLE of the labels
  vertices$id  <- NA
  myleaves <- which(is.na(match(vertices$name, hierarchy$from) ))
  nleaves <- length(myleaves)
  vertices$id[ myleaves ] <- seq(1:nleaves)
  #vertices$angle <- 90 - 360 * vertices$id / nleaves
  ##!!! dit aangepast van voorbeeld! Werkt

  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90

  #set mapping parameters:
  if(domain == "Fine Motor" ){
    vertices$angle <- 180 - 360 * vertices$id / nleaves
    vertices$hjust <- ifelse(vertices$angle < -90 | vertices$angle > 90, 1, 0)#FM
    vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
    vertices$angle <- ifelse(vertices$angle > 90, vertices$angle - 180, vertices$angle)
  }
  if( domain == "Cognitive" ){
    vertices$angle <- 180 - 360 * vertices$id / nleaves
    vertices$hjust <- ifelse(vertices$angle < -90 | vertices$angle > 90, 0, 1)
    vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)
    vertices$angle <- ifelse(vertices$angle > 90, vertices$angle - 180, vertices$angle)
    vertices$angle <- vertices$angle +40
  }
  if(domain == "Receptive"){
    vertices$angle <- 180 - 360 * vertices$id / nleaves
    vertices$hjust <- ifelse(vertices$angle < -90 | vertices$angle > 90, 1, 0)
    vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
    vertices$angle <- ifelse(vertices$angle > 90, vertices$angle - 180, vertices$angle)
    vertices$angle <- vertices$angle -40
  }
  if(domain == "Gross Motor"){
    vertices$angle <- 180 - 360 * vertices$id / nleaves
    vertices$hjust <- ifelse(vertices$angle < -90 | vertices$angle > 90, 0, 1)
    vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)
    vertices$angle <- ifelse(vertices$angle > 90, vertices$angle - 180, vertices$angle)
    vertices$angle <- vertices$angle +40
  }
  if(domain == "Expressive"){
    vertices$angle <- 90 - 360 * vertices$id / nleaves
    vertices$hjust <- ifelse(vertices$angle < -90, 0, 1)
    vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
  }


  # Create a graph object with the igraph library
  mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )
  # This is a network object, you visualize it as a network like shown in the network section!

  # The connection object must refer to the ids of the leaves:
  from <- match(eqcon$from, vertices$name)
  #from <- from[!is.na(from)]
  to <- match(eqcon$to, vertices$name)
  #to <- to[!is.na(to)]


  # With igraph:
  #plot(mygraph, vertex.label="", edge.arrow.size=0, vertex.size=2)
  inst_colors <- dmetric::get_palette("instrument")
  names(inst_colors)[1] <- "aqi"


  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
    geom_node_point(aes(filter = leaf, x = x*1.05, y = y*1.05, colour = group), size = 3)+
    geom_conn_bundle(data = get_con(from= from, to = to), colour = "skyblue", alpha = 1, width = 0.9, tension = 0.9)+
    geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust, colour = group), size=3, alpha=1) +
    scale_colour_manual(values = inst_colors)+
    theme_void() +
    theme(
      legend.position="none",
      plot.margin=unit(c(0,0,0,0),"cm"),
    ) +
    expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))+
    ggtitle(paste(domain, "Domain", sep =" "))

}

gcdg_varlist_primary <- dmetric::prepare_items(min_cat = 10)



eqset2 <- dscore::get_itemtable(items = dscore::rename_gcdg_gsed(gcdg_varlist_primary$items))[, c("item", "equate")]
eqset3 <- list()
for(eq in unique(eqset2$equate)){
  eqitems <-  eqset2 %>% filter(equate == eq) %>% select(item) %>% unlist()
  if(length(eqitems) > 1) eqset3[[eq]] <- eqitems
}

#equates <- eqset2 %>% lapply(dscore::rename_gcdg_gsed)
equates <- eqset3


domains <- c("Fine Motor", "Gross Motor", "Cognitive", "Receptive", "Expressive")

plotlist <- list()
for(p in domains){
plotlist[[p]] <- plot_map_domain(domain = p, equates = equates)
}

write_panels(plotlist,
             base_path = "docs/mapping",
             name = "mapping",
             width = 800,
             height = 800 )

write_config(
  base_path = "docs/mapping",
  id = "mapping",
  self_contained = TRUE
)


