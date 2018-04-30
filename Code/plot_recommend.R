library(igraph)
library(RColorBrewer)

x<-unique(cluster.recommend)
if (x>9){
  color <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
}else{
  color <- brewer.pal(length(x),"Set1")
}
color.recommend <- character(length(x))
legend.recommend <- clust_characteristic_cat_mec[x]
for(i in 1:length(x)){
  color.recommend[cluster.recommend %in% x[i]] <- color[i]
  legend.recommend[i] <- sub('^([^,]+,[^,]+).*', '\\1', legend.recommend[i])
}


## make graph
g <- make_star(21, mode = "undirected")

## vertices
V(g)$label <- c("Selection",names.recommend)
V(g)$col <- c("white",color.recommend)
V(g)$size <- 7

## edges
E(g)$weight <- 20*sim.recommend


## titles
main.d <- "Boardgame Recommendation"
temp <- as.character(title)
sub.d<- paste("Selection =", paste(temp[-length(temp)],collapse = ", "),"&",temp[length(temp)])
              

## plotting
plot(g, vertex.size=V(g)$size, vertex.label=V(g)$label, vertex.label.color="black", vertex.label.dist=2, vertex.color=V(g)$col,
     edge.width=E(g)$weight,
     main = main.d, sub=sub.d)


legend("left", legend = legend.recommend, col = color.recommend, pch=19)
