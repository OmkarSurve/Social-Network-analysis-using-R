getwd()
dirpath <- "C:/UIC/Spring 16/Social Media/Project/Final/New folder"
setwd(dirpath)

rm(list=ls())  
infile<-"Refugee_EdgeList.csv"

library(igraph)
refugee <- read.csv(infile, header = TRUE, sep = ",")
#Refine Data for Igraph
graph.data <- data.frame(refugee$Source,refugee$Target,refugee$Year,refugee$weight)
colnames(graph.data)[1]<-"source"
colnames(graph.data)[2]<-"target"
colnames(graph.data)[3]<-"year"
colnames(graph.data)[4]<-"weight"

#Create Graph
g_refugee=graph.data.frame(graph.data, directed = TRUE, vertices= NULL)
#USe the Largest Connected Component
g.decompose <- decompose(g_refugee)
g.refugee <- g.decompose[[1]]
ecount.full <- c("Edge Count Full",ecount(g.refugee))
vcount.full <- c("Vertex Count Full",vcount(g.refugee))
is.simple(g.refugee)
#Simplify Graph
g.refugee.simplify <- simplify(g.refugee)
graph.simplify <- get.data.frame(g.refugee.simplify)
colnames(graph.simplify)[1] <- "Source"
colnames(graph.simplify)[2] <- "Target"
write.csv(graph.simplify,file="Graph_Simplify.csv",row.names = FALSE)
is.simple(g.refugee.simplify)
ecount.simplify <- c("Edge Count Simplify",ecount(g.refugee.simplify))
vcount.simplify <- c("Vertex Count Simplify",vcount(g.refugee.simplify))

#Initial Network
plot(g.refugee.simplify,layout = layout.davidson.harel)
is.connected(g.refugee.simplify, mode = c("weak"))
is.connected(g.refugee.simplify, mode = c("strong"))
is.connected(g.refugee.simplify)

diameter(g.refugee.simplify)

igraph.options(vertex.size=3,edge.arrow.size=0.01)
V(g.refugee.simplify)$shape <- "circle"
V(g.refugee.simplify)$color <- "purple"
E(g.refugee.simplify)$width <- E(g.refugee.simplify)$weight/5
E(g.refugee.simplify)$color <-"lightblue"
V(g.refugee.simplify)$size <- sqrt(igraph::betweenness(g.refugee.simplify))/4
plot(g.refugee.simplify,layout = layout.fruchterman.reingold,vertex.label = NA)


#Betweeness
betweeness.data <- data.frame(betweenness(g.refugee.simplify,directed = TRUE))
names <- rownames(betweeness.data)
betweeness.data$Names <- names
colnames(betweeness.data)[1] <- "Betweeness"    
betweeness.data <- betweeness.data[order(betweeness.data$Betweeness,decreasing = TRUE),]
write.csv(betweeness.data,file="Betweeness.csv",row.names = FALSE)

#Closeness Centrality
diameter(g.refugee.simplify)

closeness.out.data <- data.frame(closeness(g.refugee.simplify,mode = "out"))
clo.out.names <- rownames(closeness.out.data)
closeness.out.data$Names <- clo.out.names
colnames(closeness.out.data)[1] <- "Closeness"     
closeness.out.data <- closeness.out.data[order(closeness.out.data$Closeness,decreasing = TRUE),]
write.csv(closeness.out.data,file="Closeness_Out.csv",row.names = FALSE)

closeness.in.data <- data.frame(closeness(g.refugee.simplify,mode = "in"))
clo.in.names <- rownames(closeness.in.data)
closeness.in.data$Names <- clo.in.names
colnames(closeness.in.data)[1] <- "Closeness"     
closeness.in.data <- closeness.in.data[order(closeness.in.data$Closeness,decreasing = TRUE),]
write.csv(closeness.in.data,file="Closeness_In.csv",row.names = FALSE)

#Cluster Coefficient
transitivity <- c("Transitivity",transitivity(g.refugee.simplify))
#Shortest Path
shortest.path.out.data <- data.frame(shortest.paths(g.refugee.simplify,mode = "out"))
shortest.path.out.data.names <- rownames(shortest.path.out.data)
shortest.path.out.data <- cbind(shortest.path.out.data.names, shortest.path.out.data)
colnames(shortest.path.out.data)[1] <- "Country" 
write.csv(shortest.path.out.data,file="Shortestpath_out.csv",row.names = FALSE)

library(reshape2)
shortest.path.out.data.melt <- melt(shortest.path.out.data)
colnames(shortest.path.out.data.melt)[[1]] <- "From"
colnames(shortest.path.out.data.melt)[[2]] <- "To"
shortest.path.out.data.melt <- shortest.path.out.data.melt[shortest.path.out.data.melt$value !="Inf",]
shortest.path.out.data.melt <- shortest.path.out.data.melt[shortest.path.out.data.melt$value !=0,]
shortest.path.out.data.melt <- shortest.path.out.data.melt[order(shortest.path.out.data.melt$value),]
write.csv(shortest.path.out.data.melt,file="Shortestpath_out_Melt.csv",row.names = FALSE)


# shortest.path.in.data <- data.frame(shortest.paths(g.refugee.simplify,mode = "in"))
# shortest.path.in.data.names <- rownames(shortest.path.in.data)
# shortest.path.in.data <- cbind(shortest.path.in.data.names, shortest.path.in.data)
# colnames(shortest.path.in.data)[1] <- "Country"     
# write.csv(shortest.path.in.data,file="Shortestpath_In.csv",row.names = FALSE)
# 
# shortest.path.in.data.melt <- melt(shortest.path.in.data)
# colnames(shortest.path.in.data.melt)[[1]] <- "From"
# colnames(shortest.path.in.data.melt)[[2]] <- "To"
# shortest.path.in.data.melt <- shortest.path.in.data.melt[shortest.path.in.data.melt$value !="Inf",]
# shortest.path.in.data.melt <- shortest.path.in.data.melt[shortest.path.in.data.melt$value !=0,]
# shortest.path.in.data.melt <- shortest.path.in.data.melt[order(shortest.path.in.data.melt$value),]
# write.csv(shortest.path.in.data.melt,file="Shortestpath_in_Melt.csv",row.names = FALSE)


#EigenCentrality
eigen.data <- data.frame(eigen_centrality(g.refugee.simplify,directed = TRUE, scale = TRUE)$vector)
eigen.names <- rownames(eigen.data)
eigen.data$Names <- eigen.names
colnames(eigen.data)[1] <- "Eigen"    
eigen.data <- eigen.data[order(eigen.data$Eigen,decreasing = TRUE),]
write.csv(eigen.data,file="Eigen.csv",row.names = FALSE)

network.properties <- rbind(ecount.full,vcount.full,ecount.simplify,vcount.simplify,transitivity)

#Subnet Afghanistan
neighbours <- neighbors(g.refugee.simplify, v=c('Afghanistan'))
append(neighbours$name, 'Afghanistan')
sub.net.afghan<-induced.subgraph(g.refugee.simplify, v=append(neighbours$name, 'Afghanistan'))
V(sub.net.afghan)$label.cex= 0.3
igraph.options(vertex.size=3,edge.arrow.size=0.05)
plot(sub.net.afghan, vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
     layout = layout.gem,edge.width = E(sub.net.afghan)$weight/5,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
#Subnet Congo
neighbours <- neighbors(g.refugee.simplify, v=c('Congo, the Democratic Republic of the'))
sub.net.congo<-induced.subgraph(g.refugee.simplify, v=append(neighbours$name, 'Congo, the Democratic Republic of the'))
V(sub.net.congo)$label.cex= 0.3
igraph.options(vertex.size=3,edge.arrow.size=0.05)
plot(sub.net.congo, vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
     layout = layout.gem,edge.width = E(sub.net.congo)$weight/5,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))

#Community detection
g_refugee_commun=graph.data.frame(graph.data, directed = FALSE, vertices= NULL)
#USe the Largest Connected Component
g.decompose <- decompose(g_refugee_commun)
g.refugee.commun.undir <- g.decompose[[1]]
ecount(g.refugee.commun.undir)
g.refugee.commun.undir <- simplify(g.refugee.commun.undir)
ecount(g.refugee.commun.undir)

g.refugee.commun.dir <- g.refugee.simplify

#Fast Greedy
g.refugee.fast <- fastgreedy.community(g.refugee.commun.undir, weights=E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex= 0.3
plot(g.refugee.fast,g.refugee.commun.undir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Fast greedy Algorithm")
c.m.fast <- membership(g.refugee.fast)


#Community Detection using Walktrap
g.refugee.walktrap <- walktrap.community(g.refugee.commun.dir,step = 6, weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.walktrap)
c.m.walktrap <- membership(g.refugee.walktrap)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.walktrap,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("WalkTrap Algorithm")

#Community Detection using Spinglass
g.refugee.spinglass<- spinglass.community(g.refugee.commun.dir,spins = 60, weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.spinglass)
c.m.spinglass <- membership(g.refugee.spinglass)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.spinglass,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Spinglass Algorithm")

#Community Detection using Label PRopogation
g.refugee.label<- label.propagation.community(g.refugee.commun.dir,weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.label)
c.m.label <- membership(g.refugee.label)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.label,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Label Propogation Algorithm")

#Community Detection using Girvan-Newman
g.refugee.gn<- edge.betweenness.community(g.refugee.commun.dir,weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.gn)
c.m.gn <- membership(g.refugee.gn)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.gn,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Label Propogation Algorithm")





