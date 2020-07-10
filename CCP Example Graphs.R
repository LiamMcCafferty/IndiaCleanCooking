setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")
rm(list = ls())

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse) # For data management
#library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs
library(network)

#From our calcalated statistics we will make two example graphs, one for LPG and
#one for NoLPG. They will have as similar median network sizes and median
#proportion LPG values as I can get.
#Stats:
#  LPG - Network Size : 6
#      - Density : 1
#      - LPG : 0.857
#NoLPG - Network Size : 7
#      - Density : 1
#      - LPG : 0.503

#Two minor issues, we should try to match our distribution best to our median
#  sizes. As size is likely more important we may need to air on the side of
#  fudging our proporitons a bit. The best option is to get them as close to. So
#  for a network size of 6 should probably be 5/6 LPG, and NoLPG with a network
#  size of 7 should be 4/7. I chose 4/7 as 0.42 is further from our values of
#  0.503 than 0.5713 of 4/7.

#  Density is 1 for both, so we just make all ties 2's.

									#E,1,2,3,4,5,6
lpg_mat <- rbind(c(0,2,2,2,2,2,2),#E
								 c(2,0,2,2,2,1,2),#1
								 c(2,2,0,2,2,2,2),#2
								 c(2,2,2,0,2,2,2),#3
								 c(2,2,2,2,0,2,2),#4
								 c(2,1,2,2,2,0,2),#5
								 c(2,2,2,2,2,2,0))#6

									  #E,1,2,3,4,5,6,7
nolpg_mat <- rbind(c(0,2,2,2,2,2,2,2),#E
									 c(2,0,2,2,2,2,2,2),#1
									 c(2,2,0,2,2,2,1,2),#2
									 c(2,2,2,0,2,2,2,2),#3
									 c(2,2,2,2,0,2,2,2),#4
									 c(2,2,2,2,2,0,2,2),#5
									 c(2,2,1,2,2,2,0,2),#6
									 c(2,2,2,2,2,2,2,0))#7

colnames(lpg_mat) <- rownames(lpg_mat) <- c("EGO", 1:(nrow(lpg_mat) - 1))
colnames(nolpg_mat) <- rownames(nolpg_mat) <- c("EGO", 1:(nrow(nolpg_mat) - 1))

#Presets
mat_input <- lpg_mat
number_lpg <- 2
ego_lpg <- TRUE

#Function
graph_func <- function(mat_input, ego_lpg, number_lpg = 0){
	
	netsize <- nrow(mat_input) - 1
	if(number_lpg > (netsize)){stop("number_lpg too large")}
	
	coloration <- sample(c(rep("No LPG", times = (netsize - number_lpg)),
												 rep("LPG", times = (number_lpg))))
	coloration <- c(ifelse(ego_lpg, "LPG", "No LPG"), coloration)
	
	
	#Establishing network
	net_graph <- network.initialize(nrow(mat_input), directed = FALSE)
	#Assinging tie values to network
	network.adjacency(mat_input, net_graph)
	set.edge.attribute(net_graph, "weight", mat_input[lower.tri(mat_input)])
	#Setting names for vertices
	set.vertex.attribute(net_graph, "vertex.names", colnames(mat_input))
	#Setting fill for verticies, this is our selected var
	set.vertex.attribute(net_graph, "coloration", coloration)
	#Setting up for shape of verticies
	shaper <- ifelse(colnames(mat_input) == "EGO", "Ego", "Alter")
	#Setting shape of verticie, this is ego/alter or if NA data
	set.vertex.attribute(net_graph, "shaper", shaper)
	#Setting numbers for what shape we decide each value should be
	# graph_shapes <- factor(shaper,
	# 											 levels = c("Ego", "Alter"),
	# 											 labels = c(ego_shape, alter_shape, na_shape)) %>%
	# 	as.character() %>%
	# 	as.integer()
	
	ggplot(ggnetwork(net_graph, layout = "circle"), aes(x = x, y = y, xend = xend, yend = yend)) +
		#geom_edges(aes(color = factor(weight, c(1,2)))) +
		geom_nodes(aes(shape = shaper,
									 fill = coloration), size = 10) +
		scale_fill_manual(breaks = c("LPG", "No LPG"), values = c("blue", "red"), 
											name = "Stove Ownership") +
		#scale_color_manual(breaks = c("1", "2"), values = c("blue", "red"),
		#									 labels = c("Weak Tie", "Strong Tie"), name = "Tie Strength") +
		scale_shape_manual(breaks = c("Ego", "Alter"), values = c(24, 21), name = "Node Type") +
		theme_blank() +
		guides(fill = guide_legend(override.aes = list(shape = 21))) +
		#Formats the legend which describes edge weight to the reader
		theme(legend.position = "bottom", #format the legend 
					legend.title = element_text(face = "bold", size = 15),
					legend.text = element_text(size = 10)) + 
		theme(legend.title.align = 0.5) + 
		theme(plot.title = element_text(size = 18, face = "bold")) +
		# theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
		#Formatting for legend's keys
		theme(legend.direction = 'vertical',
					legend.key.height = unit(1, "line"),
					legend.key = element_rect(colour = NA, fill = NA)) +
		# coord_equal()
		coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
	
	
}

pdf(file = "LPG Example Graph noties.pdf", width = 4, height = 5)
graph_func(lpg_mat, TRUE, 5)
dev.off()

pdf(file = "No LPG Example Graph noties.pdf", width = 4, height = 5)
graph_func(nolpg_mat, FALSE, 4)
dev.off()
