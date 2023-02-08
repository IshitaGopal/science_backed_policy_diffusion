rm(list=ls())

# Assumes working directory is the folder science_backed_policy_diffusion
setwd('/Users/ishitagopal/Box/science_backed_policy_diffusion/')


library(igraph)
library(stringr)
library(plotly)
library(stargazer)


##################################################
#################### Load data ###################
##################################################

raw_data = "Dataset/EdgeList_updated.csv"
mydata = read.csv(raw_data, header = TRUE, stringsAsFactors = FALSE)
names(mydata)

#Party information associated with each legislator
raw_data2 = "Dataset/Legislator_Party_updated.csv"
legislator_party = read.csv(raw_data2, header = TRUE, stringsAsFactors = FALSE)

#Shor McCarty dataset
raw_data3 = "Dataset/shor_mcarty_1993_to_2016_with_Id.csv"
McCarty = read.csv(raw_data3, header = TRUE, stringsAsFactors = FALSE)

########################################################
################# Summary Statistics ###################
########################################################

#Republicans in the dataset
legis_repub = legislator_party[legislator_party$Party=="R",]
legis_repub_vec = as.vector(unlist(legis_repub$Legislator_ID))

#Independents in the dataset 
legis_indep = legislator_party[legislator_party$Party=="X",]
legis_indep_vec = as.vector(unlist(legis_indep$Legislator_ID))

# Nodes not in the Shor Mcarty dataset 
legis_notMcarty = legislator_party[legislator_party$Party=="W",]
legis_notMcart_vec = as.vector(unlist(legis_notMcarty$Legislator_ID))


#Count of legislators by state 
with_state = data.frame( table(str_split_fixed(mydata$Legislator_ID, "_", 2)[,1]))
with_state = with_state[order(with_state$Freq),] #Odering by frequency

write.csv(with_state, file = "analysis/objects/legislator_by_state_updated.csv")

                   
########################################################
################# Bipartite Network ####################
########################################################

g=graph.data.frame(mydata,directed=FALSE)
bipartite.mapping(g)

# Format graph

V(g)$type <- bipartite.mapping(g)$type   # Add the "type" attribute to the network
V(g)$color <- ifelse(V(g)$type, "blue", "green") # Assigning colour to each node
V(g)$shape <- ifelse(V(g)$type, "circle", "rectangle") #Assigning shape to each mode
E(g)$color <- "light grey"

#Colour by party
#Democrats are blue
#Independents are yellow
V(g)$color[V(g)$name %in% legis_indep_vec] <- "yellow"
#Republicans are red
V(g)$color[V(g)$name %in% legis_repub_vec] <- "red"
# Nodes not in the Shor Mcarty dataset 
V(g)$color[V(g)$name %in% legis_notMcart_vec] <- "white"
        

# Bipartite plot 

set.seed(1006)

pdf("analysis/Images/bipartite_plot_updated.pdf", width = 150, height = 200) 

set.seed(1006)

plot(g,
     vertex.size=4,
     vertex.label.color="black",        # Colour of the label
     vertex.label.family="Helvetica",   # Font family of the label
     vertex.label.font=1,               # Font: 1 plain, 2 bold, 3 italics
     vertex.label.cex=3,              # Font size
     vertex.label.dist=0,               # Distance between the label and the vertex
     edge.arrow.size=2)
dev.off()


#Network summary stats
#the degree of a node i is just the number of connections it has
#Calculating degree of the legislator nodes 

lnodes = unique(mydata$Legislator_ID)
d = igraph::degree(g, lnodes)     #degrees
degree_dist = table(d)    #degree distribution

#plot degree distribution 
pdf("analysis/Images/legislator_degree_dist_updated.pdf", width = 11, height = 8) 

barplot(degree_dist,      #height
        col = "darkgrey",                               
        density = 30,                                 
        cex.names= 1,                                  
        main = "",                           
        xlab = "Number of Letters Signed",                               
        ylab = "Frequency",
        ylim = c(0,500)
)
dev.off()


#find the top 20 most connected nodes -- They are all in Shor-MCcarty
#ordering values and selecting the first 20
#gives the index values 
top_20_index = order(d, decreasing = TRUE )[1:20] 
d[top_20_index]

#Coverting to a dataframe
top_20_df = data.frame(Degree = d[top_20_index])
top_20_df <- cbind(Legislator_ID = rownames(top_20_df), top_20_df)
rownames(top_20_df) <- NULL

#Merging with Shor McCarty dataset to get Party, Name and State information 
m1 = merge(top_20_df, McCarty, by.x = "Legislator_ID", by.y = "Legislator_Id" ,all.x = TRUE)

#Selecting the desired columns 
cols = c("Legislator_ID","Degree","name","party","st")
summary_top_20 = m1[cols]
names(summary_top_20) <- c("Legislator's ID", "Degree", "Name","Party","State")
summary_top_20 = summary_top_20[order(-summary_top_20$Degree),]

#Saving for Latex
stargazer(summary_top_20, summary = FALSE, rownames = FALSE, type = "latex")


##################################################
############# Bipartite Projections ##############
##################################################

#Overlap Count/ MAtrix Multiplication method 
bipartite_matrix <- as_incidence_matrix(g)  #converts graph to a matrix 

save(list = "bipartite_matrix", file = "analysis/objects/bipartite_matrix.RData")

                                            #columns are legislators; rows are letters 
# trans_bipartite_matrix =  t(bipartite_matrix) #trnaspose it 
# 
# legis_matrix_prod = t(bipartite_matrix) %*% bipartite_matrix # to produce (legislator X legislator) network
# diag(legis_matrix_prod) <- 0 #since there are no loops
# 
# legis_overlap <- graph_from_adjacency_matrix(legis_matrix_prod, 
#                                              mode = "undirected", 
#                                              weighted = TRUE)

# #Plot all connections
# #pdf("analysis/bipartite_projections_all.pdf", width = 150, height = 250) 
# 
# plot(legis_overlap, 
#      vertex.size=6,
#      vertex.color = "light blue",
#      vertex.label.color="black",        # Colour of the label
#      vertex.label.family="Helvetica",   # Font family of the label
#      vertex.label.font=1,               # Font: 1 plain, 2 bold, 3 italics
#      vertex.label.cex=2.8,              # Font size
#      vertex.label.dist=0,               # Distance between the label and the vertex
#      edge.width=E(legis_overlap)$weight)  
# 
# #dev.off()
# 
# #Plot connections where weight is greater than 1 
# E(legis_overlap)$type = E(legis_overlap)$weight>1
# g.new <- subgraph.edges(legis_overlap,eids=which(E(legis_overlap)$type==TRUE))
# 
# #pdf("analysis/bipartite_projections_weight1.pdf", width = 150, height = 250) 
# 
# plot(g.new, 
#             edge.width=E(g.new)$weight,
#             vertex.color = "light blue",
#             vertex.size=6,
#             vertex.label.color="black",        # Colour of the label
#             vertex.label.family="Helvetica",   # Font family of the label
#             vertex.label.font=1,               # Font: 1 plain, 2 bold, 3 italics
#             vertex.label.cex=2.8,              # Font size
#             vertex.label.dist=0              # Distance between the label and the vertex
#              )  
# 
# #dev.off()
# 
# #Create Edgelist
# f = graph.adjacency(legis_matrix_prod,weighted=TRUE) 
# legis_weighted_edgelist = get.data.frame(f)
# 
# #Order df in decending value of weight
# legis_weighted_edgelist = legis_weighted_edgelist[order(-legis_weighted_edgelist$weight),] 
# 
# #Select ties with top 20 weights
# top_20_legis_ties = legis_weighted_edgelist[1:20,]
# 
# #Merging with Shor McCarty dataset to get Party, Name and State information 
# sub_McCarty = McCarty[c("name","party","st","Legislator_Id")]
# m2 = merge(top_20_legis_ties, sub_McCarty, by.x = "from", by.y = "Legislator_Id" ,all.x = TRUE )
# m3 = merge(m2, sub_McCarty, by.x = "to", by.y = "Legislator_Id" ,all.x = TRUE )
# 
# m3_reorder = m3[,c("name.x","st.x","party.x","name.y", "st.y","party.y","weight")]
# m3_reorder = m3_reorder[order(-m3_reorder$weight),]
# 
# #Rename Columns
# names(m3_reorder) <- c("Legislator 1 Name", "State 1", "Party 1",
#                            "Legislator 2 Name", "State 2", "Party 2",
#                            "Number of Co-Signatures")
# #Save for Latex
# stargazer(m3_reorder, summary = FALSE, rownames = FALSE, type = "text")
# 
# 
