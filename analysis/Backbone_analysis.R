rm(list=ls())
####################################################################
#########################       Backbone        ####################
####################################################################

setwd('/Users/ishitagopal/Box/Projects/science_backed_policy_diffusion')

library(backbone)
library(dplyr)

# Bipartite adjacency matrix -- rows are legislators and columns are the letters they sign
load('analysis/objects/bipartite_matrix.RData')
bmatrix <- t(bipartite_matrix)

# # Bipartite projection -- legislator by legislator
# bprojection <- t(bipartite_matrix)%*%(bipartite_matrix)
# 
# 
# # upper threshold = 0, 1, 2, 3, 4, 5, 6, 7
# # if two legislators have co-signed any letter (co-signature > 0), there will be an edge between the two
# universal_0 <- universal(bmatrix, upper = 0, bipartite = T)
# universal_1 <- universal(bmatrix, upper = 1, bipartite = T)
# universal_2 <- universal(bmatrix, upper = 2, bipartite = T)
# universal_3 <- universal(bmatrix, upper = 3, bipartite = T)
# universal_4 <- universal(bmatrix, upper = 4, bipartite = T)
# universal_5 <- universal(bmatrix, upper = 5, bipartite = T)
# universal_6 <- universal(bmatrix, upper = 6, bipartite = T)
# universal_7 <- universal(bmatrix, upper = 7, bipartite = T)
# 
# 
# # plot 
# graph0 <- igraph::graph_from_adjacency_matrix(universal_0$backbone, mode = "undirected")
# graph1 <- igraph::graph_from_adjacency_matrix(universal_1$backbone, mode = "undirected")
# graph2 <- igraph::graph_from_adjacency_matrix(universal_2$backbone, mode = "undirected")
# graph3 <- igraph::graph_from_adjacency_matrix(universal_3$backbone, mode = "undirected")
# graph4 <- igraph::graph_from_adjacency_matrix(universal_4$backbone, mode = "undirected")
# graph5 <- igraph::graph_from_adjacency_matrix(universal_5$backbone, mode = "undirected")
# graph6 <- igraph::graph_from_adjacency_matrix(universal_6$backbone, mode = "undirected")
# graph7 <- igraph::graph_from_adjacency_matrix(universal_7$backbone, mode = "undirected")
# 
# pdf("analysis/Images/universal.pdf", width = 20, height = 15) 
# op <- par(mfrow=c(2, 4),oma = c(0, 0, 4, 0), mar = c(0,0,4,0))
# plot(graph0, vertex.size =2, vertex.label = NA, main ="threshold = 0")
# plot(graph1, vertex.size =2, vertex.label = NA, main ="threshold = 1", layout = 'lo')
# plot(graph2, vertex.size =2, vertex.label = NA, main ="threshold = 2", layout = 'lo')
# plot(graph3, vertex.size =2, vertex.label = NA, main ="threshold = 3", layout = 'lo')
# plot(graph4, vertex.size =2, vertex.label = NA, main ="threshold = 4", layout = 'lo')
# plot(graph5, vertex.size =2, vertex.label = NA, main ="threshold = 5", layout = 'lo')
# plot(graph6, vertex.size =2, vertex.label = NA, main ="threshold = 6")
# plot(graph7, vertex.size =2, vertex.label = NA, main ="threshold = 7")
# mtext("Universal Backbone derived for various upper threshholds", outer = TRUE, cex = 1.5)
# 
# dev.off()

############## Compute bipartite backbone using hypergeometric distribution and fixed degree sequence model ############

#load("analysis/objects/hyperg_props.Rdata")
#load("analysis/objects/fdsm_props.Rdata")

# Hypergeometric distribution
hyperg_props <- hyperg(bmatrix)
save(list = "hyperg_props", file = "analysis/objects/hyperg_props.Rdata")

# Fixed Degree Sequence Model
fdsm_props <- fdsm(bmatrix, trials = 1000, sparse = TRUE, dyad=c(1,5),progress = T)  # takes 606 secs
save(list = "fdsm_props", file = "analysis/objects/fdsm_props.Rdata")


#########################  Compute Backbone Matrix ####################################

################### From hypergeometric distribution with alpha .1, .05 and .025 ##############

load("analysis/objects/hyperg_bb_ten.Rdata")

# extract backbone at different alphas
hyperg_bb_ten <- backbone.extract(hyperg_props$positive, 
                                  hyperg_props$negative, 
                                  alpha = 0.1)

hyperg_bb_five <- backbone.extract(hyperg_props$positive, 
                                   hyperg_props$negative, 
                                   alpha = .05)

hyperg_bb_two.five <- backbone.extract(hyperg_props$positive, 
                                       hyperg_props$negative, 
                                       alpha = .025)

# replace -1 with 0 
hyperg_bb_ten[hyperg_bb_ten < 0] <- 0
hyperg_bb_five[hyperg_bb_five < 0] <- 0 
hyperg_bb_two.five[hyperg_bb_two.five < 0] <- 0 

# Save Objects 
save(list = "hyperg_bb_ten", file = "analysis/objects/hyperg_bb_ten.Rdata")
save(list = "hyperg_bb_five", file = "analysis/objects/hyperg_bb_five.Rdata")
save(list = "hyperg_bb_two.five", file = "analysis/objects/hyperg_bb_two.five.Rdata")

##############################################################
######### No. of legislators  in the backbone matrix #########
##############################################################
# calculate row sums for backbone matrix which is of the form: (Legis X Legis)
# if rowsum is greater than zero, include legislator 

# count all included legislators 

hyper_legis_ten <- rowSums(hyperg_bb_ten)

###########################################################################
# Checking which legislator got relected -- IGNORE this block 
# Extract ids of non zero legislators 
# -- too see how many won re-election
non_zero <- hyper_legis_ten[hyper_legis_ten>0]
length(non_zero)

non_zero.names <- data.frame(names(non_zero)) %>%
  mutate_all(as.character)

# Load IDs 
shor_ids <- read.csv("Dataset/shor_mcarty_1993_to_2016_with_Id.csv") %>%
  mutate_all(as.character)
non_shor_ids <- read.csv("Dataset/Unique_IDs_Not_ShorMCarty.csv") %>%
  mutate_all(as.character)

# Remove duplicate ids in non_shor
non_shor_ids <- non_shor_ids[!duplicated(non_shor_ids$Legislator_Id),]


joined <- non_zero.names %>%
  left_join(shor_ids[c("Legislator_Id","name", "st")],
            by = "Legislator_Id") %>%
  left_join(non_shor_ids[c("Legislator_Id","name", "State")],
            by = "Legislator_Id") %>%
  mutate_all(as.character)

joined$name.x[is.na(joined$name.x)] <- joined$name.y[!is.na(joined$name.y)]
joined$st[is.na(joined$st)] <- joined$State[!is.na(joined$State)]

joined <- joined %>%
  select(-name.y,-State) %>%
  rename(name = name.x) 

# save as csv 
write.csv(joined, "Dataset/580_legis_from_table3_in_paper.csv")
  
###########################################################################


sum(ifelse(hyper_legis_ten > 0 ,1 , 0))

hyper_legis_five <- rowSums(hyperg_bb_five)
sum(ifelse(hyper_legis_five > 0 ,1 , 0))

hyper_legis_two.five <- rowSums(hyperg_bb_two.five)
sum(ifelse(hyper_legis_two.five > 0 ,1 , 0))

####################### Create a matrix that depicts all possible cross state ties #############

load("analysis/objects/diff_state.RData")

diff_state <- matrix(NA, nrow = nrow(hyperg_bb_ten), ncol = ncol(hyperg_bb_ten))

rownames(diff_state) <- rownames(hyperg_bb_ten)
colnames(diff_state) <- colnames(hyperg_bb_ten)

for (i in 1:nrow(diff_state)) {
  for (j in 1:ncol(diff_state)){
    state_i <- substr(rownames(diff_state)[i], 1, 2)
    state_j <- substr(colnames(diff_state)[j], 1, 2)
    diff_state[i,j] <- 1-1*(state_i==state_j)
    
  }
}                   

save(diff_state, file = "analysis/objects/diff_state.RData")

###############################################################################################

# No. of positive ties 

table(hyperg_bb_ten)[2]/2
table(hyperg_bb_five)[2]/2
table(hyperg_bb_two.five)[2]/2


# No. of cross state ties in the backbone matrix
# Calculation: elementwise multiplication 1*1  (or 1*-1) will retain the tie; 1*0 will delete the tie

hyper_crossties_ten = hyperg_bb_ten*diff_state
hyper_crossties_five = hyperg_bb_five*diff_state
hyper_crossties_two.five = hyperg_bb_two.five*diff_state

# no. of cross state positive ties 

table(hyper_crossties_ten)[2]/2
table(hyper_crossties_five)[2]/2
table(hyper_crossties_two.five)[2]/2


# No. of legislators with at least 1, 2, 3, 4, 5 crosstate ties 
# alpha = .10

sum(ifelse(rowSums(hyper_crossties_ten) >= 1, 1,0))

#a <- ifelse(rowSums(hyper_crossties_ten) >= 1, 1,0)
#b <- names(a[a==1])


sum(ifelse(rowSums(hyper_crossties_ten) >= 2, 1,0))
sum(ifelse(rowSums(hyper_crossties_ten) >= 3, 1,0))
sum(ifelse(rowSums(hyper_crossties_ten) >= 4, 1,0))
sum(ifelse(rowSums(hyper_crossties_ten) >= 5, 1,0))

# alpha = .05
sum(ifelse(rowSums(hyper_crossties_five) >= 1, 1,0))
sum(ifelse(rowSums(hyper_crossties_five) >= 2, 1,0))
sum(ifelse(rowSums(hyper_crossties_five) >= 3, 1,0))
sum(ifelse(rowSums(hyper_crossties_five) >= 4, 1,0))
sum(ifelse(rowSums(hyper_crossties_five) >= 5, 1,0))

# alpha = .025
sum(ifelse(rowSums(hyper_crossties_two.five) >= 1, 1,0))
sum(ifelse(rowSums(hyper_crossties_two.five) >= 2, 1,0))
sum(ifelse(rowSums(hyper_crossties_two.five) >= 3, 1,0))
sum(ifelse(rowSums(hyper_crossties_two.five) >= 4, 1,0))
sum(ifelse(rowSums(hyper_crossties_two.five) >= 5, 1,0))

# Graph backbone 
graph1 <- igraph::graph_from_adjacency_matrix(hyperg_bb_ten, mode = "undirected")
graph2 <- igraph::graph_from_adjacency_matrix(hyperg_bb_five, mode = "undirected")
graph3 <- igraph::graph_from_adjacency_matrix(hyperg_bb_two.five, mode = "undirected")

set.seed(100)
pdf("analysis/images/hyperg.pdf", width = 10, height = 5) 

op <- par(mfrow=c(1, 3),oma = c(0, 0, 2, 0), mar = c(0,0,4,0))

plot(graph1, vertex.label = NA, 
     layout = layout.fruchterman.reingold, 
     vertex.size = 2, 
     main="For alpha set at .1", 
     cex.main=2)

plot(graph2, vertex.label = NA, layout = layout.fruchterman.reingold, vertex.size = 2, main="For alpha set at .05", cex.main=2)
plot(graph3, vertex.label = NA, layout = layout.fruchterman.reingold, vertex.size = 2, main="For alpha set at .025", cex.main=2)

mtext("Backbone derived from hypergeometric distribution with alpha .1, .05 and .025", outer = TRUE, cex = 1.5)

dev.off()

##################### From Fixed Degree Sequence Model with alpha .1, .05 and .025 ################################

# backbone 
fdsm_props_ten <- backbone.extract(fdsm_props$positive, fdsm_props$negative, alpha = 0.1)
fdsm_props_five <- backbone.extract(fdsm_props$positive, fdsm_props$negative, alpha = .05)
fdsm_props_two.five <- backbone.extract(fdsm_props$positive, fdsm_props$negative, alpha = .025)

# replace -1 with 0 in backbone matrix 

fdsm_props_ten[fdsm_props_ten < 0] <- 0
fdsm_props_five[fdsm_props_five < 0] <- 0
fdsm_props_two.five[fdsm_props_two.five < 0] <- 0

## save 

# Save Objects 
save(list = "fdsm_props_ten", file = "analysis/objects/fdsm_props_ten.Rdata")
save(list = "fdsm_props_five", file = "analysis/objects/fdsm_props_five.Rdata")
save(list = "fdsm_props_two.five", file = "analysis/objects/fdsm_props_two.five.Rdata")


# No. of legislators in the network 

fdsm_legis_ten <- rowSums(fdsm_props_ten)
sum(ifelse(fdsm_legis_ten > 0 ,1 , 0))

fdsm_legis_five <- rowSums(fdsm_props_five)
sum(ifelse(fdsm_legis_five > 0 ,1 , 0))

fdsm_legis_two.five <- rowSums(fdsm_props_two.five)
sum(ifelse(fdsm_legis_two.five > 0 ,1 , 0))

# No. of positive ties 

table(fdsm_props_ten)[2]/2
table(fdsm_props_five)[2]/2
table(fdsm_props_two.five)[2]/2


# no. of cross state ties
fdsm_crossties_ten = fdsm_props_ten*diff_state
fdsm_crossties_five = fdsm_props_five*diff_state
fdsm_crossties_two.five = fdsm_props_two.five*diff_state

# no. of cross state ties positive ties 
table(fdsm_crossties_ten)[2]/2
table(fdsm_crossties_five)[2]/2
table(fdsm_crossties_two.five)[2]/2


# No. of legislators with at least 1, 2, 3, 4, 5 crosstate ties 
# alpha = .10

sum(ifelse(rowSums(fdsm_crossties_ten) >= 1, 1,0))
sum(ifelse(rowSums(fdsm_crossties_ten) >= 2, 1,0))
sum(ifelse(rowSums(fdsm_crossties_ten) >= 3, 1,0))
sum(ifelse(rowSums(fdsm_crossties_ten) >= 4, 1,0))
sum(ifelse(rowSums(fdsm_crossties_ten) >= 5, 1,0))

# alpha = .05
sum(ifelse(rowSums(fdsm_crossties_five) >= 1, 1,0))
sum(ifelse(rowSums(fdsm_crossties_five) >= 2, 1,0))
sum(ifelse(rowSums(fdsm_crossties_five) >= 3, 1,0))
sum(ifelse(rowSums(fdsm_crossties_five) >= 4, 1,0))
sum(ifelse(rowSums(fdsm_crossties_five) >= 5, 1,0))

# alpha = .025
sum(ifelse(rowSums(fdsm_crossties_two.five) >= 1, 1,0))
sum(ifelse(rowSums(fdsm_crossties_two.five) >= 2, 1,0))
sum(ifelse(rowSums(fdsm_crossties_two.five) >= 3, 1,0))
sum(ifelse(rowSums(fdsm_crossties_two.five) >= 4, 1,0))
sum(ifelse(rowSums(fdsm_crossties_two.five) >= 5, 1,0))


# graph backbone 
graph1 <- igraph::graph_from_adjacency_matrix(fdsm_props_ten, mode = "undirected")
graph2 <- igraph::graph_from_adjacency_matrix(fdsm_props_five, mode = "undirected")
graph3 <- igraph::graph_from_adjacency_matrix(fdsm_props_two.five, mode = "undirected")


set.seed(100)
pdf("analysis/fdsm.pdf", width = 10, height = 5) 

op <- par(mfrow=c(1, 3),oma = c(0, 0, 2, 0), mar = c(0,0,4,0))
lo <- igraph::layout_(graph, igraph::with_fr())

plot(graph1, vertex.label = NA, layout = lo, vertex.size = 2, main="For alpha set at .1", cex.main=2)
plot(graph2, vertex.label = NA, layout = lo, vertex.size = 2, main="For alpha set at .05", cex.main=2)
plot(graph3, vertex.label = NA, layout = lo, vertex.size = 2, main="For alpha set at .025", cex.main=2)
mtext("Backbone derived from the fixed degree sequence model with alpha .1, .05 and .025", outer = TRUE, cex = 1.5)

dev.off()


################# Hamming Distance ###################
library(Matrix)
#?hdist

file_names <- c('hyperg_bb_ten.RData', 'hyperg_bb_five.RData', 'hyperg_bb_two.five.RData',
                    'fdsm_props_ten.RData' ,'fdsm_props_five.RData', 'fdsm_props_two.five.RData')
lapply(file_names,load,.GlobalEnv)

# stack adjacency matrices 
graph_stack <- list(hyperg_bb_ten, hyperg_bb_five, hyperg_bb_two.five,
                    fdsm_props_ten,fdsm_props_five, fdsm_props_two.five) 


# # calculate hamming distance 
# hamming_distance <- hdist(graph_stack, mode="graph")
# 
# # extract lower triangle 
# hamming_distance <- tril(hamming_distance)
# 
# # 
# 
# data.frame(hamming_distance)
# 
# stargazer(hamming_distance, 
#           summary = F ,
#           covariate.labels=c('','0.1','0.05','0.025','0.1','0.05','0.025'),
#           column.labels = c('Hypergeometric', 'FDSM '), 
#           column.separate = c(3,3),
#           type = 'text')
# 
########################### Graph Correlations #########################
library(sna)
library(Matrix)


graph_corr <- gcor(graph_stack)
round(tril(graph_corr),3)



