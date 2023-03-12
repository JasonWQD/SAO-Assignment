# Setup -------------------------------------------------------------------
# load packages
if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, PMA, rlist, latex2exp, knitr, formatR, devtools, reshape2, 
       ggplot2, ggbiplot, factoextra, pracma, gtools,sparsepca, ggplot2)

# set seed
set.seed(321)


# import custom functions
source("Week 3/dev/functions.r")

# writing format objects
mytheme <- theme_bw() + theme(legend.position = "bottom")
mytheme <- theme_bw() + theme(legend.position = "bottom")

#------------------------ Data Processing ---------------------------
sName <- "Week 3/FIFA2017_NL.rdata"
lDFs <- Data_Prepare(sName) %>% DropAndScale()
# Processed dataframe
dfData = lDFs[[1]]

# Sub-dataframes filtered by player positions
dfDefense = lDFs[[2]]
dfMidfield = lDFs[[3]]
dfAttack = lDFs[[4]]
dfGoal = lDFs[[5]]

#------------------------PCA Using Package---------------------------
## Analysis for each position
#------------Defense--------------
lPCA_def = prcomp(dfDefense, center = FALSE, scale = FALSE)
#Scree plot 
fviz_eig(lPCA_def) 
# Variable plot
fviz_pca_var(lPCA_def,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.var = list(name =NULL, cos2 = NULL, contrib = 8) #Get 8 most important PCAs
)

#-----------------Midfield-----------------
lPCA_mid = prcomp(dfMidfield, center = FALSE, scale = FALSE)
#Scree plot 
fviz_eig(lPCA_mid) 
# Variable plot
fviz_pca_var(lPCA_mid,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.var = list(name =NULL, cos2 = NULL, contrib = 8)
)

#------------------Attack------------------
lPCA_attk = prcomp(dfAttack, center = FALSE, scale = FALSE)
#Scree plot 
fviz_eig(lPCA_attk) 
# Variable plot
fviz_pca_var(lPCA_attk,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.var = list(name =NULL, cos2 = NULL, contrib = 8)
)

#----------------Goal keeper-----------------
lPCA_goal = prcomp(dfGoal, center = FALSE, scale = FALSE)
#Scree plot 
fviz_eig(lPCA_goal) 
# Variable plot
fviz_pca_var(lPCA_goal,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.var = list(name =NULL, cos2 = NULL, contrib = 8)
)


#------------------------ Sparse PCA Manual -----------------------------
i_max <- 30
mV <- Sparse_PCA(dfData, i_max)
barplot(mV[, 3])

#------------------------ Sparse PCA Using Package -----------------------

#--------------Defense-------------
sparse_PCA_def = SPC(dfDefense, sumabs = sqrt(ncol(dfData)),K=29, center = FALSE, trace = FALSE, niter = i_max)
mV_sparse_def= sparse_PCA_def$v
l_Var_selected_def = var_spca(mV_sparse_def,8)

# Most influential variable plot
ggplot(l_Var_selected_def, aes(x=rownames(l_Var_selected_def), y=l_Var_selected_def$.)) + 
  geom_bar(stat = "identity",fill="#f68060", alpha=.6, width=.4) + coord_flip() +xlab("Variables") + ylab("Importance") +theme_bw()
# Scree plot
dfSPCAscree_def= process_Scree(sparse_PCA_def)
plotScree(dfSPCAscree_def)

#--------------Attack--------------
sparse_PCA_atk = SPC(dfAttack, sumabs = sqrt(ncol(dfData)),K=29, center = FALSE, trace = FALSE, niter = i_max)
mV_sparse_atk= sparse_PCA_atk$v
l_Var_selected_atk = var_spca(mV_sparse_atk,8)

# Most influential variable plot
ggplot(l_Var_selected_atk, aes(x=rownames(l_Var_selected_atk), y=l_Var_selected_atk$.)) + 
  geom_bar(stat = "identity",fill="#f68060", alpha=.6, width=.4) + coord_flip() +xlab("Variables") + ylab("Importance") +theme_bw()
# Scree plot
dfSPCAscree_atk= process_Scree(sparse_PCA_atk)
plotScree(dfSPCAscree_atk)

#-------------Midfield--------------
sparse_PCA_mid = SPC(dfMidfield, sumabs = sqrt(ncol(dfData)),K=29, center = FALSE, trace = FALSE, niter = i_max)
mV_sparse_mid= sparse_PCA_mid$v
l_Var_selected_mid = var_spca(mV_sparse_mid,8)

# Most influential variable plot
ggplot(l_Var_selected_mid, aes(x=rownames(l_Var_selected_mid), y=l_Var_selected_mid$.)) + 
  geom_bar(stat = "identity",fill="#f68060", alpha=.6, width=.4) + coord_flip() +xlab("Variables") + ylab("Importance") +theme_bw()
# Scree plot
dfSPCAscree_mid= process_Scree(sparse_PCA_mid)
plotScree(dfSPCAscree_mid)
  
#------------Goalkeeper-------------
sparse_PCA_gk = SPC(dfGoal, sumabs = sqrt(ncol(dfData)),K=29, center = FALSE, trace = FALSE, niter = i_max)
mV_sparse_gk= sparse_PCA_gk$v
l_Var_selected_gk = var_spca(mV_sparse_gk,8)

# Most influential variable plot
ggplot(l_Var_selected_gk, aes(x=rownames(l_Var_selected_gk), y=l_Var_selected_gk$.)) + 
  geom_bar(stat = "identity",fill="#f68060", alpha=.6, width=.4) + coord_flip() +xlab("Variables") + ylab("Importance") +theme_bw()
# Scree plot
dfSPCAscree_gk= process_Scree(sparse_PCA_gk)
plotScree(dfSPCAscree_gk)

