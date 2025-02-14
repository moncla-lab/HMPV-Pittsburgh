library(ggtree)
library(ggplot2)
library(treeio)
library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(MoMAColors)
library(cowplot)

# script to visualize phylogenies of HMPV 
setwd("~/")

pitt_global_seq_data <- read.csv("../../../../Pitt_global_metadata_seqmerge.csv")
pitt_seq_data_sub <- pitt_global_seq_data %>% select(taxaname, Ginsertion)

################
A2 <- read.beast("A2/comb/A2_pitt_all.mcc.tree")

p1 <- ggtree(A2,  mrsd='2020-04-09') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle("A2") 

p1

p1 <- p1 %<+% pitt_seq_data_sub +
  geom_tippoint(aes(color=Ginsertion), size=3, alpha=1) +
  scale_color_moma_d("Levine2")
  
p1
  #scale_color_manual(values = c("0" = "#332288", "111" = "#117733", "180" = "#44AA99", "69" = "#88CCEE"))





#################
B1 <- read.beast("B1/comb/B1_pitt.mcc.tree")

p2 <- ggtree(B1,  mrsd='2020-04-06') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.1, .85)) +
  ggtitle("B1") 

p2

p2 <- p2 %<+% pitt_seq_data_sub +
  geom_tippoint(size=3, alpha=1) 


####################
B2 <- read.beast("B2/B2_pitt/B2_pitt.mcc.tree")

p3 <- ggtree(B2,  mrsd='2020-04-20') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle("B2") 

p3

p3 <- p3 %<+% pitt_seq_data_sub +
  geom_tippoint(aes(color=Ginsertion), size=3, alpha=1) +
  scale_color_moma_d("Levine2")

p3
  
  #scale_color_manual(values = c("0" = "#004E64", "3" = "#00A5CF", "6" = "#9FFFCB", "9" = "#25A18E","12" = "#7AE582"))


####################
all_pitt <- read.beast("B2/all/pitt/All_geno_pitt.mcc.tree")

p4 <- ggtree(all_pitt,  mrsd='2020-04-20') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.1, .85)) +
  ggtitle("All Pitt") 

p4

# p4 %<+% pitt_seq_data_sub +
#   geom_tippoint(aes(color=Ginsertion),size=3, alpha=1) + 
#   scale_color_manual(values = c("0" = "#004E64", "3" = "#00A5CF", "6" = "#9FFFCB", "9" = "#25A18E","12" = "#7AE582","0" = "#332288", "111" = "#117733", "180" = "#44AA99", "69" = "#88CCEE"))
# 


###################


combined_plot2 <- plot_grid(p1, p2, p3,p4, ncol = 2)  # Arrange in a 2-column layout

# Display the combined plot
combined_plot2


##### Global trees

###############
pitt_global <- read.beast("B2/all/global_pitt/Pitt_global_metadata_seqmerge.mcc.tree")
pitt_seq_data_sub2 <- pitt_global_seq_data %>% select(taxaname, Continent, Ginsertion)
pitt_seq_data_sub2$Ginsertion <- as.character(pitt_seq_data_sub2$Ginsertion)
A2g <- read.beast("B2/A2_v2/comb/pitt_global_a2.mcc.tree")

p1 <- ggtree(A2g,  mrsd='2023-10-19') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle(" A2") 

p1

p1 <- p1 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent, shape = Ginsertion), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1) +
  scale_shape_manual(breaks = c('0','1','3','9','66','69','104','111','180','181'), values=c(19,15,17,23,25,0,2,7,8,9))
 
  #scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p1



p1v <- ggtree(A2g,  mrsd='2023-10-19', aes(color =geo )) +
 # geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  scale_color_moma_d("OKeeffe", direction = -1) +
  ggtitle(" A2") 

p1v

p1v <- p1v %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1)

#scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p1v


#################
B1g <- read.beast("B2/B1/pitt_global_b1.mcc.tree")
pitt_seq_data_sub2 <- pitt_global_seq_data %>% select(taxaname, Continent, Ginsertion)



p2 <- ggtree(B1g,  mrsd='2022-06-01') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle(" B1") 

p2

p2 <- p2 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1)
  
p2  
  #scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p2





####################
B2g <- read.beast("B2/B2_global/pitt_global_b2.mcc.tree")
pitt_seq_data_sub2 <- pitt_global_seq_data %>% select(taxaname, Continent, Ginsertion)
pitt_seq_data_sub2$Ginsertion <- as.character(pitt_seq_data_sub2$Ginsertion)


p3 <- ggtree(B2g,  mrsd='2023-06-01') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=10, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle(" B2") 

p3

p3 <- p3 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent, shape = Ginsertion), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1) +
  scale_shape_manual(breaks = c('0','1','3','4','5','6','9','12'), values=c(19,15,17,23,25,0,2,7))

  #scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p3


p3v <- ggtree(B2g,  mrsd='2023-06-01', aes(color =geo)) +
  #geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  scale_color_moma_d("OKeeffe", direction = -1) +
  ggtitle(" B2") 

p3v

p3v <- p3v %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1)

p3v


####################

pitt_global <- read.beast("all/global_pitt/comb/Pitt_global_metadata_seqmerge.mcc.tree")
pitt_seq_data_sub2 <- pitt_global_seq_data %>% select(taxaname, Continent)

p5 <- ggtree(pitt_global,  mrsd='2023-10-19') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.1, .85)) +
  ggtitle("Global") 

p5

p5 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent),size=1, alpha=1) +
  scale_color_manual(values = c("Africa" = "black", "Asia" = "black", "Europe" = "black", "North_America" = "black","Pittsburgh" = "#FB8500", "South_America" = "black"))




combined_plot3 <- plot_grid(p1, p2, p3, ncol = 2)  # Arrange in a 2-column layout

# Display the combined plot
combined_plot3




