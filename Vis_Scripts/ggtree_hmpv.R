library(ggtree)
library(ggplot2)
library(treeio)
library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(MoMAColors)

setwd("~/")

pitt_global_seq_data <- read.csv("~/")
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
B2 <- read.beast("B2/comb/B2_pitt.mcc.tree")

p3 <- ggtree(B2,  mrsd='2020-04-20') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle("B2") 

p3

p3 <- p3 %<+% pitt_seq_data_sub +
  geom_tippoint(aes(color=Ginsertion), size=3, alpha=1) +
  scale_color_moma_d("Levine2")
  
  #scale_color_manual(values = c("0" = "#004E64", "3" = "#00A5CF", "6" = "#9FFFCB", "9" = "#25A18E","12" = "#7AE582"))


####################
all_pitt <- read.beast("all/pitt/comb/All_geno_pitt.mcc.tree")

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



###############
pitt_global <- read.beast("all/global_pitt/comb/Pitt_global_metadata_seqmerge.mcc.tree")
pitt_seq_data_sub2 <- pitt_global_seq_data %>% select(taxaname, Continent)
A2g <- read.beast("all_globalsep/A2_v2/comb/pitt_global_a2.mcc.tree")

p1 <- ggtree(A2g,  mrsd='2023-10-19') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle(" A2") 

p1

p1 <- p1 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1)
 
  #scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p1





#################
B1g <- read.beast("all_globalsep/B1/comb/pitt_global_b1.mcc.tree")
pitt_global <- read.beast("all/global_pitt/comb/Pitt_global_metadata_seqmerge.mcc.tree")
pitt_seq_data_sub2 <- pitt_global_seq_data %>% select(taxaname, Continent)



p2 <- ggtree(B1g,  mrsd='2022-06-01') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle(" B1") 

p2

p2 <- p2 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1)
  
  
  #scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p2

####################
B2g <- read.beast("all_globalsep/B2/comb/pitt_global_b2.mcc.tree")

p3 <- ggtree(B2g,  mrsd='2023-06-01') +
  geom_range(range='height_0.95_HPD', color='lightgrey', alpha=5, size=0.75) +
  theme_tree2()  +
  theme(text = element_text(size = 18), legend.position = c(.2, .6)) +
  ggtitle(" B2") 

p3

p3 <- p3 %<+% pitt_seq_data_sub2 +
  geom_tippoint(aes(color=Continent), size=3, alpha=1) +
  scale_color_moma_d("OKeeffe", direction = -1)

  #scale_color_manual(values = c("Africa" = "#FB8500", "Asia" = "#FFB703", "Europe" = "#8ECAE6", "North_America" = "#219EBC","Pittsburgh" = "#126782", "South_America" = "#023047"))

p3
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




