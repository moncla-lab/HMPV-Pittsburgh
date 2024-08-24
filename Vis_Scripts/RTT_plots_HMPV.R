library(ggplot2)
library(dplyr)
library(cowplot)
# visaulize RTT regressions using data output from TempEst for maximum liklihood trees of each dataset

setwd("~/")

A2 <- read.csv("A2/tempest_res", sep = '\t')
B1 <- read.csv("B1/B1_pitt.tempest", sep = '\t')
B2 <- read.csv("B2/B2_tempest", sep = '\t')
all_pitt <- read.csv("all_geno_pitt.tempest", sep = '\t')

all_pitt_global <- read.csv("pitt_global.tempest", sep = '\t')
a2_glob <- read.csv("a2global/a2global.tempest", sep = '\t')
b1_glob <- read.csv("b1global/b2global.tempst", sep = '\t')
b2_glob <- read.csv("b2global/b2global.tempest", sep = '\t')


# Perform linear regression

# Plotting
A2plot <- ggplot(A2, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "A2", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
A2plot

B1plot <- ggplot(B1, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "B1", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
B1plot

B2plot <- ggplot(B2, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "B2", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
B2plot


allpitplot <- ggplot(all_pitt, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "All Pitt", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
allpitplot


combined_plot <- plot_grid(A2plot, B1plot, B2plot,allpitplot, ncol = 2)  # Arrange in a 2-column layout

# Display the combined plot
combined_plot

##########
# global plots


A2_globplot <- ggplot(a2_glob, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "A2 Global", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
A2_globplot



b1_globplot <- ggplot(b1_glob, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "B1 Global", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
b1_globplot


b2_globplot <- ggplot(b2_glob, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "B2 Global", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
b2_globplot



pitt_globplot <- ggplot(all_pitt_global, aes(x = date, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "All Pitt + Global", x = "Date", y = "Distance") +
  theme_minimal() +
  theme(text = element_text(size = 18))
pitt_globplot



combined_plot2 <- plot_grid(A2_globplot, b1_globplot, b2_globplot,pitt_globplot, ncol = 2)  # Arrange in a 2-column layout

# Display the combined plot
combined_plot2


