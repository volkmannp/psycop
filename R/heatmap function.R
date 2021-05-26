#-------------------------------------------------------------------------------
## Head
#  ______________
#-------------------------------------------------------------------------------

# Title:        Heatmap
# Version:      v10
# Authour:      Paul Volkmann
# Created:      10.02.2020
# Last edited:  26.05.2021

#-------------------------------------------------------------------------------
## General Comments
#  ______________
#-------------------------------------------------------------------------------

# This script provides the necessary lines of code to use the MetaCompound table to create a heatmap.
# It may be used in addition to graph.functions vXX.R by Marius Stephan.


# Load packages
library(tidyverse)
library(dplyr)
library(pheatmap)

# Harness data and merge Genotype and Environmental conditions to "Condition"
data.animal.matrix <- MetaCompound %>%
  unite(., col="Condition", Genotype, Environmental, sep= "_", remove = FALSE) %>%
  mutate_at(., vars("Animal"),list(as.character)) 
    
# Delete redundant columns
data.animal.matrix$Environmental = NULL
data.animal.matrix$Genotype = NULL
data.animal.matrix$Treatment = NULL
    
# Order factor levels of conditions for legend
data.animal.matrix$Condition <- factor(data.animal.matrix$Condition,
                                       levels = c("wt_hc", "wt_sd", "tg_hc", "tg_sd"))

# Calculate mean for every single group
length.col <- data.animal.matrix %>%
  colnames() %>%
  length() %>%
  as.numeric()

data.animal.matrix <- aggregate(data.animal.matrix[, 3:length.col],
                                list(data.animal.matrix$Condition), mean, na.rm = T)

# Convert dataframe to matrix and z-score       
data.animal.matrix <- data.animal.matrix %>%
  data.frame() %>%
  column_to_rownames("Group.1") %>%
  data.matrix() %>%
  scale()
          
# Subtract the wt_hc values from all other values
data.animal.matrix <- sweep(data.animal.matrix, 2, data.animal.matrix["wt_hc",], "-")
          
# Assign group names to groups
col.names.actual <- colnames(data.animal.matrix)
data.animal.names <- matrix(c("wt_hc", "wt_sd", "tg_hc", "tg_sd",
                          "wt_hc_mean", "wt_sd_mean", "tg_hc_mean",  "tg_sd_mean"),
                        nrow = 4, ncol =2)
data.animal.names <- as.data.frame(data.animal.names)
data.animal.names <- column_to_rownames(data.animal.names, "V1")
names(data.animal.names)[1] <- "Condition"
annotation <- list(Condition=(c(
  wt_hc_mean="#b4b4b4",
  wt_sd_mean="#3c3c3c",
  tg_hc_mean="#00BFFF",
  tg_sd_mean="#1e24fc")))

# Define legend label and size
legend_labels = c( "-2", "-1", "0", "+1", "+2")
legend_breaks = seq(-2, 2, by = 1)

# Specify color ranges
color_spec <- c("mediumpurple3", "mediumpurple1", "gray97", "tan1", "tan3")
breaksList <- c(-3, -2, -1, 1, 2, 3)
  
# Heatmapping of experiments
heatmap <- pheatmap(data.animal.matrix,
                    main = "Heatmap",
                    fontsize = 16,
                    fontsize_col = 12,
                    show_rownames = F,
                    treeheight_row = 43,
                    cellheight = 20,
                    cluster_cols = F,
                    cluster_rows = T,
                    cutree_cols = 1,
                    cutree_rows = 1,
                    gaps_col = F,
                    clustering_distance_rows = "manhattan",
                    clustering_distance_cols = "manhattan",
                    color = color_spec,
                    breaks = breaksList,
                    legend_breaks = legend_breaks,
                    legend_labels = legend_labels,
                    border_color = "grey55",
                    annotation_row = data.animal.names,
                    annotation_colors = annotation)
