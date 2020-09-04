library('treeio')
library('ggtree')
library('ggplot2')
library('ape')
library('tidytree')
library('tibble')
library('dplyr')
library('ggnewscale')
library('viridis')

require(gridExtra)

# setwd('/Users/hussais/Documents/R_plots/GGtree_code/h3neut_VCM')

newick_file <- read.newick("subs.newick")
tree_file <- as_tibble(newick_file)

#gives lables as without space at month despite not like that in newick tree file
#extract node and nonsynsubs info from substitutions.tree using matchsubs.py

#make a tibble for node vs nonsynsubs
data <- read.csv("final_nonsynsubs2.csv", stringsAsFactors = FALSE)
dt2 <- as_tibble(data)

#join the two tibbles on 'node'
y1 <- full_join(tree_file, dt2, by = 'node')
new_newick2 <- as.treedata(y1)

#plot tree corresponding to nonsynsubs and clades
tree_fig2 <- ggtree(new_newick2) + geom_tiplab(size = 1.3, align = TRUE, linesize = .2) + geom_text(aes(x = branch, label = nonsynsubs), vjust = -0.6, fontface = "bold", size = 1.75) + geom_treescale()

#same for region
region <- read.csv("h3neut_region.csv", stringsAsFactors = FALSE)

region <- apply(region, 2, function(x)
  gsub('\\s+', '', x))
region_df <- data.frame(region)

rownames(region_df) <- region_df$label
region_df$label <- NULL

# #same for fold titres
fold_titres <- read.csv("h3neut_fold.csv", stringsAsFactors=FALSE)

# ...and then convert to dataframe
fold_titres <- apply(fold_titres, 2, function(x)gsub('\\s+', '',x))
fold_titres_df <- data.frame(fold_titres)

rownames(fold_titres_df) <- fold_titres_df$label
fold_titres_df$label <- NULL

options(digits=6) # to keep two decimal spaces

# Convert values etc
fold_titres[fold_titres == "REF"] <- "0"
fold_titres[fold_titres == "L4"] <- "1"
fold_titres[fold_titres == "4"] <- "2"
fold_titres[fold_titres == "8"] <- "3"
fold_titres[fold_titres == "G8"] <- "4"
fold_titres[fold_titres == "HG160"] <- "5"
fold_titres[fold_titres == "NH160"] <- "6"
fold_titres[fold_titres == "NR"] <- "7"
# define required constant levels 
titre_levels <- c(0,1,2,3,4,5,6,7)

# remove the space from title 
fold_titres <- apply(fold_titres, 2, function(x)
  gsub('\\s+', '', x))

# ...and then convert to dataframe
fold_titres_df <- data.frame(fold_titres)

# convert rownames to label
fold_titres_df$label <- NULL


# convert the dataframe to numeric
fold_titres_df <- mutate_all(fold_titres_df[], function(x) as.numeric(as.character(x)))
rownames(fold_titres_df) <- data.frame(fold_titres)$label


# apply the levels to the dataframe columns
for (i in 1:ncol(fold_titres_df)) {
  fold_titres_df[,i] <- factor(fold_titres_df[,i], levels=titre_levels, ordered=TRUE)
  fold_titres_df[is.na(fold_titres_df)] <- 0
}

cols <- c("black","azure3", "khaki1","goldenrod1","darkorange2","green","skyblue1","red")
labs <- c("Ref","< 4","4","8","> 8",">= 160 (when homolgous titre >= 2560)", ">= 160 (No homologous titre)", "<" )

options(digits=6) # to keep two decimal spaces

# read the hi file
fold_titres <- read.csv("h3_h1_fold.csv", stringsAsFactors=FALSE)

# Convert values etc
fold_titres[fold_titres == "REF"] <- "0"
fold_titres[fold_titres == "L4"] <- "1"
fold_titres[fold_titres == "4"] <- "2"
fold_titres[fold_titres == "8"] <- "3"
fold_titres[fold_titres == "G8"] <- "4"
fold_titres[fold_titres == "HG160"] <- "5"
fold_titres[fold_titres == "NH160"] <- "6"
fold_titres[fold_titres == "NR"] <- "7"
# define required constant levels
titre_levels_hi <- c(0,1,2,3,4,5,6,7)

# remove the space from title
fold_titres_hi <- apply(fold_titres, 2, function(x)
  gsub('\\s+', '', x))

# ...and then convert to dataframe
fold_titres_hi_df <- data.frame(fold_titres)

# convert rownames to label
fold_titres_hi_df$label <- NULL


# convert the dataframe to numeric
fold_titres_hi_df <- mutate_all(fold_titres_hi_df[], function(x) as.numeric(as.character(x)))
rownames(fold_titres_hi_df) <- data.frame(fold_titres_hi)$label


# apply the levels to the dataframe columns
for (i in 1:ncol(fold_titres_hi_df)) {
  fold_titres_hi_df[,i] <- factor(fold_titres_hi_df[,i], levels=titre_levels_hi, ordered=TRUE)
  fold_titres_hi_df[is.na(fold_titres_hi_df)] <- 0
}

cols_hi <- c("black","azure3", "khaki1","goldenrod1","darkorange2","green","skyblue1","red")
labs_hi <- c("Ref","< 4","4","8","> 8",">= 160 (when homolgous titre >= 2560)", ">= 160 (No homologous titre)", "<" )

tree_fig3 <- tree_fig2 + new_scale_fill()
tree_fig4 <- gheatmap(tree_fig3,region_df, offset = 0.055, width = 0.02, font.size = 3, colnames_angle = 270, colnames_offset_y = -5.5) + scale_fill_viridis_d(option = "D", name = "WHO Region")

#alter legend position using legend.position...
tree_fig5 <- tree_fig4 + new_scale_fill()

tree_fig6 <- gheatmap(tree_fig5, fold_titres_df, offset = 0.062, width = 0.14, font.size = 1.3, colnames_angle = 270, colnames_offset_y = -11) + scale_fill_manual(values = cols, name = "VN/HI fold change", labels = labs) + theme(legend.justification = c("centre")) + theme(legend.title = element_text(size = 8)) + theme(legend.text = element_text(size = 6)) + theme(legend.key.size =  unit(0.05, "in")) + theme(legend.position = c(0.2, 0.8)) + theme(plot.margin=margin(0, 50, 15, 0)) + coord_cartesian(clip = 'off')
# tree_fig6
tree_fig7 <- tree_fig6 + new_scale_fill()

tree_fig8 <- gheatmap(tree_fig7, fold_titres_hi_df, offset = 0.1, width = 0.14, font.size = 1.3, colnames_angle = 270, colnames_offset_y = -11) + scale_fill_manual(values = cols_hi, labels = labs_hi, guide=FALSE)
#tree_fig8

ggsave(filename='vcm_heatmap.pdf', # "vcm_tree_heatmap.pdf", 
       plot = tree_fig8, 
       device = cairo_pdf, 
       width = 11.69, 
       height = 8.27, 
       units = "in")
