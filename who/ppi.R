ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE, repos='https://cran.ma.imperial.ac.uk/')
    sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('ggplot2', 'ape', 'tidytree', 'tibble', 'dplyr', 'ggnewscale', 'viridis', 'gridExtra', 'devtools', 'BiocManager')

ipak(packages)

BiocManager::install("ggtree")
BiocManager::install("treeio")

Sys.sleep(15)
