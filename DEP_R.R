#Installation of DEP (Differential Enrichment analysis of Proteomics data )
#Following the tutorial here https://bioconductor.org/packages/release/bioc/vignettes/DEP/inst/doc/DEP.html#example-dataset-ubiquitin-interactors
if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("DEP")

library("DEP")
library("dplyr")

#Loading of the data 
data <- read.delim("proteinGroupsMaxQuantLilge.txt", header=TRUE, sep="\t" )


#filter for contaminant proteins and decoy hits (already done in this dataset and doesnt work bc there are NA in it instead of ""
#data <- filter(data, Reverse != "+", Potential.contaminant != "+")

#check dimensions of data 
dim(data)
##[1] 2293  130
colnames(data)

#Interesting for our analysis are LFQ intensity
## LFQ.intensity.minus_10_1"            
##[95] "LFQ.intensity.minus_10_2"             "LFQ.intensity.minus_10_3"            
##[97] "LFQ.intensity.minus_20_1"             "LFQ.intensity.minus_20_2"            
##[99] "LFQ.intensity.minus_20_3"             "LFQ.intensity.plus_10_1"             
##[101] "LFQ.intensity.plus_10_2"              "LFQ.intensity.plus_10_3"             
##[103] "LFQ.intensity.plus_20_1"              "LFQ.intensity.plus_20_2"             
##[105] "LFQ.intensity.plus_20_3"              

#Retrieve Gene Information 
#unload dplyr/DEP because overwrite method in UniProt.ws
BiocManager::install("UniProt.ws")
suppressPackageStartupMessages({
  library(UniProt.ws)
})
up <- UniProt.ws(taxId=224308) #taxonomy of Bacillus subtilis is 224308
up #gives info about the species, just checking if it is the right one 


#Data preparation
# Are there any duplicated gene names? 
data$Gene.names %>% duplicated() %>% any()
# Make a table of duplicated gene names
data %>% group_by(Gene.names) %>% summarize(frequency = n()) %>% 
  arrange(desc(frequency)) %>% filter(frequency > 1)

## # A tibble: 5 x 2
## Gene.names frequency
## <fct>          <int>
##   asd            2
##   csrA           2
##   dapA           2
##   dapF           2
##   fur            2

#For further analysis these proteins must get unique names. Additionally, some proteins do not have an annotated gene name and for those we will use the Uniprot ID.

# Make unique names using the annotation in the "Gene.names" column as primary names and the annotation in "Protein.IDs" as name for those that do not have an gene name.
data_unique <- make_unique(data, "Gene.names", "Protein.IDs", delim = ";")

# Are there any duplicated names?
data_unique$name %>% duplicated() %>% any()

# Generate a SummarizedExperiment object by parsing condition information from the column names
LFQ_columns <- grep("LFQ.", colnames(data_unique)) # get LFQ column numbers
data_se_parsed <- make_se_parse(data_unique, LFQ_columns)
data_se_parsed


# Filter on missing values
# Plot a barplot of the protein identification overlap between samples
plot_frequency(data_se_parsed)

# Filter for proteins that are identified in all replicates of at least one condition
data_filt <- filter_missval(data_se_parsed, thr = 0)

# Less stringent filtering:
# Filter for proteins that are identified in 2 out of 3 replicates of at least one condition
#data_filt2 <- filter_missval(data_se_parsed, thr = 1)

# Plot a barplot of the number of identified proteins per samples
plot_numbers(data_filt)

# Plot a barplot of the protein identification overlap between samples
plot_coverage(data_filt)


# Normalize the data
data_norm <- normalize_vsn(data_filt)

## vsn2: 2185 x 6 matrix (1 stratum). 
## Please use 'meanSdPlot' to verify the fit.

# Plot meanSdPlot
meanSdPlot(data_norm)

# Visualize normalization by boxplots for all samples before and after normalization
plot_normalization(data_filt, data_norm)


#Impute data for missing values
# Plot a heatmap of proteins with missing values
plot_missval(data_filt)

#To check whether missing values are biased to lower intense proteins, the densities and cumulative fractions are plotted for proteins with and without missing values.
# Plot intensity distributions and cumulative fraction of proteins with and without missing values
plot_detect(data_filt)

#this shows that the missing values come from low intensities so it's leftshifted 
#left-censored imputation method, such as the quantile regression-based 
#left-censored function (“QRILC”) or random draws from a left-shifted distribution (“MinProb” and “man”)
#We try differenet imputations
# Impute missing data using random draws from a Gaussian distribution centered around a minimal value (for MNAR)
data_imp <- impute(data_norm, fun = "MinProb", q = 0.01)
##[1] 0.4277608

#other optional impute methods
# Impute missing data using random draws from a manually defined left-shifted Gaussian distribution (for MNAR)
#data_imp_man <- impute(data_norm, fun = "man", shift = 1.8, scale = 0.3)
# Impute missing data using the k-nearest neighbour approach (for MAR)
#data_imp_knn <- impute(data_norm, fun = "knn", rowmax = 0.9)
#data_imp_QRILC <- impute(data_norm, fun = "QRILC")


# Plot intensity distributions before and after imputation
plot_imputation(data_norm, data_imp)
#plot_imputation(data_norm, data_imp_man)
#plot_imputation(data_norm, data_imp_knn)
#plot_imputation(data_norm, data_imp_QRILC)



# Differential enrichment analysis  based on linear models and empherical Bayes statistics

# Test every sample versus control
#data_diff <- test_diff(data_imp, type = "control", control = "Glu")

# Test all possible comparisons of samples
data_diff_all_contrasts <- test_diff(data_imp, type = "all")

# Denote significant proteins based on user defined cutoffs
#dep <- add_rejections(data_diff , alpha = 0.05, lfc = log2(1.5))
dep_GluvsMan <- add_rejections(data_diff_all_contrasts , alpha = 0.05, lfc = log2(1.5))


# Plot a heatmap of all significant proteins with the data centered per protein
plot_heatmap(dep_GluvsMan, type = "centered", kmeans = TRUE, 
             k = 6, col_limit = 4, show_row_names = FALSE,
             indicate = c("condition", "replicate"))

# Plot a heatmap of all significant proteins (rows) and the tested contrasts (columns)
plot_heatmap(dep_GluvsMan, type = "contrast", kmeans = TRUE, 
             k = 6, col_limit = 10, show_row_names = FALSE)


# Plot the first and second principal components
plot_pca(dep_GluvsMan, x = 1, y = 2, n = 500, point_size = 4)


# Plot the Pearson correlation matrix
plot_cor(dep_GluvsMan, significant = TRUE, lower = 0, upper = 1, pal = "Reds")

library(ggplot2)
# Plot a volcano plot for the contrast 
plot_volcano(dep_GluvsMan, contrast = "minus_10__vs_minus_20_", label_size = 2, add_names = TRUE)
plot_volcano(dep_GluvsMan, contrast = "minus_10__vs_minus_20_", label_size = 2, add_names = TRUE) +coord_cartesian( xlim = c(-12, 12))


plot_volcano(dep_GluvsMan, contrast = "plus_10__vs_plus_20_", label_size = 2, add_names = TRUE)
plot_volcano(dep_GluvsMan, contrast = "plus_10__vs_plus_20_", label_size = 2, add_names = TRUE) +coord_cartesian( xlim = c(-12, 12))


VolcanoPlotMinus10vs20FixXAxis

plot_volcano(dep_GluvsMan, contrast = "minus_10__vs_plus_10_", label_size = 2, add_names = TRUE)
plot_volcano(dep_GluvsMan, contrast = "minus_10__vs_plus_10_", label_size = 2, add_names = TRUE) +coord_cartesian( xlim = c(-12, 12))

plot_volcano(dep_GluvsMan, contrast = "minus_20__vs_plus_20_", label_size = 2, add_names = TRUE)
plot_volcano(dep_GluvsMan, contrast = "minus_20__vs_plus_20_", label_size = 2, add_names = TRUE) +coord_cartesian( xlim = c(-12, 12))


# Plot a barplot for single Proteins
plot_single(dep_GluvsMan, proteins = c("algD", "algA", "algK","algE", "algF", "algL","algX", "algG"))


# Plot a barplot for the protein USP15 with the data centered
plot_single(dep_GluvsMan, proteins = "algD", type = "centered")

# Plot a frequency plot of significant proteins for the different conditions
plot_cond(dep_GluvsMan) 

#Result table
# Generate a results table
data_results <- get_results(dep_GluvsMan)

# Number of significant proteins
data_results %>% filter(significant) %>% nrow()

# Column names of the results table
colnames(data_results)

# Generate a wide data.frame
df_wide <- get_df_wide(dep_GluvsMan)
# Generate a long data.frame
df_long <- get_df_long(dep_GluvsMan)




#Get GO and Protein-Names 
keys <- df_wide$Protein.IDs
columns <- c("GENES", "PROTEIN-NAMES", "GO")
kt <- "UNIPROTKB"

#unload packages as they overwrite the "select"-method
detach(package:DEP,unload=TRUE)

#load UniProt-Webservice
library(UniProt.ws)

#some Proteins have More than one Protein Name, so we should separate and
#merge again otherwise the search has no result
library(tidyverse)

res <- NULL
for(i in keys) {
  key <- strsplit(i, ";")
  for(string in key){
    res1 <- select(up, string, columns, kt)
  }
  res1$UNIPROTKB  = paste(res1$UNIPROTKB, collapse =";")
  res1$GENES  = paste(res1$GENES, collapse =";") 
  res1$'PROTEIN-NAMES'  = paste(res1$'PROTEIN-NAMES', collapse =";")
  res1$GO  = paste(res1$GO, collapse =";")
  res1 <- unique(res1)
  res <- rbind(res, res1)
}

head(res)

#final result table merge with annotations
df_wide$Protein.names <- res$`PROTEIN-NAMES`[match(df_wide$Protein.IDs, res$UNIPROTKB)]
df_wide$GO <- res$GO[match(df_wide$Protein.IDs, res$UNIPROTKB)]
df_wide$Genes <- res$GENES[match(df_wide$Protein.IDs, res$UNIPROTKB)]


write.table(df_wide, file = "DEP_Result.txt" , sep = "\t", row.names=FALSE)
