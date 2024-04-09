suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(MASS))

set.seed(12345L)

# Load dataframe datasetAssignment2
data_raw <- read.csv("data/datasetAssignment2.csv")
data <- data_raw[, -1]
rownames(data) <- data_raw[, 1]
# fill NA values with zero
data[is.na(data)] <- 0

# Mapping column names to politician
column_mapping <- list(
  "pvoixMACRON2017"    = "Emmanuel Macron",
  "pvoixMELENCHON2017" = "Jean-Luc Mélenchon",
  "pvoixMLEPEN2017"    = "Marine Le Pen",
  "pvoixHAMON2017"     = "Benoît Hamon",
  "pvoixPOUTOU2017"    = "Philippe Poutou",
  "pvoixARTHAUD2017"   = "Nathalie Arthaud",
  "pvoixFILLON2017"    = "François Fillon",
  "pvoixMACRON2022"    = "Emmanuel Macron",
  "pvoixMELENCHON2022" = "Jean-Luc Mélenchon",
  "pvoixMLEPEN2022"    = "Marine Le Pen",
  "pvoixZEMMOUR2022"   = "Eric Zemmour",
  "pvoixPOUTOU2022"    = "Philippe Poutou",
  "pvoixARTHAUD2022"   = "Nathalie Arthaud",
  "pvoixPECRESSE2022"  = "Valérie Pécresse",
  "pvoixJADOT2022"     = "Yannick Jadot"
)

# Create socio and vote data for 2017 and 2022
socio_cols <- names(data)[!grepl("^pvoix", names(data))]
socio_data <- data[, socio_cols]

socio_cols2017 <- grep("2017$", names(socio_data), value = TRUE)
socio_cols2022 <- grep("2022$", names(socio_data), value = TRUE)

socio_data2017 <- socio_data[socio_cols2017]
socio_data2022 <- socio_data[socio_cols2022]

vote_cols <- names(data)[grepl("^pvoix", names(data))]
vote_data <- data[, vote_cols]

vote_cols2017 <- grep("2017$", names(vote_data), value = TRUE)
vote_cols2022 <- grep("2022$", names(vote_data), value = TRUE)

vote_data2017 <- vote_data[vote_cols2017]
vote_data2022 <- vote_data[vote_cols2022]

###################
### Dendrograms ###
###################

# Scale features
vote_data2017_scaled <- scale(vote_data2017)
vote_data2022_scaled <- scale(vote_data2022)

# IMPORTANT: only 6000 sampled points to reduce computation

# Plot 2017
d2017 <- dist(t(vote_data2017_scaled[sample(nrow(vote_data2017_scaled), 6000), ]), method = "euclidean")
hc2017 <- hclust(d2017, method = "ward.D")
plot(hc2017, labels = column_mapping[names(vote_data2017)])
rect.hclust(hc2017, k=4, border="red")

# Plot 2022
d2022 <- dist(t(vote_data2022_scaled[sample(nrow(vote_data2022_scaled), 6000), ]), method = "euclidean")
hc2022 <- hclust(d2022, method = "ward.D")
plot(hc2022, labels = column_mapping[names(vote_data2022)])
rect.hclust(hc2022, k=4, border="red")

###########
### PCA ###
###########

# Scale features
process2017 <- preProcess(socio_data2017, method=c("range"))
socio_data2017_scaled <- predict(process2017, socio_data2017)

process2022 <- preProcess(socio_data2022, method=c("range"))
socio_data2022_scaled <- predict(process2022, socio_data2022)

# PCA 2017
pol.pca2017 <- princomp(socio_data2017_scaled, cor=T) # based on eigenvalues
summary(pol.pca2017)

loadings_table2017 <- as.table(pol.pca2017$loadings)
print(xtable(loadings_table2017[, 1:5], type = "latex"), file = "filename2.tex")

# PCA 2022
pol.pca2022 <- princomp(socio_data2022_scaled, cor=T) # based on eigenvalues
summary(pol.pca2022)

loadings_table2022 <- as.table(pol.pca2022$loadings)
print(xtable(loadings_table2022[, 1:5], type = "latex"), file = "filename2.tex")

###########
### LDA ###
###########

# Transform Macron votes into class 0-1 for above average votes or below
mean2017 <- mean(vote_data2017$pvoixMACRON2017)
mean2022 <- mean(vote_data2022$pvoixMACRON2022)
vote_classes2017 <- as.integer(vote_data2017$pvoixMACRON2017 > mean2017)
vote_classes2022 <- as.integer(vote_data2022$pvoixMACRON2022 > mean2022)

# LDA 2017
lda2017 <- lda(vote_classes2017 ~ .,data=cbind(socio_data2017, vote_classes2017))
lda2017_table <- as.table(lda2017$scaling)
print(xtable(lda2017_table, type = "latex"), file = "filename2.tex")

# LDA 2022
lda2022 <- lda(vote_classes2022 ~ .,data=cbind(socio_data2022, vote_classes2022))
lda2022_table <- as.table(lda2022$scaling)
print(xtable(lda2022_table, type = "latex"), file = "filename2.tex")
