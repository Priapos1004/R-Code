library(readr)
library(FactoClass)
library(RColorBrewer)
library(ca)

funding=read.csv("data/Funding.csv")
as.table(as.matrix(funding))

funding_matrix <- as.matrix(data.frame(funding[,-1], row.names = funding[,1]))
correspondence_table <- addmargins(funding_matrix)

# Prop table
prob_table <- round(correspondence_table/796, 3)

# row profile
r_profile <- round(prop.table(funding_matrix, margin=1), 3)

# column profile
c_profile <- round(prop.table(funding_matrix, margin=2), 3)

# find colors
display.brewer.all()

plotct(r_profile, profiles = "row", legend.text = TRUE, col=brewer.pal(n = ncol(r_profile),name = "YlOrRd"))
plotct(c_profile, profiles = "col", legend.text = TRUE, col=brewer.pal(n = ncol(c_profile),name = "YlOrRd"))

chisq.test(prob_table)

ct.ca <- ca(r_profile)
plot.ca(ct.ca, arrows=c(TRUE,TRUE))

summary(ct.ca) # important!!!

plot.ca(ct.ca, what = c("all","all"), contrib = "relative", main="Symmetric map")

plot.ca(ct.ca, what = c("all","all"), mass=TRUE, contrib = "relative", main="Symmetric map")

principal_inertias <- ct.ca$sv # or sqrt(eigenvalues)
principal_inertias
