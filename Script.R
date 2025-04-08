# (i) Create a sub-sample of 400 observations
#install.packages(c("tidyverse","moments","ggpubr", "factoextra"))  #De-comment the code to install
library("tidyverse"); library(moments); library(ggpubr); library(factoextra)

#You may need to change/include the path of your working directory

#Import the dataset into R Studio.
dat <- read.csv("WACY-COM.csv", na.strings=NA, stringsAsFactors=TRUE)

set.seed(10657323)

#Randomly select 400 rows
selected.rows <- sample(1:nrow(dat),size=400,replace=FALSE)

#Your sub-sample of 400 observations
mydata <- dat[selected.rows,]

dim(mydata) #check the dimension of your sub-sample

# ==================================================================
# (ii) Extract only the continuous features and the APT feature from the sub-sample
df <- mydata[,c("Hits",
                "Average.Request.Size.Bytes",
                "Attack.Window.Seconds",
                "Average.Attacker.Payload.Entropy.Bits",
                "Attack.Source.IP.Address.Count",
                "Average.ping.to.attacking.IP.milliseconds",
                "Average.ping.variability",
                "Individual.URLs.requested",
                "IP.Range.Trust.Score",
                "APT")]

# ====================================================================
# (iii) Clean the extracted data

# Remove "IP.Range.Trust.Score" from further analysis
df$IP.Range.Trust.Score <- NULL

# Mask negative value (-1) in "Attack.Source.IP.Address"
df$Attack.Source.IP.Address.Count[df$Attack.Source.IP.Address.Count < 0] <- NA

# Mask extreme value (99999) in "Average.ping.to.attacking.IP.milliseconds"
df$Average.ping.to.attacking.IP.milliseconds[df$Average.ping.to.attacking.IP.milliseconds == 99999] <- NA

# Apply log-transformation
for (col in c("Average.ping.to.attacking.IP.milliseconds", "Average.ping.variability")) {
  df[[col]] <- log(df[[col]])
}

# =====================================================================
# (iv) Remove incomplete cases
df <- na.omit(df)

# ===========================================================
# (v)

# PCA (ignore APT)
pca.df <- prcomp(df[,c(1:8)], scale=TRUE)

# Display and describe the individual and cumulative proportions of variance
round(summary(pca.df)$importance, 3)

# Display and interpret the coefficients for PC1, PC2 and PC3
round(pca.df$rotation[,1:3], 3)

# ===================================================================
# (vi) PCA - Biplot
fviz_pca_biplot(pca.df,
                axes = c(1,2), #Specifying the PCs to be plotted.
                #Parameters for samples
                col.ind=df$APT, #Outline colour of the shape
                fill.ind=df$APT, #fill colour of the shape
                alpha=0.5, #transparency of the fill colour
                pointsize=4, #Size of the shape
                pointshape=21, #Type of Shape
                #Parameter for variables
                col.var="red", #Colour of the variable labels
                label="var", #Show the labels for the variables only
                repel=TRUE, #Avoid label overplotting
                addEllipses=TRUE, #Add ellipses to the plot
                legend.title=list(colour="APT",fill="APT",alpha="APT"))
# ===================================================================
# (vii) 

# Project PC1 scores onto x-axis
pc1.score.df <- data.frame(PC1=pca.df$x[, 1], APT=df$APT)

ggplot(pc1.score.df,aes(x=PC1,y=0))+
  geom_point(aes(colour=APT),alpha=0.8,size=4)+
  theme_minimal(base_size=14)+
  theme(legend.position = "top")+
  xlab("PC1");

# Project PC2 scores onto y-axis
pc2.score.df <- data.frame(PC2=pca.df$x[, 2], APT=df$APT)

ggplot(pc2.score.df,aes(x=0,y=PC2))+
  geom_point(aes(colour=APT),alpha=0.8,size=4)+
  theme_minimal(base_size=14)+
  theme(legend.position = "top")+
  ylab("PC2");








