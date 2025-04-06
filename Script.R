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

# Apply log-transformation on "Average.ping.to.attacking.IP.milliseconds"
df$Average.ping.to.attacking.IP.milliseconds <- log(df$Average.ping.to.attacking.IP.milliseconds)

# Apply log-transformation on "Average.ping.variability"
df$Average.ping.variability <- log(df$Average.ping.variability)

# =====================================================================
# (iv) Remove incomplete cases
df <- na.omit(df)

# ===========================================================
# (v) Perform PCA

# PCA (ignore APT)
pca.df <- prcomp(df[,c(1:8)], scale=TRUE)


# Q: Why data should be standardised when performing PCA?
# A: PCA tends to give greater loadings to variables with large numeric range, leading to bias in results.
#    That's why standardisation of features is recommended.

# Display and describe the individual and cumulative proportions of variance
round(summary(pca.df)$importance, 3)

# So, by looking at the first 3 PCs, we can say they cover at least 50% of the total variance
# Because their cumulative proportion is approximately 60% (0.594)

# Display and interpret the coefficients for PC1, PC2 and PC3
round(pca.df$rotation[,1:3], 3)

# Key drivers:
# PC1: 
#     - Average.Request.Size.Bytes (-0.600) => negatively correlated
#     - Attack.Window.Seconds (-0.567) => negatively correlated
# PC2: 
#     - Average.ping.to.attacking.IP.milliseconds (-0.638) => negatively correlated
#     - Average.ping.variability (-0.642) => negatively correlated
# PC3: 
#     - Hits (-0.558) => negatively correlated
#     - Individual.URLs.requested (-0.583) => negatively correlated

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

# PCA PLOT:
# **********************
# There is significant overlap between APT and non-APT groups.
# However, there are still some visible blue dots toward the left, so we can still see some separation along this direction.

# LOADING PLOTS:
# ***********************
# The loading plot shows that variables such as (ARSB), (AWS), (APV), and (APTAIM) have long vectors, indicating they are more important contributors to PC1 and PC2.
# (IUR), (ASIAC) and (Hits) => short => less contribute to the 2 PCs
# (ARSB), (AWS), (AAPEB), (IUR) and (ASIAC) => less than 90 degree => positively correlated
# Similarly, above 4 features => positively correlated with (APTAIM) and (APV)
# On the other hand, (APTAIM) and (APV) are negatively correlated with (ASIAC)
# Same thing for above 4 features with (Hits)
# (APTAIM) and (APV) form a 90 degree with (Hits) => uncorrelated

# PCA + LOADING PLOTS:
# *************************
# ASIAC = Attack.Source.IP.Address.Count
# IUR = Individual.URLs.requested
# Hits = Hits
# APTAIM = Average.ping.to.attacking.IP.milliseconds
# APV = Average.ping.variability

# => These features don’t show a strong directionality that differentiates the two groups (APT = Yes/No).
# => Both clusters contain similar or mixed values for these features.
# => It's hard to tell any differences from both groups based on these features

# ******************************
# ARSB = Average.Request.Size.Bytes
# AWS = Attack.Window.Seconds
# AAPEB = Average.Attacker.Payload.Entropy.Bits
# APTAIM = Average.ping.to.attacking.IP.milliseconds
# APV = Average.ping.variability

# => Pointing left, toward the region where the APT (blue) points are more concentrated.
# => That means higher values in these features are associated more with APT activities

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

# => PC1 is more helpful in classifying APT vs. non-APT.

# x-axis: Some separation between the red and blue points is visible, especially towards the left side.

# y-axis: Red and blue points are heavily overlapped, showing no clear separation between APT and non-APT cases.

# Key drivers for PC1 (absolute loading > 0.3)
#Average.Request.Size.Bytes	(-0.544)	
#Attack.Window.Seconds	(-0.526)	
#Average.Attacker.Payload.Entropy.Bits	(-0.345)	
#Average.ping.to.attacking.IP.milliseconds	(-0.358)	
#Average.ping.variability (-0.398)











