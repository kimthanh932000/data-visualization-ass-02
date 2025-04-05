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

# Filter out rows that contain negative value for "Attack.Source.IP.Address"
df <- df[df$Attack.Source.IP.Address.Count >= 0, ]

# Remove the max value of 99999 from "Average.ping.to.attacking.IP.milliseconds"
df <- df[df$Average.ping.to.attacking.IP.milliseconds != 99999, ]

# Apply log-transformation on "Average.ping.to.attacking.IP.milliseconds"
# and "Average.ping.variability"
df$Average.ping.to.attacking.IP.milliseconds <- log(df$Average.ping.to.attacking.IP.milliseconds)
df$Average.ping.variability <- log(df$Average.ping.variability)

# =====================================================================
# (iv) Remove incomplete cases
df <- na.omit(df)

# ===========================================================
# (v) Perform PCA

# PCA (ignore APT)
pca.df <- prcomp(df[,c(1:8)], scale=TRUE)


# Q: Why data should be standardised when performing PCA?
# A: PCA tends to give greater loadings to variables with large numeric range, leading to bias in PCA.
#    That's why standardisation of variables is recommended.

# Display and describe the individual and cumulative proportions of variance
round(summary(pca.df)$importance, 3)

# To cover at least 50% of the total variance, PC1-PC3 are needed
# Because their cumulative proportion is 60% (0.606)

# Display and interpret the coefficients for PC1, PC2 and PC3
round(pca.df$rotation[,1:3], 3)

# Key drivers:
# PC1: 
#     - Average.Request.Size.Bytes (0.544)
#     - Attack.Window.Seconds (0.526)
# PC2: 
#     - Average.ping.to.attacking.IP.milliseconds (0.590)
#     - Average.ping.variability (0.577)
# PC3: 
#     - Hits (0.542)
#     - Average.Attacker.Payload.Entropy.Bits (0.483)
#     - Individual.URLs.requested (0.581)

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
# However, there are still some visible clusters of APT activity toward the left and upper-left quadrants, suggesting some separation may exist along certain directions.
# Most red dots are grouped between -2.5 and 2.5 on the x-axis. 
# In contrast, blue dots are more spread out, ranging from around -5.0 to 3.75.

# LOADING PLOTS:
# ***********************
# The loading plot shows that variables such as (ARSB), (AWS), (APV), and (APTAIM) have long vectors, indicating they are more important contributors to PC1 and PC2. 
# ARSB, AWS, and (AAPEB) are close together and form a tight angle, suggesting they are positively correlated. 
# Similarly, APV and APTAIM also form a small angle, meaning they are highly and positively correlated. 
# However, both APV and APTAIM meet Hits at approximately 90 degrees, indicating no correlation between them. 
# Likewise, ARSB, AWS, and AAPEB form a 90 degree angle with (ASIAC), showing they are uncorrelated. 
# APV and APTAIM form an angle greater than 90 degrees with Hits, suggesting a negative correlation. 
# They are also slightly and negatively correlated with ASIAC, as the angle between them is slightly more than 90 degrees.

# PCA + LOADING PLOTS:
# *************************
# Samples in the same direction as (ARSB), (AWS), and (APV) tend to have higher values in these features compared to those on the opposite side. 
# Likewise, individuals aligned with the direction of (Hits) generally have higher values for that variable. 
# Similarly, samples positioned in the same direction as (APV) and (APTAIM) show higher values for these features than those located in the opposite direction.

# Features that help distinguish APT activity
# *********************************
# Average.Request.Size.Bytes: Long arrow pointing toward APT cluster. Suggests APTs involve larger requests.
# Attack.Window.Seconds     : Indicates more prolonged or persistent attacks—consistent with APT behavior.
# Average.Attacker.Payload.Entropy.Bits: High entropy suggests obfuscation/encryption, typical of APTs.

# ===================================================================

# ggplot(df,aes(x=Attack.Source.IP.Address.Count)) +
#   geom_histogram() + #Histogram with default settings
#   ylab("Frequency")
# 
# ggplot(df,aes(x=Average.ping.to.attacking.IP.milliseconds)) +
#   geom_histogram() + #Histogram with default settings
#   ylab("Frequency")
# 
# # boxplot(df$Average.ping.to.attacking.IP.milliseconds, main = "Boxplot of Log-Transformed Ping")
# 
# ggplot(df,aes(x=Average.ping.variability)) +
#   geom_histogram() + #Histogram with default settings
#   ylab("Frequency")


