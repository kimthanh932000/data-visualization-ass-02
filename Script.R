# (i) Create a sub-sample of 400 observations
#install.packages(c("tidyverse","moments","ggpubr"))  #De-comment the code to install
library("tidyverse"); library(moments); library(ggpubr)

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


