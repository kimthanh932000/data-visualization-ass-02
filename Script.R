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

#Show the individual and cumulative proportion of variance
summary(pca.df)


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


