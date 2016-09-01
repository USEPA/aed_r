#A web version of this (slightly different) is at:
#https://jwhollister.com/aed_r/stats.html

# URL for 2007 NLA water quality data
nla_wq_url <- "https://www.epa.gov/sites/production/files/2014-10/nla2007_chemical_conditionestimates_20091123.csv"
nla_secchi_url <- "https://www.epa.gov/sites/production/files/2014-10/nla2007_secchi_20091008.csv"
# Read into an R data.frame with read.csv
nla_wq <- read.csv(nla_wq_url, stringsAsFactors = FALSE)
nla_secchi <- read.csv(nla_secchi_url, stringsAsFactor = FALSE)

#Load dplyr into current session
library(dplyr)

#Clean up NLA Water quality
nla_wq_cln <- nla_wq %>%
  filter(VISIT_NO == 1,
         SITE_TYPE == "PROB_Lake") %>%
  select(SITE_ID,ST,EPA_REG,RT_NLA,LAKE_ORIGIN,PTL,NTL,TURB,CHLA)
#Clean up NLA Secchi
nla_secchi_cln <- nla_secchi %>%
  filter(VISIT_NO == 1) %>%
  select(SITE_ID, SECMEAN)
#Join the two together based on SITE_ID and the finally filter out NA's
nla <- left_join(x = nla_wq_cln, y = nla_secchi_cln, by = "SITE_ID") %>%
  filter(complete.cases(NTL,PTL,TURB,CHLA,SECMEAN))
tbl_df(nla)

summary(nla)

#Let's get different percentiles
quantile(nla$PTL,probs = c(0.33,0.66))

#Basic Stats
mean(nla$NTL)
median(nla$NTL)
min(nla$NTL)
max(nla$NTL)
sd(nla$NTL)
IQR(nla$NTL)
range(nla$NTL)


#Frequency Stuff 
table(nla$LAKE_ORIGIN)
x<-rbinom(10,1,0.5)
y<-rbinom(10,1,0.5)
table(x,y)

#An example with NA's
x <- c(37,22,NA,41,19)
mean(x) #Returns NA
mean(x, na.rm = TRUE)

#Let's do mean per a group - LAKE_ORIGIN
origin_means <- nla %>%
  group_by(LAKE_ORIGIN) %>%
  summarize(mean_ntl = mean(NTL),
            median_ntl = median(NTL),
            sd_ntl = sd(NTL))
origin_means

mean(nla$NTL[nla$LAKE_ORIGIN == "NATURAL"])
mean(nla$NTL[nla$LAKE_ORIGIN == "MAN-MADE"])

#A single histogram using base
hist(nla$NTL)
#Log transform it
hist(log1p(nla$NTL)) #log1p adds one to deal with zeros
#Density plot
plot(density(log1p(nla$NTL)))

#Simple boxplots
boxplot(nla$CHLA)
boxplot(log1p(nla$CHLA))
#Boxplots per group
boxplot(log1p(nla$CHLA)~nla$EPA_REG)
#Fix ordering (fix this later, jeff)
epa_reg_factor <- factor(nla$EPA_REG,
                         levels= c("Region_1",...,"Region_10"))

#A single scatterplot
plot(log1p(nla$PTL),log1p(nla$CHLA))
#A matrix of scatterplot
plot(log1p(nla[,6:10]))


#Getting super fancy with tidyr, plotly, and ggplot2 to visualize all variables
library(tidyr)
library(ggplot2)
library(plotly)

nla_gather <- gather(nla,parameter,value,6:10)
dens_gg <-ggplot(nla_gather,aes(x=log1p(value))) +
  geom_density() +
  facet_wrap("parameter") +
  labs(x="log1p of measured value")
ggplotly(dens_gg)

x<-ggplot(nla, aes(x=log1p(PTL),y=log1p(NTL))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("EPA_REG")
ggplotly(x)


#Long Format - original format for LAKE_ORIGIN and SECMEAN
t.test(nla$SECMEAN ~ nla$LAKE_ORIGIN,conf.level = 0.99)

wide_nla <- spread(nla,LAKE_ORIGIN,SECMEAN)
names(wide_nla)[9:10]<-c("man_made", "natural")
t.test(wide_nla$man_made, wide_nla$natural)

# A quick visual of this:
boxplot(log1p(nla$CHLA)~nla$RT_NLA)
# One way analysis of variance
nla_anova <- aov(log1p(CHLA)~RT_NLA, data=nla)
nla_anova #Terms
summary(nla_anova) #The table
anova(nla_anova)
anova(lm(log1p(CHLA)~RT_NLA, data=nla))

#For a pair
cor(log1p(nla$PTL),log1p(nla$NTL))
#For a correlation matrix
cor(log1p(nla[,6:10]))

# The simplest case
chla_tp <- lm(log1p(CHLA) ~ log1p(PTL), data=nla) #Creates the model
summary(chla_tp) #Basic Summary
names(chla_tp) #The bits
chla_tp$residuals
resid(chla_tp)
coef(chla_tp)

chla_tp_tn_turb <- lm(log1p(CHLA) ~ log1p(PTL) + log1p(NTL) + log1p(TURB), data = nla)
summary(chla_tp_tn_turb)

#Tidying output from models
install.packages("broom")
library(broom)
help(package="broom")
tidy(nla_anova)


