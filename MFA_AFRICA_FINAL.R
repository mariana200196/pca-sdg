library("reshape2")
library("dplyr")
library("ggpubr")
library("PerformanceAnalytics")
library("FactoMineR")
library("factoextra")
library("writexl")

#set working directory
setwd("C:/Users/maria/Desktop/Multivariate Statistics")

### GET THE DATA ###
g1 <- read.csv2(file = "Goal1_CSV.csv", dec=".")
g3 <- read.csv2(file = "Goal3_CSV.csv", dec=".")
g6 <- read.csv2(file = "Goal6_CSV.csv", dec=".")
g7 <- read.csv2(file = "Goal7_CSV.csv", dec=".")
aid <- read.csv2(file = "Aid_CSV.csv", dec=".")
regions <- read.csv2(file = "Regions_CSV.csv")
regions <- regions[ -c(1) ] #drop Goal column

### AID ###
aid_2019 <- aid[which(aid$TimePeriod=="2019"), ] #2019 is the most recent complete year
aid_2019 <- aid_2019[c("GeoAreaName", "SeriesDescription", "Value")] #relevant columns only
aid_2019 <- dcast(aid_2019, GeoAreaName ~ SeriesDescription, value.var="Value") #transform table to wide format

### GOAL 1 ###
g1_2018 <- g1[which(g1$Time_Detail=="2018"), ] #2018 is the most recent complete year
g1_2018 <- g1_2018[which(g1_2018$Sex=="BOTHSEX" | g1_2018$Sex==""), ] #BOTHSEX or empty
g1_2018 <- g1_2018[which(g1_2018$Location=="ALLAREA" | g1_2018$Location==""), ] #ALLAREA or empty
g1_2018 <- g1_2018[which(g1_2018$Age=="15+" | g1_2018$Age==""), ] #15+ or empty
g1_2018 <- g1_2018[c("GeoAreaName", "SeriesDescription", "Value")] #relevant columns only
g1_2018 <- dcast(g1_2018, GeoAreaName ~ SeriesDescription, value.var="Value") #transform table to wide format

### GOAL 3 ###
g3_2017 <- g3[which(g3$TimePeriod=="2017"), ] #2017 is the most recent complete year
g3_2017  <- g3_2017 [which(g3_2017 $Sex=="BOTHSEX" | g3_2017$Sex=="" | (g3_2017$Sex=="FEMALE" & g3_2017$SeriesDescription=="Maternal mortality ratio")), ] 
g3_2017 <- g3_2017[which(g3_2017$Age=="ALLAGE" | g3_2017$Age=="<1M" | g3_2017$Age=="<1Y" | g3_2017$Age=="30-70" | g3_2017$Age=="<5Y" | g3_2017$Age=="" ), ] 
g3_2017 <- g3_2017[c("GeoAreaName", "SeriesDescription", "Value")] #relevant columns only
g3_2017 <- dcast(g3_2017, GeoAreaName ~ SeriesDescription, value.var="Value") #transform table to wide format

### GOAL 6 ###
g6_2019 <- g6[which(g6$TimePeriod=="2019"), ] #2019 is the most recent complete year
g6_2019  <- g6_2019 [which(g6_2019 $Location=="ALLAREA" | g6_2019 $Location==""), ] #ALLAREA
g6_2019 <- g6_2019[c("GeoAreaName", "SeriesDescription", "Value")] #relevant columns only
g6_2019 <- dcast(g6_2019, GeoAreaName ~ SeriesDescription, value.var="Value") #transform table to wide format

### GOAL 7 ### 
g7_2019 <- g7[which(g7$TimePeriod=="2019"), ] #2019 is the most recent complete year
g7_2019 <- g7_2019[which(g7_2019$Location=="ALLAREA" | g7_2019$Location==""), ] #ALLAREA
g7_2019 <- g7_2019[c("GeoAreaName", "SeriesDescription", "Value")] #relevant columns only
g7_2019 <- dcast(g7_2019, GeoAreaName ~ SeriesDescription, value.var="Value") #transform table to wide format
#need to remove > or <


### OUTER JOIN ALL DATAFRAMES ###
all <- merge(x = g1_2018, y = g3_2017, by = "GeoAreaName", all = TRUE)
all <- merge(x = all, y = g6_2019, by = "GeoAreaName", all = TRUE)
all <- merge(x = all, y = g7_2019, by = "GeoAreaName", all = TRUE)
all <- merge(x = all, y = aid_2019, by = "GeoAreaName", all = TRUE)
all <- merge(x = all, y = regions, by = "GeoAreaName", all = TRUE)

all$na_count <- apply(is.na(all), 1, sum) #na per country
all <- all[which(all$na_count<5), ] #remove if country has 5 or more NA (equivalent to approx. 25% missing values)
colnames(all)

na_count_per_column <- c('na_count_per_column',
                         sum(is.na(all$`Employed population below international poverty line, by sex and age (%)`)),
                         sum(is.na(all$`Number of directly affected persons attributed to disasters per 100,000 population (number)`)),
                         sum(is.na(all$`Number of people whose damaged dwellings were attributed to disasters (number)`)),
                         sum(is.na(all$`Proportion of population using basic drinking water services, by location (%)`)),
                         sum(is.na(all$`Proportion of population using basic sanitation services, by location (%)`)),
                         sum(is.na(all$`Infant mortality rate (deaths per 1,000 live births)`)),
                         sum(is.na(all$`Malaria incidence per 1,000 population at risk (per 1,000 population)`)),
                         sum(is.na(all$`Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)`)),
                         sum(is.na(all$`Neonatal mortality rate (deaths per 1,000 live births)`)),
                         sum(is.na(all$`Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)`)),
                         sum(is.na(all$`Number of people requiring interventions against neglected tropical diseases (number)`)),
                         sum(is.na(all$`Tuberculosis incidence (per 100,000 population)`)),
                         sum(is.na(all$`Under-five mortality rate, by sex (deaths per 1,000 live births)`)),
                         sum(is.na(all$`Proportion of population practicing open defecation, by urban/rural (%)`)),
                         sum(is.na(all$`Proportion of population with basic handwashing facilities on premises, by urban/rural (%)`)),
                         sum(is.na(all$`Proportion of population with access to electricity, by urban/rural (%)`)),
                         sum(is.na(all$`Proportion of population with primary reliance on clean fuels and technology (%)`)),
                         sum(is.na(all$`Official development assistance grants for poverty reduction, by recipient countries (percentage of GNI)`)),
                         sum(is.na(all$`Total official development assistance (gross disbursement) for water supply and sanitation, by recipient countries (millions of constant 2019 United States dollars)`)),
                         sum(is.na(all$`Total official flows (disbursement) for Aid for Trade, by recipient countries (millions of constant 2019 United States dollars)`)),
                         sum(is.na(all$`Total official flows (disbursements) for agriculture, by recipient countries (millions of constant 2019 United States dollars).y`)),
                         sum(is.na(all$`Total official flows for infrastructure, by recipient countries (millions of constant 2019 United States dollars)`)),
                         sum(is.na(all$Region)),
                         sum(is.na(all$Least.Developed.Countries)),
                         sum(is.na(all$na_count))
)
all <- rbind(all, as.numeric(na_count_per_column)) #na per variable
rownames(all)
all <- all[,!sapply(all, function(x) sum(is.na(x)))>12] #drop column if more than 12 NAs (equivalent to 75% of values being NA)

all <- all[ -c(24) ] #drop na count column
all <- all[ -c(49), ] #drop na count row


### HUGE CORRELATION MATRIX ###
#all_nocat <- all[ -c(1, 22, 23) ] #remove categorical variables
#all_correlation <- cor(all_nocat, use = "complete.obs") #calculate correlation matrix and handle missing values by removing the individuals
#round(all_correlation, 2)

#chart.Correlation(all_correlation, histogram=TRUE, pch=19) #chart

#col<- colorRampPalette(c("blue", "white", "red"))(20)
#heatmap(x = g67_correlation, col = col, symm = TRUE) #heatmap


# replace na with median
all_with_medians <- all                                             
for(i in 2:ncol(all)) {                 
  all_with_medians[ , i][is.na(all_with_medians[ , i])] <- median(all_with_medians[ , i], na.rm = TRUE)
}

#rename column names
colnames(all_with_medians)[2] <- "Employed below poverty line"
colnames(all_with_medians)[3] <- "Using basic drink water services"
colnames(all_with_medians)[4] <- "Using basic sanitation services"
colnames(all_with_medians)[5] <- "Infant mortality rate"
colnames(all_with_medians)[6] <- "Malaria incidence"
colnames(all_with_medians)[7] <- "Maternal mortality rate"
colnames(all_with_medians)[8] <- "Neonatal mortality rate"
colnames(all_with_medians)[9] <- "HIV infections"
colnames(all_with_medians)[10] <- "Interventions tropical diseases"
colnames(all_with_medians)[11] <- "Tuberculosis incidence"
colnames(all_with_medians)[12] <- "Under five mortality rate"
colnames(all_with_medians)[13] <- "Open defecation"
colnames(all_with_medians)[14] <- "Handwashing on premises"
colnames(all_with_medians)[15] <- "Access electricity"
colnames(all_with_medians)[16] <- "Reliance on clean fuels"
colnames(all_with_medians)[17] <- "Grants for poverty reduction"
colnames(all_with_medians)[18] <- "Assistance water supply"
colnames(all_with_medians)[19] <- "Aid for trade"
colnames(all_with_medians)[20] <- "Flows for agriculture"
colnames(all_with_medians)[21] <- "Flows for infrastructure"
colnames(all_with_medians)[22] <- "Least Developed Countries"
colnames(all_with_medians)[23] <- "Region"

df <- data.frame(all_with_medians, row.names = 1) #make countries the index
df_ordered <- df[, c(22, 21, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)]
colnames(df_ordered)

#export to excel
#write_xlsx(df_ordered,"C:\\Users\\maria\\Desktop\\Multivariate Statistics\\MFA_df.xlsx")

### PERFORM MFA ####
mfa <- MFA(df_ordered, group = c(2, 3, 8, 4, 5), type = c("n", "s", "s", "s", "s"), name.group = c("Region", "Poverty", "Health", "Infrastructure", "Assistance"), num.group.sup = c(1), graph = FALSE)
print(mfa)

eig.val <- get_eigenvalue(mfa)
eig.val
mean(eig.val [,1])

#get eigenvalues of groups
mfa$separate.analyses$infrastructure$eig
mfa$separate.analyses$poverty$eig
mfa$separate.analyses$health$eig
mfa$separate.analyses$aid$eig
mfa$separate.analyses$region$eig

#get cor, contrib, cos2, coord of variables (PCA style?)
mfa$separate.analyses$infrastructure$var
mfa$separate.analyses$poverty$var
mfa$separate.analyses$health$var
mfa$separate.analyses$aid$var
mfa$separate.analyses$region$var

mfa$separate.analyses$infrastructure$call
mfa$separate.analyses$poverty$call
mfa$separate.analyses$health$call
mfa$separate.analyses$aid$call
mfa$separate.analyses$region$call

#get group coord, cor, contrib, cos2
mfa$group

#get individual coord, contrib, cos2
mfa$ind

mfa$inertia.ratio

mfa$summary.quanti
mfa$summary.quali

#get quantitative vars cor, coord, contrib, cos2
mfa$quanti.var
mfa$quali.var.sup

#global pca (same as quanti.var?)
mfa$global.pca$eig
mfa$global.pca$var

mfa$call$X

summary(mfa)
summary(mfa, nb.dec = 2, nbelements = Inf, ncp = 2, align.names=TRUE)
dimdesc(mfa, proba=1)

#screeplot
fviz_screeplot(mfa)
#contribution to the first dimension by group
fviz_contrib(mfa, "group", axes = 1)
#contribution to the second dimension by group
fviz_contrib(mfa, "group", axes = 2)
#contribution to Dim 1
fviz_contrib(mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")
#contribution to Dim 2
fviz_contrib(mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")

#var groups
fviz_mfa_var(mfa, "group", col.var.sup = "transparent")

#correlation between variables and dimensions
fviz_mfa_var(mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

#variable cos2
fviz_mfa_var(mfa, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
#variable contrib
fviz_mfa_var(mfa, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


#graph of individuals
ind <- get_mfa_ind(mfa)
fviz_mfa_ind(mfa, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.quali.var.sup = "transparent",
             repel = TRUE)

#categorical elipse
#fviz_ellipses(mfa, c("Least.Developed.Countries"), repel = TRUE)
#fviz_ellipses(mfa, c("Region"), repel = TRUE)
fviz_ellipses(mfa, c("Region", "Least.Developed.Countries"), repel = TRUE)

#partial individuals
#fviz_mfa_ind(mfa, partial = "all")
fviz_mfa_ind(mfa, partial = c("Egypt", "Central African Republic", "Chad"), repel = TRUE)

#partial axes
fviz_mfa_axes(mfa)


