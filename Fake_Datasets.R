library(readxl)
library(tidyverse)
library(broom)
library(stringr)
library(forcats)






# Note: Data is based off of real data, but has been fudged and variable names have changed.
Fake_Data <- read_excel("Fake_Data.xlsx")


# Creating a variable for each generation. 
Fake_Data <- Fake_Data %>%  
  dplyr::mutate("Generation" = case_when(
    Fake_Data$Year_Born <= 1927 ~ "Greatest Generation",
    Fake_Data$Year_Born <= 1945 & Fake_Data$Year_Born >= 1928 ~ "Silent Generation",
    Fake_Data$Year_Born <= 1964 & Fake_Data$Year_Born >= 1946 ~ "Baby Boomer",
    Fake_Data$Year_Born <= 1980 & Fake_Data$Year_Born >= 1965 ~ "Generation X",
    Fake_Data$Year_Born <= 1996 & Fake_Data$Year_Born >= 1981 ~ "Millenial",
    Fake_Data$Year_Born <= 2012 & Fake_Data$Year_Born >= 1997 ~ "Generation Z",
  ))

# Create "Age Range" variable.
ranges <- c("0-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")
Fake_Data <- Fake_Data %>% 
  dplyr::mutate(Age_Range = cut(Age, 
                         breaks=c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         labels=c("0-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")))

# Manually set levels for factor variables. Somewhat unnecessary as some of the data has been changed, but felt I Would keep it in.
Fake_Data$Years_Employed_Total <- ordered(Fake_Data$Years_Employed_Total, levels = c("0-3", "4-7", "8-11", "12-15", "16-19", "20-23", "24-27", "28-31", "32-35", "36-39", "40+"))
Fake_Data$Years_Employed_At_Company <- ordered(Fake_Data$Years_Employed_At_Company, levels = c("0-3", "4-7", "8-11", "12-15", "16-19", "20-23", "24-27", "28-31", "32-35", "36-39", "40+"))
Fake_Data$Married <- ordered(Fake_Data$Married, levels = c("Yes", "No"))
Fake_Data$Education <- ordered(Fake_Data$Education, levels = c("Bachelor's Degree", "Master's Degree", "Specialist Degree", "Doctoral Degree"))
Fake_Data$Second_Job <- ordered(Fake_Data$Second_Job, levels = c("Yes", "No"))
Fake_Data$Children <- ordered(Fake_Data$Children, levels = c("Yes", "No"))
Fake_Data$Live_Close_By <- ordered(Fake_Data$Live_Close_By, levels = c("Yes", "No"))
Fake_Data$Union <- ordered(Fake_Data$Union, levels = c("Yes", "No", "Former"))
Fake_Data$Generation <- ordered(Fake_Data$Generation, levels = c("Generation Z", "Millenial", "Generation X", "Baby Boomer", "Silent Generation"))
Fake_Data$Age_Range <- ordered(Fake_Data$Age_Range, levels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"))

Fake_Data <- na.omit(Fake_Data)

# Subsetting Dataset for Regression variabls. Combining individual items into factors.
Fake_Reg_Data <- data.frame ("Engagement" = c(rowMeans(Fake_Data[,1:11], na.rm = FALSE)), 
                   "Environment_Factors" = c(rowMeans(Fake_Data[,12:16], na.rm = FALSE)), 
                   "Resource_Factors" = c(rowMeans(Fake_Data[,17:19], na.rm = FALSE)), 
                   "Interpersonal_Factors" = c(rowMeans(Fake_Data[,20:25], na.rm = FALSE)), 
                   "Percieved_Organizational_Support" = c(rowMeans(Fake_Data[,26:29], na.rm = FALSE)), 
                   "Leadership" = c(rowMeans(Fake_Data[,30:34], na.rm = FALSE)),
                   "Justice" = c(rowMeans(Fake_Data[,35:39], na.rm = FALSE)),
                   "Voice" = c(rowMeans(Fake_Data[,40:45], na.rm = FALSE)),
                   "Intentions_to_Leave" = c(rowMeans(Fake_Data[,46:49], na.rm = FALSE)),
                   "Age" = Fake_Data$Age,
                   "Years_Employed_Total" = ordered(Fake_Data$Years_Employed_Total),
                   "Years_Employed_At_Company" = ordered(Fake_Data$Years_Employed_At_Company),
                   "Married" = ordered(Fake_Data$Married),
                   "Education" = ordered(Fake_Data$Education),
                   "Second_Job" = ordered(Fake_Data$Second_Job),
                   "Children" = ordered(Fake_Data$Children),
                   "Live_Close_By" = ordered(Fake_Data$Live_Close_By),
                   "Union" = ordered(Fake_Data$Union),
                   "Generation" = ordered(Fake_Data$Generation)
)

# Re-order and delete NAs. 
# Note: The method in which data was altered results in a LOT of deletions. If this occurred naturally, one should impute rather than remove.
Fake_Reg_Data <- Fake_Reg_Data[,c(1:10, 19, 11:18)] %>%
  na.omit(Fake_Reg_Data)

E_Predictors <- colnames(Fake_Reg_Data[2:19])
E_Outcome <- colnames(Fake_Reg_Data[1])

E_Models = list()
for (x in E_Outcome)
  for (y in E_Predictors)
    E_Models[[paste(x, "vs", y)]] <- lm(as.formula(paste(x, "~", y)), data = Fake_Reg_Data)


coefs = lapply(E_Models, tidy, simplify = F)
E_Coefs <- dplyr::bind_rows(coefs, .id = "model")

E_Coefs <- as.data.frame(E_Coefs)

E_Coefs$Outcome <- word(E_Coefs$model, 1)
E_Coefs$Predictor <- word(E_Coefs$model, 3)

E_Coefs[,3:6] <- E_Coefs[,3:6] %>%
  mutate(across(c(estimate, std.error, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic), ~ usefun::specify_decimal(., 2)))
E_Coefs[,3:6] <- sapply(E_Coefs[,3:6], as.numeric)



summ = lapply(E_Models, glance, simplify = F)
E_Summary <- dplyr::bind_rows(summ, .id = "model")

E_Summary <- as.data.frame(E_Summary)

E_Summary$Outcome <- word(E_Summary$model, 1)
E_Summary$Predictor <- word(E_Summary$model, 3)

E_Summary[,2:11]<- E_Summary[,2:11] %>%
  mutate(across(c(r.squared, adj.r.squared, sigma, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic, logLik, AIC, BIC, deviance), ~ usefun::specify_decimal(., 2)))
E_Summary[,2:11] <- sapply(E_Summary[,2:11], as.numeric)


########## Leave Outcome ##########
L_Predictors <- colnames(Fake_Reg_Data[, c(1:8,10:19)])
L_Outcome <- colnames(Fake_Reg_Data[9])

L_Models = list()
for (x in L_Outcome)
  for (y in L_Predictors)
    L_Models[[paste(x, "vs", y)]] <- lm(as.formula(paste(x, "~", y)), data = Fake_Reg_Data)


coefs = lapply(L_Models, tidy, simplify = F)
L_Coefs <- dplyr::bind_rows(coefs, .id = "model")

L_Coefs <- as.data.frame(L_Coefs)

L_Coefs$Outcome <- word(L_Coefs$model, 1)
L_Coefs$Predictor <- word(L_Coefs$model, 3)

L_Coefs[,3:6] <- L_Coefs[,3:6] %>%
  mutate(across(c(estimate, std.error, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic), ~ usefun::specify_decimal(., 2)))
L_Coefs[,3:6] <- sapply(L_Coefs[,3:6], as.numeric)



summ = lapply(L_Models, glance, simplify = F)
L_Summary <- dplyr::bind_rows(summ, .id = "model")

L_Summary <- as.data.frame(L_Summary)

L_Summary$Outcome <- word(L_Summary$model, 1)
L_Summary$Predictor <- word(L_Summary$model, 3)

L_Summary[,2:11]<- L_Summary[,2:11] %>%
  mutate(across(c(r.squared, adj.r.squared, sigma, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic, logLik, AIC, BIC, deviance), ~ usefun::specify_decimal(., 2)))
L_Summary[,2:11] <- sapply(L_Summary[,2:11], as.numeric)

All_Summary <- rbind(E_Summary, L_Summary)
All_Coefs <- rbind(E_Coefs, L_Coefs)





########## Diagnostics #############

# install.packages("performance")
# install.packages("see")
# install.packages("ggResidpanel")
# library(ggResidpanel)
# library(performance)
# library(see)
# library(insight)
# library(patchwork)
# install.packages("insight")
# remotes::install_github("clauswilke/ggtext")
# library(ggtext)
# library(ggplot2)



################ Multiple Regression ###################


library(relaimpo)
library(psych)
library(lm.beta)
library(rlang)



### Engagement Data ####
ME_Models <- lm(Engagement ~ ., data = Fake_Reg_Data[,1:9])

E_RWA <- calc.relimp(ME_Models, type = "lmg", rela = TRUE)
E_RWA <- as.data.frame(E_RWA$lmg)


ME_Models <- lm.beta(ME_Models)
coefs = tidy(ME_Models, simplify = F)
ME_Coefs <- dplyr::bind_rows(coefs, .id = "model")
ME_Coefs <- as.data.frame(ME_Coefs)

ME_Coefs$Importance <- E_RWA[match(ME_Coefs$term, row.names(E_RWA)),"E_RWA$lmg"]
ME_Coefs <- ME_Coefs[,2:8]

ME_Coefs[,2:7] <- ME_Coefs[,2:7] %>%
  mutate(across(c(estimate, std_estimate, std.error, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic, Importance), ~ usefun::specify_decimal(., 2)))
ME_Coefs[,2:7] <- sapply(ME_Coefs[,2:7], as.numeric)

# Summary Statistics
MEL_Models <- list(ME_Models)

summ = lapply(MEL_Models, glance, simplify = F)
ME_Summary <- dplyr::bind_rows(summ, .id = "model")
ME_Summary <- as.data.frame(ME_Summary)

ME_Summary[,2:11]<- ME_Summary[,2:11] %>%
  mutate(across(c(r.squared, adj.r.squared, sigma, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic, logLik, AIC, BIC, deviance), ~ usefun::specify_decimal(., 2)))
ME_Summary[,2:11] <- sapply(ME_Summary[,2:11], as.numeric)

colnames(ME_Summary)[1] <- "Outcome"
ME_Summary[1,1] = "Engagement"

# Donut Formatting
ME_Donut <- ME_Coefs[2:9,]
row.names(ME_Donut) <- c(1:8)


ME_Donut <- ME_Donut[,c(1,7)] %>%
  arrange(desc(Importance))



### Leave Intention Data ###

ML_Models <- lm(Intentions_to_Leave ~ . -Intentions_to_Leave, data = Fake_Reg_Data[,1:9])

# RWA
L_RWA <- calc.relimp(ML_Models, type = "lmg", rela = TRUE)
L_RWA <- as.data.frame(L_RWA$lmg)

# Coefficients
ML_Models <- lm.beta(ML_Models)
coefs = tidy(ML_Models, simplify = F)
ML_Coefs <- dplyr::bind_rows(coefs, .id = "model")
ML_Coefs <- as.data.frame(ML_Coefs)


ML_Coefs$Importance <- L_RWA[match(ML_Coefs$term, row.names(L_RWA)),"L_RWA$lmg"]
ML_Coefs <- ML_Coefs[,2:8]

ML_Coefs[,2:7] <- ML_Coefs[,2:7] %>%
  mutate(across(c(estimate, std_estimate, std.error, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic, Importance), ~ usefun::specify_decimal(., 2)))
ML_Coefs[,2:7] <- sapply(ML_Coefs[,2:7], as.numeric)


# Summary Statistics
MLL_Models <- list(ML_Models)

summ = lapply(MLL_Models, glance, simplify = F)
ML_Summary <- dplyr::bind_rows(summ, .id = "model")
ML_Summary <- as.data.frame(ML_Summary)

ML_Summary[,2:11]<- ML_Summary[,2:11] %>%
  mutate(across(c(r.squared, adj.r.squared, sigma, p.value), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic, logLik, AIC, BIC, deviance), ~ usefun::specify_decimal(., 2)))
ML_Summary[,2:11] <- sapply(ML_Summary[,2:11], as.numeric)

colnames(ML_Summary)[1] <- "Outcome"
ML_Summary[1,1] = "Intentions_to_Leave"


# Donut Formatting
ML_Donut <- ML_Coefs[2:9,]
row.names(ML_Donut) <- c(1:8)


ML_Donut <- ML_Donut[,c(1,7)] %>%
  arrange(desc(Importance))




################ Kruskal Wallis ###################
library("rstatix")


NewKruskTest <- function(dat, groupvar, var) {
  kt <- dat %>% rstatix::kruskal_test(as.formula(paste(var, "~", groupvar)))
  efsz <- dat %>% rstatix::kruskal_effsize(as.formula(paste(var, "~", groupvar)))
  results <- cbind(kt, efsz[,3:5])
  results$groupvar <- groupvar 
  results <- results[,c(10, 1:5, 7, 9)]
  return(results)
}

vars <- names(Fake_Data[,1:49])
groupvars <- names(Fake_Data[,c(50:52, 54:58, 61:62)])

All_KW = data.frame()
for(x in groupvars)
  for(y in vars)
    All_KW <- All_KW %>% rbind(NewKruskTest(dat = Fake_Data, groupvar = x, var = y))

All_KW[,3:7] <- All_KW[,3:7] %>%
  mutate(across(c(p, effsize), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(statistic), ~ usefun::specify_decimal(., 2)))
All_KW[,3:7] <- sapply(All_KW[,3:7], as.numeric)

colnames(All_KW) <- c("Demographic", "Item", "n", "Statistic", "df", "p", "EffSize", "Magnitude")

# write.csv(All_KW, "All-KW.csv", row.names = FALSE)



### Dunn's Test ###

NewDunn <- function(dat, groupvar, var) {
  results <- dat %>% rstatix::dunn_test(as.formula(paste(var, "~", groupvar)), 
                                        p.adjust.method = "bonferroni", detailed = TRUE)
  results$groupvar <- groupvar
  results <- results[,c(14,1:10, 12:13)]
  return(results)
}

vars <- names(Fake_Data[,1:49])
groupvars <- names(Fake_Data[,c(50:52, 54:58, 61:62)])

All_Dunn = data.frame()
for(x in groupvars)
  for(y in vars)
    All_Dunn <- All_Dunn %>% rbind(NewDunn(dat = Fake_Data, groupvar = x, var = y))

All_Dunn[,7:12] <- All_Dunn[,7:12] %>%
  mutate(across(c(p, p.adj), ~ usefun::specify_decimal(., 3))) %>%
  mutate(across(c(estimate, estimate1, estimate2, statistic), ~ usefun::specify_decimal(., 2)))
All_Dunn[,7:12] <- sapply(All_Dunn[,7:12], as.numeric)

colnames(All_Dunn) <- c("Demographic", "Item", "Group1", "Group2", "n1", "n2", "MeanDifference", "Mean1", "Mean2", "Statistic", "p", "p.adj", "Significance")

# write.csv(All_Dunn, "All-Dunn.csv", row.names = FALSE)

### Summary Statistics (for plotting) ###

Summs <- na.omit(Fake_Data)
Summs[,1:49] <- sapply(Summs[,1:49], as.numeric)

vars <- names(Summs[,1:49])
groupvars <- names(Summs[,c(50:52, 54:58, 61:62)])

# Demo_Summs <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("levels", "count", "mean", "sd", "median", "IQR", "Item", "Factor"))
# 
# 
# for(y in groupvars){ 
#   for(x in 1:49){ 
#     test <- group_by(Summs, !!sym(y)) %>%
#       dplyr::summarise(
#         count = n(),
#         mean = mean(Summs[[x]], na.rm = TRUE),
#         sd = sd(Summs[[x]], na.rm = TRUE),
#         median = median(Summs[[x]], na.rm = TRUE),
#         IQR = IQR(Summs[[x]], na.rm = TRUE),
#         Item = colnames(Summs)[x],
#         Factor = y )
#     colnames(test) <- c("Group", "Count", "Mean", "SD", "Median", "IQR", "Item", "Demographic")
#     Demo_Summs <- rbind(Demo_Summs, test)
#   }
# }
# 
# Demo_Summs[,3:4] <- Demo_Summs[,3:4] %>%
#   mutate(across(c(Mean, SD), ~ usefun::specify_decimal(., 2)))
# Demo_Summs[,3:4] <- sapply(Demo_Summs[,3:4], as.numeric)
# Demo_Summs <- as.data.frame(Demo_Summs)


z = 0
w = 0
pow <- data.frame(matrix(ncol = z, nrow = w))

for(y in groupvars){ 
  kablow <- Summs %>% group_by(!!sym(y)) %>% summarise_at(., vars, length)
  colnames(kablow)[1] <- "Group"
  kablow$Factor <- y
  pow <- rbind(pow, kablow)
}
pow <- pow %>% pivot_longer(., !c(Group, Factor), names_to = "Item", values_to = "Count")
pow$Item <- ordered(pow$Item)

Demo_Summs <- pow


funx <- c("mean", "sd", "median", "IQR")
for(x in funx){
  z = 0
  w = 0
  pow <- data.frame(matrix(ncol = z, nrow = w))
  for(y in groupvars){ 
    kablow <- Summs %>% group_by(!!sym(y)) %>% summarise_at(., vars, x)
    colnames(kablow)[1] <- "Group"
    kablow$Factor <- y
    pow <- rbind(pow, kablow)
  }
  pow <- pow %>% pivot_longer(., !c(Group, Factor), names_to = "Item", values_to = x)
  pow$Item <- ordered(pow$Item)
  Demo_Summs <- cbind(Demo_Summs, pow[,4])
}

Demo_Summs <- Demo_Summs[,c(1, 4:8, 3, 2)]
colnames(Demo_Summs) <- c("Group", "Count", "Mean", "SD", "Median", "IQR", "Item", "Demographic")


Demo_Summs[,3:4] <- Demo_Summs[,3:4] %>%
  mutate(across(c(Mean, SD), ~ usefun::specify_decimal(., 2)))
Demo_Summs[,3:4] <- sapply(Demo_Summs[,3:4], as.numeric)
Demo_Summs <- as.data.frame(Demo_Summs)


# write.csv(Demo_Summs,"Demographic-Summaries.csv", row.names = FALSE)


######### HeatMaps #############
library(leaflet)
library(tigris)
library(sf)

# SHP file to mape data onto, NOTE: Not real data/state.
Testing_District <- school_districts("Arkansas") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')


# These three lines would NOT be run normally. Adding so we can match a real SHP file w/ Fake data.
# For example if you had school distric data, you may simple merge(data, shp_file, by = "NAME", all.x = TRUE)
District_Codes <- (unique(Fake_Data$Dist_Code))
Testing_District$Dist_Code <- `length<-`(District_Codes, nrow(Testing_District))
Testing_District <- Testing_District[,c(16,1:15)]

# Creating dataframe to merge with shp data. Can then use the group-by variable (in this case "dist_code") to tie shape to data.
Fake_District_Data <- as.data.frame(Fake_Data[,c(60, 1:49)])
  Fake_District_Data$Engagement <- c(rowMeans(Fake_Data[,1:11], na.rm = FALSE))
  Fake_District_Data$Environment_Factors <- c(rowMeans(Fake_Data[,12:16], na.rm = FALSE)) 
  Fake_District_Data$Resource_Factors <- c(rowMeans(Fake_Data[,17:19], na.rm = FALSE)) 
  Fake_District_Data$Interpersonal_Factors <- c(rowMeans(Fake_Data[,20:25], na.rm = FALSE))
  Fake_District_Data$Perceived_Organizational_Support <- c(rowMeans(Fake_Data[,26:29], na.rm = FALSE))
  Fake_District_Data$Leadership <- c(rowMeans(Fake_Data[,30:34], na.rm = FALSE))
  Fake_District_Data$Justice <- c(rowMeans(Fake_Data[,35:39], na.rm = FALSE))
  Fake_District_Data$Voice <- c(rowMeans(Fake_Data[,40:45], na.rm = FALSE))
  Fake_District_Data$Intentions_to_leave <- c(rowMeans(Fake_Data[,46:49], na.rm = FALSE))
  
Fake_District_Counts <- Fake_District_Data %>%
  na.omit() %>%
  count(Dist_Code)
Fake_District_Counts <- sapply(Fake_District_Counts, as.numeric)
Fake_District_Counts <- as.data.frame(Fake_District_Counts)


Fake_District_Data <- Fake_District_Data %>%
  na.omit() %>%
  drop_na(Dist_Code) %>% 
  group_by(Dist_Code) %>%
  summarise_all("mean") %>%
  mutate(across(2:59, ~ usefun::specify_decimal(., 2))) # reduce decimal places to 2 (annoyingly will make data class = char)
Fake_District_Data[,2:59] <- sapply(Fake_District_Data[,2:59], as.numeric) # make data numeric again

Fake_District_Data <- cbind(Fake_District_Data, as.numeric(Fake_District_Counts[,2]))
colnames(Fake_District_Data)[60] <- "n"


# Voila, Now you have a variable ready to be worked with in leaflet.
Testing_District <- merge(Testing_District, Fake_District_Data, by = "Dist_Code", all.x = TRUE)

Testing_District_Data <- st_set_geometry(Testing_District, NULL)




##### Saving Relevant Data to more rapidly load into the APP (Data not large enough to warrant external database) #####


DataSetList <- list("E_Summary" = E_Summary,
                    "L_Summary" = L_Summary,
                    "E_Coefs" = E_Coefs,
                    "L_Coefs" = L_Coefs,
                    "E_Models" = E_Models,
                    "L_Models" = L_Models,
                    "All_Summary" = All_Summary,
                    "ME_Summary" = ME_Summary,
                    "ML_Summary" = ML_Summary,
                    "ME_Coefs" = ME_Coefs,
                    "ML_Coefs" = ML_Coefs,
                    "ME_Donut" = ME_Donut,
                    "ML_Donut" = ML_Donut,
                    "All_KW" = All_KW,
                    "All_Dunn" = All_Dunn,
                    "Demo_Summs" = Demo_Summs,
                    "Testing_District" = Testing_District,
                    "Testing_District_Data" = Testing_District_Data,
                    "Fake_Data" = Fake_Data,
                    "Fake_District_Data" = Fake_District_Data,
                    "Fake_Reg_Data" = Fake_Reg_Data)

lapply(names(DataSetList), function(x) 
  saveRDS(DataSetList[[x]], paste0("Data/",x, ".rds")))
