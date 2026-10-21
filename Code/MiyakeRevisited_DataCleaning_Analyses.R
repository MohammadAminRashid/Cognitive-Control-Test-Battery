#-------------------------------------------------------------------------------

# Miyake revisited: Validating the factor structure of an open-source cognitive control test battery

# Cleaning and analyses

#-------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lavaan)
library(semPlot)
library(haven)
library(psych)
library(MVN)
library(car)
library(Hmisc) 
library(splithalf)

# Set working directory where data is stored
setwd("C:/Users/u0123959/OneDrive - KU Leuven/Documents/Doctoraat/Valideringsstudie/Preprocessing/Miyake 2.0/Documenten voor op OSF")
# Import data
MiyakeRevisited_DataAggr <- read_csv("MiyakeRevisited_DataAggr.csv")

#-------------------------------------------------------------------------------
# 1. Untrimmed data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------
# Make a list of all relevant measures
Data_List <- list(MiyakeRevisited_DataAggr$Antisac_Mean_ERR,
                  MiyakeRevisited_DataAggr$GoNoGo_Mean_ERR_NoGo,
                  MiyakeRevisited_DataAggr$Stroop_Median_RT_C, MiyakeRevisited_DataAggr$Stroop_Median_RT_I, MiyakeRevisited_DataAggr$Stroop_Median_RT_StroopEffect,
                  MiyakeRevisited_DataAggr$nBack_Mean_ERR_Hit,
                  MiyakeRevisited_DataAggr$KeepTrack_ERR,
                  MiyakeRevisited_DataAggr$LetMem_ERR,
                  MiyakeRevisited_DataAggr$PlusMinus_Time_Add, MiyakeRevisited_DataAggr$PlusMinus_Time_Subtr, MiyakeRevisited_DataAggr$PlusMinus_Time_Switch, MiyakeRevisited_DataAggr$PlusMinus_Time_Switch_Cost,
                  MiyakeRevisited_DataAggr$NumLet_Median_RT_Num, MiyakeRevisited_DataAggr$NumLet_Median_RT_Let, MiyakeRevisited_DataAggr$NumLet_Median_RT_Switch, MiyakeRevisited_DataAggr$NumLet_Median_RT_SwitchCost_Local,
                  MiyakeRevisited_DataAggr$GlobLoc_Median_RT_No_Switch, MiyakeRevisited_DataAggr$GlobLoc_Median_RT_Switch, MiyakeRevisited_DataAggr$GlobLoc_Median_RT_Switch_Cost
)

# Make empty list to append all output to
meansData <- c()
mediansData  <- c()
sdsData  <- c()
rangesData  <- c()

# Compute mean, SD and range of all measures in MiyakeData_List
for (i in Data_List){
  meanOutput <- mean(i, na.rm = T)
  meansData <- append(meansData, meanOutput)
  sdOutput <- sd(i, na.rm = T)
  sdsData <- append(sdsData, sdOutput)
  rangeOutput <- range(i, na.rm = T)
  rangesData <- append(rangesData, rangeOutput)
}
# Round decimals
meansData <- round(meansData, 2)
sdsData <- round(sdsData, 2)
rangesData <- round(rangesData, 2)

#-------------------------------------------------------------------------------
# Skewness & kurtosis
#-------------------------------------------------------------------------------
# Make empty list to append all output to
skewnessData <- c()
kurtosisData <- c()
ShapiroData <- c()

# Compute mean, SD and range of each dependent measure
for (i in Data_List){
  skewnessOutput <- skew(i, na.rm = T)
  skewnessData <- append(skewnessData, skewnessOutput)
  kurtosisOutput <- kurtosi(i, na.rm = T)
  kurtosisData <- append(kurtosisData, kurtosisOutput)
  ShapiroOutput <- shapiro.test(i)
  ShapiroData <- append(ShapiroData, ShapiroOutput)
}
# Round decimals
skewnessData <- round(skewnessData, 2)
kurtosisData <- round(kurtosisData, 2)

#-------------------------------------------------------------------------------
# Test for multivariate normality
#-------------------------------------------------------------------------------
# Select 9 dependent variables from all the measures in the data frame
dependent_var <- MiyakeRevisited_DataAggr %>% 
  select(Antisac_Mean_ERR, GoNoGo_Mean_ERR_NoGo, Stroop_Median_RT_StroopEffect,
         GlobLoc_Median_RT_Switch_Cost,PlusMinus_Time_Switch_Cost, NumLet_Median_RT_SwitchCost_Local,
         KeepTrack_ERR,LetMem_ERR, nBack_Mean_ERR_Hit)

# Testing for multivariate normality
result1 <- mvn(dependent_var, mvnTest = 'mardia', univariateTest = 'SW', multivariatePlot = "qq")

#-------------------------------------------------------------------------------
# Spearman Correlation matrix
#-------------------------------------------------------------------------------
# Compute a correlation matrix for all these variables
data1.rcorr.SP <- rcorr(as.matrix(dependent_var), type = "spearman")
# Store the estimates & p-values in a new object
data1.coeff.SP <- data1.rcorr.SP$r
data1.p.SP <- data1.rcorr.SP$P
# Round the number of decimals to 2
data1.coeff.SP <- round(data1.coeff.SP, 2)
data1.p.SP <- round(data1.p.SP, 3)

#-------------------------------------------------------------------------------
# Pearson Correlation matrix
#-------------------------------------------------------------------------------
# Compute a correlation matrix for all these variables
data1.rcorr.PS <- rcorr(as.matrix(dependent_var), type = "pearson")
# Store the estimates & p-values in a new object
data1.coeff.PS <- data1.rcorr.PS$r
data1.p.PS <- data1.rcorr.PS$P
# Round the number of decimals to 2
data1.coeff.PS <- round(data1.coeff.PS, 2)
data1.p.PS <- round(data1.p.PS, 3)

#-------------------------------------------------------------------------------
# Confirmatory Factor Analysis based on the uncleaned (UC) dataset
#-------------------------------------------------------------------------------

## One-factor model
# Build the 1 factor model
Model_1factor_UC <- 'CogntiveControl =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect +
                   GlobLoc_Median_RT_Switch_Cost + PlusMinus_Time_Switch_Cost + NumLet_Median_RT_SwitchCost_Local +
                   KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit'
# Fit the model
fit_1factor_UC <- cfa(Model_1factor_UC , data = MiyakeRevisited_DataAggr, std.lv = TRUE)
# Make the model visual
semPaths(fit_1factor_UC, 'Model_1factor_UC',  'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_1factor_UC, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit?
fitMeasures(fit_1factor_UC)

## Three-factor model
# Build the 3 factor model 
Model_3factor_UC <- 'Inhibition =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect
                  Switching =~  GlobLoc_Median_RT_Switch_Cost + PlusMinus_Time_Switch_Cost + NumLet_Median_RT_SwitchCost_Local
                  Updating =~ KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                  '
# Fit the model
fit_3factor_UC <- cfa(Model_3factor_UC, data = MiyakeRevisited_DataAggr, std.lv = TRUE)
# Make the model visual
semPaths(fit_3factor_UC, 'Model_3factor_UC', 'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_3factor_UC, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit
fitMeasures(fit_3factor_UC)

## Bi-factor model
# Build a bi-factor higher order model
Model_Bifactor_UC <- ' Switching =~ GlobLoc_Median_RT_Switch_Cost + PlusMinus_Time_Switch_Cost + NumLet_Median_RT_SwitchCost_Local
                   Updating =~ KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                   
                   CC =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect +
                   GlobLoc_Median_RT_Switch_Cost + PlusMinus_Time_Switch_Cost + NumLet_Median_RT_SwitchCost_Local +
                   KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                   
                   
                   CC ~~ 0 * Switching
                   CC ~~ 0 * Updating
                   Switching ~~ 0 * Updating
                  '

# Fit the model
fit_Bifactor_UC <- cfa(Model_Bifactor_UC, data =  MiyakeRevisited_DataAggr, std.lv = TRUE)
# Make the model visual
semPaths(fit_Bifactor_UC, 'Model_Bifactor_UC', 'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_Bifactor_UC, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit?
fitMeasures(fit_Bifactor_UC)


# Compare 1-factor & 3-factor model using ANOVA
anova(fit_1factor_UC, fit_3factor_UC)
# Compare 1-factor & bi-factor model using ANOVA
anova(fit_1factor_UC, fit_Bifactor_UC)
# Compare 3-factor & bi-factor model using ANOVA
anova(fit_3factor_UC, fit_Bifactor_UC)


#-------------------------------------------------------------------------------
# 2. Data Cleaning: Remove participant outliers
#-------------------------------------------------------------------------------

## 1. Antisaccade task
# Compute group mean
Antisac_Mean <- mean(MiyakeRevisited_DataAggr$Antisac_Mean_ERR)
# Compute group standard deviation
Antisac_SD <- sd(MiyakeRevisited_DataAggr$Antisac_Mean_ERR)
# Computer lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- Antisac_Mean - 3*Antisac_SD
UpperLimit <- Antisac_Mean + 3*Antisac_SD
# Identify participant outliers that are outside the +/- 3 SD range: 7 participants
participant_outliers_Antisaccade <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, Antisac_Mean_ERR) %>% 
  filter(Antisac_Mean_ERR < LowerLimit | Antisac_Mean_ERR > UpperLimit)
# Remove participant outliers by putting the remaining participants in a new dataframe
# According to the participant outliers, there are only participants with an ERR that is too high (lower limit is negative)
Antisac <- MiyakeRevisited_DataAggr %>% 
  filter(Antisac_Mean_ERR < UpperLimit)

## 2. Go No-Go task
# Compute group mean
GoNoGo_Mean <- mean(MiyakeRevisited_DataAggr$GoNoGo_Mean_ERR_Overall)
# Compute group SD
GoNoGo_SD <- sd(MiyakeRevisited_DataAggr$GoNoGo_Mean_ERR_Overall)
# Compute lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- GoNoGo_Mean - 3*GoNoGo_SD
UpperLimit <- GoNoGo_Mean + 3*GoNoGo_SD
# Identify participant outliers that are outside the +/- 3 SD range: 7 participants: 3 participants
participant_outliers_GoNoGo <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, GoNoGo_Mean_ERR_Overall) %>% 
  filter(GoNoGo_Mean_ERR_Overall < LowerLimit | GoNoGo_Mean_ERR_Overall > UpperLimit)
# Remove participant outliers by putting the remaining participants in a new dataframe
# According to the participant outliers, there are only participants with an ERR that is too high (lower limit is negative)
GoNoGo <- MiyakeRevisited_DataAggr %>% 
  filter(GoNoGo_Mean_ERR_Overall < UpperLimit)

## 3. Stroop task
# Compute group mean
Stroop_Mean <- mean(MiyakeRevisited_DataAggr$Stroop_Mean_RT_Total)
# Compute group SD
Stroop_SD <- sd(MiyakeRevisited_DataAggr$Stroop_Mean_RT_Total)
# Computer lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- Stroop_Mean - 3*Stroop_SD
UpperLimit <- Stroop_Mean + 3*Stroop_SD
# Identify participant outliers that are outside the +/- 3 SD range: 4 participants
participant_outliers_Stroop <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, Stroop_Mean_RT_Total) %>% 
  filter(Stroop_Mean_RT_Total < LowerLimit | Stroop_Mean_RT_Total > UpperLimit)
# Remove participant outliers by putting the remaining participants in a new dataframe
# According to the participant outliers, there are only participants with a mean RT that is too high (too slow)
Stroop <- MiyakeRevisited_DataAggr %>% 
  filter(Stroop_Mean_RT_Total < UpperLimit)

## 4. Local-Global task
# Import raw data file as mean reaction time for all trials is not in the MiyakaRevisited_DataAggr dataframe
X4_LocalGlobal_RawData <- read_csv("4. LocalGlobal_RawData.csv")
# Only keep trials that are correctly solved
LocalGlobal <- X4_LocalGlobal_RawData %>% 
  filter(ACC == 1)
# Aggregate the trials into a mean RT for each participant
LocalGlobal <- LocalGlobal %>%  
  dplyr::group_by(ParticipantNr) %>% 
  dplyr::summarise(LocalGlobal_Mean_RT = mean(RT_1000))
# Bind this vector into the original data frame
MiyakeRevisited_DataAggr <- merge(MiyakeRevisited_DataAggr, LocalGlobal,sort=FALSE)
# Compute group mean
LocalGlobal_Mean <- mean(MiyakeRevisited_DataAggr$LocalGlobal_Mean_RT)
# Compute group SD
LocalGlobal_SD <- sd(MiyakeRevisited_DataAggr$LocalGlobal_Mean_RT)
# Compute lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- LocalGlobal_Mean - 3*LocalGlobal_SD
UpperLimit <- LocalGlobal_Mean + 3*LocalGlobal_SD
# Identify participant outliers that are outside the +/- 3 SD range: 2 participants
participant_outliers_LocalGlobal <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, LocalGlobal_Mean_RT) %>% 
  filter(LocalGlobal_Mean_RT < LowerLimit | LocalGlobal_Mean_RT > UpperLimit)
# Remove participant outliers by putting the remaining participants in a new dataframe
# According to the participant outliers, there are only participants with a mean RT that is too high (too slow)
LocalGlobal <- MiyakeRevisited_DataAggr %>% 
  filter(LocalGlobal_Mean_RT < UpperLimit)

## 5. Plus Minus
# Compute group mean
PlusMinus_Mean <- mean(MiyakeRevisited_DataAggr$PlusMinus_Mean_Time)
# Compute group SD
PlusMinus_SD <- sd(MiyakeRevisited_DataAggr$PlusMinus_Mean_Time)
# Computer lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- PlusMinus_Mean - 3*PlusMinus_SD
UpperLimit <- PlusMinus_Mean + 3*PlusMinus_SD
# Identify participant outliers
participant_outliers_PlusMinus <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, PlusMinus_Mean_Time) %>% 
  filter(PlusMinus_Mean_Time < LowerLimit | PlusMinus_Mean_Time > UpperLimit)
# According to the participant outliers, there are only participants with an ERR that is too high (lower limit is negative)
# Remove participant outliers by putting the remaining participants in a new dataframe
PlusMinus <- MiyakeRevisited_DataAggr %>% 
  filter(PlusMinus_Mean_Time < UpperLimit)

## 6. Number-Letter
# Compute group mean
NumberLetter_Mean <- mean(MiyakeRevisited_DataAggr$NumLet_Mean_RT_Total)
# Compute group SD
NumberLetter_SD <- sd(MiyakeRevisited_DataAggr$NumLet_Mean_RT_Total)
# Compute lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- NumberLetter_Mean - 3*NumberLetter_SD
UpperLimit <- NumberLetter_Mean + 3*NumberLetter_SD
# Identify participant outliers that are outside the +/- 3 SD range: 2 participants
participant_outliers_NumberLetter <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, NumLet_Mean_RT_Total) %>% 
  filter(NumLet_Mean_RT_Total < LowerLimit | NumLet_Mean_RT_Total > UpperLimit)
# According to the participant outliers, there are only participants with a mean RT that is too high (too slow)
# Remove participant outliers by putting the remaining participants in a new dataframe
NumberLetter <- MiyakeRevisited_DataAggr %>% 
  filter(NumLet_Mean_RT_Total < UpperLimit)

## 7. Keep Track
# Compute group mean
KeepTrack_Mean <- mean(MiyakeRevisited_DataAggr$KeepTrack_ERR)
# Compute group SD
KeepTrack_SD <- sd(MiyakeRevisited_DataAggr$KeepTrack_ERR)
# Computer lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- KeepTrack_Mean - 3*KeepTrack_SD
UpperLimit <- KeepTrack_Mean + 3*KeepTrack_SD
# Identify participant outliers that are outside the +/- 3 SD range: 2 participants
participant_outliers_KeepTrack <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, KeepTrack_ERR) %>% 
  filter(KeepTrack_ERR < LowerLimit | KeepTrack_ERR > UpperLimit)
# According to the participant outliers, there are only participants with an ERR that is too high
# Remove participant outliers by putting the remaining participants in a new dataframe
KeepTrack <- MiyakeRevisited_DataAggr %>% 
  filter(KeepTrack_ERR < UpperLimit)

## 8. Letter Memory
# Compute group mean
LetterMemory_Mean <- mean(MiyakeRevisited_DataAggr$LetMem_ERR)
# Compute group SD
LetterMemory_SD <- sd(MiyakeRevisited_DataAggr$LetMem_ERR)
# Compute lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- LetterMemory_Mean - 3*LetterMemory_SD
UpperLimit <- LetterMemory_Mean + 3*LetterMemory_SD
# Identify participant outliers that are outside the +/- 3 SD range: 2 participants
participant_outliers_LetterMemory <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, LetMem_ERR) %>% 
  filter(LetMem_ERR < LowerLimit | LetMem_ERR > UpperLimit)
# According to the participant outliers, there are only participants with an ERR that is too high (lower limit is negative)
# Remove participant outliers by putting the remaining participants in a new dataframe
LetterMemory <- MiyakeRevisited_DataAggr %>% 
  filter(LetMem_ERR < UpperLimit)

## 9. N-Back
# Compute group mean
NBack_Mean <- mean(MiyakeRevisited_DataAggr$nBack_Mean_ERR_Total)
# Compute group SD
NBack_SD <- sd(MiyakeRevisited_DataAggr$nBack_Mean_ERR_Total)
# Compute lower & upper limit by determining values that are 3x the SD below & above the mean
LowerLimit <- NBack_Mean - 3*NBack_SD
UpperLimit <- NBack_Mean + 3*NBack_SD
# Identify participant outliers that are outside the +/- 3 SD range: 2 participants
participant_outliers_NBack <- MiyakeRevisited_DataAggr %>% 
  select(ParticipantNr, nBack_Mean_ERR_Total) %>% 
  filter(nBack_Mean_ERR_Total < LowerLimit | nBack_Mean_ERR_Total > UpperLimit)
# According to the participant outliers, there are only participants with an ERR that is too high (lower limit is negative)
# Remove participant outliers by putting the remaining participants in a new dataframe
NBack <- MiyakeRevisited_DataAggr %>% 
  filter(nBack_Mean_ERR_Total < UpperLimit)

# Now we have separate dataframes of each task with outliers removed, but we want
# each outlier to be removed on every task and not on that particular task.
# Therefore, we will collect all outliers together and then remove them from the
# original dataframe.

#-------------------------------------------------------------------------------
# Remove all participant outliers from all tasks
# Identify unique participant outliers
participant_outliers <- c(# Antisaccade outliers
                          32,43,57,72,117,142,143,150,
                          # Go No-Go outliers
                          10,72,174,
                          # Stroop outliers
                           55,183,191,259,268,
                          # Local-Global outliers
                          18,58,268,
                          # Plus-Minus outliers
                          58,171,183,266,268,
                          # Number-Letter outliers
                          172,198,
                          # Keep Track outliers
                          7,72,
                          # Letter Memory outliers
                          10,34,72,
                          # N-back outliers
                          10,57,72,183)

participant_outliers <- sort(participant_outliers)
participant_outliers <- unique(participant_outliers)
# Remove participant outliers (23 in total)
MiyakeRevisited_DataAggr2 <- MiyakeRevisited_DataAggr %>%
  filter(!(ParticipantNr %in% participant_outliers))

#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------
# Make a list of all relevant measures
Data_List2 <- list(MiyakeRevisited_DataAggr2$Antisac_Mean_ERR,
                   MiyakeRevisited_DataAggr2$GoNoGo_Mean_ERR_NoGo,
                   MiyakeRevisited_DataAggr2$Stroop_Mean_RT_C, MiyakeRevisited_DataAggr2$Stroop_Median_RT_C,MiyakeRevisited_DataAggr2$Stroop_Mean_RT_I, MiyakeRevisited_DataAggr2$Stroop_Median_RT_I, MiyakeRevisited_DataAggr2$Stroop_Mean_RT_StroopEffect, MiyakeRevisited_DataAggr2$Stroop_Median_RT_StroopEffect,
                   MiyakeRevisited_DataAggr2$nBack_Mean_ERR_Hit,
                   MiyakeRevisited_DataAggr2$KeepTrack_ERR,
                   MiyakeRevisited_DataAggr2$LetMem_ERR,
                   MiyakeRevisited_DataAggr2$PlusMinus_Time_Add, MiyakeRevisited_DataAggr2$PlusMinus_Time_Subtr, MiyakeRevisited_DataAggr2$PlusMinus_Time_Switch, MiyakeRevisited_DataAggr2$PlusMinus_Time_Switch_Cost,
                   MiyakeRevisited_DataAggr2$NumLet_Mean_RT_Num, MiyakeRevisited_DataAggr2$NumLet_Median_RT_Num, MiyakeRevisited_DataAggr2$NumLet_Mean_RT_Let, MiyakeRevisited_DataAggr2$NumLet_Median_RT_Let, MiyakeRevisited_DataAggr2$NumLet_Mean_RT_Switch, MiyakeRevisited_DataAggr2$NumLet_Median_RT_Switch, MiyakeRevisited_DataAggr2$NumLet_Mean_RT_SwitchCost_Local, MiyakeRevisited_DataAggr2$NumLet_Median_RT_SwitchCost_Local,
                   MiyakeRevisited_DataAggr2$GlobLoc_Mean_RT_No_Switch, MiyakeRevisited_DataAggr2$GlobLoc_Median_RT_No_Switch, MiyakeRevisited_DataAggr2$GlobLoc_Mean_RT_Switch, MiyakeRevisited_DataAggr2$GlobLoc_Median_RT_Switch, MiyakeRevisited_DataAggr2$GlobLoc_Mean_RT_Switch_Cost, MiyakeRevisited_DataAggr2$GlobLoc_Median_RT_Switch_Cost
)

# Make empty list to append all output to
meansData2 <- c()
sdsData2 <- c()
rangesData2 <- c()

# Compute mean, SD and range of all measures in Data_List
for (i in Data_List2){
  meanOutput2 <- mean(i, na.rm = T)
  meansData2 <- append(meansData2, meanOutput2)
  sdOutput2 <- sd(i, na.rm = T)
  sdsData2 <- append(sdsData2, sdOutput2)
  rangeOutput2 <- range(i, na.rm = T)
  rangesData2 <- append(rangesData2, rangeOutput2)
}
# Round decimals
meansData2 <- round(meansData2, 2)
sdsData2 <- round(sdsData2, 2)
rangesData2 <- round(rangesData2, 2)

#-------------------------------------------------------------------------------
# Skewness & kurtosis
#-------------------------------------------------------------------------------

# Make empty list to append all output to
skewnessData2 <- c()
kurtosisData2 <- c()
ShapiroData2 <- c()

# Compute skewness, kurtosis and Shapiro-Wilk indices for each dependent measure
for (i in Data_List2){
  skewnessOutput2 <- skew(i, na.rm = T)
  skewnessData2 <- append(skewnessData2, skewnessOutput2)
  kurtosisOutput2 <- kurtosi(i, na.rm = T)
  kurtosisData2 <- append(kurtosisData2, kurtosisOutput2)
  ShapiroOutput2 <- shapiro.test(i)
  ShapiroData2 <- append(ShapiroData2, ShapiroOutput2)
}

# Round decimals
skewnessData2 <- round(skewnessData2, 2)
kurtosisData2 <- round(kurtosisData2, 2)

#-------------------------------------------------------------------------------
# Test for multivariate normality
#-------------------------------------------------------------------------------
# Select 9 dependent measures from data frame
dependent_var <- MiyakeRevisited_DataAggr2 %>% 
  select(Antisac_Mean_ERR, GoNoGo_Mean_ERR_NoGo, Stroop_Median_RT_StroopEffect,
         GlobLoc_Median_RT_Switch_Cost,PlusMinus_Time_Switch_Cost,, NumLet_Median_RT_SwitchCost_Local,
         KeepTrack_ERR,LetMem_ERR, nBack_Mean_ERR_Hit)

# Testing for multivariate normality
result2 <- mvn(dependent_var, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# 3. Remove trial outliers from tasks with reaction time based measures
# Except for the Plus-Minus task as this task only has one trial per condition
#-------------------------------------------------------------------------------

## 3. Stroop task
# Identify trial outliers
# Import raw data file
X3_Stroop_RawData <- read_csv("3. Stroop_RawData.csv")
# Filter out participants outliers for this particular task; see previous data cleaning step
Stroop <- X3_Stroop_RawData %>% 
  filter(ParticipantNr != 55 & ParticipantNr != 183 & ParticipantNr != 191 & ParticipantNr != 259 & ParticipantNr != 268)
# Create RT_100
Stroop <- Stroop %>% 
  mutate(RT_1000 = RT*1000)
# Only keep trials that are correctly solved & main trials
Stroop <- Stroop %>% 
  filter(ACC == 1& Procedure == 'main')
# Compute for each participant the individual mean and SD and put it into a new data frame
Stroop_means_sds <- Stroop %>%
  dplyr::group_by(ParticipantNr)%>%
  dplyr::summarise(ind_mean_RT = mean(RT_1000),
                   ind_sd_RT = sd(RT_1000)
  )
# Merge the Stroop and Stroop_means_sds
Stroop <- merge(Stroop,Stroop_means_sds,sort=FALSE)
# Compute upper and lower limit of each participant
# Compute lower & upper limit of each individual participant by determining values that are 3x the SD below & above individual mean
Stroop <- Stroop %>% 
  mutate(LowerLimit = ind_mean_RT - 3* ind_sd_RT,
         UpperLimit = ind_mean_RT + 3* ind_sd_RT)

# Identify trial outliers that are outside the +/- 3 SD range of the individual mean: 175 trials
trial_outliers_Stroop <- Stroop %>% 
  filter(RT_1000 < LowerLimit | RT_1000 > UpperLimit)
# Remove these trial outliers that are 3 SD away from the individual mean
# Convert the RT to NA if the trial is an outlier
for (i in 1:nrow(Stroop)){
  if (Stroop$RT_1000[i] < Stroop$LowerLimit[i] | Stroop$RT_1000[i] > Stroop$UpperLimit[i] ){
    Stroop$RT_1000[i] = NA
  }
}
# Listwise deletion: Remove the trial outliers
Stroop <- na.omit(Stroop)
# Aggregate the trials into a median RT for each participant
Stroop_aggr <- Stroop %>%  
  dplyr::group_by(ParticipantNr, Congruency) %>% 
  dplyr::summarise(Stroop_Median_RT = median(RT_1000))
# Create a wide data frame and keep all necessary variables
Stroop_aggr <- Stroop_aggr %>% 
  spread(key = Congruency, value = Stroop_Median_RT)
# Rename these 2 new variables
names(Stroop_aggr)[names(Stroop_aggr) == "C"] <- "Stroop_Median_RT_C_withoutOutliers"
names(Stroop_aggr)[names(Stroop_aggr) == "I"] <- "Stroop_Median_RT_I_withoutOutliers"
# Make a new variable for the adjusted Stroop effect
Stroop_aggr <- Stroop_aggr %>% 
  mutate(Stroop_Median_RT_StroopEffect_withoutOutliers = Stroop_Median_RT_I_withoutOutliers - Stroop_Median_RT_C_withoutOutliers)
# Merge the aggregated data frame and the Stroop data frame
MiyakeRevisited_DataAggr <- merge(MiyakeRevisited_DataAggr, Stroop_aggr, by = "ParticipantNr", all = TRUE)

## 4. Local-Global task
# Identify trial outliers
# Re-import raw data
# Import raw data file
X4_LocalGlobal_RawData <- read_csv("4. LocalGlobal_RawData.csv")
# Filter out participants outliers for this particular task; see previous data cleaning step
LocalGlobal <- X4_LocalGlobal_RawData %>% 
  filter(ParticipantNr != 18 & ParticipantNr != 58 & ParticipantNr != 268)
# Create RT_100
LocalGlobal <- LocalGlobal %>% 
  mutate(RT_1000 = RT*1000)
# Only keep trials that are correctly solved & main trials
LocalGlobal <- LocalGlobal %>% 
  filter(ACC == 1& Procedure == 'main')
# Compute for each participant the individual mean and SD and put it into a new data frame
LocalGlobal_means_sds <- LocalGlobal %>%
  dplyr::group_by(ParticipantNr)%>%
  dplyr::summarise(ind_mean_RT = mean(RT_1000),
                   ind_sd_RT = sd(RT_1000)
  )
# Merge the LocalGlobal and the LocalGlobal_means_sds
LocalGlobal <- merge(LocalGlobal,LocalGlobal_means_sds,sort=FALSE)
# Compute lower & upper limit of each individual participant by determining values that are 3x the SD below & above individual mean
LocalGlobal <- LocalGlobal %>% 
  mutate(LowerLimit = ind_mean_RT - 3* ind_sd_RT,
         UpperLimit = ind_mean_RT + 3* ind_sd_RT)
# Identify trial outliers that are outside the +/- 3 SD range of the individual mean: 302 trials
trial_outliers_LocalGlobal <- LocalGlobal %>% 
  filter(RT_1000 < LowerLimit | RT_1000 > UpperLimit)
# Remove these trial outliers that are 3 SD away from the individual mean
# Convert the RT to NA if the trial is an outlier
for (i in 1:nrow(LocalGlobal)){
  if (LocalGlobal$RT_1000[i] < LocalGlobal$LowerLimit[i] | LocalGlobal$RT_1000[i] > LocalGlobal$UpperLimit[i] ){
    LocalGlobal$RT_1000[i] = NA
  }
}
# Listwise deletion: Remove the trial outliers
LocalGlobal <- na.omit(LocalGlobal)
# Aggregate the trials into a median RT for each participant
LocalGlobal_aggr <- LocalGlobal %>%  
  dplyr::group_by(ParticipantNr, Switch) %>% 
  dplyr::summarise(LocalGlobal_Median_RT = median(RT_1000))
# Create a wide data frame and keep all necessary variables
LocalGlobal_aggr <- LocalGlobal_aggr %>% 
  spread(key = Switch, value = LocalGlobal_Median_RT)
# Rename these 2 new variables
names(LocalGlobal_aggr)[names(LocalGlobal_aggr) == "0"] <- "LocalGlobal_Median_RT_No_Switch_withoutOutliers"
names(LocalGlobal_aggr)[names(LocalGlobal_aggr) == "1"] <- "LocalGlobal_Median_RT_Switch_withoutOutliers"
# Make new variable for the adjusted switch cost
LocalGlobal_aggr <- LocalGlobal_aggr %>% 
  mutate(LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = LocalGlobal_Median_RT_Switch_withoutOutliers - LocalGlobal_Median_RT_No_Switch_withoutOutliers)
# Merge the aggregated data frame and the LocalGlobal data frame
MiyakeRevisited_DataAggr <- merge(MiyakeRevisited_DataAggr, LocalGlobal_aggr, by = "ParticipantNr", all = TRUE)

## 6. Number-Letter
# Identify trial outliers
# Import raw data file
X6_NumberLetter_RawData <- read_csv("6. NumberLetter_RawData.csv")
# Filter out participants outliers for this particular task; see previous data cleaning step
NumLet <- X6_NumberLetter_RawData %>% 
  filter(ParticipantNr != 172 & ParticipantNr != 198)
# Create RT_100
NumLet <- NumLet %>% 
  mutate(RT_1000 = RT*1000)
# Only keep trials that are correctly solved & main trials
NumLet <- NumLet %>% 
  filter(Procedure == 'main')
# We need to create a new variable that indicates whether the trial was a no-switch trial in the Number or Letter blocks or a switch trial in the mix block
# Create a variable with no values that will indicate this
NumLet <- add_column(NumLet, Detect_Local_Switch = NA)
# Assign a value for each row
for (i in 1:nrow(NumLet)){
  if (NumLet$Block[i] == 'NUM'){
    NumLet$Detect_Local_Switch[i] = 'Num'
  }
  if (NumLet$Block[i] == 'LET'){
    NumLet$Detect_Local_Switch[i] = 'Let'
  }
  if (NumLet$Switch[i] == 1){
    NumLet$Detect_Local_Switch[i] = 'Switch'
  }
  if (NumLet$Switch[i] == 0 & NumLet$Block[i] == 'MIX'){
    NumLet$Detect_Local_Switch[i] = 'Not necessary'
  }
}
# Delete not necessary trials: i.e., no-switch trials in the mix block
NumLet <- NumLet %>% 
  filter(Detect_Local_Switch != 'Not necessary')
# Only keep trials that are correctly solved
NumLet <- NumLet %>% 
  filter(ACC == 1)
# Compute for each participant the individual mean and SD and put it into a new data frame
NumLet_means_sds <- NumLet %>%
  dplyr::group_by(ParticipantNr)%>%
  dplyr::summarise(ind_mean_RT = mean(RT_1000),
                   ind_sd_RT = sd(RT_1000)
  )
# Merge the NumLet and NumLet_means_sds
NumLet <- merge(NumLet,NumLet_means_sds,sort=FALSE)
# Compute lower & upper limit of each individual participant by determining values that are 3x the SD below & above individual mean
NumLet <- NumLet %>% 
  mutate(LowerLimit = ind_mean_RT - 3* ind_sd_RT,
         UpperLimit = ind_mean_RT + 3* ind_sd_RT)
# Identify trial outliers that are outside the +/- 3 SD range of the individual mean: 859 trials
trial_outliers_NumLet <- NumLet %>% 
  filter(RT_1000 < LowerLimit | RT_1000 > UpperLimit)
# Remove these trial outliers that are 3 SD away from the individual mean
# Convert the RT to NA if the trial is an outlier
for (i in 1:nrow(NumLet)){
  if (NumLet$RT_1000[i] < NumLet$LowerLimit[i] | NumLet$RT_1000[i] > NumLet$UpperLimit[i] ){
    NumLet$RT_1000[i] = NA
  }
}
# Remove unnecessary columns
NumLet <- subset(NumLet, select = -c(22:24))
# Listwise deletion: Remove the trial outliers
NumLet <- na.omit(NumLet)
# Aggregate the trials into a median RT for each participant
NumLet_aggr <- NumLet %>%  
  dplyr::group_by(ParticipantNr, Detect_Local_Switch) %>% 
  dplyr::summarise(NumLet_Median_RT = median(RT_1000))
# Create a wide data frame and keep all necessary variables
NumLet_aggr <- NumLet_aggr %>% 
  spread(key = Detect_Local_Switch, value = NumLet_Median_RT) 
# Rename these 2 new variables
names(NumLet_aggr)[names(NumLet_aggr) == "Num"] <- "NumberLetter_Median_RT_Num_withoutOutliers"
names(NumLet_aggr)[names(NumLet_aggr) == "Let"] <- "NumberLetter_Median_RT_Let_withoutOutliers"
names(NumLet_aggr)[names(NumLet_aggr) == "Switch"] <- "NumberLetter_Median_RT_Switch_withoutOutliers"
# Make new variable for the adjusted switch cost
NumLet_aggr <- NumLet_aggr %>% 
  mutate(NumberLetter_Median_RT_Switch_Cost_withoutOutliers = NumberLetter_Median_RT_Switch_withoutOutliers - (NumberLetter_Median_RT_Num_withoutOutliers+NumberLetter_Median_RT_Let_withoutOutliers)/2)
# Merge the aggregated data frame and the NumberLetter data frame
MiyakeRevisited_DataAggr <- merge(MiyakeRevisited_DataAggr, NumLet_aggr, by = "ParticipantNr", all = TRUE)

#-------------------------------------------------------------------------------
# Remove all participant outliers from all tasks from the original data frame (20 in total)
MiyakeRevisited_DataAggr3 <- MiyakeRevisited_DataAggr %>%
  filter(!(ParticipantNr %in% participant_outliers))

#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------
# Make a list of all relevant measures
Data_List3 <- list(MiyakeRevisited_DataAggr3$Antisac_Mean_ERR,
                   MiyakeRevisited_DataAggr3$GoNoGo_Mean_ERR_NoGo,
                   MiyakeRevisited_DataAggr3$Stroop_Median_RT_C_withoutOutliers, MiyakeRevisited_DataAggr3$Stroop_Median_RT_I_withoutOutliers, MiyakeRevisited_DataAggr3$Stroop_Median_RT_StroopEffect_withoutOutliers,
                   MiyakeRevisited_DataAggr3$nBack_Mean_ERR_Hit,
                   MiyakeRevisited_DataAggr3$KeepTrack_ERR,
                   MiyakeRevisited_DataAggr3$LetMem_ERR,
                   MiyakeRevisited_DataAggr3$PlusMinus_Time_Add, MiyakeRevisited_DataAggr3$PlusMinus_Time_Subtr, MiyakeRevisited_DataAggr3$PlusMinus_Time_Switch, MiyakeRevisited_DataAggr3$PlusMinus_Time_Switch_Cost,
                   MiyakeRevisited_DataAggr3$NumberLetter_Median_RT_Num_withoutOutliers, MiyakeRevisited_DataAggr3$NumberLetter_Median_RT_Let_withoutOutliers, MiyakeRevisited_DataAggr3$NumberLetter_Median_RT_Switch_withoutOutliers, MiyakeRevisited_DataAggr3$NumberLetter_Median_RT_Switch_Cost_withoutOutliers,
                   MiyakeRevisited_DataAggr3$LocalGlobal_Median_RT_No_Switch_withoutOutliers, MiyakeRevisited_DataAggr3$LocalGlobal_Median_RT_Switch_withoutOutliers, MiyakeRevisited_DataAggr3$LocalGlobal_Median_RT_Switch_Cost_withoutOutliers
)
# Make empty list to append all output to
meansData3 <- c()
mediansData3 <- c()
sdsData3 <- c()
rangesData3 <- c()

# Compute mean, SD and range of all measures in Data_List
for (i in Data_List3){
  meanOutput3 <- mean(i, na.rm = T)
  meansData3 <- append(meansData3, meanOutput3)
  sdOutput3 <- sd(i, na.rm = T)
  sdsData3 <- append(sdsData3, sdOutput3)
  rangeOutput3 <- range(i, na.rm = T)
  rangesData3 <- append(rangesData3, rangeOutput3)
}

# Round decimals
meansData3 <- round(meansData3, 2)
sdsData3 <- round(sdsData3, 2)
rangesData3 <- round(rangesData3, 2)

#-------------------------------------------------------------------------------
# Skewness & kurtosis
#-------------------------------------------------------------------------------

# Make empty list to append all output to
skewnessData3 <- c()
kurtosisData3 <- c()
ShapiroData3 <- c()

# Compute mean, SD and range of each dependent measure
for (i in Data_List3){
  skewnessOutput3 <- skew(i, na.rm = T)
  skewnessData3 <- append(skewnessData3, skewnessOutput3)
  kurtosisOutput3 <- kurtosi(i, na.rm = T)
  kurtosisData3 <- append(kurtosisData3, kurtosisOutput3)
  ShapiroOutput3 <- shapiro.test(i)
  ShapiroData3 <- append(ShapiroData3, ShapiroOutput3)
}

# Round decimals
skewnessData3 <- round(skewnessData3, 2)
kurtosisData3 <- round(kurtosisData3, 2)

#-------------------------------------------------------------------------------
# Test for multivariate normality
#-------------------------------------------------------------------------------
# Select 9 dependent variables from all the variables data frame
dependent_var <- MiyakeRevisited_DataAggr3 %>% 
  select(Antisac_Mean_ERR, GoNoGo_Mean_ERR_NoGo, Stroop_Median_RT_StroopEffect_withoutOutliers,
         LocalGlobal_Median_RT_Switch_Cost_withoutOutliers,PlusMinus_Time_Switch_Cost,NumberLetter_Median_RT_Switch_Cost_withoutOutliers,
         KeepTrack_ERR,LetMem_ERR, nBack_Mean_ERR_Hit)

# Testing voor multivariate normality
result3 <- mvn(dependent_var, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# 4. Transformations
# Data is still not multivariate normally distributed; see line 634
# Therefore, transformations ared needed
#-------------------------------------------------------------------------------
# Select 9 dependent measures + the participant identifier from the data frame
dependent_var_participant <-  MiyakeRevisited_DataAggr3 %>% 
  select(ParticipantNr, Antisac_Mean_ERR, GoNoGo_Mean_ERR_NoGo, Stroop_Median_RT_StroopEffect_withoutOutliers,
         KeepTrack_ERR, LetMem_ERR, nBack_Mean_ERR_Hit,
         PlusMinus_Time_Switch_Cost, NumberLetter_Median_RT_Switch_Cost_withoutOutliers, LocalGlobal_Median_RT_Switch_Cost_withoutOutliers)
# Transform each dependent measure; use the transformation that works best
# Note that a constant value was added to the values of the dependent measures 
# (depending on the minimum value) for the square root and cube root transformations,
# as these transformations are only with positive values.
# See also below to compare all different transformations that were used and compared
data_transformed <- transform(dependent_var_participant, Antisac_Mean_ERR = asin(sqrt(Antisac_Mean_ERR)),
                              GoNoGo_Mean_ERR_NoGo = asin(sqrt(GoNoGo_Mean_ERR_NoGo)),
                              Stroop_Median_RT_StroopEffect_withoutOutliers = sqrt(Stroop_Median_RT_StroopEffect_withoutOutliers +221),
                              KeepTrack_ERR = asin(sqrt(KeepTrack_ERR)),
                              LetMem_ERR = asin(sqrt(LetMem_ERR)),
                              nBack_Mean_ERR_Hit = asin(sqrt(nBack_Mean_ERR_Hit)),
                              NumberLetter_Median_RT_Switch_Cost_withoutOutliers = (NumberLetter_Median_RT_Switch_Cost_withoutOutliers +15)^(1/3)
)

#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------
# Make a list of all relevant variables
Data_List4 <- list(data_transformed$Antisac_Mean_ERR,
                   data_transformed$GoNoGo_Mean_ERR_NoGo,
                   data_transformed$Stroop_Median_RT_StroopEffect_withoutOutliers,
                   data_transformed$LocalGlobal_Median_RT_Switch_Cost_withoutOutliers,
                   data_transformed$NumberLetter_Median_RT_Switch_Cost_withoutOutliers,
                   data_transformed$PlusMinus_Time_Switch_Cost,
                   data_transformed$KeepTrack_ERR,
                   data_transformed$LetMem_ERR,
                   data_transformed$nBack_Mean_ERR_Hit
                   
)

# Make empty list to append all output to
meansData4 <- c()
mediansData4 <- c()
sdsData4 <- c()
rangesData4 <- c()

# Compute geometric mean, SD and range of each manifest variable
for (i in Data_List4){
  meanOutput4 <- mean(i, na.rm = T)
  meansData4 <- append(meansData4, meanOutput4)
  sdOutput4 <- sd(i, na.rm = T)
  sdsData4 <- append(sdsData4, sdOutput4)
  rangeOutput4 <- range(i, na.rm = T)
  rangesData4 <- append(rangesData4, rangeOutput4)
}

# Round decimals
meansData4 <- round(meansData4, 2)
sdsData4 <- round(sdsData4, 2)
rangesData4 <- round(rangesData4, 2)

#-------------------------------------------------------------------------------
# Skewness & kurtosis
#-------------------------------------------------------------------------------

# Make empty list to append all output to
skewnessData4 <- c()
kurtosisData4 <- c()
ShapiroData4 <- c()

# Compute geometric mean, SD and range of each manifest variable
for (i in Data_List4){
  skewnessOutput4 <- skew(i, na.rm = T)
  skewnessData4 <- append(skewnessData4, skewnessOutput4)
  kurtosisOutput4 <- kurtosi(i, na.rm = T)
  kurtosisData4 <- append(kurtosisData4, kurtosisOutput4)
  ShapiroOutput4 <- shapiro.test(i)
  ShapiroData4 <- append(ShapiroData4, ShapiroOutput4)
}

# Round decimals
skewnessData4 <- round(skewnessData4, 2)
kurtosisData4 <- round(kurtosisData4, 2)

#-------------------------------------------------------------------------------
# Multivariate normality
#-------------------------------------------------------------------------------
# Remove ParticipantNr from data frame to have an object with the 9 dependent measures
data_transformed2 <- data_transformed %>% 
  select(-ParticipantNr)                      
# Testing voor multivariate normality
result4 <- mvn(data_transformed2, mvnTest = 'mardia', univariateTest = 'SW', multivariatePlot = "qq")

#-------------------------------------------------------------------------------
# 5. Remove multivariate outliers
#-------------------------------------------------------------------------------

# Check again the multivariate outliers after the transformations have been performed
plot1 <- mvn(data_transformed2, multivariatePlot = 'qq',multivariateOutlierMethod = 'quan')
plot2 <- mvn(data_transformed2, multivariatePlot = 'qq',multivariateOutlierMethod = 'adj')

# Compute Mahalanobis distance to see which observations are not multivariate normally distributed
mahal_dist <- mahalanobis(data_transformed2, colMeans(data_transformed2), cov(data_transformed2))
# Add the Mahalanobis distance to the original dataframe
data_transformed$mahal_dist <- mahal_dist
# Sort the dataframe by the Mahalanobis distance in descending order
sorted_data_transformed <- data_transformed[order(-data_transformed$mahal_dist), ]
# Now check on the plot where the cut-off will be made
# The numbers on the graph show the row numbers of the outlying observations
# Remove multivariate outliers according to the adjusted quantile method (n=14)
multivariate_outliers <- c(271,97,140,122,284,201,29,173,91,123,164,73,269,65)
multivariate_outliers <- sort(multivariate_outliers)
multivariate_outliers <- unique(multivariate_outliers)
# Remove participant outliers (14 in total)
data_final <- data_transformed %>%
  filter(!(ParticipantNr %in% multivariate_outliers))

#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------
# Make a list of all relevant variables
Data_List5 <- list(data_final$Antisac_Mean_ERR,
                   data_final$GoNoGo_Mean_ERR_NoGo,
                   data_final$Stroop_Median_RT_StroopEffect_withoutOutliers,
                   data_final$LocalGlobal_Median_RT_Switch_Cost_withoutOutliers,
                   data_final$NumberLetter_Median_RT_Switch_Cost_withoutOutliers,
                   data_final$PlusMinus_Time_Switch_Cost,
                   data_final$KeepTrack_ERR,
                   data_final$LetMem_ERR,
                   data_final$nBack_Mean_ERR_Hit
                   
)

# Make empty list to append all output to
meansData5 <- c()
mediansData5 <- c()
sdsData5 <- c()
rangesData5 <- c()

# Compute geometric mean, SD and range of each manifest variable
for (i in Data_List5){
  meanOutput5 <- mean(i, na.rm = T)
  meansData5 <- append(meansData5, meanOutput5)
  sdOutput5 <- sd(i, na.rm = T)
  sdsData5 <- append(sdsData5, sdOutput5)
  rangeOutput5 <- range(i, na.rm = T)
  rangesData5 <- append(rangesData5, rangeOutput5)
}

# Round decimals
meansData5 <- round(meansData5, 2)
sdsData5 <- round(sdsData5, 2)
rangesData5 <- round(rangesData5, 2)

#-------------------------------------------------------------------------------
# Skewness & kurtosis
#-------------------------------------------------------------------------------

# Make empty list to append all output to
skewnessData5 <- c()
kurtosisData5 <- c()
ShapiroData5 <- c()

# Compute  mean, SD and range of each manifest variable
for (i in Data_List5){
  skewnessOutput5 <- skew(i, na.rm = T)
  skewnessData5 <- append(skewnessData5, skewnessOutput5)
  kurtosisOutput5 <- kurtosi(i, na.rm = T)
  kurtosisData5 <- append(kurtosisData5, kurtosisOutput5)
  ShapiroOutput5 <- shapiro.test(i)
  ShapiroData5 <- append(ShapiroData5, ShapiroOutput5)
}

# Round decimals
skewnessData5 <- round(skewnessData5, 2)
kurtosisData5 <- round(kurtosisData5, 2)

#-------------------------------------------------------------------------------
# Test for multivariate normality
#-------------------------------------------------------------------------------
# Remove ParticipantNr from dataframe
data_final2 <- data_final %>% 
  select(Antisac_Mean_ERR, GoNoGo_Mean_ERR_NoGo, Stroop_Median_RT_StroopEffect_withoutOutliers,
         LocalGlobal_Median_RT_Switch_Cost_withoutOutliers,PlusMinus_Time_Switch_Cost, NumberLetter_Median_RT_Switch_Cost_withoutOutliers,
         KeepTrack_ERR, LetMem_ERR, nBack_Mean_ERR_Hit)

# Testing voor multivariate normality
result5 <- mvn(data_final2, mvnTest = 'mardia', univariateTest = 'SW',  multivariatePlot = "qq")

# Multivariate outliers: check with Chi Square QQ plots
plot4 <- mvn(data_final2, multivariatePlot = 'qq',multivariateOutlierMethod = 'quan')
plot5 <- mvn(data_final2, multivariatePlot = 'qq',multivariateOutlierMethod = 'adj')
plot6 <- mvn(data_final2, multivariatePlot = 'qq')

par(mar = c(5, 8, 4, 2)) 
par(cex.axis = 2, cex.lab = 2)  # Adjust the value as needed for your desired font size

#-------------------------------------------------------------------------------
# Confirmatory Factor Analysis
#-------------------------------------------------------------------------------

## One-factor model
# Build the 1 factor model
Model_1factor <- 'CogntiveControl =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect_withoutOutliers +
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + PlusMinus_Time_Switch_Cost + NumberLetter_Median_RT_Switch_Cost_withoutOutliers +
                   KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit'
# Fit the model
fit_1factor <- cfa(Model_1factor , data = data_final, std.lv = TRUE)
# Make the model visual
semPaths(fit_1factor, 'Model_1factor',  'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_1factor, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit?
fitMeasures(fit_1factor)

## Three-factor model
# Build the 3 factor model 
Model_3factor <- 'Inhibition =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect_withoutOutliers
                  Switching =~ LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + PlusMinus_Time_Switch_Cost + NumberLetter_Median_RT_Switch_Cost_withoutOutliers
                  Updating =~ KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                  '
# Fit the model
fit_3factor <- cfa(Model_3factor, data = data_final, std.lv = TRUE)
# Make the model visual
semPaths(fit_3factor, 'Model_3factor', 'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_3factor, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit
fitMeasures(fit_3factor)

## Bi-factor model
# Build a 2-factor higher order model
Model_Bifactor <- ' Switching =~ LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + PlusMinus_Time_Switch_Cost + NumberLetter_Median_RT_Switch_Cost_withoutOutliers
                   Updating =~ KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                   
                   CC =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect_withoutOutliers +
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + PlusMinus_Time_Switch_Cost + NumberLetter_Median_RT_Switch_Cost_withoutOutliers +
                   KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                   
                   
                   CC ~~ 0 * Switching
                   CC ~~ 0 * Updating
                   Switching ~~ 0 * Updating
                  '

# Fit the model
fit_Bifactor <- cfa(Model_Bifactor, data = data_final, std.lv = TRUE)
# Make the model visual
semPaths(fit_Bifactor, 'Model_Bifactor', 'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_Bifactor, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit?
fitMeasures(fit_Bifactor)

# Build a 2-factor higher order model with a variance constraint for the Keep Track task
Model_Bifactor_constraint <- ' Switching =~ LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + PlusMinus_Time_Switch_Cost + NumberLetter_Median_RT_Switch_Cost_withoutOutliers
                   Updating =~ KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                   
                   CC =~ Antisac_Mean_ERR + GoNoGo_Mean_ERR_NoGo + Stroop_Median_RT_StroopEffect_withoutOutliers +
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + PlusMinus_Time_Switch_Cost + NumberLetter_Median_RT_Switch_Cost_withoutOutliers +
                   KeepTrack_ERR + LetMem_ERR + nBack_Mean_ERR_Hit
                   
                   
                   CC ~~ 0 * Switching
                   CC ~~ 0 * Updating
                   Switching ~~ 0 * Updating
                   
                   KeepTrack_ERR ~~ 0*KeepTrack_ERR
                   '
# Fit the model
fit_Bifactor_constraint <- cfa(Model_Bifactor_constraint, data = data_final, std.lv = TRUE)
# Make the model visual
semPaths(fit_Bifactor_constraint, 'Model_Bifactor_constraint', 'stand', bg = 'white', style =  'LISREL', color = '#52BDEC', rotation = 1, edge.color = '#2F4D5D', edge.label.cex =3, mar = c(8,0.75, 8, 0.75))
# Are all relations significant?
summary(fit_Bifactor_constraint, fit.measures = T, standardized = T, rsq = T)
# Does the model have a good fit?
fitMeasures(fit_Bifactor_constraint)


# Compare 1-factor & 3-factor model using ANOVA
anova(fit_1factor, fit_3factor)
# Compare 1-factor & bi-factor model using ANOVA
anova(fit_1factor, fit_Bifactor_constraint)
# Compare 3-factor & bi-factor model using ANOVA
anova(fit_3factor, fit_Bifactor_constraint)

#-------------------------------------------------------------------------------
# Correlation matrix
#-------------------------------------------------------------------------------
# Compute a correlation matrix for all these variables
data_final2.rcorr <- rcorr(as.matrix(data_final2))
# Store the estimates & p-values in a new object
data_final2.coeff <- data_final2.rcorr$r
data_final2.p <- data_final2.rcorr$P
# Round the number of decimals to 2
data_final2.coeff <- round(data_final2.coeff, 2)
data_final2.p <- round(data_final2.p, 3)

#-------------------------------------------------------------------------------
# Reaction Time task
#-------------------------------------------------------------------------------
# Combine all participants that are not included in the final sample
pp_not_in_sample <- c(# Antisaccade outliers
                      32,43,57,72,117,142,143,150,
                      # Go No-Go outliers
                      10,72,174,
                      # Stroop outliers
                      55,183,191,259,268,
                      # Local-Global outliers
                      18,58,268,
                      # Plus-Minus outliers
                      58,171,183,266,268,
                      # Number-Letter outliers
                      172,198,
                      # Keep Track outliers
                      7,72,
                      # Letter Memory outliers
                      10,34,72,
                      # N-back outliers
                      10,57,72,183,
                      # Multivariate outliers
                      271,97,140,122,284,201,29,173,91,123,164,73,269,65)
# Sort the outliers numerically
pp_not_in_sample <- sort(pp_not_in_sample)
# Only keep unique participant numbers
pp_not_in_sample <- unique(pp_not_in_sample)

# Filter these participant out to compute reaction time on Reaction Time Task
ReactionTimeData <- MiyakeRevisited_DataAggr %>%
  filter(!(ParticipantNr %in% pp_not_in_sample)) %>% 
  select(ParticipantNr,RT_Median_RT, RT_SD_RT)
# Compute descriptive statistics
mean(ReactionTimeData$RT_Median_RT)
sd(ReactionTimeData$RT_Median_RT)

#-------------------------------------------------------------------------------
# NASA-task Load Index  
#-------------------------------------------------------------------------------

# Filter these participant out to compute reaction time on the NASA Task
NASAData <- MiyakeRevisited_DataAggr %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Remove empty rows
NASAData <- NASAData[1:289,]
# Make a list of all relevant NASA measures
NASA_List <- list(NASAData$NASA_Antisaccade_Performance,
                  NASAData$NASA_Antisaccade_Effort,
                  NASAData$NASA_GoNoGo_Performance,
                  NASAData$NASA_GoNoGo_Effort,
                  NASAData$NASA_Stroop_Performance,
                  NASAData$NASA_Stroop_Effort,
                  NASAData$NASA_LocalGlobal_Performance,
                  NASAData$NASA_LocalGlobal_Effort,
                  NASAData$NASA_PlusMinus_Performance,
                  NASAData$NASA_PlusMinus_Effort,
                  NASAData$NASA_NumberLetter_Performance,
                  NASAData$NASA_NumberLetter_Effort,
                  NASAData$NASA_KeepTrack_Performance,
                  NASAData$NASA_KeepTrack_Effort,
                  NASAData$NASA_LetterMemory_Performance,
                  NASAData$NASA_LetterMemory_Effort,
                  NASAData$NASA_NBack_Performance,
                  NASAData$NASA_NBack_Effort)

# Make empty list to append all output to
NASA_Mean <- c()
NASA_SD <- c()
# Compute geometric mean, SD and range of each manifest variable
for (i in NASA_List){
  NASA_Mean_Output <- mean(i, na.rm = T)
  NASA_Mean <- append(NASA_Mean, NASA_Mean_Output)
  NASA_SD_Output <- sd(i, na.rm = T)
  NASA_SD <- append(NASA_SD, NASA_SD_Output)
}
# Round decimals
NASA_Mean <- round(NASA_Mean, 2)
NASA_SD <- round(NASA_SD, 2)

#-------------------------------------------------------------------------------
# Demographic questionnaire
#-------------------------------------------------------------------------------

# Filter these participant out to compute reaction time on the NASA Task
DemographicData <- MiyakeRevisited_DataAggr %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Remove empty rows & only keep columns with the demographic data
DemographicData <- DemographicData[1:247,c(1,227:239)]

# Age
mean(DemographicData$Age)
sd(DemographicData$Age)
range(DemographicData$Age)

# Biological sex
table(DemographicData$BiologicalSex)

# Sleep Normal
mean(DemographicData$SleepNormal)
sd(DemographicData$SleepNormal)

# Sleep Today
mean(DemographicData$SleepToday)
sd(DemographicData$SleepToday)

# Coffee Normal
mean(DemographicData$CoffeeNormal)
sd(DemographicData$CoffeeNormal)

# Coffee Today
mean(DemographicData$CoffeeToday)
sd(DemographicData$CoffeeToday)

# Alcohol Normal
mean(DemographicData$AlcoholNormal)
sd(DemographicData$AlcoholNormal)

# Alcohol Today
mean(DemographicData$AlcoholToday)
sd(DemographicData$AlcoholToday)

# Colorblind
table(DemographicData$ColorBlind)

# Diseases
table(DemographicData$Diseases)

# Medication
table(DemographicData$Medication)

#-------------------------------------------------------------------------------
# Raven Standard Progressive Matrices
#-------------------------------------------------------------------------------
# Only keep participants in the final sample
RavenData <- MiyakeRevisited_DataAggr %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Select the relevant columns
RavenData <- RavenData[1:247, c(1,259,260)]

# Raw score
mean(RavenData$Raven_RawScore)
sd(RavenData$Raven_RawScore)

# Percentile score
mean(RavenData$Raven_PercentileScore)
sd(RavenData$Raven_PercentileScore)

#-------------------------------------------------------------------------------
# Split-half reliability
#-------------------------------------------------------------------------------

# 1. Antisaccade task
# Import raw data file
Antisaccade <- read_csv("1. Antisaccade_RawData.csv")
# Remove participant & multivariate outliers
Antisaccade <- Antisaccade %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Compute split-half reliability
Reliability_Antisaccade <- splithalf(data = Antisaccade, outcome = "accuracy",
                                     score = "average",
                                     permutations = 10000,
                                     halftype = "random",
                                     var.ACC = "ERR",
                                     var.participant = "ParticipantNr",
                                     average = "mean")

# 2. Go No-Go task
# Import raw data file
GoNoGo <- read_csv("2. GoNoGo_RawData.csv")
# Remove participant & multivariate outliers
GoNoGo <- GoNoGo %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Compute split-half reliability
# We compute a separate reliability coefficient for "Go" and for "NoGo" trials"
Reliability_GoNoGo <- splithalf(data = GoNoGo, outcome = "accuracy",
                                score = "average",
                                permutations = 10000,
                                var.ACC = "ERR",
                                var.participant = "ParticipantNr",
                                average = "mean",
                                halftype = "random",
                                conditionlist = c("Go", "NoGo"),
                                var.condition = "target")

# 3. Stroop task
# Import raw data file
Stroop <- read_csv("3. Stroop_RawData.csv")
# Remove participant & multivariate outliers
Stroop <- Stroop %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Only keep trials that are correctly solved
Stroop <- Stroop %>% 
  filter(ACC == 1)
# Compute for each participant the individual mean and SD and put it into a new data frame
Stroop_means_sds <- Stroop %>%
  group_by(ParticipantNr)%>%
  summarise(ind_mean_RT = mean(RT_1000),
            ind_sd_RT = sd(RT_1000)
  )
# Merge the Stroop and Stroop_means_sds
Stroop <- merge(Stroop,Stroop_means_sds,sort=FALSE)
# Compute upper and lower limit of each participant
# Compute lower & upper limit of each individual participant by determining values that are 3x the SD below & above individual mean
Stroop <- Stroop %>% 
  mutate(LowerLimit = ind_mean_RT - 3* ind_sd_RT,
         UpperLimit = ind_mean_RT + 3* ind_sd_RT)
# Identify trial outliers that are outside the +/- 3 SD range
trial_outliers_Stroop <- Stroop %>% 
  filter(RT_1000 < LowerLimit | RT_1000 > UpperLimit)
# Remove these trial outliers that are 3 SD away from the individual mean
# Convert the RT to NA if the trial is an outlier
for (i in 1:nrow(Stroop)){
  if (Stroop$RT_1000[i] < Stroop$LowerLimit[i] | Stroop$RT_1000[i] > Stroop$UpperLimit[i] ){
    Stroop$RT_1000[i] = NA
  }
}
# Listwise deletion: Remove the trial outliers
Stroop <- na.omit(Stroop)
# Compute split-half reliability
# We compute the reliability coefficient based on the difference between congruent (C) and incongruent (I) trials
Reliability_Stroop <- splithalf(data = Stroop,
                                outcome = "RT",
                                score= "difference",
                                permutations = 10000,
                                halftype = "random",
                                var.RT = "RT_1000",
                                var.participant = "ParticipantNr",
                                average = "median",
                                var.compare = "Congruency",
                                compare1 = "C",
                                compare2 = "I")

# 4. Local-Global task
# Import raw data file
LocalGlobal <-  read_csv("4. LocalGlobal_RawData.csv")
# Remove participant & multivariate outliers
LocalGlobal <- LocalGlobal %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Only keep trials that are correctly solved
LocalGlobal <- LocalGlobal %>% 
  filter(ACC == 1)
# Compute for each participant the individual mean and SD and put it into a new data frame
LocalGlobal_means_sds <- LocalGlobal %>%
  group_by(ParticipantNr)%>%
  summarise(ind_mean_RT = mean(RT_1000),
            ind_sd_RT = sd(RT_1000)
  )
# Merge the LocalGlobal and the LocalGlobal_means_sds
LocalGlobal <- merge(LocalGlobal,LocalGlobal_means_sds,sort=FALSE)
# Compute lower & upper limit of each individual participant by determining values that are 3x the SD below & above individual mean
LocalGlobal <- LocalGlobal %>% 
  mutate(LowerLimit = ind_mean_RT - 3* ind_sd_RT,
         UpperLimit = ind_mean_RT + 3* ind_sd_RT)
# Identify trial outliers that are outside the +/- 3 SD range: 302 trials
trial_outliers_LocalGlobal <- LocalGlobal %>% 
  filter(RT_1000 < LowerLimit | RT_1000 > UpperLimit)
# Remove these trial outliers that are 3 SD away from the individual mean
# Convert the RT to NA if the trial is an outlier
for (i in 1:nrow(LocalGlobal)){
  if (LocalGlobal$RT_1000[i] < LocalGlobal$LowerLimit[i] | LocalGlobal$RT_1000[i] > LocalGlobal$UpperLimit[i] ){
    LocalGlobal$RT_1000[i] = NA
  }
}
# Listwise deletion: Remove the trial outliers
LocalGlobal <- na.omit(LocalGlobal)
# Compute split-half reliability
# We compute the reliability coefficient based on the difference between no-switch (0) and switch (1) trials
Reliability_LocalGlobal <- splithalf(data = LocalGlobal,
                                     outcome = "RT",
                                     score= "difference",
                                     permutations = 10000,
                                     halftype = "random",
                                     var.RT = "RT_1000",
                                     var.participant = "ParticipantNr",
                                     average = "median",
                                     var.compare = "Switch",
                                     compare1 = "0",
                                     compare2 = "1")

# 6. Number-Letter task
# Import raw data file
NumberLetter <- read_csv("6. NumberLetter_RawData.csv")
# Remove participant & multivariate outliers
NumberLetter <- NumberLetter %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# We need to create a new variable that indicates whether the trial was a no-switch trial in the Number or Letter blocks or a switch trial in the mix block
# Create a variable with no values that will indicate this
NumberLetter <- add_column(NumberLetter, Detect_Local_Switch = NA)
# Assign a value for each row
for (i in 1:nrow(NumberLetter)){
  if (NumberLetter$Block[i] == 'NUM'){
    NumberLetter$Detect_Local_Switch[i] = 'Num'
  }
  if (NumberLetter$Block[i] == 'LET'){
    NumberLetter$Detect_Local_Switch[i] = 'Let'
  }
  if (NumberLetter$Switch[i] == 1){
    NumberLetter$Detect_Local_Switch[i] = 'Switch'
  }
  if (NumberLetter$Switch[i] == 0 & NumberLetter$Block[i] == 'MIX'){
    NumberLetter$Detect_Local_Switch[i] = 'Not necessary'
  }
}
# Delete not necessary trials: i.e., no-switch trials in the mix block
NumberLetter <- NumberLetter %>% 
  filter(Detect_Local_Switch != 'Not necessary')
# Only keep trials that are correctly solved
NumberLetter <- NumberLetter %>% 
  filter(ACC == 1)
# Compute for each participant the individual mean and SD and put it into a new data frame
NumberLetter_means_sds <- NumberLetter %>%
  group_by(ParticipantNr)%>%
  summarise(ind_mean_RT = mean(RT_1000),
            ind_sd_RT = sd(RT_1000)
  )
# Merge the NumberLetter and NumberLetter_means_sds
NumberLetter <- merge(NumberLetter,NumberLetter_means_sds,sort=FALSE)
# Compute lower & upper limit of each individual participant by determining values that are 3x the SD below & above individual mean
NumberLetter <- NumberLetter %>% 
  mutate(LowerLimit = ind_mean_RT - 3* ind_sd_RT,
         UpperLimit = ind_mean_RT + 3* ind_sd_RT)
# Identify trial outliers that are outside the +/- 3 SD range: 859 trials
trial_outliers_NumberLetter <- NumberLetter %>% 
  filter(RT_1000 < LowerLimit | RT_1000 > UpperLimit)
# Remove these trial outliers that are 3 SD away from the individual mean
# Convert the RT to NA if the trial is an outlier
for (i in 1:nrow(NumberLetter)){
  if (NumberLetter$RT_1000[i] < NumberLetter$LowerLimit[i] | NumberLetter$RT_1000[i] > NumberLetter$UpperLimit[i] ){
    NumberLetter$RT_1000[i] = NA
  }
}
# Listwise deletion: Remove the trial outliers
NumberLetter <- na.omit(NumberLetter)
# Compute split-half reliability
# We compute the reliability coefficient based on the difference between the trials in the Number and Letter blocks (0) and switch trials in the switch block (1)
Reliability_NumberLetter <- splithalf(data = NumberLetter,
                                      outcome = "RT",
                                      score= "difference",
                                      permutations = 10000,
                                      halftype = "random",
                                      var.RT = "RT_1000",
                                      var.participant = "ParticipantNr",
                                      average = "median",
                                      var.compare = "Switch",
                                      compare1 = "0",
                                      compare2 = "1")


# 8. Letter-Memory task
# Import raw data file
LetterMemory <- read_csv("8. LetterMemory_RawData.csv")
# Remove participant & multivariate outliers
LetterMemory <- LetterMemory %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Remove trials with only 4 letters
LetterMemory <- LetterMemory %>% 
  filter(!nLetters == 4)
# Compute ERR percentage
LetterMemory <- LetterMemory %>% 
  mutate(ERR_pct = ERR/4)
# Compute split-half reliability
Reliability_LetterMemory <- splithalf(data = LetterMemory, outcome = "accuracy",
                                      score = "average",
                                      permutations = 10000,
                                      halftype = "random",
                                      var.ACC = "ERR_pct",
                                      var.participant = "ParticipantNr",
                                      average = "mean")

# 9. N-back task
# Import raw data file
NBack <- read_csv("9. NBack_RawData.csv")
# Remove participant & multivariate outliers
NBack <- NBack %>%
  filter(!(ParticipantNr %in% pp_not_in_sample))
# Compute split-half reliability
# We compute a separate reliability coefficient for "No Hit" (0) and for "Hit" (1) trials"
Reliability_NBack <- splithalf(data = NBack, outcome = "accuracy",
                               score = "average",
                               permutations = 10000,
                               var.ACC = "ERR",
                               var.participant = "ParticipantNr",
                               average = "mean",
                               halftype = "random",
                               conditionlist = c("0", "1"),
                               var.condition = "Hit"
)

#-------------------------------------------------------------------------------
# Other transformations
#-------------------------------------------------------------------------------

# Select 9 dependent measures + the participant identifier from the data frame
# Note: we use the MiyakeRevisited_DataAggr3 data frame that was created earlier
# This data frame already has removed the participant and trial outliers
# Transformations were peformed in a next step
dependent_var_participant <- MiyakeRevisited_DataAggr3 %>% 
  select(ParticipantNr, Antisac_Mean_ERR, GoNoGo_Mean_ERR_NoGo, Stroop_Median_RT_StroopEffect_withoutOutliers,
         LocalGlobal_Median_RT_Switch_Cost_withoutOutliers,PlusMinus_Time_Switch_Cost, NumberLetter_Median_RT_Switch_Cost_withoutOutliers,
         KeepTrack_ERR, LetMem_ERR, nBack_Mean_ERR_Hit)
# Remove ParticipantNr from dataframe
dependent_var <- dependent_var_participant %>% 
  select(-ParticipantNr)   
#-------------------------------------------------------------------------------
# Arc sine transformation on ERR
#-------------------------------------------------------------------------------
data2 <- transform(dependent_var, Antisac_Mean_ERR = asin(sqrt(Antisac_Mean_ERR)),
                   GoNoGo_Mean_ERR_NoGo = asin(sqrt(GoNoGo_Mean_ERR_NoGo)),
                   KeepTrack_ERR = asin(sqrt(KeepTrack_ERR)),
                   LetMem_ERR = asin(sqrt(LetMem_ERR)),
                   nBack_Mean_ERR_Hit = asin(sqrt(nBack_Mean_ERR_Hit)))
# Note that only ERR outcomes measures can be used for this transformation
# Testing voor multivariate normality
result2 <- mvn(data2, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Natural logarithm
#-------------------------------------------------------------------------------
# Note that a constant value was added to the values of the dependent measures
# As it is only possible to use a logarithm transformations with positive values
# The constant values were chosen based on the minimum value of the measure
data3 <- transform(dependent_var, 
                   Antisac_Mean_ERR = log(Antisac_Mean_ERR + 1),
                   GoNoGo_Mean_ERR_NoGo = log(GoNoGo_Mean_ERR_NoGo +1),
                   Stroop_Median_RT_StroopEffect_withoutOutliers = log(Stroop_Median_RT_StroopEffect_withoutOutliers +221),
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = log(LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + 411),
                   PlusMinus_Time_Switch_Cost = log(PlusMinus_Time_Switch_Cost +57),
                   NumberLetter_Median_RT_Switch_Cost_withoutOutliers = log(NumberLetter_Median_RT_Switch_Cost_withoutOutliers +15),
                   KeepTrack_ERR = log(KeepTrack_ERR +1),
                   LetMem_ERR = log(LetMem_ERR +1),
                   nBack_Mean_ERR_Hit = log(nBack_Mean_ERR_Hit +1)
)
# Testing voor multivariate normality
result3 <- mvn(data3, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Square
#-------------------------------------------------------------------------------
# As it is only possible to use a square transformation with positive values
# The constant values were chosen based on the minimum value of the measure
data4 <- transform(dependent_var, 
                   Antisac_Mean_ERR = (Antisac_Mean_ERR)^2,
                   GoNoGo_Mean_ERR_NoGo = (GoNoGo_Mean_ERR_NoGo)^2,
                   Stroop_Median_RT_StroopEffect_withoutOutliers = (Stroop_Median_RT_StroopEffect_withoutOutliers +221)^2,
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = (LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + 411)^2,
                   PlusMinus_Time_Switch_Cost = (PlusMinus_Time_Switch_Cost +57)^2,
                   NumberLetter_Median_RT_Switch_Cost_withoutOutliers = (NumberLetter_Median_RT_Switch_Cost_withoutOutliers +15)^2,
                   KeepTrack_ERR = (KeepTrack_ERR)^2,
                   LetMem_ERR = (LetMem_ERR)^2,
                   nBack_Mean_ERR_Hit = (nBack_Mean_ERR_Hit)^2)
# Testing voor multivariate normality
result4 <- mvn(data4, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Square root
#-------------------------------------------------------------------------------
# As it is only possible to use a square root transformation with positive values
# The constant values were chosen based on the minimum value of the measure
data5 <- transform(dependent_var, 
                   Antisac_Mean_ERR = sqrt(Antisac_Mean_ERR),
                   GoNoGo_Mean_ERR_NoGo = sqrt(GoNoGo_Mean_ERR_NoGo),
                   Stroop_Median_RT_StroopEffect_withoutOutliers = sqrt(Stroop_Median_RT_StroopEffect_withoutOutliers +221),
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = sqrt(LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + 411),
                   PlusMinus_Time_Switch_Cost = sqrt(PlusMinus_Time_Switch_Cost +57),
                   NumberLetter_Median_RT_Switch_Cost_withoutOutliers = sqrt(NumberLetter_Median_RT_Switch_Cost_withoutOutliers +15),
                   KeepTrack_ERR = sqrt(KeepTrack_ERR),
                   LetMem_ERR = sqrt(LetMem_ERR),
                   nBack_Mean_ERR_Hit = sqrt(nBack_Mean_ERR_Hit))
# Testing voor multivariate normality
result5 <- mvn(data5, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Cube root
#-------------------------------------------------------------------------------
# As it is only possible to use a cube root transformation with positive values
# The constant values were chosen based on the minimum value of the measure
data6 <- transform(dependent_var, 
                   Antisac_Mean_ERR = (Antisac_Mean_ERR)^(1/3),
                   GoNoGo_Mean_ERR_NoGo = (GoNoGo_Mean_ERR_NoGo)^(1/3),
                   Stroop_Median_RT_StroopEffect_withoutOutliers = (Stroop_Median_RT_StroopEffect_withoutOutliers +221)^(1/3),
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = (LocalGlobal_Median_RT_Switch_Cost_withoutOutliers + 411)^(1/3),
                   PlusMinus_Time_Switch_Cost = (PlusMinus_Time_Switch_Cost +57)^(1/3),
                   NumberLetter_Median_RT_Switch_Cost_withoutOutliers = (NumberLetter_Median_RT_Switch_Cost_withoutOutliers +15)^(1/3),
                   KeepTrack_ERR = (KeepTrack_ERR)^(1/3),
                   LetMem_ERR = (LetMem_ERR)^(1/3),
                   nBack_Mean_ERR_Hit = (nBack_Mean_ERR_Hit)^(1/3))
# Testing voor multivariate normality
result6 <- mvn(data6, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Inverse
#-------------------------------------------------------------------------------
data7 <- transform(dependent_var, 
                   Antisac_Mean_ERR = 1/(Antisac_Mean_ERR + 1),
                   GoNoGo_Mean_ERR_NoGo = 1/(GoNoGo_Mean_ERR_NoGo +1),
                   Stroop_Median_RT_StroopEffect_withoutOutliers = 1/(Stroop_Median_RT_StroopEffect_withoutOutliers),
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = 1/(LocalGlobal_Median_RT_Switch_Cost_withoutOutliers),
                   PlusMinus_Time_Switch_Cost = 1/(PlusMinus_Time_Switch_Cost),
                   NumberLetter_Median_RT_Switch_Cost_withoutOutliers = 1/(NumberLetter_Median_RT_Switch_Cost_withoutOutliers),
                   KeepTrack_ERR = 1/(KeepTrack_ERR +1),
                   LetMem_ERR = 1/(LetMem_ERR +1),
                   nBack_Mean_ERR_Hit = 1/(nBack_Mean_ERR_Hit +1))
# Testing voor multivariate normality
result7 <- mvn(data7, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Yeo-Johnson transformation
#-------------------------------------------------------------------------------
data8 <- transform(dependent_var, 
                   Antisac_Mean_ERR = yjPower(Antisac_Mean_ERR, lambda = 2, jacobian.adjusted = F),
                   GoNoGo_Mean_ERR_NoGo = yjPower(GoNoGo_Mean_ERR_NoGo, lambda = 2, jacobian.adjusted = F),
                   Stroop_Median_RT_StroopEffect_withoutOutliers = yjPower(Stroop_Median_RT_StroopEffect_withoutOutliers, lambda = 2, jacobian.adjusted = F),
                   LocalGlobal_Median_RT_Switch_Cost_withoutOutliers = yjPower(LocalGlobal_Median_RT_Switch_Cost_withoutOutliers, lambda = 2, jacobian.adjusted = F),
                   PlusMinus_Time_Switch_Cost = yjPower(PlusMinus_Time_Switch_Cost, lambda = 2, jacobian.adjusted = F),
                   NumberLetter_Median_RT_Switch_Cost_withoutOutliers = yjPower(NumberLetter_Median_RT_Switch_Cost_withoutOutliers , lambda = 2, jacobian.adjusted = F),
                   KeepTrack_ERR = yjPower(KeepTrack_ERR , lambda = 2, jacobian.adjusted = F),
                   LetMem_ERR = yjPower(LetMem_ERR , lambda = 2, jacobian.adjusted = F),
                   nBack_Mean_ERR_Hit = yjPower(nBack_Mean_ERR_Hit , lambda = 2, jacobian.adjusted = F))
# Testing voor multivariate normality
result8 <- mvn(data8, mvnTest = 'mardia', univariateTest = 'SW')

#-------------------------------------------------------------------------------
# Logit transformation for ERR measures
#-------------------------------------------------------------------------------
data9 <- transform(dependent_var, 
                   Antisac_Mean_ERR = logit(Antisac_Mean_ERR),
                   GoNoGo_Mean_ERR_NoGo = logit(GoNoGo_Mean_ERR_NoGo),
                   KeepTrack_ERR = logit(KeepTrack_ERR),
                   LetMem_ERR = logit(LetMem_ERR),
                   nBack_Mean_ERR_Hit = logit(nBack_Mean_ERR_Hit)
)
# Note that only ERR outcomes measures can be used for this transformation
# Testing voor multivariate normality
result9 <- mvn(data9, mvnTest = 'mardia', univariateTest = 'SW')