
PU <- read.csv("pastUse.csv", header = T)
df <- read.csv("pastUse.csv", header = T)


df <- df[ , -1]

library(dplyr)

#pre-processing / cleaning

df <- rename(df, isProvince = PROVINCE)  #change the name of the column

colRecode1 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 10, "Newfoundland and labrador", df[, i])
    df[, i] <<- ifelse(df[, i] == 11, "Prince Edward Island", df[, i])
    df[, i] <<- ifelse(df[, i] == 12, "Nova Scotia", df[, i])
    df[, i] <<- ifelse(df[, i] == 13, "New Brunswick", df[, i])
    df[, i] <<- ifelse(df[, i] == 24, "Quebec", df[, i])
    df[, i] <<- ifelse(df[, i] == 35, "Ontario", df[, i])
    df[, i] <<- ifelse(df[, i] == 46, "Manitoba", df[, i])
    df[, i] <<- ifelse(df[, i] == 47, "Saskatchewan", df[, i])
    df[, i] <<- ifelse(df[, i] == 48, "Alberta", df[, i])
    df[, i] <<- ifelse(df[, i] == 59, "British Columbia", df[, i])
  }
}

colRecode1 (1,1) 

df <- rename(df, isRegion = REGION)  #change the name of the column

colRecode2 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "Atlantic Region", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "Quebec", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "Ontario", df[, i])
    df[, i] <<- ifelse(df[, i] == 4, "Manitoba / Saskatchewan", df[, i])
    df[, i] <<- ifelse(df[, i] == 5, "Alberta", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "British Columbia", df[, i])
  }
}

colRecode2 (2,2) 

df <- rename(df, isCommunity = G_URBRUR)  #change the name of the column
#df <- mutate(df, totalEdu = G_CEDUC + G_CSTUD) 

colRecode3 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 01, "Montreal", df[, i])
    df[, i] <<- ifelse(df[, i] == 02, "Toronto", df[, i])
    df[, i] <<- ifelse(df[, i] == 03, "Vancouver", df[, i])
    df[, i] <<- ifelse(df[, i] == 04, "Other urbcan excluding Prince Edward Island", df[, i])
    df[, i] <<- ifelse(df[, i] == 05, "Rural excluding Prince Edward Island", df[, i])
    df[, i] <<- ifelse(df[, i] == 06, "Prince Edward Island", df[, i])
  }
}

colRecode3 (3,3) 

df <- rename(df, isAgeGroups = GCAGEGR6)  #change the name of the column
#df <- mutate(df, totalEdu = G_CEDUC + G_CSTUD) 

colRecode4 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 01, "16 to 24", df[, i])
    df[, i] <<- ifelse(df[, i] == 02, "25 to 34", df[, i])
    df[, i] <<- ifelse(df[, i] == 03, "35 to 44", df[, i])
    df[, i] <<- ifelse(df[, i] == 04, "45 to 54", df[, i])
    df[, i] <<- ifelse(df[, i] == 05, "55 to 64", df[, i])
    df[, i] <<- ifelse(df[, i] == 06, "64 and older", df[, i])
  }
}

colRecode4 (4,4) 

df <- rename(df, isGender = CSEX)  #change the name of the column
#df <- mutate(df, totalEdu = G_CEDUC + G_CSTUD) 

colRecode5 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "Male", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "Female", df[, i])
  }
}

colRecode5 (5,5) 

df <- rename(df,  isRespondentHighestEducationLevel = G_CEDUC)  #change the name of the column

colRecode6 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "High school or less", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "College or some post-secondary", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "University certificate or degree", df[, i])
  }
}

colRecode6 (6,6) 

df <- rename(df,  isStudentRespondent = G_CSTUD)  #change the name of the column

colRecode7 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "YES", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "NO", df[, i])
  }
}

colRecode7 (7,7) 

df <- rename(df,  isHouseholdHighestEduc = G_HEDUC)  #change the name of the column

df <- rename(df,  isStudentinHH = G_HSTUD)  #change the name of the column

colRecode12 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "YES", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "NO", df[, i])
  }
}

colRecode12 (12,12) 

df <- rename(df,  isEVPersonalUse = EV_Q01)  #change the name of the column

colRecode13 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "YES", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "NO", df[, i])
  }
}

colRecode13 (13,13) 

df <- rename(df,  isLabourForce = G_CLFSST)  #change the name of the column

colRecode8 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "Employed", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "Unemployed", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "Not in the labour force", df[, i])
  }
}

colRecode8 (8,8) 

df <- rename(df,  isHHType = GFAMTYPE)  #change the name of the column

colRecode9 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "Single family HH with unmarried children under 16", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "Single family HH without unmarried children under 16", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "One person HH", df[, i])
    df[, i] <<- ifelse(df[, i] == 4, "Multi family HH", df[, i])
  }
}

colRecode9 (9,9) 

df <- rename(df,  isNoofpersoninHH = G_HHSIZE)  #change the name of the column

colRecode11 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 4, "4 or more", df[, i])
  }
}

colRecode11 (11,11) 

df <- rename(df,  isInternetuseYears = EV_Q02)  #change the name of the column

colRecode14 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "Less than a year", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "1 to 2 years", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "2 to 5 years", df[, i])
    df[, i] <<- ifelse(df[, i] == 4, "4 or more years", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "Valid skip", df[, i])
    df[, i] <<- ifelse(df[, i] == 7, "Dont know", df[, i])
    df[, i] <<- ifelse(df[, i] == 8, "Refusal", df[, i])
  }
}

colRecode14 (14,14) 

df <- rename(df,  isPUPersonalUse = PU_Q01)  #change the name of the column

colRecode15 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "YES", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "NO", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "Valid Skip", df[, i])
  }
}

colRecode15 (15,15)

df <- rename(df,  isPULastTimeUsed = PU_Q02)  #change the name of the column

colRecode16 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "1 to 2 Years", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "2 to 5 years", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "5 or more years", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "Valid Skip", df[, i])
    df[, i] <<- ifelse(df[, i] == 7, "Dont Know", df[, i])
  }
}

colRecode16 (16,16)

df <- rename(df,  isPUHowOften = PU_Q03)  #change the name of the column

colRecode17 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "Atleast once a day", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "Atleast once a week", df[, i])
    df[, i] <<- ifelse(df[, i] == 3, "Atleast once a month", df[, i])
    df[, i] <<- ifelse(df[, i] == 4, "Less than once a month", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "Valid Skip", df[, i])
    df[, i] <<- ifelse(df[, i] == 7, "Dont Know", df[, i])
  }
}

colRecode17 (17,17)

df <- rename(df,  isTooCostly = PU_Q06A)  #change the name of the column
df <- rename(df,  isNoNeed = PU_Q06E)  #change the name of the column
df <- rename(df,  isNoTime = PU_Q06J)  #change the name of the column
df <- rename(df,  isNoComputerAccess = PU_Q06K)  #change the name of the column

colRecode18 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "YES", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "NO", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "Valid Skip", df[, i])
    df[, i] <<- ifelse(df[, i] == 7, "Dont Know", df[, i])
  }
}

colRecode18 (18,21)

df <- rename(df,  isOther = PU_G06)  #change the name of the column

colRecode22 <- function(startCol, endCol) {
  for (i in startCol:endCol) {
    df[, i] <<- ifelse(df[, i] == 1, "YES", df[, i])
    df[, i] <<- ifelse(df[, i] == 2, "NO", df[, i])
    df[, i] <<- ifelse(df[, i] == 6, "Valid Skip", df[, i])
    df[, i] <<- ifelse(df[, i] == 9, "Not Stated", df[, i])
  }
}

colRecode22 (22,22)

colR

library("ggplot2")
library("tidyverse")
library("dplyr")


#trimmed dataframes based on each column
trimmedHO <- filter(df, isPUHowOften!='Valid Skip' & isPUHowOften!='Dont Know')
trimmedLU <- filter(df, isPULastTimeUsed!='Valid Skip' & isPULastTimeUsed!='Dont Know')
trimmedRYU <- filter(df, isPUPersonalUse!='Valid Skip')

#checking to see if the data trimmed properly
table(df$isPUPersonalUse)
table(df$isPULastTimeUse)
table(df$isPUHowOften)

#frequency dataframes based on gender for each column
freqHO_gen <- as.data.frame(table(trimmedHO$isPUHowOften, trimmedHO$isGender))
freqHO_gen <- rename(freqHO_gen,  Internet_Use = Var1)
freqHO_gen <- rename(freqHO_gen,  Gender = Var2)
freqHO_gen <- rename(freqHO_gen,  Frequency_of_Respondents = Freq)
freqHO_gen


freqRYU_gen <- as.data.frame(table(trimmedRYU$isPUPersonalUse, trimmedRYU$isGender))
freqRYU_gen <- rename(freqRYU_gen,  Past_12_Month_Use = Var1)
freqRYU_gen <- rename(freqRYU_gen,  Gender = Var2)
freqRYU_gen <- rename(freqRYU_gen,  Frequency_of_Respondents = Freq)
freqRYU_gen

freqLU_gen <- as.data.frame(table(trimmedLU$isPULastTimeUsed, trimmedLU$isGender))
freqLU_gen <- rename(freqLU_gen,  Last_Internet_Use = Var1)
freqLU_gen <- rename(freqLU_gen,  Gender = Var2)
freqLU_gen <- rename(freqLU_gen,  Frequency_of_Respondents = Freq)
freqLU_gen

#plots
howoften_GEN <- ggplot(freqHO_gen, aes(x=Internet_Use, y=Frequency_of_Respondents, fill=Gender, label=Frequency_of_Respondents))+geom_bar(stat="identity")+geom_text(size=5, position=position_stack(vjust=0.5))+scale_fill_manual(values = c("Pink", "Lightblue"))+theme(panel.background = element_rect(fill = "#887DA7", color = "black"))+labs(title="Frequency of Internet Use By Gender")
howoften_GEN

lastuse_GEN <- ggplot(freqLU_gen, aes(x=Last_Internet_Use, y=Frequency_of_Respondents, fill=Gender, label=Frequency_of_Respondents))+geom_bar(stat="identity")+geom_text(size=5, position=position_stack(vjust=0.5))+scale_fill_manual(values = c("Pink", "Lightblue"))+theme(panel.background = element_rect(fill = "#7BA66E", color = "black"))+labs(title="Last Time Internet Use By Gender")
lastuse_GEN

recentyearuse_GEN <- ggplot(freqRYU_gen, aes(x=Past_12_Month_Use, y=Frequency_of_Respondents, fill=Gender, label=Frequency_of_Respondents))+geom_bar(stat="identity")+geom_text(size=5, position=position_stack(vjust=0.5))+scale_fill_manual(values = c("Pink", "Lightblue"))+theme(panel.background = element_rect(fill = "#887DA7", color = "black"))+labs(title="Past 12 Month Internet Use by Gender")
recentyearuse_GEN


#Gunjan

hist(PU$G_HSTUD,xlim = c(1,2),xaxt= "n", xlab= "Student Household Internet Use", yaxt= "n", main = "")
axis(1, at = 1:2, labels = c("Yes","NO") )

DataP <- aggregate(PU$EV_Q02, by=list(PU$G_HEDUC), FUN = sum )
print(DataP)
pie(DataP$x, DataP$Group.1,col = rainbow(length(DataP$x)),clockwise = TRUE, main = "Internet use by Education")
legend("topright", c("High school or less", "College or some post-secondary","University certificate or degree"), 
       cex = 0.8, fill = rainbow(length(DataP$x)))

library(epiDisplay)
tab1(df$isInternetuseYears, sort.group = "decreasing", cum.percent = TRUE, main = "Respondent Distribution of Years of Internet Use")


#jitter plots labor force by province and internet use by province
ggplot(df, aes(x = isLabourForce, fill = isHHType)) + geom_bar(position = "fill") + labs(y = "Proportion")
ggplot(df, aes(x = isNoofpersoninHH, y = isHHType, color = "black", size = isLabourForce)) + geom_point(alpha = .6) + labs(title = "Academic salary by rank, years of service, and years since degree")
mosaicplot(isStudentRespondent~isLabourForce,data=df,col=c("Light Blue","Light Yellow", "Pink"))



#Stacked bar chart showing provinces by age group
p2 <- ggplot(data = df, aes(x = isProvince, fill =isAgeGroups))
p2 <- p2 + geom_bar()
p2 <- p2 + theme_minimal()
p2 <- p2  + theme(axis.text.x = element_text(angle = 90,hjust = 0)) + labs(title="Age Group of Provinces")
p2

#Stacked bar chart showing Province by Gender
freqprov_gen <- as.data.frame(table(df$isProvince, df$isGender))
freqprov_gen <- rename(freqprov_gen,  Province = Var1)
freqprov_gen <- rename(freqprov_gen,  Gender = Var2)
freqprov_gen <- rename(freqprov_gen,  Frequency_of_Respondents = Freq)
freqprov_gen

p5 <- ggplot(freqprov_gen, aes(x=Province, y=Frequency_of_Respondents, fill=Gender, label=Frequency_of_Respondents))+geom_bar(stat="identity")+geom_text(size=2, position=position_stack(vjust=0.5))+scale_fill_manual(values = c("Pink", "Lightblue"))+labs(title="Gender Count by Province")+theme(axis.text.x = element_text(angle = 90,hjust =0 ))
p5



#Reasons for non-internet use barplot
trimmedcostly <- filter(df, isTooCostly!='Valid Skip' & isTooCostly!='Dont Know')
trimmednoneed <- filter(df, isNoNeed!='Valid Skip' & isNoNeed!='Dont Know')
trimmednotime <- filter(df, isNoTime!='Valid Skip' & isNoTime!='Dont Know')
trimmednocomputer <- filter(df, isNoComputerAccess!='Valid Skip' & isNoComputerAccess!='Dont Know')
trimmedother <- filter(df, isOther!='Valid Skip' & isOther!='Not Stated')

#frequency dataframes based on gender for each column

freqcostly_highed <- as.data.frame(table(trimmedcostly$isTooCostly, trimmedcostly$isRespondentHighestEducationLevel))
freqcostly_highed <- rename(freqcostly_highed,  Too_Costly = Var1)
freqcostly_highed <- rename(freqcostly_highed,  Respondent_Highest_Education = Var2)
freqcostly_highed <- rename(freqcostly_highed,  Frequency_of_Respondents = Freq)
freqcostly_highed

freqneed_highed <- as.data.frame(table(trimmednoneed$isNoNeed, trimmednoneed$isRespondentHighestEducationLevel))
freqneed_highed <- rename(freqneed_highed,  No_Need = Var1)
freqneed_highed <- rename(freqneed_highed,  Respondent_Highest_Education = Var2)
freqneed_highed <- rename(freqneed_highed,  Frequency_of_Respondents = Freq)
freqneed_highed

freqtime_highed <- as.data.frame(table(trimmednotime$isNoTime, trimmednotime$isRespondentHighestEducationLevel))
freqtime_highed <- rename(freqtime_highed,  No_Time = Var1)
freqtime_highed <- rename(freqtime_highed,  Respondent_Highest_Education = Var2)
freqtime_highed <- rename(freqtime_highed,  Frequency_of_Respondents = Freq)
freqtime_highed

freqcomp_highed <- as.data.frame(table(trimmednocomputer$isNoComputerAccess, trimmednocomputer$isRespondentHighestEducationLevel))
freqcomp_highed <- rename(freqcomp_highed,  No_Computer = Var1)
freqcomp_highed <- rename(freqcomp_highed,  Respondent_Highest_Education = Var2)
freqcomp_highed <- rename(freqcomp_highed,  Frequency_of_Respondents = Freq)
freqcomp_highed

freqother_highed <- as.data.frame(table(trimmedother$isOther, trimmedother$isRespondentHighestEducationLevel))
freqother_highed <- rename(freqother_highed,  Other_Reason = Var1)
freqother_highed <- rename(freqother_highed,  Respondent_Highest_Education = Var2)
freqother_highed <- rename(freqother_highed,  Frequency_of_Respondents = Freq)
freqother_highed

#Bar graph plots for the given variables.

costly_highed <- ggplot(freqcostly_highed, aes(x=Respondent_Highest_Education, y=Frequency_of_Respondents, fill=Too_Costly, label = Frequency_of_Respondents)) + geom_bar(stat="identity") + geom_col() +
  geom_text(size=5, position=position_stack(vjust=0.5))+
  scale_x_discrete(guide = guide_axis(n.dodge=4))+
  scale_fill_manual(values = c("coral1", "darkseagreen"))+
  theme(panel.background = element_rect(fill = "#FCFBCC", color = "black"))+
  labs(title="Reason: Too Costly?") 
costly_highed



need_highed <- ggplot(freqneed_highed, aes(x=Respondent_Highest_Education, y=Frequency_of_Respondents, fill=No_Need, label = Frequency_of_Respondents)) + geom_bar(stat="identity") + geom_col() +
  geom_text(size=5, position=position_stack(vjust=0.5))+
  scale_x_discrete(guide = guide_axis(n.dodge=4))+
  scale_fill_manual(values = c("coral1", "darkseagreen"))+
  theme(panel.background = element_rect(fill = "#FCFBCC", color = "black"))+
  labs(title="Reason: No Need?") 
need_highed

time_highed <- ggplot(freqtime_highed, aes(x=Respondent_Highest_Education, y=Frequency_of_Respondents, fill=No_Time, label = Frequency_of_Respondents)) + geom_bar(stat="identity") + geom_col() +
  geom_text(size=5, position=position_stack(vjust=0.5))+
  scale_x_discrete(guide = guide_axis(n.dodge=4))+
  scale_fill_manual(values = c("coral1", "darkseagreen"))+
  theme(panel.background = element_rect(fill = "#FCFBCC", color = "black"))+
  labs(title="Reason: No Time?") 
time_highed

comp_highed <- ggplot(freqcomp_highed, aes(x=Respondent_Highest_Education, y=Frequency_of_Respondents, fill=No_Computer, label = Frequency_of_Respondents)) + geom_bar(stat="identity") + geom_col() +
  geom_text(size=5, position=position_stack(vjust=0.5))+
  scale_x_discrete(guide = guide_axis(n.dodge=4))+
  scale_fill_manual(values = c("coral1", "darkseagreen"))+
  theme(panel.background = element_rect(fill = "#FCFBCC", color = "black"))+
  labs(title="Reason: No Computer?") 
comp_highed

other_highed <- ggplot(freqother_highed, aes(x=Respondent_Highest_Education, y=Frequency_of_Respondents, fill=Other_Reason, label = Frequency_of_Respondents)) + geom_bar(stat="identity") + geom_col() +
  geom_text(size=5, position=position_stack(vjust=0.5))+
  scale_x_discrete(guide = guide_axis(n.dodge=4))+
  scale_fill_manual(values = c("coral1", "darkseagreen"))+
  theme(panel.background = element_rect(fill = "#FCFBCC", color = "black"))+
  labs(title="Reason: Other") 
other_highed
