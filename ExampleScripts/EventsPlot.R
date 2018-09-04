ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis("optionA") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis("optionB") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis("optionC") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis("optionD") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="B") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="C") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="D") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme_bw()
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme_bw(plot.background = element_rect(fill="black"))
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme(plot.background = element_rect(fill="black"))
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme(panel.background = element_rect(fill="black"))
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_tile() + scale_fill_viridis(option="A") + theme(panel.background = element_rect(fill="black"))
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_tile() + scale_fill_viridis(option="A"
)
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme(panel.background = element_rect(fill="black"))
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme(panel.background = element_rect(fill="black"), legend.position = "bottom")
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_bin2d(binwidth = c(0.25, 500)) + scale_fill_viridis(option="A") + theme(panel.background = element_rect(fill="black"), legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rep(1:360, times = 1, each = 60)
rep(1:360, times = 1, each = 60) %>% tbl_df
rm(list = ls())
cohortData <- function(pathToFiles, fileType, cohortInfo) {
filePattern <- stringr::str_c("_", fileType, ".csv")
fileList <- list.files(path = pathToFiles, pattern = filePattern)
for (fileName in fileList) {
if (!exists("outputTbl")) {
startDateInfo <- fileName
fileToLoad <- stringr::str_c(pathToFiles, fileName, sep = "/")
outputTbl <- readr::read_csv(fileToLoad, col_type = cols()) %>%
tibble::add_column(metaData = fileName) %>%
tibble::add_column(startDate = startDateInfo)
}
if (exists("outputTbl")) {
fileToLoad <- stringr::str_c(pathToFiles, fileName, sep = "/")
tmp <- readr::read_csv(fileToLoad, col_type = cols()) %>%
tibble::add_column(metaData = fileName) %>%
tibble::add_column(startDate = startDateInfo)
outputTbl <- rbind(outputTbl, tmp)
rm(tmp)
}
}
outputTbl <- outputTbl %>% tibble::add_column(cohortInfo)
outputTbl$startDate <- outputTbl$startDate %>%
str_replace("_.*", "")
outputTbl <- outputTbl %>%
separate(metaData, c("date", "experiment", "regimen", "group", "subjectID", "eventType"), sep = "_")
outputTbl$eventType <- outputTbl$eventType %>%
str_replace(".csv", "")
outputTbl$date <- as.Date(outputTbl$date)
outputTbl$startDate <- as.Date(outputTbl$startDate)
return(outputTbl)
}
getwd()
setwd("./Dropbox (Partners HealthCare)/Suman/Data/IVSA/Male-Female/")
cohortData("./Cohort01/_csvFiles", "rewards", "01")
cohortData <- function(pathToFiles, fileType, cohortInfo) {
filePattern <- stringr::str_c("_", fileType, ".csv")
fileList <- list.files(path = pathToFiles, pattern = filePattern)
for (fileName in fileList) {
if (!exists("outputTbl")) {
startDateInfo <- fileName
fileToLoad <- stringr::str_c(pathToFiles, fileName, sep = "/")
outputTbl <- readr::read_csv(fileToLoad, col_type = cols()) %>%
tibble::add_column(metaData = fileName) %>%
tibble::add_column(startDate = startDateInfo)
}
if (exists("outputTbl")) {
fileToLoad <- stringr::str_c(pathToFiles, fileName, sep = "/")
tmp <- readr::read_csv(fileToLoad, col_type = cols()) %>%
tibble::add_column(metaData = fileName) %>%
tibble::add_column(startDate = startDateInfo)
outputTbl <- rbind(outputTbl, tmp)
rm(tmp)
}
}
outputTbl <- outputTbl %>% tibble::add_column(cohortInfo)
outputTbl$startDate <- outputTbl$startDate %>%
str_replace("_.*", "")
outputTbl <- outputTbl %>%
separate(metaData, c("date", "experiment", "regimen", "group", "subjectID", "eventType"), sep = "_")
outputTbl$eventType <- outputTbl$eventType %>%
str_replace(".csv", "")
outputTbl$date <- as.Date(outputTbl$date)
outputTbl$startDate <- as.Date(outputTbl$startDate)
outputTbl$experiment <- as.factor(outputTbl$experiment)
outputTbl$regimen <- as.factor(outputTbl$regimen)
outputTbl$group <- as.factor(outputTbl$group)
outputTbl$subjectID <- as.factor(outputTbl$subjectID)
outputTbl$eventType <- as.factor(outputTbl$eventType)
return(outputTbl)
}
cohortData("./Cohort01/_csvFiles", "rewards", "01")
rewards <- cohortData("./Cohort01/_csvFiles", "rewards", "01")
str(rewards)
cohortData <- function(pathToFiles, fileType, cohortInfo) {
filePattern <- stringr::str_c("_", fileType, ".csv")
fileList <- list.files(path = pathToFiles, pattern = filePattern)
for (fileName in fileList) {
if (!exists("outputTbl")) {
startDateInfo <- fileName
fileToLoad <- stringr::str_c(pathToFiles, fileName, sep = "/")
outputTbl <- readr::read_csv(fileToLoad, col_type = cols()) %>%
tibble::add_column(metaData = fileName) %>%
tibble::add_column(startDate = startDateInfo)
}
if (exists("outputTbl")) {
fileToLoad <- stringr::str_c(pathToFiles, fileName, sep = "/")
tmp <- readr::read_csv(fileToLoad, col_type = cols()) %>%
tibble::add_column(metaData = fileName) %>%
tibble::add_column(startDate = startDateInfo)
outputTbl <- rbind(outputTbl, tmp)
rm(tmp)
}
}
outputTbl <- outputTbl %>% tibble::add_column(cohortInfo)
outputTbl$startDate <- outputTbl$startDate %>%
str_replace("_.*", "")
outputTbl <- outputTbl %>%
separate(metaData, c("date", "experiment", "regimen", "group", "subjectID", "eventType"), sep = "_")
outputTbl$eventType <- outputTbl$eventType %>%
str_replace(".csv", "")
outputTbl$date <- as.Date(outputTbl$date)
outputTbl$startDate <- as.Date(outputTbl$startDate)
outputTbl$experiment <- as.factor(outputTbl$experiment)
outputTbl$regimen <- as.factor(outputTbl$regimen)
outputTbl$group <- as.factor(outputTbl$group)
outputTbl$subjectID <- as.factor(outputTbl$subjectID)
outputTbl$eventType <- as.factor(outputTbl$eventType)
outputTbl$cohortInfo <- as.factor(outputTbl$cohortInfo)
return(outputTbl)
}
rewards <- cohortData("./Cohort01/_csvFiles", "rewards", "01")
str(rewards)
corrLevers <- cohortData("./Cohort01/_csvFiles", "corrLever", "01")
incorrLevers <- cohortData("./Cohort01/_csvFiles", "incorrLever", "01") %>% print
rewards <- rewards %>% filter(regime != "3H")
rewards <- rewards %>% filter(regimen != "3H")
corrLevers <- corrLevers %>% filter(regimen != "3H")
incorrLevers <- incorrLevers %>% filter(regimen != "3H")
rewards
start <- c("2012-08-21", "2012-09-01", "2012-08-15", "2012-09-18")
end <- c("2012-09-16", "2012-09-06", "2012-08-22", "2012-10-11")
elapsed.time <- start %--% end
as.duration(elapsed.time)
as.duration(elapsed.time) / ddays(1)
rewards %>% mutate(day = as.duration(startDate %--% date)) %>% print
rewards %>% mutate(day = as.duration(startDate %--% date)) / dday(1) %>% print
rewards %>% mutate(day = as.duration(startDate %--% date)) / ddays(1) %>% print
rewards %>% mutate(day = as.duration(startDate %--% date) / ddays(1)) %>% print
rewards <- rewards %>% mutate(day = as.duration(startDate %--% date) / ddays(1)) %>% print
rewards <- rewards %>% select(-day)
rewards
rewards <- rewards %>% mutate(day = as.duration(startDate %--% date) / ddays(1)) %>% print
str(rewards)
str(as.factor(rewards$day))
str(as.factor(rewards$day + 1))
rewards <- rewards %>% select(-day)
rewards <- rewards %>% mutate(day = (as.duration(startDate %--% date) / ddays(1)) + 1) %>% print
rewards %>% select(day, regimen, group) %>% print
rewards %>% select(day, regimen, group, eventTime) %>% print
rewards %>% select(day, regimen, group, eventTime) %>% print %>% group_by(day, regimen, group, eventTime) %>% count()
rewardFreq <- rewards %>% select(day, regimen, group, eventTime) %>% group_by(day, regimen, group, eventTime) %>% count()
rewardFreq_ShA <- rewardFreq %>% filter(regimen == "1H")
rewardFreq_LgA <- rewardFreq %>% filter(regimen == "3H")
rewardFreq_LgA <- rewardFreq %>% filter(regimen == "6H")
str(rewardFreq)
rm(rewardFreq_.*)
rm(rewardFreq_*.)
rm("rewardFreq_*.")
rm("rewardFreq_.*")
rm(rewardFreq_LgA)
rm(rewardFreq_ShA)
rewardFreq$day %>% max()
rep(1:33)
rep(1:2160, rep = 33)
rep(1:2160, times = 33)
rewardFreq
rewardFreq %>% sort(-desc(eventTime))
rewardFreq %>% arrange(-desc(eventTime))
rewardFreq %>% group_by(regimen, group) %>% arrange(-desc(eventTime))
rewardFreq %>% group_by(day, group) %>% arrange(-desc(eventTime))
rewardFreq %>% group_by(day, group) %>% arrange(-desc(day, eventTime))
rewardFreq %>% group_by(day, group) %>% arrange(-desc(c(day, eventTime)))
rewardFreq %>% group_by(day, group) %>% arrange(-desc(day)) %>% arrange(-desc(eventTime))
rewardFreq
rewardFreq %>% arrange(-desc(day))
rewardFreq %>% arrange(-desc(day)) %>% arrange(-desc(eventTime))
rewardFreq %>% arrange(-desc(day, eventTime))
rewardFreq %>% arrange(-desc(c(day, eventTime))
)
rewardFreq %>% arrange(-desc(c("day", "eventTime")))
rewardFreq %>% group_by(day) %>% arrange(eventTime)
rewardFreq_Females <- rewardFreq %>% filter(group == "FEMALES") %>% select(-c(regimen, group))
rewardFreq %>% ungroup()
rewardFreq <- rewardFreq %>% ungroup()
rewardFreq_Females <- rewardFreq %>% filter(group == "FEMALES") %>% select(-c(regimen, group))
View(rewardFreq_Females)
rewardFreq_Females
rewardFreq_Females %>% group_by(day) %>% arrange(eventTime)
dayCol <- tbl_df(rep(1:33, times = 2160))
dayCol
dayCol <- tbl_df(rep(1:33, count = 2160))
dayCol
dayCol <- tbl_df(rep(1:33, each = 2160, times = 33))
dayCol
rep(1:33, each = 2160) %>% as.factor()
dayCol <- tbl_df(rep(1:33, each = 2160))
dayCol
View(dayCol)
dayCol$value %>% rename(day)
dayCol$day <- dayCol$value
dayCol
dayCol$value %>% del
dayCol <- select(day)
dayCol <- dayCol %>% select(day)
dayCol
rep(1:2160, times = 33)
dayCol$eventTime <- rep(1:2160, times = 33)
dayCol
dayCol$n <- rep(0, times = 71280)
dayCol
rewardFreq_Females
dayCol$n <- rep(0, times = 71280) %>% as.integer
dayCol
rewardFreq_Females$n <- rewardFreq_Females$n %>% as.double
rewardFreq_Females
dayCol <- dayCol %>% as.integer
dayCol <- dayCol %>% map(as.integer)
dayCol
dayCol <- dayCol %>% tbl_df
dayCol
rewardFreq_Females
dayCol <- dayCol %>% map(as.double) %>% tbl_df
dayCol
dayCol <- dayCol %>% select(-n)
dayCol
left_join(dayCol, rewardFreq_Females)
tmp <- left_join(dayCol, rewardFreq_Females)
View(tmp)
tmp[is.na(tmp)] <- 0
tmp
View(tmp)
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis() +
theme_bw()
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme_bw()
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme(panel.background = element_rect(fill = "black"))
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme(panel.background = element_rect(fill = "black", panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme_bw()
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C", direction = -1) +
theme_bw()
ggplot(data = rewardFreq_Females, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "D", direction = -1) +
theme_bw()
rewardFreq_Females
rewardFreq_Females$eventTime_min <- rep(1:360, count = 60)
rewardFreq_Females$eventTime_min <- rep(1:360, times = 60)
ggplot(data = tmp, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "D", direction = -1) +
theme_bw()
ggplot(data = tmp, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "D") +
theme_bw()
ggplot(data = tmp, aes(x = eventTime, y = day, fill = n)) +
geom_tile() +
scale_fill_viridis(option = "C") +
theme_bw()
rep(1:360, count = 60)
rep(1:360, times = 60)
rep(1:360, each = 60)
rep(1:360, each = 60, times = 33)
tmp$eventTime_min <- rep(1:360, each = 60, times = 33)
rep(1:2, each = 2, times = 3)
View(tmp)
rep(1:360, each = 60, times = 33)
rep(1:360, each = 60)
rep(1:360, each = 6)
dayCol$eventTime <- rep(1:21600, times = 33)
rm(dayCol)
rm(tmp)
rewardFreq_Females
ggplot(rewardFreq_Females, aes(eventTime, day, fill = n)) + scale_fill_viridis(direction = -1)
ggplot(rewardFreq_Females, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(direction = -1)
ggplot(rewardFreq_Females, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(direction = -1) + theme_minimal()
ggplot(rewardFreq_Females, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis() + theme_minimal()
rewardFreq_Females
rewardFreq
rewardFreq_Males <- rewardFreq %>% filter(group == "MALES") %>% select(-c(regimen group))
rewardFreq_Males <- rewardFreq %>% filter(group == "MALES") %>% select(-c(regimen group))
rewardFreq_Males <- rewardFreq %>% filter(group == "MALES")
rewardFreq_Males <- rewardFreq %>% filter(group == "MALES") %>% select(-c(regimen group))
rewardFreq_Males <- rewardFreq %>% filter(group == "MALES") %>% select(-c(regimen, group))
ggplot(rewardFreq_Males, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis() + theme_minimal()
ggplot(rewardFreq_Males, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "C", direction = -1) +
theme(panel.background = "black")
ggplot(rewardFreq_Males, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "C", direction = -1) +
theme(panel.background = element_rect("black"))
ggplot(rewardFreq_Males, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "C") +
theme(panel.background = element_rect("black"))
ggplot(rewardFreq_Males, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "C") +
theme(panel.background = element_rect("black"),
panel.grid = element_blank())
rewardFreq_Females
plotData.females$day <- rep(1:21600, times = 33)
plotData_females <- rep(1:21600, times = 33)
plotData_females
plotData_females <- rep(1:21600, times = 33) %>% tbl_df
plotData_females
plotData_females$eventTime <- rep(1:21600, times = 33) %>% tbl_df
rm(plotData_females)
plotData_females$eventTime <- rep(1:21600, times = 33) %>% tbl_df
plotData_females <- NULL
plotData_females$eventTime <- rep(1:21600, times = 33) %>% tbl_df
plotData_females
rm(plotData_females)
rewardsFreq_Female s
rewardFreq_Males
rep(1:33, count = 21600)
rep(1:33, times = 21600)
rep(1:33, count = 21600)
rep(1:33, each = 21600)
plotData.females <- tbl_df(day = rep(1:33, each = 21600), eventTime = rep(1:21600, times = 33)))
plotData.females <- tbl_df(day = rep(1:33, each = 21600), eventTime = rep(1:21600, times = 33))
data.Day <- rep(1:33, each = 21600) %>% tbl_df
data.Day
data.Day %>% rename(value, day)
data.Day %>% rename(value = day)
data.Day$day <- data.Day$value
data.Day
data.Day <- data.Day %>% select(-value)
data.Day
data.Day$eventTime <- rep(1:21600, times = 33)
data.Day
left_join(data.Day, rewardFreq_Females)
plot.Data <- left_join(data.Day, rewardFreq_Females)
plot.Data %>% rename(n = eventFreq)
rename(plot.Data, n = eventFreq)
plot.Data[is.na] <- 0
plot.Data[is.na(plot.Data)] <- 0
plot.Data
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis() + theme(
panel.background = element_rect("black"),
panel.grid = element_blank()
)
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "C") + theme(
panel.background = element_rect("black"),
panel.grid = element_blank()
)
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "A") + theme(
panel.background = element_rect("black"),
panel.grid = element_blank()
)
plot.Data <- left_join(data.Day, rewardFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() + scale_fill_viridis(option = "A") + theme(
panel.background = element_rect("black"),
panel.grid = element_blank()
)
View(rewards)
View(rewardFreq)
corrLeverFreq <- corrLevers %>% select(day, regimen, group, eventTime) %>% count()
corrrLeverFreq <- corrLevers %>% select(day, regiment, group, eventTime) %>% group_by(day, regimen, group, eventTime) %>% count()
corrrLeverFreq <- corrLevers %>% select(day, regimen, group, eventTime) %>% group_by(day, regimen, group, eventTime) %>% count()
View(corrLevers)
corrLevers <- corrLevers %>% mutate(day = (as.duration(startDate %--% date) / ddays(1)) + 1)
corrrLeverFreq <- corrLevers %>% select(day, regimen, group, eventTime) %>% group_by(day, regimen, group, eventTime) %>% count()
View(rewardFreq)
corrLeverFreq <- corrLevers %>% select(day, regimen, group, eventTime) %>% group_by(day, regimen, group, eventTime) %>% count()
rm(corrrLeverFreq)
corrLeverFreq_Females <- corrLeverFreq %>% filter(group == "FEMALES") %>% select(-c(regimen, group))
View(corrLeverFreq_Females)
corrLeverFreq_Females <- corrLeverFreq_Females %>% select(-c(regimen, group))
corrLeverFreq_Females <- corrLeverFreq_Females %>% ungroup() %>% select(-c(regimen, group))
corrLeverFreq_Males <- corrLeverFreq %>% select(group == "MALES") %>% ungroup() %>% select(-c(regimen, group))
corrLeverFreq
corrLeverFreq_Males <- corrLeverFreq %>% select(group == "MALES")
str(corrLeverFreq)
corrLeverFreq_Males <- corrLeverFreq %>% filter(group == "MALES") %>% ungroup() %>% select(-c(regimen, group))
plot.Data <- left_join(data.Day, corrLeverFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
ggplot(plot.Data, aes(eventTime, day, fill = n) + geom_tile() + scale_fill_viridis(option = "A") +
theme(
panel.background = element_rect("black"),
panel.grid = element_blank()
)
)
plot.Data
plot.Data <- left_join(data.Day, rewardFreq_Males)
plot.Data
plot.Data[is.na(plot.Data)] <- 0
plot.Data
ggplot(plot.Data, aes(eventTime, day, fill = n) + geom_tile() + scale_fill_viridis(option = "A") +
theme(
panel.background = element_rect("black"),
panel.grid = element_blank()
))
plot.Data
setwd()
setwd("~")
getwd()
q()
ggplot(plot.Data, aes(eventTime, day, fill = n) + geom_tile() + scale_fill_viridis(option = "A")
)
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() +scale_fill_viridis(option = "A")
library(tidyverse)
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() +scale_fill_viridis(option = "A")
library(viridis)
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() +scale_fill_viridis(option = "A")
plot.Data <- left_join(data.Day, corrLeverFreq_Females)
plot.Data[is.na(plot.Data)] <- 0
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() +scale_fill_viridis(option = "A")
plot.Data <- left_join(data.Day, corrLeverFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() +scale_fill_viridis(option = "A")
data.Day
rep(1:360, count = 60, times = 33)
rep(1:360, each = 60, times = 33)
data.Day$minBin <- rep(1:360, each = 60, times = 33)
data.Day
data.Day <- data.Day %>% select(-minBin)
data.Day
plot.Data <- left_join(data.Day, rewardFreq_Females)
plot.Data
plot.Data[is.na(plot.Data)] <- 0
plot.Data
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data
plot.Data %>% group_by(day, eventTime) %>% count()
View(plot.Data)
plot.Data %>% group_by(day, eventTime, n) %>% count()
plot.Data %>% group_by(day, eventTime, n) %>% count() %>% filter(n == 0)
plot.Data %>% group_by(day, eventTime, n) %>% count() %>% filter(n != 0)
plot.Data %>% group_by(day, eventTime) %>% summarise()
plot.Data %>% group_by(day, eventTime) %>% summarise(n)
plot.Data %>% group_by(day, eventTime) %>% summarise(sum())
plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = n)) + geom_tile() +scale_fill_viridis(option = "A")
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A")
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, rewardFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
View(plot.Data)
plot.Data <- left_join(data.Day, rewardFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, corrLeverFreq_Females)
plot.Data[is.na(plot.Data)] <- 0
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, corrLeverFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, corrLeverFreq_Males)
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data[is.na(plot.Data)] <- 0
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_tile() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster() +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster(interpolate = TRUE) +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, corrLeverFreq_Females)
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data[is.na(plot.Data)] <- 0
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster(interpolate = TRUE) +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, rewardFreq_Females)
plot.Data[is.na(plot.Data)] <- 0
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster(interpolate = TRUE) +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, rewardFreq_Males)
plot.Data[is.na(plot.Data)] <- 0
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster(interpolate = TRUE) +scale_fill_viridis(option = "A") + theme(panel.background = element_blank(), panel.grid = element_blank())
plot.Data <- left_join(data.Day, rewardFreq_Females)
plot.Data[is.na(plot.Data)] <- 0
plot.Data$eventTime <- rep(1:360, each = 60, times = 33)
plot.Data <- plot.Data %>% group_by(day, eventTime) %>% summarise(eventFreq = sum(n))
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster(interpolate = TRUE) +scale_fill_viridis(option = "D") + theme(panel.background = element_blank(), panel.grid = element_blank())
ggplot(plot.Data, aes(eventTime, day, fill = eventFreq)) + geom_raster() +scale_fill_viridis(option = "D") + theme(panel.background = element_blank(), panel.grid = element_blank())
getwd()
q()
savehistory("~/Untitled.Rhistory")
