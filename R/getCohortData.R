#'  Gather a particular data type (correct lever press, incorrect lever press, reward dispension) for an experimental group
#'
#'  \code{getCohortData} uses stringr, readr, tibble, and tidyr to collate the data from all days of an experiment

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
    stringr::str_replace("_.*", "")
  outputTbl <- outputTbl %>%
    tidyr::separate(metaData, c("date", "experiment", "regimen", "group", "subjectID", "eventType"), sep = "_")
  outputTbl$eventType <- outputTbl$eventType %>%
    stringr::str_replace(".csv", "")
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
