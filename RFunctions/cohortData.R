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
