#' Second-resolution active lever presses and rewards
#'
#' \code{makeEventTimeDf} returns a second-resolution dataframe of active lever presses and reward dispensions
#'
#' For this function to work properly the input data format needs to be the following ...
#'
#' @param ... Event time dataframe in a format shown below
#' 
#' sessionNum |  cohort      | subjectID | eventType   | eventTime
#' -----------|--------------|-----------|-------------|-----------
#'           1|  ShA-Female  | SG7       | rewards     | ...
#' -----------|--------------|-----------|-------------|-----------
#'	     1|  ShA-Female  | SG7       | corrLever   | ...
#' -----------|--------------|-----------|-------------|-----------
#' ...
#' ...
#' 
#' @return ... A dataframe that contains columns that contain timestamps from 1 to 3600 secs 
#' 	and frequency of reward and active lever press during each of those timestamps
#'
#'
#' 
#' @examples ...
#' 	makeEventTimeDf(inputEventTimeData)
#' 
#' @author Suman Guha (github.com/sumankguha)

makeEventTimeDf <- function(inputEventTimeData) {
  nestedData_rewards <- 
    inputEventTimeData %>%
#   filter(subjectID %in% subjectList) %>%
#   select(-date, -infusionState, -regimen, -group) %>%
    filter(eventType == "rewards") %>%
    group_by(sessionNum, cohort, subjectID, eventType, eventTime) %>%
    summarize(rewardFreq = n()) %>%    
    arrange(sessionNum, cohort, subjectID) %>%    
    nest(data = c(eventTime, rewardFreq)) %>% 
    ungroup() %>%
    group_by(sessionNum, cohort, subjectID) %>%
    pivot_wider(names_from = eventType, values_from = data)
  
  nestedData_corrLever <- 
    inputEventTimeData %>%
#   filter(subjectID %in% subjectList) %>%
#   select(-date, -infusionState, -regimen, -group) %>%
    filter(eventType == "corrLever") %>%
    group_by(sessionNum, cohort, subjectID, eventType, eventTime) %>%    
    summarize(corrLeverFreq = n()) %>%    
    arrange(sessionNum, cohort, subjectID) %>%    
    nest(data = c(eventTime, corrLeverFreq)) %>% 
    pivot_wider(names_from = eventType, values_from = data)
  
  nestedData <- 
    left_join(nestedData_rewards, nestedData_corrLever, by = c("sessionNum", "cohort", "subjectID"))
  
  rm(nestedData_rewards, nestedData_corrLever)
  
  eventTimeMatrix <- 
    tibble(sessionNum = rep(1:23, each = 3600), 
           eventTime = rep(1:3600, 23)) %>% 
    nest(data = c(eventTime)) %>% 
    set_names(c("sessionNum", "eventTime"))

  nestedData <- 
    left_join(nestedData, eventTimeMatrix, by = "sessionNum")

  nestedData <- 
    nestedData %>%
    mutate(eventTimeMatrix = 
               map2(eventTime, rewards, ~ left_join(.x, .y, by = c("eventTime")))) %>% 
    select(-rewards, -eventTime)

  nestedData <- 
    nestedData %>% 
    mutate(eventTimeMatrix = 
              map2(eventTimeMatrix, corrLever, ~ left_join(.x, .y, by = c("eventTime")))) %>% 
    select(-corrLever)

  rm(eventTimeMatrix)
  
  nestedData <- 
    nestedData %>% 
    unnest(cols = c(eventTimeMatrix)) %>% 
    replace(is.na(.), 0)
  
  return(nestedData)
}
