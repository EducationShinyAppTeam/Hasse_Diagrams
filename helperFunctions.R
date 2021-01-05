library(stringr)

# Survey Link Function ----
linkSurvey <- function(repoName){
  link <- paste0(
    "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=",
    repoName
  )
  return(link)
}

# Reset Inputs Function ----
resetInputs <- function(session, defaults = NULL, textList = NULL,
                        numberList = NULL, switchList = NULL, checkList = NULL,
                        radioList = NULL, selectList = NULL){
  if (is.null(defaults)) {
    defaults <- list(
      "text" = "",
      "number" = 2,
      "switch" = FALSE,
      "check" = FALSE,
      "radio" = "Fixed",
      "select" = "Not nested"
    )
  }
  if (!is.null(textList)) {
    for (i in 1:length(textList)) {
      updateTextInput(
        session = session,
        inputId = textList[i],
        value = defaults$text
      )
    }
  }

  if (!is.null(numberList)) {
    for (i in 1:length(numberList)) {
      updateNumericInput(
        session = session,
        inputId = numberList[i],
        value = defaults$number
      )
    }
  }

  if (!is.null(switchList)) {
    for (i in 1:length(switchList)) {
      updateSwitchInput(
        session = session,
        inputId = switchList[i],
        value = defaults$switch
      )
    }
  }

  if (!is.null(checkList)) {
    for (i in 1:length(checkList)) {
      updateCheckboxInput(
        session = session,
        inputId = checkList[i],
        value = defaults$check
      )
    }
  }

  if (!is.null(radioList)) {
    for (i in 1:length(radioList)) {
      updateRadioButtons(
        session = session,
        inputId = radioList[i],
        selected = defaults$radio
      )
    }
  }

  if (!is.null(selectList)) {
    for (i in 1:length(selectList)) {
      updateSelectInput(
        session = session,
        inputId = selectList[i],
        selected = defaults$select
      )
    }
  }
}

# Get Input Names Function ----
getInputNames <- function(pattern, input = input){
  temp1 <- grep(
    pattern = pattern,
    x = names(input),
    value = TRUE
  )
  return(sort(temp1))
}

# Reduce Degrees of Freedom Function ----
reduceDF <- function(dataFrame){
  temp1 <- dataFrame
  temp2 <- sapply(X = dataFrame$labels, FUN = grepl, pattern = "\\(")
  for (i in 1:length(temp2)) {
    if (temp2[i]) {
      temp1$labels[i] <- gsub(
        pattern = "\\({1,}",
        replacement = "",
        x = temp1$labels[i]
      )
      temp1$labels[i] <- gsub(
        pattern = "\\){1,}",
        replacement = "",
        x = temp1$labels[i]
      )
    }
  }
  for (i in 1:(nrow(temp1) - 1)) {
    elements <- unlist(strsplit(x = temp1$labels[i], split = " \U00D7 ", fixed = TRUE))
    for (j in (i + 1):(nrow(temp1))) {
      if (all(stringr::str_detect(string = temp1$labels[j], pattern = elements))) {
        temp1$df[j] <- temp1$df[j] - temp1$df[i]
      }
    }
  }
  temp1$labels <- dataFrame$labels
  return(temp1)
}

# Make Ordering Matrix Function ----
makeOrderMatrix <- function(labelList, block = NULL, covariates = NULL){
  if (length(block) == 0) {block <- NULL}
  if (length(covariates) == 0) {covariates <- NULL}
  orderMat <- matrix(
    data = FALSE,
    nrow = length(labelList),
    ncol = length(labelList),
    dimnames = list(labelList, labelList)
  )
  temp1 <- labelList
  temp2 <- sapply(X = temp1, FUN = grepl, pattern = "\\(")
  for (i in 1:length(temp2)) {
    if (temp2[i]) {
      temp1[i] <- gsub(
        pattern = "\\({1,}",
        replacement = "",
        x = temp1[i]
      )
      temp1[i] <- gsub(
        pattern = "\\){1,}",
        replacement = "",
        x = temp1[i]
      )
    }
  }
  for (i in 1:length(labelList)) {
    if (grepl(pattern = "\\(Error\\)", x = labelList[i])) {
      next
    } else {
      elements <- unlist(strsplit(x = temp1[i], split = " \U00D7 ", fixed = TRUE))
      for (j in (i + 1):length(labelList)) {
        if (grepl(pattern = "Grand Mean", x = temp1[i], fixed = TRUE)) {
          orderMat[labelList[i], labelList[j]] <- TRUE
        } else if (
          all(
            any(labelList[j] != block, is.null(block), is.na(block)),
            any(!(labelList[j] %in% covariates), is.null(covariates), is.na(covariates)),
            all(stringr::str_detect(string = temp1[j], pattern = elements))
          )) {
          orderMat[labelList[i], labelList[j]] <- TRUE
        } else if (grepl(pattern = "\\(Error\\)", x = labelList[j])) {
          orderMat[labelList[i], labelList[j]] <- TRUE
        }
      }
    }
  }
  return(orderMat)
}