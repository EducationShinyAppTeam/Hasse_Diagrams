# Helper Functions for the Hasse Diagrams Shiny App

# Add/Remove Parentheses Functions ----
removeParens <- function(text){
  tempText <- text
  tempText <- gsub(
    pattern = "\\({1,}",
    replacement = "",
    x = tempText
  )
  tempText <- gsub(
    pattern = "\\){1,}",
    replacement = "",
    x = tempText
  )
  return(tempText)
}

checkParens <- function(text){
  openP <- grepl(
    pattern = "\\(",
    x = text
  )
  closeP <- grepl(
    pattern = "\\)",
    x = text
  )
  return(openP || closeP)
}

addParens <- function(text) {
  tempText <- text
  check <- checkParens(tempText)
  if (check) {
    tempText <- removeParens(tempText)
  }
  return(paste0("(", tempText, ")"))
}

# One Level Nest Adjustments ----
oneLevelNest <- function(x, dataFrame) {
  if (grepl(pattern = "Nested", x = dataFrame$type[x])) {
    target <- gsub(
      pattern = "Nested in ",
      replacement = "",
      x = dataFrame$type[x]
    )
    levels <- dataFrame$levels[x]
    targetLevels <- dataFrame[which(dataFrame$name == target), "levels"]
    return(targetLevels * levels)
  } else {
    return(dataFrame$levels[x])
  }
}

oneDegreeNest <- function(x, dataFrame) {
  if (grepl(pattern = "Nested", x = dataFrame$type[x])) {
    target <- gsub(
      pattern = "Nested in ",
      replacement = "",
      x = dataFrame$type[x]
    )
    levels <- dataFrame$levels[x]
    targetDF <- dataFrame[which(dataFrame$name == target), "degrees"]
    return(levels - targetDF - 1)
  } else {
    return(dataFrame$degrees[x])
  }
}

# Remove Nest from Names ----
removeNest <- function(text) {
  temp1 <- gsub(
    pattern = "\\sNested in\\s\\w*\\s\U00D7",
    replacement = " \U00D7",
    x = text
  )
  return(temp1)
}

# Reset Inputs Function ----
resetInputs <- function(session, defaults = NULL, textList = NULL,
                        numberList = NULL, switchList = NULL, checkList = NULL,
                        radioList = NULL, selectList = NULL){
  if (is.null(defaults)) {
    defaults <- list(
      "text" = "",
      "number" = 3,
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

# Get Interaction Ranks ----
getIntRank <- function(text, dataFrame) {
  parts <- unlist(stringr::str_split(
    string = text,
    pattern = " \U00D7 "
  ))
  rankList <- dataFrame %>%
    dplyr::select(useName, rank) %>%
    dplyr::filter(useName %in% parts)
  maxRank <- max(rankList$rank, na.rm = FALSE)
  return(maxRank + 1)
}

# Clean Interaction Names ----
cleanInteractions <- function(text) {
  temp1 <- gsub(
    pattern = "Nested in",
    replacement = "\U00D7",
    x = text
  )
  parts <- unique(unlist(stringr::str_split(
    string = temp1,
    pattern = " \U00D7 "
  )))
  counts <- stringr::str_count(string = temp1, pattern = parts)
  if (max(counts) > 1) {
    maxPart <- parts[which(counts == max(counts))]
    components <- unlist(stringr::str_split(
      string = text,
      pattern = " \U00D7 "
    ))
    components <- components[-which(components == maxPart)]

    return(paste(components, collapse = " \U00D7 "))
  } else {
    return(text)
  }
}

cleanInteractions2 <- function(text) {
  if (checkParens(text)) {
    return(addParens(text))
  } else {
    return(text)
  }
}

# Fix Degrees of Freedom for Higher Order Terms ----
fixDegrees <- function(target, dataFrame) {
  targetLevels <- as.numeric(dataFrame[which(dataFrame$useName == target), "levels"])
  # Clean useNames
  target <- removeParens(target)
  dataFrame$useName <- sapply(
    X = dataFrame$useName,
    FUN = removeParens,
    USE.NAMES = FALSE
  )

  # Drop non-used rows
  dataFrame <- dataFrame %>%
    dplyr::filter(
      type == "main" | type == "nest" | type == "ho"
    )

  # Get base elements of target
  baseElements <- unlist(
    strsplit(
      x = target,
      split = " \U00D7 ",
      fixed = TRUE
    ))

  # Get all combinations of base elements
  parents <- list()
  for (i in 1:(length(baseElements) - 1)) {
    parents[[paste0("gen", i)]] <- t(
      combn(
        x = baseElements,
        m = i,
        simplify = TRUE,
        FUN = paste,
        collapse = " \U00D7 "
      )
    )
  }
  parents <- unlist(parents, use.names = FALSE)

  adj <- sum(
    dataFrame[which(dataFrame$useName %in% parents), "degrees"],
    na.rm = TRUE
  )
  newDegrees <- targetLevels - adj - 1
  return(newDegrees)
}

# Make Hasse Diagram Elements
hasseElements <- function(dataFrame, dfCheck) {
  # Drop the Reference Row
  dataFrame <- dataFrame %>%
    dplyr::filter(
      type != "ref"
    )

  dataFrame$useName <- sapply(
    X = dataFrame$useName,
    FUN = removeParens,
    USE.NAMES = FALSE
  )

  matSize <- nrow(dataFrame)

  # Make Labels
  if (dfCheck) {
    labels <- sapply(
      X = 1:matSize,
      FUN = function(x){
        paste(
          dataFrame[x, "levels"],
          dataFrame[x, "nodeName"],
          dataFrame[x, "degrees"]
        )
      }
    )
  } else {
    labels <- sapply(
      X = 1:matSize,
      FUN = function(x){
        paste(
          dataFrame[x, "levels"],
          dataFrame[x, "nodeName"]
        )
      }
    )
  }

  # Make Matrix
  diagramMat <- matrix(
    data = rep(FALSE, times = (matSize^2)),
    nrow = matSize,
    ncol = matSize,
    dimnames = list(labels, labels)
  )

  # Adjust Matrix
  for (i in 1:matSize) {
    if (dataFrame[i, "type"] == "action") {
      # Section Action to be superior to all others
      diagramMat[i, 2:matSize] <- TRUE
    } else if (dataFrame[i, "type"] == "block" || dataFrame[i, "type"] == "cov") {
      diagramMat[i, which(dataFrame$type == "error")] <- TRUE
    } else if (dataFrame[i, "type"] == "nest") {
      parent <- gsub(
        pattern = paste0(dataFrame[i, "nodeName"], " Nested in "),
        replacement = "",
        x = dataFrame[i, "useName"]
      )
      rows <- which(dataFrame$nodeName == parent)
      diagramMat[rows, i] <- TRUE

      parentOf <- stringr::str_which(
        string = dataFrame$useName,
        pattern = paste0(dataFrame[i, "useName"], "\\b")
      )
      parentOf <- parentOf[-1]
      parentOf <- c(parentOf, matSize)
      diagramMat[i, parentOf] <- TRUE

    } else if (dataFrame[i, "type"] == "main") {
      parentOf <- stringr::str_which(
        string = dataFrame$useName,
        pattern = paste0(dataFrame[i, "useName"], "\\b")
      )
      parentOf <- parentOf[-1]
      parentOf <- c(parentOf, matSize)
      diagramMat[i, parentOf] <- TRUE
    } else if (dataFrame[i, "type"] == "ho") {
      currentName <- dataFrame[i, "useName"]
      baseElements <- unlist(
        strsplit(
          x = currentName,
          split = " \U00D7 ",
          fixed = TRUE
        )
      )

      parentOf <- stringr::str_which(
        string = dataFrame$useName,
        pattern = paste0(baseElements, collapse = ".*")
      )
      parentOf <- parentOf[-1]
      parentOf <- c(parentOf, matSize)
      diagramMat[i, parentOf] <- TRUE
    }
  }

  return(list(matrix = diagramMat, labels = labels))
}