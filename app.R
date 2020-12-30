# Load Packages
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(hasseDiagram)
library(shinyMatrix)

# Load additional dependencies and setup functions
linkSurvey <- function(repoName){
  link <- paste0(
    "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=",
    repoName
  )
  return(link)
}

resetInputs <- function(session, textList = NULL, numberList = NULL,
                        switchList = NULL, checkList = NULL){
  if (!is.null(textList)) {
    for (i in 1:length(textList)) {
      updateTextInput(
        session = session,
        inputId = textList[i],
        value = ""
      )
    }
  }

  if (!is.null(numberList)) {
    for (i in 1:length(numberList)) {
      updateNumericInput(
        session = session,
        inputId = numberList[i],
        value = 2
      )
    }
  }

  if (!is.null(switchList)) {
    for (i in 1:length(switchList)) {
      updateSwitchInput(
        session = session,
        inputId = switchList[i],
        value = FALSE
      )
    }
  }

  if (!is.null(checkList)) {
    for (i in 1:length(checkList)) {
      updateCheckboxInput(
        session = session,
        inputId = checkList[i],
        value = FALSE
      )
    }
  }
}

getInputNames <- function(pattern, input = input){
  temp1 <- grep(
    pattern = pattern,
    x = names(input),
    value = TRUE
  )
  return(sort(temp1))
}

mePlaceholder <- "E.g., dosage, sex, age group"
blockPlaceholder <- "E.g., field"
covarPlaceholder <- "E.g., age, months experience, hours worked"
grandMeanRow <- data.frame(
  levels = 1,
  labels = "Grand Mean",
  df = 1
)

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "black",
    ### Create the app header ----
    dashboardHeader(
      title = "Hasse Diagrams",
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = linkSurvey("Hasse_Diagrams")
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Build", tabName = "build", icon = icon("gears")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Hasse Diagrams"),
          p("This apps will help you explore Hasse diagrams as well as help you
            build your own."),
          h2("Instructions"),
          p("There are two major parts of this app:"),
          tags$ul(
            tags$li(
              tags$strong("Explore a Diagram: "),
              "where you can learn more about Hasse diagrams including their use
              and how to read them."
            ),
            tags$li(
              tags$strong("Build a Diagram: "),
              "where you can use this app to help you build a Hasse diagram.
              You'll be able to save the picture to place in another file as well
              as copy the R code that would create the diagram."
            )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/14/2020 by NJH.")
          )
        ),
        #### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Exploring Hasse Diagrams"),
          p("This page will be filled in later. ")
        ),
        #### Build a Diagram Page ----
        tabItem(
          tabName = "build",
          withMathJax(),
          h2("Build a Hasse Diagram"),
          p("Use the following interface to help build your own Hasse diagram."),
          br(),
          tabsetPanel(
            id = "builder",
            type = "tabs",
            ##### First Entry Tab ----
            tabPanel(
              title = "first",
              br(),
              h3("Factors, Blocks, and Covariates"),
              textInput(
                inputId = "mainEffects",
                label = "Enter your main effect(s); use a comma to separate terms.",
                placeholder = mePlaceholder,
                width = "75%"
              ),
              br(),
              textInput(
                inputId = "block",
                label = "Enter what you're using as a block; leave empty for no
                  block.",
                placeholder = blockPlaceholder,
                width = "75%"
              ),
              br(),
              textInput(
                inputId = "covariates",
                label = "Enter your covariate(s); use a comma to separate
                  covariates and empty for none.",
                placeholder = covarPlaceholder,
                width = "75%"
              ),
              br(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "reset1",
                    label = "Reset",
                    size = "large",
                    style = "warning"
                  )
                ),
                column(
                  offset = 8,
                  width = 2,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "next1",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large"
                    )
                  )
                )
              )
            ),
            ##### Second Entry Tab ----
            tabPanel(
              title = "second",
              br(),
              h3("Adjust Main Effects (Levels, Fixed/Random Effect), Blocks,
                 and Total Sample Size"),
              br(),
              h4("Main Effects"),
              uiOutput("mainList"),
              h4("Block"),
              uiOutput("blockList"),
              h4("Sample Size"),
              numericInput(
                inputId = "totalSize",
                label = "Total sample size",
                value = "",
                min = 1,
                step = 1
              ),
              br(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "back1",
                    label = "Back",
                    icon = icon("backward"),
                    size = "large"
                  )
                ),
                column(
                  offset = 3,
                  width = 2,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "reset2",
                      label = "Reset page",
                      icon = icon("eraser"),
                      size = "large",
                      style = "warning"
                    )
                  )
                ),
                column(
                  offset = 3,
                  width = 2,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "next2",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large"
                    )
                  )
                )
              )
            ),
            ##### Third Entry Tab ----
            tabPanel(
              title = "third",
              br(),
              h3("Adjust Interactions"),
              p(
                "All possible interactions of your main effects appear below with
                the appropriate fixed/random effect already applied. Please verify
                the number of levels for each one."
              ),
              p(
                tags$strong("To delete an interaction, enter 0 for the number of
                            levels.")
              ),
              matrixInput(
                inputId = "interactionLevels",
                label = "Interaction Levels",
                value = matrix("", 1, 1),
                rows = list(names = TRUE),
                cols = list(names = FALSE),
                class = "numeric"
              ),
              p(tags$em("Note: "), "Be sure to click outside of the input boxes
                before you press the Next button to ensure that your entry is
                recorded."),
              br(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "back2",
                    label = "Back",
                    icon = icon("backward"),
                    size = "large"
                  )
                ),
                column(
                  offset = 3,
                  width = 2,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "reset3",
                      label = "Reset page",
                      icon = icon("eraser"),
                      size = "large",
                      style = "warning"
                    )
                  )
                ),
                column(
                  offset = 3,
                  width = 2,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "next3",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large"
                    )
                  )
                )
              )
            ),
            ##### Output Tab ----
            tabPanel(
              title = "fourth",
              br(),
              h3("Your Hasse Diagram"),
              p(
                "Look through the following diagram to ensure that all of the
                appropriate elements are in their correct places. If not, please
                go back and make the appropriate edits."
              ),
              div(
                style = "text-align: center;",
                plotOutput("hasseDiagram")
              ),
              p(
                "To copy/paste your diagram, right-click (secondary click) on
                the diagram and select 'Copy Image'. Then in your word processing
                program (e.g., Word, Docs), press the Paste key. You may also save
                the image to a file by choosing the 'Save Image as...' option
                insted of 'Copy Image'."
              ),
              h3("Generating Code"),
              p(
                "If you are using R Markdown (or R), you can copy the following
                code to paste into a code chunk or your console to create your
                Hasse diagram.",
                br(),
                br(),
                tags$em("Note: "),
                "you'll need to first have the appropriate packages installed."
              ),
              br(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "back3",
                    label = "Back",
                    icon = icon("backward"),
                    size = "large"
                  )
                ),
                column(
                  offset = 3,
                  width = 2,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "reset4",
                      label = "Start over",
                      icon = icon("exclamation-triangle"),
                      size = "large",
                      style = "danger"
                    )
                  )
                ),
                column(
                  offset = 2,
                  width = 3,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "next4",
                      label = "Advanced edit",
                      icon = icon("user-edit"),
                      size = "large"
                    )
                  )
                )
              )
            ),
            ##### Advanced Edit Tab ----
            tabPanel(
              title = "fifth",
              br(),
              h3("Advanced Editing"),
              p(
                "If something looks off with your diagram, you may edit it here.
                You may edit the labels and the organization matrix. Use the
                buttons to add/remove rows as needed."
              ),
              p(
                "If you are unsure about how to use this page, please return to
                the prior pages and work through the diagram wizard."
              ),
              tags$ul(
                tags$li("matrixInput"),
                tags$li("button to update matrix"),
                tags$li("button to add row/column"),
                tags$li("button to remove row/column"),
                tags$li("button to update diagrams"),
                tags$li("button to go back WITHOUT changes"),
                tags$li("button to start over")
              )
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## Info Button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = tags$div(
          "Build your own Hasse Diagram on the Build page.",
          tags$ol(
            tags$li(
              "Enter all of the main effects, as well as a block and covariates
              in the appropriate blanks. Leave blank to omit and use commas to
              separate distinct terms. Press 'Next' when finished."
            ),
            tags$li(
              "Enter in the number of levels for each term and whether or not a
              term should be treated as a fixed or random effect. All possible
              interactions are listed be default; check the delete box to remove
              unneeded interactions. Enter the total sample size for the study.
              Press 'Next' when finished."
            )
          )
        ),
        html = TRUE
      )
    }
  )

  ## Set up Reactive Values ----
  labels <- reactiveValues()
  finalMatrix <- reactiveVal(grandMeanRow)

  ## First go button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )

  ## Builder Page 1 ----
  ### Reset Page 1 button ----
  observeEvent(
    eventExpr = input$reset1,
    handlerExpr = {
      resetInputs(
        session = session,
        textList = c("mainEffects", "block", "covariates")
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ### Move to Page 2 button ----
  observeEvent(
    eventExpr = input$next1,
    handlerExpr = {
      #### Check for Main effects
      if (is.null(input$mainEffects) || input$mainEffects == "") {
        sendSweetAlert(
          session = session,
          title = "Error",
          type = "error",
          text = "You must enter at least one main effect."
        )
      } else {
        #### Get and split labels ----
        labels$mainEffects <- unlist(
          strsplit(
            x = input$mainEffects,
            split = "[\\,\\;]\\s{0,1}"
          )
        )
        labels$block <- unlist(
          strsplit(
            x = input$block,
            split = "[\\,\\;]\\s{0,1}"
          )
        )
        labels$covariates <- unlist(
          strsplit(
            x = input$covariates,
            split = "[\\,\\;]\\s{0,1}"
          )
        )


        #### Set up second tab ----
        output$mainList <- renderUI(
          expr = {
            tagList(
              tags$ul(
                lapply(
                  X = 1:length(labels$mainEffects),
                  FUN = function(i) {
                    tags$li(
                      tags$strong(labels$mainEffects[i]),
                      fluidRow(
                        column(
                          offset = 0,
                          width = 4,
                          numericInput(
                            inputId = paste0("me", i, "Levels"),
                            label = "Number of levels",
                            value = 3,
                            min = 2,
                            step = 1
                          ),
                        ),
                        column(
                          offset = 0,
                          width = 8,
                          switchInput(
                            inputId = paste0("me", i, "Random"),
                            label = "Fixed or random effect",
                            onLabel = "Random",
                            offLabel = "Fixed",
                            value = FALSE,
                            width = "auto",
                            size = "normal"
                          )
                        )
                      )
                    )
                  }
                )
              )
            )
          }
        )

        output$blockList <- renderUI(
          expr = {
            tagList(
              if (length(labels$block) < 1 || is.null(length(labels$block))) {
                p(
                  "You've not indicated that there is a block in this model."
                )
              } else if (length(labels$block) >= 2 ) {
                p("This version of the app only supports one block. Please go back
                to the previous page and adjust your block entry.")
              } else {
                tags$ul(
                  tags$li(
                    tags$strong(labels$block),
                    fluidRow(
                      column(
                        offset = 0,
                        width = 4,
                        numericInput(
                          inputId = "blockLevels",
                          label = "Number of levels",
                          value = 3,
                          min = 2,
                          step = 1
                        ),
                      ),
                      column(
                        offset = 0,
                        width = 8,
                        switchInput(
                          inputId = "blockRandom",
                          label = "Fixed or random effect",
                          onLabel = "Random",
                          offLabel = "Fixed",
                          value = FALSE,
                          width = "auto",
                          size = "normal"
                        )
                      )
                    )
                  )
                )
              }
            )
          }
        )

        #### Move
        updateTabsetPanel(
          session = session,
          inputId = "builder",
          selected = "second"
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Builder Page 2 ----
  ### Go back to Page 1 ----
  observeEvent(
    eventExpr = input$back1,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "first"
      )

      resetInputs(
        session = session,
        textList = NULL,
        numberList = c(
          getInputNames(pattern = "^me[[:digit:]]{0,}Levels$", input = input),
          "totalSize"
        ),
        switchList = getInputNames(pattern = "Random$", input = input),
        checkList = NULL
      )
    }
  )

  ### Clear Page 2 ----
  observeEvent(
    eventExpr = input$reset2,
    handlerExpr = {
      resetInputs(
        session = session,
        textList = NULL,
        numberList = c(
          getInputNames(pattern = "^me[[:digit:]]{0,}Levels$", input = input),
          "totalSize"
        ),
        switchList = getInputNames(pattern = "Random$", input = input),
        checkList = NULL
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ### Go to Page 3 ----
  observeEvent(
    eventExpr = input$next2,
    handlerExpr = {
      #### Move tabs ----
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "third"
      )

      #### Apply Random to Main Effects ----
      temp1 <- getInputNames(pattern = "^me[[:digit:]]{0,}Random$", input = input)
      for (i in 1:length(temp1)) {
        if (input[[temp1[i]]]) {
          labels$mainEffects[i] <- paste0("(", labels$mainEffects[i], ")")
        }
      }

      #### Apply Random to Block ----
      if (!is.null(input$blockRandom) && input$blockRandom) {
        labels$block <- paste0("(", labels$block, ")")
      }

      #### Create interactions list ----
      labels$interactions <- list()
      for (i in 2:length(labels$mainEffects)) {
        labels$interactions[[paste0("l", i)]] <- t(
          combn(
            x = labels$mainEffects,
            m = i,
            simplify = TRUE,
            FUN = paste,
            collapse = " X "
          )
        )
      }
      labels$interactions <- unlist(labels$interactions)

      #### Apply Random Effects to Interactions ----
      temp1 <- sapply(X = labels$interactions, FUN = grepl, pattern = "\\(")
      for (i in 1:length(temp1)) {
        if (temp1[i]) {
          labels$interactions[i] <- gsub(
            pattern = "\\({1,}",
            replacement = "",
            x = labels$interactions[i]
          )
          labels$interactions[i] <- gsub(
            pattern = "\\){1,}",
            replacement = "",
            x = labels$interactions[i]
          )
          labels$interactions[i] <- paste0("(", labels$interactions[i], ")")
        }
      }

      #### Get level values of Main Effects ----
      temp1 <- getInputNames(pattern = "^me[[:digit:]]+Levels$", input = input)
      temp2 <- c()
      for (i in 1:length(temp1)) {
        temp2 <- c(temp2, input[[temp1[i]]])
      }

      #### Set initial interaction levels ----
      temp3 <- list()
      for (i in 2:length(labels$mainEffects)) {
        temp3[[paste0("l", i)]] <- combn(
          x = temp2,
          m = i,
          simplify = TRUE,
          FUN = prod
        )
      }
      labels$intLevels <- lapply(temp3, function(x) x[!is.na(x)])
      labels$intLevels <- unlist(labels$intLevels)

      #### Create the interaction level matrix and display ----
      interactionMatrix <- matrix(
        data = labels$intLevels,
        nrow = length(labels$interactions),
        ncol = 1
      )
      row.names(interactionMatrix) <- unname(labels$interactions)
      updateMatrixInput(
        session = session,
        inputId = "interactionLevels",
        value = interactionMatrix
      )
      remove(list = c("temp1", "temp2", "temp3"))
    }
  )

  ## Builder Page 3 ----

  ### Go back to Page 2 ----
  observeEvent(
    eventExpr = input$back2,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "second"
      )
      finalMatrix(grandMeanRow)
      interactionMatrix <- NULL
      labels$intLevels <- NULL
      labels$interactions <- NULL
    }
  )

  ### Go to Page 4 ----
  observeEvent(
    eventExpr = input$next3,
    handlerExpr = {
      #### Move tab ----
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "fourth"
      )
      #### Create matrix of labels and levels ----
      #print(names(input))
      temp1 <- getInputNames("^me[[:digit:]]{0,}Levels$", input = input)
      temp2 <- c()
      for (i in 1:length(temp1)) {
        temp2 <- c(temp2, input[[temp1[i]]])
      }
      temp3 <- data.frame(
        levels = temp2,
        labels = labels$mainEffects,
        df = temp2 - 1
      )
      temp4 <- data.frame(
        levels = unname(input$interactionLevels[,1]),
        labels = unlist(dimnames(input$interactionLevels)[1]),
        df = unname(input$interactionLevels[,1])
      )
      finalMatrix(
        rbind(grandMeanRow, temp3, temp4)
      )
      remove(list = c("temp1", "temp2", "temp3", "temp4"))

      finalMatrix(subset(x = finalMatrix(), subset = levels > 0))
      print(finalMatrix())
    }
  )


  ## Builder Page 4 ----
  ### Build Label List ----
  # masterLabels <- eventReactive(
  #   eventExpr = input$next2,
  #   valueExpr = {
  #     usedFreedom <- 1
  #    c(
  #      "1 Grand Mean 1",
  #      for (i in length(labels$mainEffects)) {
  #        paste(
  #          input[[paste0("me", i, "Levels")]],
  #          ifelse(
  #            test = input[[paste0("me", i, "Random")]] == FALSE,
  #            yes = paste0("(", labels$mainEffects[i], ")"),
  #            no = labels$mainEffects[i]
  #          ),
  #          input[[paste0("me", i, "Levels")]] - 1
  #        )
  #        usedFreedom <- usedFreedom + input[[paste0("me", i, "Levels")]] - 1
  #      },
  #      if (nrow(labels$interactions) >= 1) {
  #        for (i in nrow(labels$interactions)) {
  #          if (input[[paste0("inter"), i]] == TRUE) {
  #            next
  #          } else {
  #            paste(
  #              input[[paste0("inter", i, "Levels")]],
  #              ifelse(
  #                test = input[[paste0("inter", i, "Random")]] == FALSE,
  #                yes = paste0("(", labels$interactions[i, 1], " X ",
  #                             labels$interactions[i, 2], ")"),
  #                no = paste0(labels$interactions[i, 1], " X ",
  #                            labels$interactions[i, 2])
  #              ),
  #              input[[paste0("inter", i, "Levels")]] -
  #            )
  #          }
  #        }
  #      }
  #      if (length(labels$block) = 1) {
  #        paste(
  #          input$blockLevels,
  #          labels$block,
  #          input$blockLevels - 1
  #        )
  #        usedFreedom <- usedFreedom + input$blockLevels - 1
  #      },
  #      if (length(label$covariates) >= 1) {
  #        for (i in length(labels$covariates)) {
  #          paste(
  #            "Cov",
  #            labels$covariates[i],
  #            1
  #          )
  #          usedFreedom <- usedFreedom + 1
  #        }
  #      },
  #      paste(
  #        input$totalSize,
  #        "(Error)",
  #        input$totalSize - usedFreedom
  #      )
  #    )
  #   }
  # )

  ### Go back to Page 2 ----
  observeEvent(
    eventExpr = input$back3,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "third"
      )
    }
  )

  ### Start Over ----
  observeEvent(
    eventExpr = c(input$reset4, input$reset5),
    handlerExpr = {
      resetInputs(
        session = session,
        textList = c("mainEffects", "block", "covariates"),
        numberList = c(
          getInputNames(pattern = "Levels$", input = input),
          "totalSize"
        ),
        switchList = getInputNames(pattern = "Random$", input = input),
        checkList = NULL
      )

      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "first"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ### Go to Advanced Edit page ----
  observeEvent(
    eventExpr = input$next4,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "fifth"
      )
    }
  )

  ## Advanced Edit Page ----

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
