# Load Packages
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(hasseDiagram)
library(shinyMatrix)
library(stringr)

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
    elements <- unlist(strsplit(x = temp1$labels[i], split = " X ", fixed = TRUE))
    for (j in (i + 1):(nrow(temp1))) {
      if (all(stringr::str_detect(string = temp1$labels[j], pattern = elements))) {
        temp1$df[j] <- temp1$df[j] - temp1$df[i]
      }
    }
  }
  temp1$labels <- dataFrame$labels
  return(temp1)
}

makeDiagMatrix <- function(labelList, block = NULL, covariates = NULL){
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
      elements <- unlist(strsplit(x = temp1[i], split = " X ", fixed = TRUE))
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
            div(class = "updated", "Last Update: 1/4/2021 by NJH.")
          )
        ),
        #### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Exploring Hasse Diagrams"),
          p(
            "Hasse diagrams (also known as poset or factor structure diagrams)
            serve as a way to visualize the model for an experiment or observational
            study. In Analysis of Variance/Design of Experiments settings, a
            Hasse diagram allows the statistician to display relationships between
            factors in the study without having to resort of algebraic notation.
            Each node in the Hasse diagram corresponds to a term in the algebraic
            model as well as a screen that you pass data through."
          ),
          p(
            "Further, the Hasse diagram allows the user to keep track of how many
            levels each factor/term has, whether of the factor/term is a fixed or
            random effect, as well as the number of degrees of freedom are available
            both for each term and the overall model."
          ),
          box(
            title = "Oneway Example and Explanation of Components",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            p(
              "Let's look at a Hasse diagram from a Oneway ANOVA model involving
            four different dosages of a new drug to reduce how long a person feels
            neck pain after tacking the new drug."
            ),
            plotOutput("exampleHD1"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('exampleHD1').setAttribute('aria-label',
              `Hasse diagram for oneway anova model involving drug dosage levels`)
              })"
            )),
            p(
              "All Hasse diagrams have two common nodes: the Grand Mean term at the
            top of the diagram and an Error term at the bottom. Hasse diagrams
            are linked to the Factor Effects model of ANOVA rather than the Cell
            Means model. All other nodes depend upon the model you're building.
            In our example, we have one factor--the drug dosage each person gets.
            Since this is a main effect, this node goes in-between the Grand Mean
            and the Error."
            ),
            p(
              "You'll notice that there are numbers of either side of each term.",
              br(),
              br(),
              "To the left, the number represents the number of levels for that term.
            There will always be just 1 Grand Mean value for any model. In our
            example, we have four different dosages, thus there are four levels
            to the dosage factor. The number of levels for the Error term is the
            same as the total sample size for the study. In our case, we have 40
            people taking part. Further, we know we have the possibility of a
            balanced design since 40/4 = 10 (no remainder). Keep in mind that a
            remainder of 0 does not guarantee a balanced design.",
              br(),
              br(),
              "To the right, the number represents the degrees of freedom used by
            that particular term. The Grand Mean value will always use one degree
            of freedom (as will each covariate). For all other terms, the degrees
            of freedom will be the term's number of levels minus the degrees of
            freedom used by each connected term located higher up in the diagram."
            ),
            p(
              "You'll also notice that Error appears inside a set of parentheses.
            Parentheses around a term's label denote that we consider that term
            to be Random Effect. (No parentheses then means a Fixed Effect.) The
            final node in the Hasse diagram will always be an Error term and will
            always be a Random Effect.
            "
            )
          ),
          box(
            title = "A More Complicated Example",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            p(
              "Let's revisit the neck pain relief example. This time, the client
              wants to account for how old the person is (in years) as well as
              which of 5 participating clincs the person receives treatment. In
              addition to the dosage, we will also look at the effect of the
              person's sex (female, intersex, male) and the interaction of the
              dosage and sex. This sets up a two-way ANOVA model with both a block
              (clinic) and a covariate (age). Turning this into a Hasse diagram,
              we get the following:"
            ),
            plotOutput("exampleHD2"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('exampleHD2').setAttribute('aria-label',
              `Hasse diagram for twoeway anova model involving drug dosage levels,
              patient sex, a block for clinic, and covariate of patient age.`)
              })"
            )),
            p(
              "You'll notice that both the block (Clinc) and the covariate (Age
              of the person) appear below the Grand Mean and above the Error term.
              Keep in mind that we do not cross (or nest) either blocks or covariates
              with our main effects. You'll also notice that instead of a number
              of levels, the Age term has 'Cov' on the left to denote that this
              is a covariate."
            ),
            p(
              "Since we now have two main effects, we will use a two-way model
              and look at the interaction of those effects (i.e., Dosage X Sex).
              Interactions (and other higher order terms) appear below (nested in)
              the lower order main effects involved. When it comes to Hasse diagrams,
              the lower order an effect is, the higher up in the diagram the term
              appears."
            )
          ),
          p(
            "You can use the Hasse diagram to help you identify the correct
            denominator to use in forming the ", tags$em("F"), "-ratio to test a
            term in the model. Simply follow these steps:",
            tags$ol(
              tags$li("Locate the term/node you want to test in the diagram."),
              tags$li("Identify all terms/nodes below that node which involve a
                      Random Effect. Note: follow the arrows down from the target
                      node."),
              tags$li(
                "Check the eligibility of the random effect nodes:",
                tags$ul(
                  tags$li(tags$strong("Unrestricted: "), "all terms are eligibile."),
                  tags$li(tags$strong("Restricted: "), "only terms which do not
                          contain any new fixed effect factors are eligible")
                )
              ),
              tags$li("Identify which of the eligible term(s) is located the closest
                      to the target term/node. That is, which one(s) is highest
                      up the diagram. This is your Leading Term"),
              tags$li("If there is only one Leading Term, use that term's Mean
                      Square as the denominator."),
              tags$li("If there are multiple Leading Terms, you'll have to use
                      an approximate test.")
            )
          )
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
            type = "hidden",
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
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('hasseDiagram').setAttribute('aria-label',
              `Your Hasse diagram`)
              })"
              )),
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
                verbatimTextOutput("rCode"),
                br(),
                tags$em("Note: "),
                "you'll need to first have the appropriate packages installed as
                well as loaded in your current R session."
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
              matrixInput(
                inputId = "advEdit",
                label = "Hasse Diagram Matrix",
                value = matrix("", 1, 1),
                class = "character",
                rows = list(names = TRUE, editableNames = TRUE),
                cols = list(names = TRUE)
              ),
              fluidRow(
                column(
                  width = 4,
                  bsButton(
                    inputId = "updateLabels",
                    label = "Update row/column labels",
                    size = "large"
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "addRowCol",
                    label = "Add row/column",
                    size = "large",
                    style = "warning"
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "removeRowCol",
                    label = "Remove row/column",
                    size = "large",
                    style = "danger"
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "updateDiagram",
                    label = "Update diagram",
                    size = "large",
                    style = "default"
                  )
                )
              ),
              h3("Updated Diagram"),
              div(
                style = "text-align: center;",
                plotOutput("newHasseDiagram")
              ),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('newHasseDiagram').setAttribute('aria-label',
              `Your updated Hasse diagram`)
              })"
              )),
              h3("Updated Generating Code"),
              p(
                "If you are using R Markdown (or R), you can copy the following
                code to paste into a code chunk or your console to create your
                Hasse diagram.",
                br(),
                verbatimTextOutput("newRCode"),
              ),
              fluidRow(
                column(
                  width = 6,
                  bsButton(
                    inputId = "back4",
                    label = "Back (no changes)",
                    icon = icon("backward"),
                    size = "large"
                  )
                ),
                column(
                  width = 6,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "reset5",
                      label = "Start Over",
                      icon = icon("exclamation-triangle"),
                      size = "large",
                      style = "danger"
                    )
                  )
                )
              )
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020), boastUtils: BOAST Utilities.
            (v. 0.1.10.2), [R Package] Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Borges Ribeiro, B. (2018), shinydashboard: Create dashboards
            with 'Shiny'.(v. 0.7.1) [R package] Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J. J., Xie, Y., McPherson, J. (2020),
            shiny: Web application framework for R. (v. 1.5.0) [R package] Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Ciomek, K. (2017), hasseDiagram: Drawing Hasse diagram. (v. 0.1.3)
            [R Package] Available from https://CRAN.R-project.org/package=hasseDiagram"
          ),
          p(
            class = "hangingindent",
            "Lohr, S. L. (1995), 'Hasse Diagrams in Statistical Consulting and
            Teaching.'", tags$em("The American Statistician"), " 49, 376-381."
          ),
          p(
            class = "hangingindent",
            "Neudecker, A. (2020), shinyMatrix: Shiny matrix input field. (v. 0.4.0)
            [R Package] Available from https://CRAN.R-project.org/package=shinyMatrix"
          ),
          p(
            class = "hangingindent",
            "Oehlert, G. W. (2010), ", tags$em("A First Course in Design and
            Analysis of Experiments."), " Creative Commons: BY-NC-ND 3.0."
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2020), shinyWidgets: Custom
            inputs widgets for shiny. (v. 0.5.4) [R Package] Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2019), stringr: Simple, consistent wrappers for common
            string operations. (v. 1.4.0) [R package] Availabe from
            https://CRAN.R-project.org/package=stringr"
          )
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
  finalLabelFrame <- reactiveVal(grandMeanRow)

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

  ## Explore Page ----
  output$exampleHD1 <- renderPlot(
    expr = {
      labs <- c("1 Grand Mean 1", "4 Dosage 3", "40 (Error) 36")
      mat <- matrix(data = F, nrow = 3, ncol = 3)
      mat[1, c(2,3)] <- T
      mat[2, 3] <- T
      hasseDiagram::hasse(
        data = mat,
        labels = labs
      )
    }
  )

  output$exampleHD2 <- renderPlot(
    expr = {
      labs <- c("1 Grand Mean 1", "5 Clinic 4", "4 Dosage 3", "3 Sex 2",
                "Cov Age 1", "12 Dosage X Sex 6", "600 (Error) 583")
      mat <- matrix(data = F, nrow = 7, ncol = 7)
      mat[1, c(2:7)] <- T
      mat[c(2, 5, 6), 7] <- T
      mat[c(3:4), c(6:7)] <- T
      hasseDiagram::hasse(
        data = mat,
        labels = labs
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
          title = "Main Effect Error",
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
      #### Error Check ----
      if (is.null(input$totalSize) || input$totalSize <= 0 || is.na(input$totalSize)) {
        sendSweetAlert(
          session = session,
          title = "Sample Size Error",
          type = "error",
          text = "You've not entered a valid sample size. Please fix this issue."
        )
      } else {

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

        #### Get level values of Main Effects ----
        temp1 <- getInputNames(pattern = "^me[[:digit:]]+Levels$", input = input)
        temp2 <- c()
        for (i in 1:length(temp1)) {
          temp2 <- c(temp2, input[[temp1[i]]])
        }

        #### Create interactions list ----
        if (length(labels$mainEffects) >= 2) {
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
        } else {
          interactionMatrix <- matrix(
            data = 0,
            nrow = 1,
            ncol = 1
          )
          row.names(interactionMatrix) <- "No interactions possible."
          updateMatrixInput(
            session = session,
            inputId = "interactionLevels",
            value = interactionMatrix
          )
        }

        #### Move tabs ----
        updateTabsetPanel(
          session = session,
          inputId = "builder",
          selected = "third"
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
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
      finalLabelFrame(grandMeanRow)
      interactionMatrix <- NULL
      labels$intLevels <- NULL
      labels$interactions <- NULL
    }
  )

  ### Go to Page 4 ----
  observeEvent(
    eventExpr = input$next3,
    handlerExpr = {
      #### Create matrix of labels and levels ----
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
      finalLabelFrame(
        rbind(grandMeanRow, temp3, temp4)
      )
      remove(list = c("temp1", "temp2", "temp3", "temp4"))
      finalLabelFrame(subset(x = finalLabelFrame(), subset = levels > 0))
      finalLabelFrame(reduceDF(dataFrame = finalLabelFrame()))

      ##### Add Block and Covariates ----
      if (!is.null(labels$block) && length(labels$block) == 1) {
        finalLabelFrame(
          rbind(
            finalLabelFrame(),
            data.frame(
              levels = input$blockLevels,
              labels = labels$block,
              df = input$blockLevels - 1
            )
          )
        )
      }
      if (!is.null(labels$covariates)) {
        temp1 <- data.frame(
          levels = rep("Cov", times = length(labels$covariates)),
          labels = labels$covariates,
          df = rep(1, times = length(labels$covariates))
        )
        finalLabelFrame(
          rbind(
            finalLabelFrame(),
            temp1
          )
        )
      }
      ##### Add Final Error Term ----
      finalLabelFrame(
        rbind(
          finalLabelFrame(),
          data.frame(
            levels = input$totalSize,
            labels = "(Error)",
            df = input$totalSize - sum(finalLabelFrame()$df)
          )
        )
      )
      labels$matrixLabels <- paste(
        finalLabelFrame()$levels,
        finalLabelFrame()$labels,
        finalLabelFrame()$df
      )

      #### Error Check and Move tab ----
      if (any(finalLabelFrame()$df <= 0)) {
        sendSweetAlert(
          session = session,
          title = "Degree of Freedom Error",
          type = "error",
          text = "Your inputs have resulted in one or more terms have 0 or negative
          degrees of freedom. You will need to go back and check both your Total
          Sample Size as well as any interactions."
        )
      } else {
        updateTabsetPanel(
          session = session,
          inputId = "builder",
          selected = "fourth"
        )
      }

    }
  )


  ## Builder Page 4 ----
  output$hasseDiagram <- renderPlot(
    expr = {
      hasseDiagram::hasse(
        data = makeDiagMatrix(
          labelList = finalLabelFrame()$labels,
          block = labels$block,
          covariates = labels$covariates
        ),
        labels = labels$matrixLabels
      )
    }
  )

  output$rCode <- renderText({
    labs <- paste0('"', labels$matrixLabels, '"', collapse = ", ")
    mat <- paste0(unname(
            obj = makeDiagMatrix(
              labelList = finalLabelFrame()$labels,
              block = labels$block,
              covariates = labels$covariates
            ),
            force = TRUE
          ),
          collapse = ", "
    )
     paste0(
       'modelLabels <- c(', labs, ')
modelMatrix <- matrix(
  data = c(', mat, '),
  nrow = ', length(finalLabelFrame()$labels), ',
  ncol = ', length(finalLabelFrame()$labels), '
  byrow = FALSE
)
hasseDiagram::hasse(
 data = modelMatrix,
 labels = modelLabels
)'
     )
  })

  ### Go back to Page 3 ----
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
      updateTabsetPanel(
        session = session,
        inputId = "builder",
        selected = "first"
      )
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
  observeEvent(
    eventExpr = input$builder,
    handlerExpr = {
      if (input$builder == "fifth") {
        orderMat <- makeDiagMatrix(
          labelList = finalLabelFrame()$labels,
          block = labels$block,
          covariates = labels$covariates
        )
        dimnames(orderMat) <- list(labels$matrixLabels, labels$matrixLabels)
        updateMatrixInput(
          session = session,
          inputId = "advEdit",
          value = orderMat
        )
      }
    }
  )

  observeEvent(
    eventExpr = input$updateLabels,
    handlerExpr = {
      mat <- input$advEdit
      dimnames(mat)[[2]] <- dimnames(mat)[[1]]
      updateMatrixInput(
        session = session,
        inputId = "advEdit",
        value = mat
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$addRowCol,
    handlerExpr = {
      mat <- rbind(input$advEdit, rep(FALSE, ncol(input$advEdit)))
      mat <- cbind(mat, rep(FALSE, nrow(mat)))
      dimnames(mat) <- list(c(dimnames(input$advEdit)[[1]],
                              "new term"), c(dimnames(input$advEdit)[[2]], "new term"))
      updateMatrixInput(
        session = session,
        inputId = "advEdit",
        value = mat
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$removeRowCol,
    handlerExpr = {
      mat <- input$advEdit[-nrow(input$advEdit), -ncol(input$advEdit)]
      updateMatrixInput(
        session = session,
        inputId = "advEdit",
        value = mat
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$updateDiagram,
    handlerExpr = {
      mat <- as.matrix(input$advEdit)
      mat <- apply(X = mat, MARGIN = c(1, 2), FUN = as.logical)
      output$newHasseDiagram <- renderPlot(
        expr = {
          hasseDiagram::hasse(
            data = mat
          )
        }
      )

      output$newRCode <- renderText({
        labs <- paste0('"', dimnames(input$advEdit)[[1]], '"', collapse = ", ")
        mat <- paste0(
          unname(obj = mat, force = TRUE),
          collapse = ", "
        )
        paste0(
          'modelLabels <- c(', labs, ')
modelMatrix <- matrix(
  data = c(', mat, '),
  nrow = ', length(dimnames(input$advEdit)[[1]]), ',
  ncol = ', length(dimnames(input$advEdit)[[1]]), '
  byrow = FALSE
)
hasseDiagram::hasse(
 data = modelMatrix,
 labels = modelLabels
)'
        )
      })
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
