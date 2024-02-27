# Load Packages ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(hasseDiagram)
library(shinyMatrix)
library(stringr)
library(dplyr)
library(tibble)
library(DT)
library(rclipboard)

# Define global constants and functions, load data ----
## Helper functions are located in the following file
source(file = "helperFunctions.R", local = TRUE)

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
        boastUtils::surveyLink(name = "Hasse_Diagrams")
      ),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/',
                     icon("house")))
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Example Diagrams", tabName = "example", icon = icon("book-open-reader")),
        menuItem("Diagram Wizard", tabName = "wizard", icon = icon("hat-wizard")),
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
          p("This app will help you explore Hasse diagrams as well as help you
            build your own."),
          h2("Instructions"),
          p("There are two major parts of this app:"),
          tags$ul(
            tags$li(
              tags$strong("Example Diagrams: "),
              "where you can learn more about Hasse diagrams including their use
              and how to read them."
            ),
            tags$li(
              tags$strong("Diagram Wizard: "),
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
            div(class = "updated", "Last Update: 2/27/2024 by NJH.")
          )
        ),
        #### Example Page ----
        tabItem(
          tabName = "example",
          withMathJax(),
          h2("Example Hasse Diagrams"),
          p(
            "Hasse diagrams (also known as poset or factor structure diagrams)
            serve as a way to visualize the model for an experiment or observational
            study. In Analysis of Variance/Design of Experiments settings, a
            Hasse diagram allows the statistician to display relationships between
            factors in the study without having to resort to algebraic notation.
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
              "All Hasse diagrams have two common nodes: the Action (topmost)
              and the Measurement Units (bottom-most) nodes. The Action node is
              where you would place the action which directly lead to the generation
              of the response. Many times, we replace the action with the phase
              'Grand Mean'. The Measurement Unit node is where we place the name
              for the objects/living beings we're getting values of the response
              from. In generic Hasse diagrams and as a default, we use the phrase
              'Error'. Hasse diagrams are linked to the Factor Effects model of
              ANOVA rather than the Cell Means model. All other nodes depend upon
              the model you're building. In our example, we have one factor--the
              drug dosage each person gets. Since this is a main effect, this
              node goes in-between the Grand Mean (the Action node) and the
              Error (the Measurement Unit node)."
            ),
            p(
              "You'll notice that there are numbers of either side of each term.
              To the left, the number represents the number of levels for that term.
              There will always be just 1 Grand Mean value for any model. In our
              example, we have four different dosages, thus there are four levels
              to the dosage factor. The number of levels for the Error term is the
              same as the total sample size for the study. In our case, we have 40
              people taking part. Further, we know we have the possibility of a
              balanced design since 40/4 = 10 (no remainder). Keep in mind that a
              remainder of 0 does not guarantee a balanced design. (For example,
              assigning 20, 10, 5, and 5 people to the four treatments gives a
              remainder of 0 but is not a balanced design.)",
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
            term in the model ", tags$strong("for balanced designs."), "Simply
            follow these steps:",
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
        #### Set up the Diagram Wizard Page ----
        tabItem(
          tabName = "wizard",
          withMathJax(),
          rclipboardSetup(),
          h2("Hasse Diagram Wizard"),
          p("Follow the prompts to create a Hasse Diagram."),
          tabsetPanel(
            id = "diagramWiz",
            type = "hidden",
            ##### Step 1-Action and Measurement Units ----
            tabPanel(
              title = "First Step",
              value = "S1",
              h3("Response"),
              p("Please provide/describe the the primary/surogate response for
                your study."),
              textInput(
                inputId = "responseName",
                label = "Name the response for the study",
                value = "response",
                placeholder = "name of response"
              ),
              h3("Main Action"),
              p("Please provide the following information about the action which
                leads to the primary/surogate response for your study. You may
                choose to use the default values of 'Grand Mean' and 1 if you
                wish."),
              fluidRow(
                column(
                  width = 4,
                  textInput(
                    inputId = "actionName",
                    label = "Name the action you're studying",
                    value = "Grand Mean",
                    placeholder = "name of action"
                  )
                ),
                column(
                  width = 2,
                  numericInput(
                    inputId = "actionCount",
                    label = "Number of actions",
                    value = 1,
                    min = 1,
                    step = 1
                  )
                )
              ),
              h3("Measurement Units"),
              p("Please provide the following information about your measurement
                units. You may choose to use the default values of '(Error)',
                Random Effect, and 50 if you wish."),
              fluidRow(
                column(
                  width = 4,
                  textInput(
                    inputId = "errorName",
                    label = "Measurement unit",
                    value = "(Error)",
                    placeholder = "measurement unit name"
                  )
                ),
                column(
                  width = 3,
                  radioButtons(
                    inputId = "errorType",
                    label = "Fixed or Random Effect",
                    choices = c("Fixed", "Random"),
                    selected = "Random",
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  numericInput(
                    inputId = "totalSampleSize",
                    label = "Total sample size",
                    value = 50,
                    min = 2,
                    step = 1
                  )
                )
              ),
              p(tags$em("Note: "), "Parentheses will be added/removed from your measurement unit
                name automatically based upon your selection for Fixed or Random
                Effect."),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  div(
                    style = "text-align: left;",
                    bsButton(
                      inputId = "wizReset1",
                      label = "Reset page",
                      icon = icon("eraser"),
                      size = "large",
                      style = "warning"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 8,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "wizFS2",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large"
                    )
                  )
                )
              )
            ),
            ##### Step 2-Names of Factors, Blocks, Covariates ----
            tabPanel(
              title = "Second Step",
              value = "S2",
              h3("Factors, Blocks, and Covariates"),
              p("Please provide the names for any factors, blocks, and/or
                covariates in your study. You do NOT need to provide any
                information regarding levels, fixed/random effects, crossing, or
                nesting at this time."),
              textInput(
                inputId = "factorNames",
                label = "Enter your factor(s); use a comma to separate terms.",
                value = "",
                placeholder = mePlaceholder,
                width = "75%"
              ),
              br(),
              textInput(
                inputId = "blockNames",
                label = "Enter what you're using as a block; leave empty for no
                block (limit one).",
                value = "",
                placeholder = blockPlaceholder,
                width = "75%"
              ),
              br(),
              textInput(
                inputId = "covaryNames",
                label = "Enter your covariate(s); use a comma to separate
                covariates and empty for none.",
                value = "",
                placeholder = covarPlaceholder,
                width = "75%"
              ),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  div(
                    style = "text-align: left;",
                    bsButton(
                      inputId = "wizBS1",
                      label = "Back",
                      icon = icon("backward"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "wizReset2",
                      label = "Reset page",
                      icon = icon("eraser"),
                      style = "warning",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "wizFS3",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large"
                    )
                  )
                )
              )
            ),
            ##### Step 3-Levels and Design ----
            tabPanel(
              title = "Third Step",
              value = "S3",
              h3("Setting Design Specifications"),
              p("Please provide the following information about the factors and
                the block (if included)."),
              h4("Main Effect(s)"),
              uiOutput("wizMainList"),
              h4("Block"),
              uiOutput("wizBlockList"),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  div(
                    style = "text-align: left;",
                    bsButton(
                      inputId = "wizBS2",
                      label = "Back",
                      icon = icon("backward"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 1,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "wizReset3",
                      label = "Reset page",
                      icon = icon("eraser"),
                      style = "warning",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "wizSaveValues1",
                      label = "Save values",
                      icon = icon("floppy-disk"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "wizFS4",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large",
                      disabled = TRUE
                    )
                  )
                )
              ),
              div(
                style = "text-align: right;",
                p("Press the Save values button to unlock the Next button.")
              )
            ),
            ##### Step 4-Adjust Higher Order Terms ----
            tabPanel(
              title = "Fourth Step",
              value = "S4",
              h3("Adjust Higher Order Terms"),
              p("Based upon your prior inputs, we've generated the higher order
                terms, both crossing and nesting. We've additionally estimated
                initial number of levels for each term. Please review these terms
                and adjust the number of levels as necessary. (You may safely
                ignore the rank column.)"),
              p(strong("To delete a term, enter 0 for the number of levels.")),
              fluidRow(
                column(
                  width = 7,
                  matrixInput(
                    inputId = "wizHOLevels",
                    label = "Levels for higher order terms",
                    value = matrix("", 1, 1),
                    rows = list(names = TRUE),
                    cols = list(names = TRUE),
                    class = "numeric"
                  ),
                  p(tags$em("Note: "), "Be sure to click outside of the input
                    boxes before you click any other buttons to ensure that your
                    entry is properly recorded."),
                ),
                column(
                  width = 5,
                  offset = 0,
                  h4("Tracking Degrees of Freedom"),
                  p(
                    "Tracking Degrees of Freedom (i.e., 'YES') will show you the
                    total number of degrees of freedom, how many you've used, and
                    how many are remaining for your model up to the higher order
                    terms. This will update and run error checks when you press
                    'Save values'. 'NO' will hide the table, suppress error checks,
                    and omit degrees of freedom in the diagram."
                  ),
                  switchInput(
                    inputId = "wizTrackDF",
                    label = "Track DF?",
                    value = TRUE,
                    onLabel = "YES",
                    offLabel = "NO",
                    onStatus = "success",
                    offStatus = "danger",
                    size = "large",
                    width = "200%"
                  ),
                  DT::dataTableOutput("wizDFTracker")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 2,
                  div(
                    style = "text-align: left;",
                    bsButton(
                      inputId = "wizBS3",
                      label = "Back",
                      icon = icon("backward"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 1,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "wizReset4",
                      label = "Reset page",
                      icon = icon("eraser"),
                      style = "warning",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "wizSaveValues2",
                      label = "Save values",
                      icon = icon("floppy-disk"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 0,
                  div(
                    style = "text-align: right;",
                    bsButton(
                      inputId = "wizFS5",
                      label = "Next",
                      icon = icon("forward"),
                      size = "large",
                      disabled = TRUE
                    )
                  )
                )
              ),
              div(
                style = "text-align: right;",
                p("Press the Save values button to unlock the Next button.")
              )
            ),
            ##### Step 5-Hasse Diagram ----
            tabPanel(
              title = "Fifth Step",
              value = "S5",
              h3("Your Hasse Diagram"),
              p("Look through the following diagram to ensure that all of the
                appropriate elements are in their correct places. If not, please
                go back and make the appropriate edits."),
              plotOutput("wizHasseDiagram"),
              p("To copy/paste your diagram, right-click (secondary click) on the
                diagram and select 'Copy Image'. Then in your word processing
                program (e.g., Word, Docs), press the Paste key. You may also
                save the image to a file by choosing the 'Save Image as...'
                option insted of 'Copy Image'."),
              h3("Generating Code"),
              p("If you are using R Markdown (or R), you can copy the following
                code to paste into a code chunk or your console to create your
                Hasse diagram."),
              fluidRow(
                class = "d-flex",
                column(
                  width = 8,
                  verbatimTextOutput("wizRCode")
                ),
                column(
                  class = "d-flex",
                  width = 4,
                  div(
                    class = "align-self-center",
                    uiOutput("clipboard")
                  )
                )
              ),
              p(tags$em("Note: "), "you'll need to first have the appropriate
                packages installed as well as loaded in your current R session."),
              fluidRow(
                column(
                  width = 2,
                  div(
                    style = "text-align: left;",
                    bsButton(
                      inputId = "wizBS4",
                      label = "Back",
                      icon = icon("backward"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "wizReset5",
                      label = "Start over",
                      icon = icon("triangle-exclamation"),
                      style = "danger",
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 2,
                  offset = 3,
                  div(
                    style = "text-align: right;",
                    # Future Development
                    # bsButton(
                    #   inputId = "wizAdvEdit",
                    #   label = "Advanced edit",
                    #   icon = icon("user-edit"),
                    #   style = "warning",
                    #   size = "large"
                    # )
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
            "Chang, W., Cheng, J., Allaire, J. J., Sivert, C., Schloerke, Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021),
            shiny: Web application framework for R. (v. 1.6.0) [R package]
            Available from https://CRAN.R-project.org/package=shiny"
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
            "Müller, K., and Wickham, H. (2021). tibble: Simple data frames.
            (v. 3.0.6) [R package] Availlable from
            https://CRAN.R-project.org/package=tibble"
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
            "Perrier, V., Meyer, F., Granjon, D. (2021), shinyWidgets: Custom
            inputs widgets for shiny. (v. 0.5.7) [R Package] Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2019), stringr: Simple, consistent wrappers for common
            string operations. (v. 1.4.0) [R package] Availabe from
            https://CRAN.R-project.org/package=stringr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., and Müller, K. (2021). dplyr:
            A grammar of data manipulation. (v. 1.0.4) [R package] Available from
            https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., Tan, X. (2021), DT: A Wrapper of the JavaScript
            Library 'DataTables' (v. 0.17) [R package] Availabe from
            https://CRAN.R-project.org/package=DT"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
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
          "Use the app in two ways:",
          tags$ol(
            tags$li(
              "Click on the ", tags$strong("Example"), " page to explore how to
              read Hasse Diagrams and how to use them to figure out the proper
              denominator to use to test a give node/term."
            ),
            tags$li(
              "Click on the ", tags$strong("Diagram Wizard"), " page to use the
              app to build your own Hasse diagram. The wizard will walk you through
              the necessary steps and then produce a diagram you can save as well
              as the R code to re-create the diagram."
            )
          )
        ),
        html = TRUE
      )
    }
  )

  ## Set up Reactive Values ----
  wizLabels <- reactiveValues()
  dfTable <- reactiveValues()

  ## First go button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "example"
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

  ## Diagram Wizard ----
  ### Actions From Step 1 (wizFS2) ----
  #### Create Base Node Table ----
  baseDF <- eventReactive(
    eventExpr = input$wizFS2,
    valueExpr = {
      data.frame(
        type = c("ref", "action"),
        useName = c("Total", "Action"),
        nodeName = c("Total", input$actionName),
        levels = as.character(c(input$totalSampleSize, input$actionCount)),
        degrees = c(input$totalSampleSize, input$actionCount),
        rank = c(0, 1)
      )
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  #### Error Check and Move ----
  observeEvent(
    eventExpr = input$wizFS2,
    handlerExpr = {
      actionNameError <- FALSE
      errorNameError <- FALSE
      responseModelError <- FALSE
      #### Check for Errors
      if (is.null(input$actionName) || input$actionName == "") {
        actionNameError <- TRUE
      }
      if (is.null(input$errorName) || input$errorName == "") {
        errorNameError <- TRUE
      }
      if (input$responseName == input$actionName ||
          input$responseName == removeParens(input$errorName) ||
          input$actionName == removeParens(input$errorName)) {
        responseModelError <- TRUE
      }

     if (responseModelError) {
        sendSweetAlert(
          session = session,
          title = "Response Model Error",
          text = paste("Opps! Looks like you're trying to use the response in",
                       "your model. For example, you're trying to use the response",
                       "as your Main Action and/or your measurement unit. You
                       can't do this. You'll need to fix this issue."),
          type = "error"
        )
     } else {
       #### Fix any errors with inputs
       if (actionNameError && !errorNameError) {
         sendSweetAlert(
           session = session,
           title = "Error in Action Name",
           text = paste("Error in the action name; using 'Grand Mean' instead and",
                        "moving on. You may use the Back button to return to the",
                        "previous screen and change the action name."),
           type = "error"
         )
         updateTextInput(
           session = session,
           inputId = "actionName",
           value = "Grand Mean"
         )
       } else if (!actionNameError && errorNameError) {
         sendSweetAlert(
           session = session,
           title = "Error in Measurement Unit",
           text = paste("Error in the measurement unit name; using '(Error)'",
                        "instead and moving on. You may use the Back button to",
                        "return to the previous screen and change the measurement",
                        "unit name."),
           type = "error"
         )
         updateTextInput(
           session = session,
           inputId = "errorName",
           value = "(Error)"
         )
       } else if (actionNameError && errorNameError) {
         sendSweetAlert(
           session = session,
           title = "Error in Action Name & Measurement Unit",
           text = paste("Error in both the action and measurement unit names;",
                        "using 'Grand Mean' and '(Error)' instead and moving on.",
                        "You may use the Back button to return to the previous ",
                        "screen and change both values to something else."),
           type = "error"
         )
         updateTextInput(
           session = session,
           inputId = "actionName",
           value = "Grand Mean"
         )
         updateTextInput(
           session = session,
           inputId = "errorName",
           value = "(Error)"
         )
       }

       #### Apply Fixed/Random Effect to Measurement Units
       if (input$errorType == "Fixed") {
         if (input$errorName == "") {
           tempErrorName <- "(Error)"
         } else {
           tempErrorName <- input$errorName
         }
         tempErrorName <- removeParens(text = tempErrorName)

         updateTextInput(
           session = session,
           inputId = "errorName",
           value = tempErrorName
         )
       } else if (input$errorType == "Random") {
         if (input$errorName == "") {
           updateTextInput(
             session = session,
             inputId = "errorName",
             value = "(Error)"
           )
         } else {
           tempErrorName <- addParens(text = input$errorName)
           updateTextInput(
             session = session,
             inputId = "errorName",
             value = tempErrorName
           )
         }
       }

       ##### Move
       updateTabsetPanel(
         session = session,
         inputId = "diagramWiz",
         selected = "S2"
       )
      }

    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ### Actions From Step 2 (Build Step 3; wizFS3) ----
  #### Error Check and Split Strings ----
  observeEvent(
    eventExpr = input$wizFS3,
    handlerExpr = {
      #### Error Checking
      if (is.null(input$factorNames) || input$factorNames == "") {
        sendSweetAlert(
          session = session,
          title = "Missing Factors",
          text = "You must enter at least one factor.",
          type = "error"
        )
      } else {
        ##### Split apart factor, block, and covariate names
        wizLabels$mainEffects <- unlist(
          strsplit(
            x = input$factorNames,
            split = "[\\,\\;]\\s{0,1}"
          )
        )
        wizLabels$block <- unlist(
          strsplit(
            x = input$blockNames,
            split = "[\\,\\;]\\s{0,1}"
          )
        )
        wizLabels$covariates <- unlist(
          strsplit(
            x = input$covaryNames,
            split = "[\\,\\;]\\s{0,1}"
          )
        )

        ##### Error Check-duplicated names
        if (length(wizLabels$mainEffects) != length(unique(wizLabels$mainEffects))) {
          sendSweetAlert(
            session = session,
            title = "Duplicated Factors",
            text = paste("You appear to have used the same name for a factor",
                         "more than once. Please review that entry and ensure",
                         "that you only use each factor name once."),
            type = "error"
          )
        } else if (any(wizLabels$mainEffects %in% wizLabels$block) ||
            any(wizLabels$mainEffects %in% wizLabels$covariates) ||
            any(wizLabels$block %in% wizLabels$covariates)) {
          sendSweetAlert(
            session = session,
            title = "Duplicated Names",
            text = "You have the same name listed in more than once place (factor,
            block, and/or covariate. Please look at your entries and adjust.",
            type = "error"
          )
        } else {
          ##### Build Step 3 Page
          output$wizMainList <- renderUI(
            expr = {
              tagList(
                tags$ul(
                  lapply(
                    X = 1:length(wizLabels$mainEffects),
                    FUN = function(i) {
                      tags$li(
                        tags$strong(wizLabels$mainEffects[i]),
                        fluidRow(
                          column(
                            offset = 0,
                            width = 3,
                            numericInput(
                              inputId = paste0("wizME", i, "Levels"),
                              label = "Number of levels",
                              value = 3,
                              min = 2,
                              step = 1
                            ),
                          ),
                          column(
                            offset = 0,
                            width = 3,
                            radioButtons(
                              inputId = paste0("wizME", i, "Random"),
                              label = "Type of effect",
                              choices = c("Fixed", "Random"),
                              selected = "Fixed",
                              inline = TRUE
                            )
                          ),
                          column(
                            offset = 0,
                            width = 3,
                            selectInput(
                              inputId = paste0("wizME", i, "NestedIn"),
                              label = "Effect is nested in",
                              choices = c(
                                "Not nested",
                                wizLabels$mainEffects[-i]
                              ),
                              selected = "Not nested"
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
          output$wizBlockList <- renderUI(
            expr = {
              tagList(
                if (length(wizLabels$block) < 1 || is.null(length(wizLabels$block))) {
                  p(
                    "You've not indicated that there is a block in this model."
                  )
                } else if (length(wizLabels$block) >= 2 ) {
                  p("This version of the app only supports one block. Please go back
                to the previous page and adjust your block entry.")
                } else {
                  tags$ul(
                    tags$li(
                      tags$strong(wizLabels$block),
                      fluidRow(
                        column(
                          offset = 0,
                          width = 4,
                          numericInput(
                            inputId = "wizBlockLevels",
                            label = "Number of levels",
                            value = 3,
                            min = 2,
                            step = 1
                          ),
                        ),
                        column(
                          offset = 0,
                          width = 8,
                          radioButtons(
                            inputId = "wizBlockRandom",
                            label = "Type of effect",
                            choices = c("Fixed", "Random"),
                            selected = "Fixed",
                            inline = TRUE
                          )
                        )
                      )
                    )
                  )
                }
              )
            }
          )

          ##### Move
          updateTabsetPanel(
            session = session,
            inputId = "diagramWiz",
            selected = "S3"
          )
        }
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ### Actions From Step 3A (wizSaveValues1) ----
  #### Adjust Fixed/Random Main and Block Effects ----
  observeEvent(
    eventExpr = input$wizSaveValues1,
    handlerExpr = {
      #### Apply Fixed/Random Notation to node names
      ##### Main Effects
      mainRandomInputs <- getInputNames(
        pattern = "^wizME[[:digit:]]{0,}Random$",
        input = input
      )
      mainLevelInputs <- getInputNames(
        pattern = "^wizME[[:digit:]]{0,1}Levels$",
        input = input
      )
      mainNestInputs <- getInputNames(
        pattern = "^wizME[[:digit:]]{0,1}NestedIn$",
        input = input
      )
      sapply(
        X = 1:length(wizLabels$mainEffects),
        FUN = function(x) {
          if (input[[mainRandomInputs[x]]] == "Random") {
            wizLabels$mainEffects[x] <- addParens(wizLabels$mainEffects[x])
            updateRadioButtons(
              session = session,
              inputId = mainRandomInputs[x],
              selected = "Random"
            )
            updateNumericInput(
              session = session,
              inputId = mainLevelInputs[x],
              value = input[[mainLevelInputs[x]]]
            )
            updateSelectInput(
              session = session,
              inputId = mainNestInputs[x],
              selected = input[[mainNestInputs[x]]]
            )
          } else {
            wizLabels$mainEffects[x] <- removeParens(wizLabels$mainEffects[x])
            updateRadioButtons(
              session = session,
              inputId = mainRandomInputs[x],
              selected = "Fixed"
            )
            updateNumericInput(
              session = session,
              inputId = mainLevelInputs[x],
              value = input[[mainLevelInputs[x]]]
            )
            updateSelectInput(
              session = session,
              inputId = mainNestInputs[x],
              selected = input[[mainNestInputs[x]]]
            )
          }
        }
      )
      ##### Block
      if (length(wizLabels$block) >= 1) {
        sapply(
          X = 1:length(wizLabels$block),
          FUN = function(x) {
            if (input$wizBlockRandom == "Random") {
              wizLabels$block[x] <- addParens(wizLabels$block[x])
              updateRadioButtons(
                session = session,
                inputId = "wizBlockRandom",
                selected = "Random"
              )
              updateNumericInput(
                session = session,
                inputId = "wizBlockLevels",
                value = input$wizBlockLevels
              )
            } else {
              wizLabels$block[x] <- removeParens(wizLabels$block[x])
              updateRadioButtons(
                session = session,
                inputId = "wizBlockRandom",
                selected = "Fixed"
              )
              updateNumericInput(
                session = session,
                inputId = "wizBlockLevels",
                value = input$wizBlockLevels
              )
            }
          }
        )
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  #### Nested Values ----
  nestList <- eventReactive(
    eventExpr = input$wizSaveValues1,
    valueExpr = {
      nestInputs <- getInputNames(
        pattern = "^wizME[[:digit:]]{0,}NestedIn$",
        input = input
      )
      temp1 <- data.frame(
        current = wizLabels$mainEffects
      )
      temp1$nestedIn <- sapply(
        X = 1:nrow(temp1),
        FUN = function(x) {
          input[[nestInputs[x]]]
        }
      )
      temp2 <- temp1 %>%
        dplyr::filter(
          nestedIn != "Not nested"
        )
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  #### Create Mains Node Table ----
  mainsDF <- eventReactive(
    eventExpr = input$wizSaveValues1,
    valueExpr = {
      newFrame <- baseDF()

      ##### Add Block to table
      if (length(wizLabels$block) >= 1) {
        temp1 <- data.frame(
          type = rep("block", length(wizLabels$block)),
          useName = wizLabels$block,
          nodeName = wizLabels$block,
          levels = as.character(input$wizBlockLevels),
          degrees = input$wizBlockLevels - 1,
          rank = rep(2, length(wizLabels$block))
        )
        newFrame <- rbind(
          newFrame,
          temp1
        )
      }

      ##### Add Covariates to table
      if (length(wizLabels$covariates) >= 1) {
        temp1 <- data.frame(
          type = rep("cov", times = length(wizLabels$covariates)),
          useName = wizLabels$covariates,
          nodeName = wizLabels$covariates,
          levels = rep("cov", times = length(wizLabels$covariates)),
          degrees = rep(1, times = length(wizLabels$covariates)),
          rank = rep(2, times = length(wizLabels$covariates))
        )
        newFrame <- rbind(
          newFrame,
          temp1
        )
      }

      ##### Add Main Effects to table
      temp1 <- getInputNames("^wizME[[:digit:]]{0,}Levels$", input = input)
      temp2 <- c()
      for (i in 1:length(temp1)) {
        temp2 <- c(temp2, input[[temp1[i]]])
      }
      temp3 <- temp2 - 1

      temp1 <- data.frame(
        type = rep("main", times = length(wizLabels$mainEffects)),
        useName = wizLabels$mainEffects,
        nodeName = wizLabels$mainEffects,
        levels = as.character(temp2),
        degrees = temp3,
        rank = rep(2, times = length(wizLabels$mainEffects))
      )

      ###### Adjust for nesting
      if (length(nestList()$current >= 1)) {
        temp1 <- temp1 %>%
          dplyr::mutate(
            type = ifelse(
              test = nodeName %in% nestList()$current,
              yes = "nest",
              no = type
            ),
            useName = ifelse(
              test = nodeName %in% nestList()$current,
              yes = paste(nodeName, "Nested in", nestList()$nestedIn),
              no = nodeName
            ),
            rank = ifelse(
              test = nodeName %in% nestList()$current,
              yes = rank + 1,
              no = rank
            )
          )
        temp1$levels <- sapply(
          X = 1:nrow(temp1),
          FUN = oneLevelNest,
          dataFrame = temp1
        )
        temp1$degrees <- sapply(
          X = 1:nrow(temp1),
          FUN = oneDegreeNest,
          dataFrame = temp1
        )
      }

      rbind(
        newFrame,
        temp1
      )
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  #### Build List of Interactions ----
  crossList <- eventReactive(
    eventExpr = input$wizSaveValues1,
    valueExpr = {
      temp1 <- mainsDF()
      temp1 <- temp1 %>%
        dplyr::filter(
          type == "main" | type == "nest"
        )

      if (nrow(temp1) >= 2) {
        rawInteractions <- list()
        intLevels <- list()
        for (i in 2:nrow(temp1)) {
          rawInteractions[[paste0("int", i)]] <- t(
            combn(
              x = temp1$useName,
              m = i,
              simplify = TRUE,
              FUN = paste,
              collapse = " \U00D7 "
            )
          )
          intLevels[[paste0("int", i)]] <- combn(
            x = as.numeric(temp1$levels),
            m = i,
            simplify = TRUE,
            FUN = prod
          )
        }
        rawInteractions <- unlist(rawInteractions, use.names = FALSE)
        intLevels <- unlist(
          lapply(intLevels, function(x) x[!is.na(x)]),
          use.names = FALSE
        )

        intRanks <- sapply(
          X = rawInteractions,
          FUN = getIntRank,
          dataFrame = temp1,
          USE.NAMES = FALSE
        )

        rawInteractions <- sapply(
          X = rawInteractions,
          FUN = cleanInteractions,
          USE.NAMES = FALSE
        )

        temp2 <- data.frame(
          names = rawInteractions,
          levels = intLevels,
          ranks = intRanks
        ) %>%
          dplyr::distinct(names, .keep_all = TRUE) %>%
          dplyr::filter(
            !(names %in% temp1$useName)
          )

        temp2$names <- sapply(
          X = temp2$names,
          FUN = cleanInteractions2,
          USE.NAMES = FALSE
        )

        matrix(
          data = c(temp2$levels, temp2$ranks),
          nrow = nrow(temp2),
          ncol = 2,
          byrow = FALSE,
          dimnames = list(
            temp2$names,
            c("levels", "rank")
          )
        )
      } else {
        matrix(
          data = c(0, 0),
          nrow = 1,
          ncol = 2,
          byrow = FALSE,
          dimnames = list("No interactions possible", c("levels", "rank"))
        )
      }
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  #### Unlock FS4 and Change Name ----
  observeEvent(
    eventExpr = input$wizSaveValues1,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "wizFS4",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "wizSaveValues1",
        label = "Values saved",
        disabled = TRUE
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### Actions from Step 3B (wizFS4) ----
  #### Build Higher Order Term Page (Step 4) ----
  observeEvent(
    eventExpr = input$wizFS4,
    handlerExpr = {
      ### Build and Display the HO Term Matrix
      updateMatrixInput(
        session = session,
        inputId = "wizHOLevels",
        value = crossList()
      )

      ### Build Initial DF Tracking Table
      temp1 <- mainsDF()
      temp2 <- data.frame(
        Type = c("Total", "Used", "Remaining"),
        Count = c(temp1[which(temp1$type == "ref"), "degrees"],
                  sum(temp1[-which(temp1$type  == "ref"), "degrees"]),
                  temp1[which(temp1$type == "ref"), "degrees"] -
                    sum(temp1[-which(temp1$type  == "ref"), "degrees"])
        )
      )
      dfTable$data <- temp2

      ### Move
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S4"
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### DF Tracker Table ----
  output$wizDFTracker <- DT::renderDataTable(
    expr = {
      validate(
        need(
          expr = input$wizTrackDF,
          message = "Enable Tracking to see table."
        )
      )
        dfTable$data
      },
    caption = "Degrees of Freedom Table",
    rownames = TRUE,
    style = "bootstrap4",
    options = list(
      responsive = TRUE,
      scrollX = FALSE,
      ordering = FALSE,
      paging = FALSE,
      lengthChange = FALSE,
      pageLength = 3,
      searching = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = "dt-center", targets = 2)
      )
    )
  )

  ### Actions from Step 4a (wizSaveValues2) ----
  #### Update DF Tables via eR ----
  fullDF <- eventReactive(
    eventExpr = input$wizSaveValues2,
    valueExpr = {
      ##### Load Mains DF
      temp1 <- mainsDF()

      ##### Get HO levels and convert
      temp2 <- as.data.frame(input$wizHOLevels)
      temp2 <- tibble::rownames_to_column(temp2, var = "useName")
      temp2 <- temp2 %>%
        dplyr::mutate(
          type = "ho",
          .before = 1
        ) %>%
        dplyr::mutate(
          degrees = NA,
          .before = rank
        ) %>%
        dplyr::mutate(
          nodeName = removeNest(useName),
          .after = useName
        )
      masterDF <- rbind(
        temp1,
        temp2
      )

      ##### Remove Zero Level Terms
      masterDF <- masterDF %>%
        filter(
          levels > 0
        )

      ##### Fix Missing Degrees of Freedom
      for (i in 1:nrow(masterDF)) {
        if (is.na(masterDF$degrees[i])) {
          masterDF$degrees[i] = fixDegrees(target = masterDF$useName[i], dataFrame = masterDF)
        } else {
          next
        }
      }


      ##### Add Error Term
      errDF <- data.frame(
        type = "error",
        useName = "measUnit",
        nodeName = input$errorName,
        levels = input$totalSampleSize,
        degrees = masterDF[which(masterDF$type == "ref"), "degrees"] -
          sum(masterDF[-which(masterDF$type  == "ref"), "degrees"]),
        rank = max(masterDF$rank, na.rm = TRUE) + 1
      )

      rbind(
        masterDF,
        errDF
      )

    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  #### Update Tracking Table, Error Check DF, Unlock ----
  observeEvent(
    eventExpr = input$wizSaveValues2,
    handlerExpr = {
      temp1 <- fullDF()
      ##### Update dfTable
      temp2 <- data.frame(
        Type = c("Total", "Used", "Remaining"),
        Count = c(temp1[which(temp1$type == "ref"), "degrees"],
                  sum(temp1[-which(temp1$type %in% c("ref", "error")), "degrees"]),
                  temp1[which(temp1$type == "ref"), "degrees"] -
                    sum(temp1[-which(temp1$type %in% c("ref", "error")), "degrees"])
        )
      )
      dfTable$data <- temp2

      ##### Check Degrees of Freedom
      errorCheck <- FALSE
      if (input$wizTrackDF) {
        ##### Check for positive Remaining DF
        if (dfTable$data[[which(dfTable$data$Type == "Remaining"), "Count"]] <= 0) {
          errorCheck <- TRUE
          sendSweetAlert(
            session = session,
            title = "Non-positive Remaing Degrees of Freedom",
            text = paste("Your current design results in either 0 or a negative",
                         "number of remaining degrees of freedom. This results",
                         "in a non-estimable function. You will need to adjust",
                         "your model OR turn off tracking for Degrees of Freedom",
                         "to continue."),
            type = "error"
          )
        }

        ##### Check for positive Term DF
        if (any(temp1$degrees <= 0)) {
          errorCheck <- TRUE
          sendSweetAlert(
            session = session,
            title = "Non-positive Term Degrees of Freedom",
            text = paste("Your current design results in a term having either 0",
                         "or a negative number of Degrees of Freedom. The term's",
                         "effects will not be estimable. You will need to adjust",
                         "your model OR turn off tracking for Degrees of Freedom",
                         "to continue."),
            type = "error"
          )
        }
      }
      if (!errorCheck) {
        ##### Change Name and Disable Self
        updateButton(
          session = session,
          inputId = "wizSaveValues2",
          label = "Values saved",
          disabled = TRUE
        )

        ##### Unlock wizFS5
        updateButton(
          session = session,
          inputId = "wizFS5",
          disabled = FALSE
        )
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### Actions from Step 4b (wizFS5) ----
  observeEvent(
    eventExpr = input$wizFS5,
    handlerExpr = {
      #### Create list with diagram matrix and labels
      hasseList <- hasseElements(dataFrame = fullDF(), dfCheck = input$wizTrackDF)

      #### Build Diagram
      output$wizHasseDiagram <- renderPlot(
        expr = {
          hasseDiagram::hasse(
            data = hasseList$matrix,
            labels = hasseList$labels
          )
        },
        alt = "Your Hasse diagram"
      )

      #### Build Code
      labs <- paste0('"', hasseList$labels, '"', collapse = ", ")
      mat <- paste0(unname(
        obj = hasseList$matrix,
        force = TRUE
      ),
      collapse = ", "
      )
      genCode <- paste0(
        'library(hasseDiagram)
modelLabels <- c(', labs, ')
modelMatrix <- matrix(
  data = c(', mat, '),
  nrow = ', length(hasseList$labels), ',
  ncol = ', length(hasseList$labels), ',
  byrow = FALSE
)
hasseDiagram::hasse(
 data = modelMatrix,
 labels = modelLabels
)'
      )

      output$wizRCode <- renderText({
        genCode
      })

      #### Clipboard ----
      output$clipboard <- renderUI({
        rclipButton(
          inputId = "clipBtn",
          label = "Copy code",
          clipText = genCode,
          icon = icon("copy"),
          class = "btn-lg"
        )
      })

      #### Move
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S5"
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### Reset Buttons ----
  #### Reset Step 1 ----
  observeEvent(
    eventExpr = c(input$wizReset1, input$wizReset5),
    handlerExpr = {
      updateTextInput(
        session = session,
        inputId = "responseName",
        value = "response"
      )
      updateTextInput(
        session = session,
        inputId = "actionName",
        value = "Grand Mean"
      )
      updateTextInput(
        session = session,
        inputId = "errorName",
        value = "(Error)"
      )
      updateNumericInput(
        session = session,
        inputId = "actionCount",
        value = 1
      )
      updateRadioButtons(
        session = session,
        inputId = "errorRandom",
        selected = "Random"
      )
      updateNumericInput(
        session = session,
        inputId = "totalSampleSize",
        value = 50
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  #### Reset Step 2 ----
  observeEvent(
    eventExpr = c(input$wizReset2, input$wizReset5),
    handlerExpr = {
      updateTextInput(
        session = session,
        inputId = "factorNames",
        value = "",
        placeholder = mePlaceholder
      )
      updateTextInput(
        session = session,
        inputId = "blockNames",
        value = "",
        placeholder = blockPlaceholder
      )
      updateTextInput(
        session = session,
        inputId = "covaryNames",
        value = "",
        placeholder = covarPlaceholder
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  #### Reset Step 3 ----
  observeEvent(
    eventExpr = c(input$wizReset3, input$wizReset5),
    handlerExpr = {
      resetInputs(
        session = session,
        numberList = c(
          getInputNames(
            pattern = "^wizME[[:digit:]]{0,1}Levels$",
            input = input
          ),
          "wizBlockLevels"
        ),
        radioList = c(
          getInputNames(
            pattern = "^wizME[[:digit:]]{0,1}Random$",
            input = input
          ),
          "wizBlockRandom"
        ),
        selectList = getInputNames(
          pattern = "^wizME[[:digit:]]{0,1}NestedIn$",
          input = input
        )
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  #### Reset Step 4 ----
  observeEvent(
    eventExpr = c(input$wizReset4, input$wizReset5),
    handlerExpr = {
      updateMatrixInput(
        session = session,
        inputId = "wizHOLevels",
        value = crossList()
      )
      updateSwitchInput(
        session = session,
        inputId = "wizTrackDF",
        value = TRUE
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  #### Clear All ----
  observeEvent(
    eventExpr = input$wizReset5,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S1"
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### Additional Navigation Buttons ----
  observeEvent(
    eventExpr = input$wizBS1,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S1"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  observeEvent(
    eventExpr = input$wizBS2,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S2"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  observeEvent(
    eventExpr = input$wizBS3,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S3"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  observeEvent(
    eventExpr = input$wizBS4,
    handlerExpr = {
      updateTabsetPanel(
        session = session,
        inputId = "diagramWiz",
        selected = "S4"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Passive Actions for Steps 3 and 4 ----
  observeEvent(
    eventExpr = c(
      input$wizBlockLevels,
      input$wizBlockRandom,
      paste0(
        "input[[",
        getInputNames(
          pattern = "^wizME[[:digit:]]{0,1}Levels$",
          input = input),
        "]]"
      ),
      input$wizReset3,
      input$wisBS2
    ),
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "wizSaveValues1",
        label = "Save values",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "wizFS4",
        disabled = TRUE
      )
    }
  )

  observeEvent(
    eventExpr = c(
      input$wizHOLevels,
      input$wizTrackDF,
      input$wizReset4,
      input$wizBS3
    ),
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "wizSaveValues2",
        label = "Save values",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "wizFS5",
        disabled = TRUE
      )
    }
  )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
