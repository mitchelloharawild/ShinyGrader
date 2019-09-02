library(shiny)
library(shinydashboard)
library(shinycssloaders)
# library(shinyAce)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Shiny Grader",
      titleWidth = "200px",
      tags$li(class = "dropdown", a(href="https://github.com/mitchelloharawild/ShinyGrader", target="_blank", span(icon("github"), " GitHub")))
    ),
    dashboardSidebar(
      width = "200px",
      sidebarMenu(
        menuItem("Upload", tabName = "tab_upload", icon = icon("upload")),
        menuItem("Grade", tabName = "tab_grade", icon = icon("check")),
        br(),
        tags$li(
          downloadLink("export",
                     style = "margin: 0;",
                     label = NULL,
                     class = "",
                     icon("floppy-o"),
                     span("Export Grades")
          )
        )
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem("tab_upload",
                uiOutput(
                  "ui_cache"
                ),
                box(
                  title = "Assignment files input (ZIP)",
                  fileInput("files", label = NULL, accept = "application/zip"),
                  width = 12,
                  status = "primary",
                  collapsible = TRUE
                ),
                box(
                  title = "Evaluation environment input (ZIP)",
                  fileInput("wd_files", label = NULL, accept = "application/zip"),
                  width = 12,
                  status = "primary",
                  collapsible = TRUE
                ),
                withSpinner(uiOutput("out_status"))
        ),
        
        tabItem("tab_grade", 
                column(6,
                  box(
                    uiOutput("out_select_code"),
                    # aceEditor("out_code", height = "600px", value = "\r", mode = "r", wordWrap = TRUE,
                    #           readOnly = TRUE),
                    textAreaInput("out_code", NULL, height="600px"),
                    # div(style="height:600px;overflow-y:scroll",
                    # ),
                    width = 12
                  ),
                  box(
                    title = textOutput("out_feedback"),
                    column(8, textAreaInput("feedback", NULL, rows = 4, placeholder = "Brief feedback")),
                    column(4,
                      textInput("grade", NULL, placeholder = "Grade"),
                      actionButton("btn_prev", NULL, icon = icon("arrow-left")),
                      actionButton("btn_next", NULL, icon = icon("arrow-right"))
                    ),
                    width = 12
                  )
                ),

                uiOutput("out_html")
        )
      )
    )
  )
)
