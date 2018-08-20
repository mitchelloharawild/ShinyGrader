library(shiny)
library(tidyverse)
library(purrr)
library(rlang)
library(shinyAce)

options(shiny.maxRequestSize=50*1024^2)

shinyServer(
  function(input, output, session) {
    submission_dirs <- reactive({
      req(input$files)
      unlink(path <- file.path(tempdir(), input$files$name), recursive = TRUE)
      dir.create(path)
      unzip(input$files$datapath, exdir = path)
      list.dirs(path, full.names = TRUE, recursive = FALSE)
    })
    
    v <- reactiveValues(
      idx = 1
    )
    
    current_student <- reactive({
      submission_dirs()[v$idx]
    })
    
    current_rmds <- reactive({
      list.files(current_student(), pattern = ".rmd|.Rmd|.RMD", full.names = TRUE, recursive = TRUE)
    })
    
    current_htmls <- reactive({
      list.files(current_student(), pattern = ".html", full.names = TRUE, recursive = TRUE)
    })
    
    output$out_status <- renderUI({
      div(
        valueBox(length(submission_dirs()), "Total submissions", icon = icon("file-code-o"))
      )
    })
    
    output$out_select_code <- renderUI({
      radioButtons("current_rmd", NULL, basename(current_rmds()), inline = TRUE)
    })
    
    observe({
      req(input$current_rmd)
      match_rmd <- current_rmds()[basename(current_rmds()) == input$current_rmd]
      if(length(match_rmd)==1){
        code <- paste0(readLines(match_rmd), collapse = "\n")
      }
      else{
        code <- "Could not read code"
      }
      updateAceEditor(session, "out_code", value = code, mode = "r", theme = "ambiance")
    })
    
    output$out_html <- renderUI({
      current_htmls() %>% 
        map(includeHTML) %>% 
        map(div, style = 'overflow-y: scroll; height: 800px') %>% 
        map2(basename(current_htmls()), ~ tabPanel(.y, .x)) %>%
        invoke(tabBox, .)
    })
    
    observeEvent(input$btn_prev, {
      v$idx <- pmax(1, v$idx - 1)
    })
    observeEvent(input$btn_next, {
      v$idx <- pmin(length(submission_dirs()), v$idx + 1)
    })
    
    output$export <- downloadHandler(
      filename = function() {
        paste0("marks-", Sys.time(), ".xlsx")
      },
      content = function(file){
        write_csv(grade_data(), path = file)
      }
    )
  }
)