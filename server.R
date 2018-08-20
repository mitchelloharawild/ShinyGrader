library(shiny)
library(tidyverse)
library(purrr)
library(rlang)
library(shinyAce)

options(shiny.maxRequestSize=50*1024^2)

shinyServer(
  function(input, output, session) {
    submissionZip <- reactive({
      if(isTruthy(input$files)){
        file.copy(input$files$datapath, "submissions.zip")
      }
      if(file.exists("submissions.zip") && file.exists("cache.RData")){
        "submissions.zip"
      }
      else{
        req(FALSE)
      }
    })
    
    submission_dirs <- reactive({
      unlink(path <- file.path(tempdir(), "ShinyGrader_submissions"), recursive = TRUE)
      dir.create(path)
      unzip(submissionZip(), exdir = path)
      list.dirs(path, full.names = TRUE, recursive = FALSE)
    })
    
    if(file.exists("submissions.zip") && file.exists("cache.RData")){
      load("cache.RData")
      v <- invoke(reactiveValues, cache)
    }
    else{
      v <- reactiveValues(
        idx = 1,
        out = list()
      )
    }
    
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
    
    output$file_input <- renderUI({
      box(
        title = "File Input (ZIP)",
        fileInput("files", label = NULL, accept = "application/zip"),
        width = 12,
        status = "primary",
        collapsible = TRUE
      )
    })
    
    output$out_html <- renderUI({
      current_htmls() %>% 
        map(includeHTML) %>% 
        map(div, style = 'overflow-y: scroll; height: 800px') %>% 
        map2(basename(current_htmls()), ~ tabPanel(.y, .x)) %>%
        invoke(tabBox, .)
    })
    
    updateGrades <- function(){
      v$out[[basename(current_student())]] <- list(grade = input$grade, feedback = input$feedback)
    }
    
    restoreGrades <- function(){
      updateTextAreaInput(session, "feedback", value = v$out[[basename(current_student())]]$feedback%||%"")
      updateTextInput(session, "grade", value = v$out[[basename(current_student())]]$grade%||%"")
    }
    
    observeEvent(input$btn_prev, {
      updateGrades()
      v$idx <- pmax(1, v$idx - 1)
      restoreGrades()
    })
    observeEvent(input$btn_next, {
      updateGrades()
      v$idx <- pmin(length(submission_dirs()), v$idx + 1)
      restoreGrades()
    })
    
    grade_data <- reactive({
      updateGrades()
      new_tibble(list(id = names(v$out), grade = map_chr(v$out, "grade"), feedback = map_chr(v$out, "feedback")))
    })
    
    output$export <- downloadHandler(
      filename = function() {
        paste0("marks-", Sys.time(), ".csv")
      },
      content = function(file){
        write_csv(grade_data(), path = file)
      }
    )
    
    onStop(function(){
      cache <- isolate(reactiveValuesToList(v))
      save(cache, file = "cache.RData")
    })
  }
)