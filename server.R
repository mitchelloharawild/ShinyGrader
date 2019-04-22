library(shiny)
library(tidyverse)
library(purrr)
library(rlang)
# library(shinyAce)

options(shiny.maxRequestSize=100*1024^2)

shinyServer(
  function(input, output, session) {
    submissionZip <- reactive({
      if(isTruthy(input$files)){
        file.copy(input$files$datapath, "submissions.zip", overwrite = TRUE)
        v$idx <- 1
        v$out <- list()
        v$display_trigger <- FALSE
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
        out = list(),
        display_trigger = FALSE
      )
    }
    
    current_student <- reactive({
      submission_dirs()[v$idx]
    })
    
    current_rmds <- reactive({
      restoreGrades()
      list.files(current_student(), pattern = ".rmd|.Rmd|.RMD", full.names = TRUE, recursive = TRUE)
    })
    
    current_htmls <- reactive({
      v$display_trigger
      list.files(current_student(), pattern = ".html", full.names = TRUE, recursive = TRUE)
    })
    
    output$out_status <- renderUI({
      div(
        valueBox(length(submission_dirs()), "Total submissions", icon = icon("file-code-o"))
      )
    })
    
    observeEvent(input$btn_knit, {
      x <- file.path(current_student(), input$current_rmd)
      lns <- read_lines(x)
      lns[grepl("opts_chunk\\$set", lns)] <- "knitr::opts_chunk$set(echo = TRUE, error = TRUE)"
      write_lines(lns, x)
      
      # system2(file.path(R.home("bin"), "Rscript"),
      #         c("-e", shQuote(paste0("rmarkdown::render(", shQuote(x),
      #           ", output_format = ", shQuote("html_document"), ", output_dir = ", shQuote(current_student()), ")"))))

      rmarkdown::render(x, output_format = "html_document", output_dir = current_student(), 
                        envir = new.env(parent = globalenv()))
      
      v$display_trigger <- !(v$display_trigger)
    })
    
    output$out_select_code <- renderUI({
      if(length(current_rmds()) > 0){
        fluidRow(
          column(10, radioButtons("current_rmd", NULL, basename(current_rmds()), inline = TRUE)),
          column(2, actionButton("btn_knit", label = "Knit", icon = icon("file-code")))
        )
      }
      else{
        radioButtons("current_rmd", NULL, "No RMD found", inline = TRUE)
      }
    })
    
    output$out_code <- renderText({
      req(input$current_rmd, current_rmds())
      v$display_trigger
      match_rmd <- current_rmds()[match(input$current_rmd,basename(current_rmds()), nomatch = 1)]
      print(match_rmd)
      if(!is.na(match_rmd)){
        code <- paste0(readLines(match_rmd), collapse = "\n")
      }
      else{
        code <- "Could not read code"
      }
      # updateAceEditor(session, "out_code", value = code)
      code
    })
    
    output$ui_cache <- renderUI({
      if(file.exists("submissions.zip") && file.exists("cache.RData")){
        infoBox(
          title = NULL,
          value = div(
            h3("Cached assignment submissions found."),
            h4("CAUTION: Uploading new submissions will clear the cache and all marking will be lost!")
          ),
          width = 12,
          icon = icon("info"),
          color = "green"
        )
      } else {
        NULL
      }
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
      
      # Save cache to disk
      cache <- isolate(reactiveValuesToList(v))
      save(cache, file = "cache.RData")
    }
    
    restoreGrades <- function(){
      updateTextAreaInput(session, "feedback", value = v$out[[basename(current_student())]]$feedback%||%"")
      updateTextInput(session, "grade", value = v$out[[basename(current_student())]]$grade%||%"")
    }
    
    observeEvent(input$btn_prev, {
      updateGrades()
      v$idx <- pmax(1, v$idx - 1)
    })
    
    observeEvent(input$btn_next, {
      updateGrades()
      v$idx <- pmin(length(submission_dirs()), v$idx + 1)
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