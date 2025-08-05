library(shiny)               # UI and server for the app
library(shinyFiles)         # File selection from directories
library(VennDiagram)        # For generating Venn diagrams
library(ComplexUpset)       # For creating UpSet plots
library(DT)                 # For rendering DataTables in UI
library(tools)              # Utility functions for file paths
library(tibble)             # Tidyverse tibble data structure
library(dplyr)              # Tidyverse data manipulation
library(purrr)              # Tidyverse functional programming tools
library(colourpicker)       # UI color input elements
library(ggplot2)            # General plotting
library(plotly)             # Interactive plots (used earlier)

# UI layout definition
ui <- fluidPage(
  div(
    style = "text-align: center;",
    h1("Overlap"),
    h4("A Tool for Intersection Study with Venn and UpSet Plots")
  ),
  hr(),
  
  # File selection input
  div(style = "text-align: center;",
      p("Select a folder containing gene lists in TXT or CSV format (one gene per line starting from the 1st line)."),
      shinyDirButton("folder", "Choose Data", "Please select a folder containing all gene lists", icon = icon("folder"))
  ),
  br(),
  
  uiOutput("fileInputsHeader"),
  uiOutput("fileInputs"),
  
  fluidRow(
    column(
      width = 6,
      div(
        class = "alert alert-info",
        style = "height: 100%;",
        tags$h4("Single Analysis"),
        tags$p("One Venn/UpSet plot and one CSV table of intersections will be generated for the selected files.")
      )
    ),
    column(
      width = 6,
      div(
        class = "alert alert-success",
        style = "height: 100%;",
        tags$h4("Batch Analysis"),
        tags$p("Each pattern will be used to match sample names and generate a separate Venn/UpSet plot and intersection table.")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      div(
        style = "text-align: center;",
        actionButton("analyze", "Single Analyze", class = "btn-primary")
      )
    ),
    column(
      width = 6,
      div(
        style = "text-align: center;",
        textAreaInput("batch_patterns", "Batch Patterns (one per line)", rows = 4, 
                      placeholder = "e.g.\nctrl\ntreated\nday1", width = "100%"),
        actionButton("run_batch", "Batch Analysis", class = "btn-success")
      )
    )
  ),
  hr(),
  
  conditionalPanel(
    condition = "input.analyze > 0",
    # Venn plot controls
    fluidRow(
      column(3, numericInput("venn_width", "Venn Plot Width (px)", min = 300, max = 1000, value = 500, step = 10)),
      column(3, numericInput("venn_height", "Venn Plot Height (px)", min = 300, max = 1000, value = 500, step = 10))
    ),
    uiOutput("vennColorUI"),

    fluidRow(
      column(2, numericInput("venn_alpha", "Transparency", min = 0, max = 1, value = 0.5, step = 0.1)),
      column(2, selectInput("venn_printmode", "Print Mode", choices = c("raw", "percent"), selected = c("raw", "percent"), multiple = TRUE)),
      column(2, numericInput("venn_margin", "Margin", min = 0, max = 1, value = 0.1, step = 0.01)),
      column(2, numericInput("venn_fontsize", "Font Size", min = 5, max = 20, value = 12, step = 1)),
      column(2, numericInput("venn_lwd", "Line Thickness", min = 0.5, max = 5, value = 1, step = 0.1))
    )
  ),
  uiOutput("vennOptions"),
  conditionalPanel(
    condition = "input.analyze > 0",
    uiOutput("vennUI"),
    # Download buttons for Venn
    div(style = "text-align: center;",
        downloadButton("downloadVennPNG", "PNG Download"),
        downloadButton("downloadVennPDF", "PDF Download")
    ),
    hr()
  ),
  
  # UpSet plot and download
  fluidRow(
    column(3, numericInput("upset_width", "UpSet Plot Width (px)", min = 300, max = 1500, value = 800, step = 50)),
    column(3, numericInput("upset_height", "UpSet Plot Height (px)", min = 300, max = 1500, value = 600, step = 50)),
    column(3, numericInput("upset_fontsize", "Font Size", min = 5, max = 20, value = 12, step = 1)),
    column(3, numericInput("upset_cutoff", "Min. Intersection Size", min = 0, max = 1000, value = 0, step = 1))
  ),
  uiOutput("upsetUI"),
  br(),
  div(style = "text-align: center;",
      downloadButton("downloadUpSetPNG", "PNG Download"),
      downloadButton("downloadUpSetPDF", "PDF Download")
  ),
  hr(),
  
  # Table of intersections
  DTOutput("intersectionTable"),
  
  br(),
  div(style = "text-align: center;",
      downloadButton("download", "Export Table CSV")
  ),
  hr()
)

# Server logic
server <- function(input, output, session) {
  # Setup volume roots
  volumes <- c(Home = fs::path_home(), getVolumes()())
  shinyDirChoose(input, "folder", roots = volumes, session = session)
  
  selected_files <- reactiveVal(NULL)
  
  # Load files on folder selection
  observeEvent(input$folder, {
    req(input$folder)
    folder_path <- parseDirPath(volumes, input$folder)
    files <- list.files(folder_path, pattern = "\\.(txt|csv)$", full.names = TRUE)
    # Summarize metadata of files
    data_info <- tibble(
      use = TRUE,
      filename = basename(files),
      sample_name = tools::file_path_sans_ext(basename(files)),
      nrow = NA,
      preview = ""
    )
    # Read and preview each file
    for (i in seq_along(files)) {
      lines <- readLines(files[i], warn = FALSE)
      data_info$nrow[i] <- length(lines)
      data_info$preview[i] <- paste(lines, collapse = ", ")
    }
    selected_files(list(path = files, info = data_info))
  })
  
  # Table header for file inputs
  output$fileInputsHeader <- renderUI({
    req(selected_files())
    fluidRow(
      column(1, checkboxInput("toggle_all", NULL, value = TRUE)),
      column(3, strong("File Name")),
      column(3, strong("Set Name")),
      column(2, strong("Set Size")),
      column(3, strong("Values"))
    )
  })
  
  # Toggle selection of all files
  observeEvent(input$toggle_all, {
    req(selected_files())
    data <- selected_files()$info
    for (i in seq_len(nrow(data))) {
      updateCheckboxInput(session, paste0("use_", i), value = input$toggle_all)
    }
  })
  
  # UI for file inputs and previews
  output$fileInputs <- renderUI({
    req(selected_files())
    data <- selected_files()$info
    lapply(seq_len(nrow(data)), function(i) {
      fluidRow(
        column(1, checkboxInput(paste0("use_", i), NULL, value = data$use[i])),
        column(3, verbatimTextOutput(paste0("fname_", i))),
        column(3, textInput(paste0("sample_", i), NULL, value = data$sample_name[i])),
        column(2, verbatimTextOutput(paste0("nrow_", i))),
        column(3, verbatimTextOutput(paste0("preview_", i)))
      )
    })
  })
  
  # Output previews for each file
  observe({
    req(selected_files())
    data <- selected_files()$info
    lapply(seq_len(nrow(data)), function(i) {
      output[[paste0("fname_", i)]] <- renderText(data$filename[i])
      output[[paste0("nrow_", i)]] <- renderText(data$nrow[i])
      output[[paste0("preview_", i)]] <- renderText(data$preview[i])
    })
  })
  
  # Venn plot color selection UI
  output$vennColorUI <- renderUI({
    req(selected_files())
    data <- selected_files()$info
    used <- sapply(seq_len(nrow(data)), function(i) input[[paste0("use_", i)]])
    selected <- which(used)
    n_sets <- length(selected)
    cols <- list(
      colourInput("venn_col1", "Circle Color 1", value = "black"),
      colourInput("venn_col2", "Circle Color 2", value = "black"),
      colourInput("venn_col3", "Circle Color 3", value = "black"),
      colourInput("venn_col4", "Circle Color 4", value = "black"),
      colourInput("venn_col5", "Circle Color 5", value = "black")
    )
    fills <- list(
      colourInput("venn_fill1", "Fill Color 1", value = "skyblue"),
      colourInput("venn_fill2", "Fill Color 2", value = "pink"),
      colourInput("venn_fill3", "Fill Color 3", value = "lightgreen"),
      colourInput("venn_fill4", "Fill Color 4", value = "lightyellow"),
      colourInput("venn_fill5", "Fill Color 5", value = "lightgray")
    )
    catpos <- list(
      sliderInput("venn_catpos1", "Label Position 1", value = -30, min = -180, max = 180, step = 5),
      sliderInput("venn_catpos2", "Label Position 2", value = 30, min = -180, max = 180, step = 5),
      sliderInput("venn_catpos3", "Label Position 3", value = 180, min = -180, max = 180, step = 5),
      sliderInput("venn_catpos4", "Label Position 4", value = 0, min = -180, max = 180, step = 5),
      sliderInput("venn_catpos5", "Label Position 5", value = 0, min = -180, max = 180, step = 5)
    )
    catdist <- list(
      numericInput("venn_catdist1", "Label Distance 1", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
      numericInput("venn_catdist2", "Label Distance 2", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
      numericInput("venn_catdist3", "Label Distance 3", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
      numericInput("venn_catdist4", "Label Distance 4", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
      numericInput("venn_catdist5", "Label Distance 5", value = 0.01, min = -0.5, max = 0.5, step = 0.01)
    )
    tagList(
      fluidRow(lapply(cols[seq_len(n_sets)], function(x) column(2, x))),
      fluidRow(lapply(fills[seq_len(n_sets)], function(x) column(2, x))),
      fluidRow(lapply(catpos[seq_len(n_sets)], function(x) column(2, x))),
      fluidRow(lapply(catdist[seq_len(n_sets)], function(x) column(2, x)))
    )
  })
  
  # Optional Venn parameter
  output$vennOptions <- renderUI({
    req(selected_files())
    if (input$analyze == 0) return(NULL)  # server-side visibility logic
    
    data <- selected_files()$info
    used <- sapply(seq_len(nrow(data)), function(i) input[[paste0("use_", i)]])
    selected <- which(used)
    
    extra <- list(column(4, sliderInput("venn_rotation", "Rotation (degree)", min = 0, max = 360, value = 0, step = 5)))
    if (length(selected) == 2) {
      extra <- append(extra, list(column(2, checkboxInput("venn_hypertest", "Hypergeometric Test", value = FALSE)),
                                  column(2, numericInput("venn_population", "Total Population", value = NA, min = 1, step = 1))))
    } else if (length(selected) == 3) {
      extra <- append(extra, list(column(3, checkboxInput("venn_reverse", "Reverse Order", value = FALSE))))
    }
    
    fluidRow(extra)
  })
  
  # Centered Venn UI
  output$vennUI <- renderUI({
    fluidRow(
      column(12, div(style = "display: flex; justify-content: center; align-items: center;", plotOutput("vennPlot", height = paste0(input$venn_height, "px"), width = paste0(input$venn_width, "px"))))
    )
  })
  
  # Prepare gene presence matrix
  result_data <- eventReactive(input$analyze, {
    req(selected_files())
    data <- selected_files()$info
    files <- selected_files()$path
    used <- sapply(seq_len(nrow(data)), function(i) input[[paste0("use_", i)]])
    names <- sapply(seq_len(nrow(data)), function(i) input[[paste0("sample_", i)]])
    selected <- which(used)
    req(length(selected) >= 1)
    gene_lists <- lapply(selected, function(i) readLines(files[i]))
    names(gene_lists) <- names[selected]
    all_genes <- unique(unlist(gene_lists))
    gene_presence <- sapply(gene_lists, function(glist) all_genes %in% glist)
    gene_mat <- as.data.frame(gene_presence)
    rownames(gene_mat) <- all_genes
    gene_mat
  })
  
  # Venn plot rendering
  output$vennPlot <- renderPlot({
    mat <- result_data()
    if (ncol(mat) < 2 || ncol(mat) > 5) {
      plot.new()
      text(0.5, 0.5, "Only 2~5 sets are supported by Venn diagram", cex = 1.5)
      return()
    }
    grid.newpage()
    venn.plot <- venn.diagram(
      x = lapply(colnames(mat), function(n) rownames(mat)[mat[[n]]]),
      category.names = colnames(mat),
      filename = NULL,
      output = TRUE,
      height = input$venn_height,
      width = input$venn_width,
      resolution = 100,
      units = "px",
      cat.cex = input$venn_fontsize / 10,
      cex = input$venn_fontsize / 10,
      lwd = input$venn_lwd,
      col = c(input$venn_col1, input$venn_col2, input$venn_col3, input$venn_col4, input$venn_col5)[seq_len(ncol(mat))],
      fill = c(input$venn_fill1, input$venn_fill2, input$venn_fill3, input$venn_fill4, input$venn_fill5)[seq_len(ncol(mat))],
      cat.pos = c(input$venn_catpos1, input$venn_catpos2, input$venn_catpos3, input$venn_catpos4, input$venn_catpos5)[seq_len(ncol(mat))],
      cat.dist = c(input$venn_catdist1, input$venn_catdist2, input$venn_catdist3, input$venn_catdist4, input$venn_catdist5)[seq_len(ncol(mat))],
      alpha = input$venn_alpha,
      margin = input$venn_margin,
      reverse = if (!is.null(input$venn_reverse)) input$venn_reverse else FALSE,
      rotation.degree = input$venn_rotation,
      print.mode = input$venn_printmode,
      hyper.test = if (!is.null(input$venn_hypertest)) input$venn_hypertest else FALSE,
      total.population = if (!is.null(input$venn_population)) input$venn_population else NA
    )
    grid.draw(venn.plot)
  })
  
  output$upsetUI <- renderUI({
    div(style = "display: flex; justify-content: center; align-items: center;",
        plotOutput("upsetPlot", height = paste0(input$upset_height, "px"), width = paste0(input$upset_width, "px")))
  })
  
  # Render UpSet plot
  output$upsetPlot <- renderPlot({
    mat <- result_data()
    upset(
      as.data.frame(mat),
      intersect = colnames(mat),
      name = "Genes",
      min_size = input$upset_cutoff,
      base_annotations = list(
        'Intersection size' = intersection_size(text = list(size = input$upset_fontsize))
      )
    ) + theme_minimal(base_size = input$upset_fontsize)
  })
  
  # Render intersection table
  output$intersectionTable <- renderDT({
    mat <- result_data()
    bin <- apply(mat, 1, paste0, collapse = "")
    split(rownames(mat), bin) %>%
      enframe(name = "Pattern", value = "Genes") %>%
      unnest(Genes) %>%
      datatable(filter = "top", options = list(pageLength = 10))
  })
  
  # CSV table download
  output$download <- downloadHandler(
    filename = function() "intersection_results.csv",
    content = function(file) {
      mat <- result_data()
      bin <- apply(mat, 1, paste0, collapse = "")
      df <- split(rownames(mat), bin) %>%
        enframe(name = "Pattern", value = "Genes") %>%
        unnest(Genes)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Venn PDF download
  output$downloadVennPDF <- downloadHandler(
    filename = function() "venn_plot.pdf",
    content = function(file) {
      mat <- result_data()
      if (ncol(mat) <= 5) {
        pdf(file, width = input$venn_width / 100, height = input$venn_height / 100)
        venn.plot <- venn.diagram(
          x = lapply(colnames(mat), function(n) rownames(mat)[mat[[n]]]),
          category.names = colnames(mat),
          filename = NULL,
          output = TRUE,
          height = input$venn_height,
          width = input$venn_width,
          resolution = 100,
          units = "px",
          cat.cex = input$venn_fontsize / 10,
          cex = input$venn_fontsize / 10,
          lwd = input$venn_lwd,
          col = c(input$venn_col1, input$venn_col2, input$venn_col3, input$venn_col4, input$venn_col5)[seq_len(ncol(mat))],
          fill = c(input$venn_fill1, input$venn_fill2, input$venn_fill3, input$venn_fill4, input$venn_fill5)[seq_len(ncol(mat))],
          cat.pos = c(input$venn_catpos1, input$venn_catpos2, input$venn_catpos3, input$venn_catpos4, input$venn_catpos5)[seq_len(ncol(mat))],
          cat.dist = c(input$venn_catdist1, input$venn_catdist2, input$venn_catdist3, input$venn_catdist4, input$venn_catdist5)[seq_len(ncol(mat))],
          alpha = input$venn_alpha,
          margin = input$venn_margin,
          reverse = if (!is.null(input$venn_reverse)) input$venn_reverse else FALSE,
          rotation.degree = input$venn_rotation,
          print.mode = input$venn_printmode,
          hyper.test = input$venn_hypertest, 
          total.population = input$venn_population
        )
        grid.draw(venn.plot)
        dev.off()
      }
    }
  )
  
  # Venn PNG download
  output$downloadVennPNG <- downloadHandler(
    filename = function() "venn_plot.png",
    content = function(file) {
      mat <- result_data()
      if (ncol(mat) <= 5) {
        png(file, width = input$venn_width, height = input$venn_height)
        venn.plot <- venn.diagram(
          x = lapply(colnames(mat), function(n) rownames(mat)[mat[[n]]]),
          category.names = colnames(mat),
          filename = NULL,
          output = TRUE,
          height = input$venn_height,
          width = input$venn_width,
          resolution = 100,
          units = "px",
          cat.cex = input$venn_fontsize / 10,
          cex = input$venn_fontsize / 10,
          lwd = input$venn_lwd,
          col = c(input$venn_col1, input$venn_col2, input$venn_col3, input$venn_col4, input$venn_col5)[seq_len(ncol(mat))],
          fill = c(input$venn_fill1, input$venn_fill2, input$venn_fill3, input$venn_fill4, input$venn_fill5)[seq_len(ncol(mat))],
          cat.pos = c(input$venn_catpos1, input$venn_catpos2, input$venn_catpos3, input$venn_catpos4, input$venn_catpos5)[seq_len(ncol(mat))],
          cat.dist = c(input$venn_catdist1, input$venn_catdist2, input$venn_catdist3, input$venn_catdist4, input$venn_catdist5)[seq_len(ncol(mat))],
          alpha = input$venn_alpha,
          margin = input$venn_margin,
          reverse = if (!is.null(input$venn_reverse)) input$venn_reverse else FALSE,
          rotation.degree = input$venn_rotation,
          print.mode = input$venn_printmode,
          hyper.test = input$venn_hypertest, 
          total.population = input$venn_population
        )
        grid.draw(venn.plot)
        dev.off()
      }
    }
  )
  
  # UpSet plot PDF download
  output$downloadUpSetPDF <- downloadHandler(
    filename = function() "upset_plot.pdf",
    content = function(file) {
      mat <- result_data()
      p <- upset(
        as.data.frame(mat),
        intersect = colnames(mat),
        name = "Genes",
        min_size = input$upset_cutoff,
        base_annotations = list(
          'Intersection size' = intersection_size(text = list(size = input$upset_fontsize))
        )
      ) + theme_minimal(base_size = input$upset_fontsize)
      ggsave(file, p, width = input$upset_width / 100, height = input$upset_height / 100, dpi = 300, device = "pdf")
    }
  )
  
  # UpSet plot PNG download
  output$downloadUpSetPNG <- downloadHandler(
    filename = function() "upset_plot.png",
    content = function(file) {
      mat <- result_data()
      p <- upset(
        as.data.frame(mat),
        intersect = colnames(mat),
        name = "Genes",
        min_size = input$upset_cutoff,
        base_annotations = list(
          'Intersection size' = intersection_size(text = list(size = input$upset_fontsize))
        )
      ) + theme_minimal(base_size = input$upset_fontsize)
      ggsave(file, p, width = input$upset_width / 100, height = input$upset_height / 100, dpi = 300, device = "png")
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
