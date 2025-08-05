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
library(readxl)
library(shinyjs)

`%||%` <- function(a, b) if (!is.null(a)) a else b

read_and_summarize_files <- function(paths, selections, headerline) {
  data_info <- tibble(
    use = selections,
    filename = basename(paths),
    sample_name = tools::file_path_sans_ext(basename(paths)),
    nrow = NA,
    preview = "",
    values = vector("list", length(paths))
  )
  for (i in seq_along(paths)) {
    if (grepl("\\.xlsx$", paths[i])) {
      data <- readxl::read_excel(paths[i], col_names = headerline)
    } else {
      data <- read.table(paths[i],
                         header = headerline,
                         sep = ifelse(grepl("\\.csv$", paths[i]), ",", "\t"),
                         stringsAsFactors = FALSE)
    }
    lines <- unique(data[[1]])
    data_info$nrow[i] <- length(lines)
    data_info$preview[i] <- ifelse(length(lines) > 5,
                                   paste(c(lines[1:5], "..."), collapse = ", "),
                                   paste(lines, collapse = ", "))
    data_info$values[[i]] <- lines
  }
  return(data_info)
}

read_and_summarize_excel <- function(file, selections, headerline) {
  sheets <- readxl::excel_sheets(file)
  data_info <- tibble(
    use = selections,
    filename = sheets,
    sample_name = sheets,
    nrow = NA,
    preview = "",
    values = vector("list", length(sheets))
  )
  for (i in seq_along(sheets)) {
    data <- readxl::read_excel(file, sheet = sheets[i], col_names = headerline)
    lines <- unique(data[[1]])
    data_info$nrow[i] <- length(lines)
    data_info$preview[i] <- ifelse(length(lines) > 5,
                                   paste(c(lines[1:5], "..."), collapse = ", "),
                                   paste(lines, collapse = ", "))
    data_info$values[[i]] <- lines
  }
  return(data_info)
}

read_and_summarize_rds <- function(file, selections, headerline) {
  data <- readRDS(file)
  if (!is.list(data)) {
    stop("The R object must be a list.")
  }
  if (is.null(names(data))) {
    names(data) <- paste0("Set", seq_along(data))
  }
  data_info <- tibble(
    use = selections,
    filename = names(data),
    sample_name = names(data),
    nrow = NA,
    preview = "",
    values = vector("list", length(data))
  )
  for (i in seq_along(data)) {
    if (is.data.frame(data[[i]])) {
      lines <- unique(rownames(data[[i]]))
    } else if (is.vector(data[[i]])) {
      lines <- unique(data[[i]])
    } else {
      stop("Each element of the list must be a vector or a data frame.")
    }
    if (headerline) {
      lines <- lines[-1]  # Remove the header line if present
    } 
    data_info$nrow[i] <- length(lines)
    data_info$preview[i] <- ifelse(length(lines) > 5,
                                   paste(c(lines[1:5], "..."), collapse = ", "),
                                   paste(lines, collapse = ", "))
    data_info$values[[i]] <- lines
  }
  return(data_info)
}

generateVennPlot <- function(mat, input) {
  n_sets <- ncol(mat)
  sets <- lapply(colnames(mat), function(n) rownames(mat)[mat[[n]]])
  category.names <- c(input$venn_setname1, input$venn_setname2, input$venn_setname3, input$venn_setname4, input$venn_setname5)[seq_len(n_sets)]
  fill <- c(input$venn_fill1, input$venn_fill2, input$venn_fill3, input$venn_fill4, input$venn_fill5)[seq_len(n_sets)]
  col <- c(input$venn_col1, input$venn_col2, input$venn_col3, input$venn_col4, input$venn_col5)[seq_len(n_sets)]
  cat.pos <- c(input$venn_catpos1, input$venn_catpos2, input$venn_catpos3, input$venn_catpos4, input$venn_catpos5)[seq_len(n_sets)]
  cat.dist <- c(input$venn_catdist1, input$venn_catdist2, input$venn_catdist3, input$venn_catdist4, input$venn_catdist5)[seq_len(n_sets)]
  vp <- venn.diagram(
    x = sets,
    category.names = category.names,
    filename = NULL,  # Don't write yet
    output = TRUE,
    height = input$venn_height,
    width = input$venn_width,
    resolution = 100,
    units = "px",
    cat.cex = input$venn_fontsize / 10,
    cex = input$venn_fontsize / 10,
    lwd = input$venn_lwd,
    col = col,
    fill = fill,
    cat.pos = cat.pos,
    cat.dist = cat.dist,
    alpha = input$venn_alpha,
    margin = input$venn_margin,
    reverse = if (!is.null(input$venn_reverse)) input$venn_reverse else FALSE,
    rotation.degree = input$venn_rotation,
    print.mode = input$venn_printmode,
    hyper.test = if (!is.null(input$venn_hypertest)) input$venn_hypertest else FALSE,
    total.population = ifelse(is.null(input$venn_population), NA, input$venn_population)
  )
  return(vp)
}

vennModule <- function(id, sets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    n_sets <- length(sets)
    
    # UI
    output$vennUI <- renderUI({
      if (n_sets < 2 || n_sets > 5) {
        return(div(style = "text-align: center; color: darkred; font-weight: bold;", "Please select 2 to 5 sets to enable Venn options and plot."))
      }
      
      setnames <- list(
        textInput(ns("venn_setname1"), "Set Name 1", value = names(sets)[1]),
        textInput(ns("venn_setname2"), "Set Name 2", value = names(sets)[2]),
        textInput(ns("venn_setname3"), "Set Name 3", value = names(sets)[3]),
        textInput(ns("venn_setname4"), "Set Name 4", value = names(sets)[4]),
        textInput(ns("venn_setname5"), "Set Name 5", value = names(sets)[5])
      )
      cols <- list(
        colourpicker::colourInput(ns("venn_col1"), "Circle Color 1", value = "black"),
        colourpicker::colourInput(ns("venn_col2"), "Circle Color 2", value = "black"),
        colourpicker::colourInput(ns("venn_col3"), "Circle Color 3", value = "black"),
        colourpicker::colourInput(ns("venn_col4"), "Circle Color 4", value = "black"),
        colourpicker::colourInput(ns("venn_col5"), "Circle Color 5", value = "black")
      )
      fills <- list(
        colourpicker::colourInput(ns("venn_fill1"), "Fill Color 1", value = "skyblue"),
        colourpicker::colourInput(ns("venn_fill2"), "Fill Color 2", value = "pink"),
        colourpicker::colourInput(ns("venn_fill3"), "Fill Color 3", value = "lightgreen"),
        colourpicker::colourInput(ns("venn_fill4"), "Fill Color 4", value = "lightyellow"),
        colourpicker::colourInput(ns("venn_fill5"), "Fill Color 5", value = "lightgray")
      )
      catpos <- list(
        sliderInput(ns("venn_catpos1"), "Label Position 1", value = -30, min = -180, max = 180, step = 5),
        sliderInput(ns("venn_catpos2"), "Label Position 2", value = 30, min = -180, max = 180, step = 5),
        sliderInput(ns("venn_catpos3"), "Label Position 3", value = 180, min = -180, max = 180, step = 5),
        sliderInput(ns("venn_catpos4"), "Label Position 4", value = 0, min = -180, max = 180, step = 5),
        sliderInput(ns("venn_catpos5"), "Label Position 5", value = 0, min = -180, max = 180, step = 5)
      )
      catdist <- list(
        numericInput(ns("venn_catdist1"), "Label Distance 1", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
        numericInput(ns("venn_catdist2"), "Label Distance 2", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
        numericInput(ns("venn_catdist3"), "Label Distance 3", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
        numericInput(ns("venn_catdist4"), "Label Distance 4", value = 0.01, min = -0.5, max = 0.5, step = 0.01),
        numericInput(ns("venn_catdist5"), "Label Distance 5", value = 0.01, min = -0.5, max = 0.5, step = 0.01)
      )
      tagList(
        fluidRow(
          column(3, numericInput(ns("venn_width"), "Venn Plot Width (px)", min = 300, max = 1000, value = 500, step = 10)),
          column(3, numericInput(ns("venn_height"), "Venn Plot Height (px)", min = 300, max = 1000, value = 500, step = 10))
        ),
        
        fluidRow(lapply(setnames[seq_len(n_sets)], function(x) column(2, x))),
        fluidRow(lapply(cols[seq_len(n_sets)], function(x) column(2, x))),
        fluidRow(lapply(fills[seq_len(n_sets)], function(x) column(2, x))),
        fluidRow(lapply(catpos[seq_len(n_sets)], function(x) column(2, x))),
        fluidRow(lapply(catdist[seq_len(n_sets)], function(x) column(2, x))),
        
        fluidRow(
          column(2, numericInput(ns("venn_alpha"), "Transparency", min = 0, max = 1, value = 0.5, step = 0.1)),
          column(2, selectInput(ns("venn_printmode"), "Print Mode", choices = c("raw", "percent"), selected = c("raw", "percent"), multiple = TRUE)),
          column(2, numericInput(ns("venn_margin"), "Margin", min = 0, max = 1, value = 0.1, step = 0.01)),
          column(2, numericInput(ns("venn_fontsize"), "Font Size", min = 5, max = 20, value = 12, step = 1)),
          column(2, numericInput(ns("venn_lwd"), "Line Thickness", min = 0.5, max = 5, value = 1, step = 0.1))
        ),
        renderVennExtraOptions(ns, n_sets)
      )
    })
    
    # Plot
    venn_mat <- reactive({
      all_genes <- unique(unlist(sets))
      gene_mat <- sapply(sets, function(glist) all_genes %in% glist)
      gene_df <- as.data.frame(gene_mat)
      rownames(gene_df) <- all_genes
      gene_df
    })
    
    output$vennPlot <- renderPlot({
      mat <- venn_mat()
      grid.newpage()
      vp <- generateVennPlot(mat, input)
      grid.draw(vp)
    })
    
    output$vennplotUI <- renderUI({
      req(!is.null(input$venn_height), !is.null(input$venn_width))
      tagList(
        fluidRow(
          column(12, div(style = "display: flex; justify-content: center; align-items: center;", plotOutput(ns("vennPlot"), height = paste0(input$venn_height, "px"), width = paste0(input$venn_width, "px"))))
        ),
        div(style = "text-align: center;",
            downloadButton(ns("downloadVennPNG"), "PNG Download"),
            downloadButton(ns("downloadVennPDF"), "PDF Download")
        ),
        hr()
      )
    })
    
    # PNG Download
    output$downloadVennPNG <- downloadHandler(
      filename = function() paste0("venn_plot_", id, ".png"),
      content = function(file) {
        mat <- venn_mat()
        if (ncol(mat) >= 2 && ncol(mat) <= 5) {
          png(file, width = input$venn_width, height = input$venn_height, res = 100)
          grid.newpage()
          vp <- generateVennPlot(mat, input)
          grid.draw(vp)
          dev.off()
        }
      }
    )
    
    # PDF Download
    output$downloadVennPDF <- downloadHandler(
      filename = function() paste0("venn_plot_", id, ".pdf"),
      content = function(file) {
        mat <- venn_mat()
        if (ncol(mat) >= 2 && ncol(mat) <= 5) {
          pdf(file, width = input$venn_width / 100, height = input$venn_height / 100)
          grid.newpage()
          vp <- generateVennPlot(mat, input)
          grid.draw(vp)
          dev.off()
        }
      }
    )
  })
}

# A helper function that returns extra Venn options based on selected set count
renderVennExtraOptions <- function(ns, n_sets) {
  extra <- list(
    column(4, sliderInput(ns("venn_rotation"), "Rotation (degree)", min = 0, max = 360, value = 0, step = 5))
  )
  if (n_sets == 2) {
    extra <- append(extra, list(
      column(2, checkboxInput(ns("venn_hypertest"), "Hypergeometric Test", value = FALSE)),
      column(2, numericInput(ns("venn_population"), "Total Population", value = NA, min = 1, step = 1))
    ))
  } else if (n_sets == 3) {
    extra <- append(extra, list(
      column(3, checkboxInput(ns("venn_reverse"), "Reverse Order", value = FALSE))
    ))
  }
  fluidRow(extra)
}

ui <- fluidPage(
  useShinyjs(),
  
  div(
    style = "text-align: center;",
    h1("Overlap"),
    h4("A Tool for Intersection Study with Venn and UpSet Plots")
  ),
  hr(),
  
  # File selection input
  fluidRow(
    column(
      width = 4,
      div(style = "text-align: center;",
          p(strong("Select a folder containing gene lists"), br(), "One gene per line in the 1st column of each txt or csv file"),
          shinyDirButton("folder", "Load Data Folder", "Please select a folder containing all gene lists", icon = icon("folder"))
      ),
    ),
    column(
      width = 4,
      div(style = "border-left: 1px solid #ccc; border-right: 1px solid #ccc; text-align: center;",
          p(strong("Load gene list from an Excel table"), br(), "One gene per line in the 1st column of each sheet"),
          shinyFilesButton("excel_file", "Load Excel", "Please select an Excel file", multiple = FALSE)
      ),
    ),
    column(
      width = 4,
      div(style = "text-align: center;",
          p(strong("Load gene list from an rds file"), br(), "The R object should be a named list of vectors of genes"),
          shinyFilesButton("rds_file", "Load RDS", "Please select an RDS file", multiple = FALSE)
      )
    )
  ),
  br(),
  
  fluidRow(
    column(width = 5, offset = 9, uiOutput("fileToggleHeader"))
  ),
  
  uiOutput("fileInputsHeader"),
  uiOutput("fileInputs"),
  br(),
  
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
        actionButton("analyze", "Single Analyze", class = "btn-primary", disabled = TRUE)
      )
    ),
    column(
      width = 6,
      div(
        style = "text-align: center;",
        textAreaInput("batch_patterns", "Batch Patterns (one per line)", rows = 4, 
                      placeholder = "e.g.\nctrl\ntreated\nday1", width = "100%"),
        actionButton("run_batch", "Batch Analysis", class = "btn-success", disabled = TRUE)
      )
    )
  ),
  hr(),
  
  uiOutput("singlePanel"),  # Will be shown when "Single Analyze" is clicked
  uiOutput("batchPanel")   # Will be shown when "Batch Analysis" is clicked
  
)

server <- function(input, output, session) {
  # Setup volume roots
  volumes <- c(Home = fs::path_home(), getVolumes()())
  shinyDirChoose(input, "folder", roots = volumes, session = session)
  shinyFileChoose(input, "excel_file", roots = volumes, session = session, filetype = c("xlsx"))
  shinyFileChoose(input, "rds_file", roots = volumes, session = session, filetype = c("rds"))
  
  selected_files <- reactiveVal(NULL)
  
  # Load files on folder selection
  observeEvent(input$folder, {
    req(input$folder)
    folder_path <- parseDirPath(volumes, input$folder)
    files <- list.files(folder_path, pattern = "\\.(txt|csv|xlsx)$", full.names = TRUE)
    
    data_info <- read_and_summarize_files(files, FALSE, headerline = input$headerline %||% FALSE)
    selected_files(list(path = files, info = data_info, source = "folder"))
    updateCheckboxInput(session, "toggle_all", value = FALSE) # Reset toggle_all when new files are loaded
  })
  
  # Load files from Excel
  observeEvent(input$excel_file, {
    req(input$excel_file)
    file_path <- parseFilePaths(volumes, input$excel_file)$datapath[1]
    req(!is.na(file_path), file.exists(file_path))
    data_info <- read_and_summarize_excel(file_path, FALSE, headerline = input$headerline %||% FALSE)
    selected_files(list(path = file_path, info = data_info, source = "excel"))
    updateCheckboxInput(session, "toggle_all", value = FALSE) # Reset toggle_all when new files are loaded
  })
  
  # Load files from RDS
  observeEvent(input$rds_file, {
    req(input$rds_file)
    file_path <- parseFilePaths(volumes, input$rds_file)$datapath[1]
    req(!is.na(file_path), file.exists(file_path))
    data_info <- read_and_summarize_rds(file_path, FALSE, headerline = input$headerline %||% FALSE)
    selected_files(list(path = file_path, info = data_info, source = "rds"))
    updateCheckboxInput(session, "toggle_all", value = FALSE) # Reset toggle_all when new files are loaded
  })
  
  # Toggle header for file inputs
  output$fileToggleHeader <- renderUI({
    req(selected_files())
    checkboxInput("headerline", "Omit the first value.", value = FALSE)
  })
  
  # Table header for file inputs
  output$fileInputsHeader <- renderUI({
    req(selected_files())
    fluidRow(
      column(1, checkboxInput("toggle_all", NULL, value = input$toggle_all %||% FALSE)),
      column(3, strong("Original Name")),
      column(3, strong("Set Name")),
      column(2, strong("Set Size")),
      column(3, strong("Values"))
    )
  })
  
  observeEvent(input$headerline, {
    updateCheckboxInput(session, "headerline", value = input$headerline)
    current_use <- sapply(seq_len(nrow(selected_files()$info)), function(i) input[[paste0("use_", i)]] %||% TRUE)
    files <- selected_files()$path
    if (selected_files()$source == "folder") {
      data <- read_and_summarize_files(files, current_use, headerline = input$headerline)
    } else if (selected_files()$source == "excel") {
      data <- read_and_summarize_excel(files, current_use, headerline = input$headerline)
    } else if (selected_files()$source == "rds") {
      data <- read_and_summarize_rds(files, current_use, headerline = input$headerline)
    }
    selected_files(list(path = files, info = data, source = selected_files()$source))
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
  
  mode <- reactiveVal(NULL)  # Can be "single", "batch", or NULL
  observeEvent(input$analyze, {
    mode("single")
  })
  observeEvent(input$run_batch, {
    mode("batch")
  })
  
  observe({
    req(selected_files())
    data <- selected_files()$info
    # Extract current checkbox states and coerce to logical vector safely
    used <- vapply(seq_len(nrow(data)), function(i) {
      val <- input[[paste0("use_", i)]]
      isTRUE(val)  # This returns TRUE if checked, FALSE otherwise
    }, logical(1))
    # Enable or disable buttons based on selection
    if (sum(used) > 0) {
      shinyjs::enable("analyze")
      shinyjs::enable("run_batch")
    } else {
      shinyjs::disable("analyze")
      shinyjs::disable("run_batch")
    }
  })
  
  # Single analysis button
  output$singlePanel <- renderUI({
    req(mode() == "single")
    mat <- venn_mat()
    n_sets <- if (!is.null(mat)) ncol(mat) else 0
    
    if (n_sets >= 2 && n_sets <= 5) {
      tagList(
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
        ),
        uiOutput("vennOptions"),
        uiOutput("vennUI"),
        div(style = "text-align: center;",
            downloadButton("downloadVennPNG", "PNG Download"),
            downloadButton("downloadVennPDF", "PDF Download")
        ),
        hr()
      )
    } else {
      tagList(
        div(style = "text-align: center; color: darkred; font-weight: bold;", "Please select 2 to 5 sets to enable Venn options and plot."),
        hr()
      )
    }
  })
  
  batch_groups <- reactiveVal(list()) # store the list of matched groups, each group is a list of selected_files()$info indices
  observeEvent(input$run_batch, {
    req(selected_files())
    patterns <- strsplit(input$batch_patterns, "\\n")[[1]]
    patterns <- trimws(patterns[patterns != ""])
    data <- selected_files()$info
    matched <- list()
    for (pat in patterns) {
      idx <- grep(pat, data$sample_name)
      if (length(idx) > 1) {
        matched[[pat]] <- idx
      } else {
        showNotification(paste("Pattern", pat, "matched", length(idx), "sets, which is unable to proceed."), type = "error")
      }
    }
    batch_groups(matched)
  })
  
  output$batchPanel <- renderUI({
    req(mode() == "batch")
    groups <- batch_groups()
    data <- selected_files()$info
    
    tabs <- lapply(names(groups), function(pat) {
      idx <- groups[[pat]]
      sets <- lapply(idx, function(i) data$values[[i]])
      names(sets) <- data$sample_name[idx]
      
      patid <- gsub("[^a-zA-Z0-9]", "_", pat)
      vennModule(patid, sets)
      tabPanel(
        pat, 
        uiOutput(NS(patid, "vennUI")),
        uiOutput(NS(patid, "vennplotUI"))
      )
    })
    
    do.call(tabsetPanel, tabs)
  })
  
  # Venn plot color selection UI
  output$vennColorUI <- renderUI({
    req(selected_files())
    data <- selected_files()$info
    used <- sapply(seq_len(nrow(data)), function(i) input[[paste0("use_", i)]])
    selected <- which(used)
    n_sets <- length(selected)
    setnames <- list(
      textInput("venn_setname1", "Set Name 1", value = data$sample_name[selected[1]] %||% "Set 1"),
      textInput("venn_setname2", "Set Name 2", value = data$sample_name[selected[2]] %||% "Set 2"),
      textInput("venn_setname3", "Set Name 3", value = data$sample_name[selected[3]] %||% "Set 3"),
      textInput("venn_setname4", "Set Name 4", value = data$sample_name[selected[4]] %||% "Set 4"),
      textInput("venn_setname5", "Set Name 5", value = data$sample_name[selected[5]] %||% "Set 5")
    )
    cols <- list(
      colourpicker::colourInput("venn_col1", "Circle Color 1", value = "black"),
      colourpicker::colourInput("venn_col2", "Circle Color 2", value = "black"),
      colourpicker::colourInput("venn_col3", "Circle Color 3", value = "black"),
      colourpicker::colourInput("venn_col4", "Circle Color 4", value = "black"),
      colourpicker::colourInput("venn_col5", "Circle Color 5", value = "black")
    )
    fills <- list(
      colourpicker::colourInput("venn_fill1", "Fill Color 1", value = "skyblue"),
      colourpicker::colourInput("venn_fill2", "Fill Color 2", value = "pink"),
      colourpicker::colourInput("venn_fill3", "Fill Color 3", value = "lightgreen"),
      colourpicker::colourInput("venn_fill4", "Fill Color 4", value = "lightyellow"),
      colourpicker::colourInput("venn_fill5", "Fill Color 5", value = "lightgray")
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
      fluidRow(lapply(setnames[seq_len(n_sets)], function(x) column(2, x))),
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
    renderVennExtraOptions(session$ns, length(selected))
  })
  
  # Prepare gene presence matrix
  venn_mat <- eventReactive(input$analyze, {
    req(selected_files())
    data <- selected_files()$info
    used <- sapply(seq_len(nrow(data)), function(i) input[[paste0("use_", i)]])
    names <- sapply(seq_len(nrow(data)), function(i) input[[paste0("sample_", i)]])
    selected <- which(used)
    req(length(selected) >= 1)
    gene_lists <- data$values[selected]
    names(gene_lists) <- names[selected]
    all_genes <- unique(unlist(gene_lists))
    gene_presence <- sapply(gene_lists, function(glist) all_genes %in% glist)
    gene_mat <- as.data.frame(gene_presence)
    rownames(gene_mat) <- all_genes
    gene_mat
  })
  
  # Centered Venn UI
  output$vennUI <- renderUI({
    fluidRow(
      column(12, div(style = "display: flex; justify-content: center; align-items: center;", plotOutput("vennPlot", height = paste0(input$venn_height, "px"), width = paste0(input$venn_width, "px"))))
    )
  })
  
  # Venn plot rendering
  output$vennPlot <- renderPlot({
    mat <- venn_mat()
    grid.newpage()
    vp <- generateVennPlot(mat, input)
    grid.draw(vp)
  })
  
  output$downloadVennPNG <- downloadHandler(
    filename = function() paste0("venn_plot_", Sys.Date(), ".png"),
    content = function(file) {
      mat <- venn_mat()
      if (ncol(mat) >= 2 && ncol(mat) <= 5) {
        png(file, width = input$venn_width, height = input$venn_height, res = 100)
        grid.newpage()
        vp <- generateVennPlot(mat, input)
        grid.draw(vp)
        dev.off()
      }
    }
  )
  output$downloadVennPDF <- downloadHandler(
    filename = function() paste0("venn_plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      mat <- venn_mat()
      if (ncol(mat) >= 2 && ncol(mat) <= 5) {
        pdf(file, width = input$venn_width / 100, height = input$venn_height / 100)  # inches
        grid.newpage()
        vp <- generateVennPlot(mat, input)
        grid.draw(vp)
        dev.off()
      }
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
