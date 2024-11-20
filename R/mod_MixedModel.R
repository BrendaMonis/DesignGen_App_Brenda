#' MixedModel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

# ui part ----
mod_MixedModel_ui <- function(id){ 
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:8000px",
             box(width = 12, 
                 p(HTML("On this page, you can perform analyses using mixed models. Before starting your analysis, you can filter your data to meet your requirements. Once your analysis is complete, you can review or export previous results for future reference.
                 <ul>
                  Please keep the following points in mind:
                 <ul>
                 <li>Follow each step and press the button at the end of each one;</li>
                 <li>If you select something incorrectly or wish to change, you can return to the specific section to modify it and proceed with the subsequent steps;</li>
                 <li>The 'sommer' package performs the analysis, so its syntax should be considered.</li>
                        </ul>")),
             ),
             
             # Choose the experiment design
             box(width = 12,
                 h4("Experiment Design"),
                 p("Please select the design used in your experimental data."), 
                 selectInput(
                   ns("design"), 
                   label = NULL,
                   choices = list("Randomized Complete Block Design" = "block", "Alpha Lattice Design" = "lattice", "Split-Plot Design" = "Split"), 
                   selected = "block"
                 )
                 
             ),
             
             # Input the file
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input File",
                 p("If you don't have any data, you may download an example file to understand how the app works."),
                 downloadButton(ns("data_example")), hr(),
                 p("Please upload your data file here:"),
                 fileInput(ns("data_input"), label = h6("Select file: .csv .xls .txt"), multiple = F), hr(),
                 p("To proceed to the next step, please select the separator and then click the 'Load File' button 
                   to ensure that your uploaded file or the example file is processed correctly."),
                 
                 # Input Control
                 hr(),
                 box(width = 4,
                     radioButtons(ns("data_sep"), label = p("Choose the Separator:"),
                                  choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                  selected = ",") 
                 ), 
                 box(width = 8, 
                     uiOutput(ns("data_dynamic_view"))
                 ),
                 
                 # Read the file
                 hr(),
                 actionButton(ns("data_load"), "Load file", icon("file-text")),
                 br(),
                 h6("Click here to proceed to the next step.")
             ),
             
             # Data Filtering
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary", title = "Data Filtering",
                 box(width = 12,
                     radioButtons(ns("filter_choice"), label = p("Do you need to filter your data?"),
                                  choices = c("Yes", "No"),
                                  selected = "No"),
                     p("If you don't need to filter your data, just press the 'Data Filters' button and continue with the next steps.")
                 ),
                 
                 uiOutput(ns("filter_dynamic_factor")),
                 br(),
                 uiOutput(ns("filter_dynamic_button")),
                 br(),
                 uiOutput(ns("filter_dynamic_level")),
                 
                 actionButton(ns("filter_ready"), "Filter Data", icon("filter")),
                 br(),
                 h6("Click here to proceed to the next step")
             ),
             
             # Choose Parameters
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select parameters",
                 box(width = 4,
                     radioButtons(ns("trait"), label = p("Choose the trait to be evaluated:"),
                                  choices = "Press 'Filter Data' button to update",
                                  selected = "Press 'Filter Data' button to update"),
                 ),
                 box(width = 4,
                     radioButtons(ns("fixed_ef"), label = p("Choose the fixed effect to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update")
                 ),
                 box(width = 4,
                     radioButtons(ns("random_ef"), label = p("Choose the random effect to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update")
                 ),
                 hr(),
                 actionButton(ns("parameter_choice"), "Model Parameters", icon("check")),
                 br(),
                 h6("Click here to proceed to the next step")
             ),
             
             #Define the model
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Define the Model",
                 p("If you intend to include a pedigree matrix, please upload a .csv file following the example below."),
                 downloadButton(ns("pedigree_example")), 
                 hr(),
                 
                 h4("Pedigree Matrix"),
                 p("Please upload your pedigree file here:"),
                 fileInput("pedigree", label = NULL),
                 hr(),
                 
                 p("The analysis is performed using the 'sommer' package, so ensure your model adheres to its syntax. If you’ve uploaded a pedigree matrix, 
                   include it in the model using the symbol A. Additionally, make sure to use the exact column names from your dataset when defining variables in the model."),
                 p(HTML("For more details, please consult the
                        <a href= 'https://www.rdocumentation.org/packages/sommer/versions/4.1.2/topics/mmer' target = '_blank' > R documentation. </a>")),
                 hr(),
                 
                 textInput(ns("fixed"), label = p("Fixed:"), value = "Weight ~ Environment + Environment:Block"),
                 textInput(ns("random"), label = p("Random:"), value = "~ Genotype + Environment:Genotype"),
                 textInput(ns("rcov"), label = p("rcov:"), value = "~ units"), 
                 hr(),
                 
                 actionButton(ns("analysis_run"), "Run analysis",icon("refresh")), br(),
                 h6("Click here and then expand the 'Results' section to access the analyses.")
             ), hr(),
             
             # Results
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Results:",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Variance Components",
                     DT::dataTableOutput(ns("varcomp_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "AIC and BIC",
                     DT::dataTableOutput(ns("aic_bic_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "BLUPs",
                     DT::dataTableOutput(ns("blups_table_out"))
                 ),
                 
                 # -----------------------------
                 box(width = 12, solidHeader = TRUE, status="info", title = "Select the Random Effect Variables:", #
                     selectInput(
                       inputId = ns("genotypes"),
                       label = h6("Random Effect Variables:"), #
                       choices = "This will be updated",
                       selected = "This will be updated",
                       multiple = TRUE
                     ),
                     actionButton(ns("plot_ready"), "Run plots", icon("graph")),
                 ),
                 
                 box(id = ns("box_int"), width = 12, solidHeader = TRUE, collapsible = TRUE,  collapsed = FALSE, status="primary", title = actionLink(inputId = ns("intID"), label = "Interaction Graph"),
                     column(12,
                            column(3,
                                   tags$head(tags$style(".butt{background-color:#add8e6; border-color: #add8e6; color: #337ab7;}")),
                                   downloadButton(ns('bn_download'), "Download", class = "butt")
                            ),
                            column(3,
                                   radioButtons(ns("fformat"), "File type", choices=c("png","tiff","jpeg","pdf", "RData"), selected = "png", inline = T)
                            ),                     
                            column(2,
                                   numericInput(ns("width_int"), "Width (mm)", value = 180),
                            ),
                            column(2,
                                   numericInput(ns("height_int"), "Height (mm)", value = 120),
                            ),
                            column(2,
                                   numericInput(ns("dpi_int"), "DPI", value = 300)
                            )), br(), 
                     column(12,
                            hr(),
                            plotOutput(ns("plot_int"))
                            
                            # -----------------------------
                     )),
                 # Download
                 p("Click here to download the complete analysis data in '.RData' format.  
                    Once you import this into R or RStudio, an object named 'mixedmodel' will be created, enabling you to work with it."),
                 downloadButton(ns('download_rdata'), "Download .RData", class = "butt") 
             )
    )
  )
}

#' MixedModel Server Function
#'
#' @import sommer
#' 
#' @noRd 
#' 
#Server part ----
mod_MixedModel_server <- function(input, output, session){
  ns <- session$ns
  
  # Download input
  output$data_example <- downloadHandler(
    filename = function() {
      paste("example_data.csv")
    },
    
    content = function(file) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else if (input$design == "lattice") {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_split.csv", package = "StatGenESALQ"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  # Data processing using .csv .xls .txt files.
  observeEvent(input$data_sep, {
    output$data_dynamic_view <- renderUI({
      if (is.null(input$data_input)) {
        output$dataview <- renderUI({
          return(p("Please upload your file to update this section."))
        })
      } else {
        dat <- read.csv(input$data_input$datapath, sep = input$data_sep)
        output$dataview <- renderTable({
          return(head(dat))
        })
      }
    })
  })  
  
  # Download pedigree
  output$pedigree_example <- downloadHandler(
    filename = function() {
      paste("pedigree.csv")
    },
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_pedigree.csv", package = "StatGenESALQ"), row.names = 1, header = T)
      write.csv(dat, file = file)
    } 
  )
  
  # Data Loading  
  button1 <- eventReactive(input$data_load, {
    if (is.null(input$data_input$datapath)) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else if (input$design == "lattice") {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_split.csv", package = "StatGenESALQ"))
      }
    } else {
      dat <- read.csv(input$data_input$datapath, sep = input$data_sep)
    }
    cat(colnames(dat))
    dat
  })
  
  # Dynamic UI for filtering data
  observeEvent(input$data_load, {
    output$filter_dynamic_factor <- renderUI({
      req(input$filter_choice == "Yes")
      data_names <- colnames(button1())
      
      box(width = 12,
          checkboxGroupInput(ns("factor"), label = p("Choose the factors to be filtered:"),
                             choices = data_names,
                             selected = data_names[1])
      )
    })
    showNotification("Data loaded")
  })
  
  observeEvent(input$data_load, {
    output$filter_dynamic_button <- renderUI({
      req(input$filter_choice == "Yes")
      dat <- button1()
      
      actionButton(ns("filter_in_process"), "Select levels", icon("plus"))
    })
  })
  
  # Reactive filter data
  observeEvent(input$data_load, {
    output$filter_dynamic_level <- renderUI({
      req(input$filter_choice == "Yes")
      
      num <- length(input$factor)
      col_names <- input$factor
      
      lapply(seq_len(num), function(i) {
        box(width = 12,
            checkboxGroupInput(ns(paste0("filter", i)),
                               label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                               choices = "Press 'Select levels' button to update")
        )
      })
    })
  })
  
  observeEvent(input$filter_in_process, {
    req(input$filter_choice == "Yes")
    
    dat <- button1()
    if (length(input$factor) > 0) {
      n <- length(input$factor)
      for (i in 1:n) {
        dat[[input$factor[i]]] <- as.factor(dat[[input$factor[i]]])
      }
    }
    
    num <- length(input$factor)
    col_names <- input$factor
    
    lapply(seq_len(num), function(i) {
      if(is.factor(dat[[input$factor[i]]])) {
        box(width = 12,
            updateCheckboxGroupInput(session, paste0("filter", i),
                                     label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                                     choices = unique(dat[[input$factor[i]]]))
        )
      }
    })
  })
  
  # Data Filtering
  button2 <- eventReactive(input$filter_ready, {
    if(input$filter_choice == "Yes") {
      dat <- button1()
      if (length(input$factor) > 0) {
        n <- length(input$factor)
        for (i in 1:n) {
          dat[[input$factor[i]]] <- as.factor(dat[[input$factor[i]]])
        }
      }
      
      num <- length(input$factor)
      col_names <- input$factor
      
      for (i in 1:num) {
        dat <- dat %>%
          filter(dat[[input$factor[i]]] %in% c(input[[paste0("filter", i)]])) %>%
          droplevels()
      }
      dat
    } else {
      dat <- button1()
      dat
    }
  })
  
  # Update choices for analysis
  observeEvent(input$filter_ready, {
    data_names <- colnames(button1())
    
    updateRadioButtons(session, "trait",
                       label="Choose the trait to be evaluated:",
                       choices = data_names,
                       selected = unlist(data_names)[1])
    
    updateRadioButtons(session, "fixed_ef",
                       label="Choose the fixed effect to be evaluated:",
                       choices = data_names)
    
    updateRadioButtons(session, "random_ef",
                       label="Choose the random effect to be evaluated:",
                       choices = data_names)
  })
  
  observeEvent(input$parameter_choice, {
    showNotification("Parameters chosen")
  })
  
  # -----------------------------
  
  observeEvent(input$analysis_run, {
    dat <- button1()
    gen_choices <- as.list(unique(dat[[input$random_ef]]))
    names(gen_choices) <- unique(dat[[input$random_ef]])
    
    updateSelectInput(session = session,
                      inputId = "genotypes",
                      label = "Random Effect Variables:",
                      choices = gen_choices,
                      selected=unlist(gen_choices)[1])
  })
  
  ##### -----------------------------
  
  # Analysis function
  button3 <- eventReactive(input$analysis_run, {
    withProgress(message = 'Building analysis', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      req(input$parameter_choice)
      dat <- button2()
      
      if (ncol(dat) > 0) {
        for (col_name in colnames(dat)) {
          if (col_name == input$trait) {
            dat[[col_name]] <- as.double(dat[[col_name]])
          } else {
            dat[[col_name]] <- as.factor(dat[[col_name]])
          }
        }
      }
      
      if(!is.null(input$pedigree)) A <- read.csv(input$pedigree$datapath, row.names = 1, header = T)
      
      # Input the model
      mod <- mmer(fixed = as.formula(input$fixed), 
                  random = as.formula(input$random), 
                  rcov = as.formula(input$rcov),
                  data = dat)
      
      # Results
      summary_mod <- summary(mod)
      aic_bic <- data.frame(AIC = mod$AIC, BIC = mod$BIC)
      BLUPs <- data.frame(ID = levels(dat[[input$random_ef]]), BLUPs = mod$U[[input$random_ef]])
      rownames(BLUPs) <- NULL
      
      incProgress(0.25, detail = paste("Doing part", 2))
      list(mod, summary_mod, aic_bic, BLUPs)
    })
  })
  
  # Output for variance components
  output$varcomp_out <- DT::renderDataTable({
    dat <- data.frame(button3()[[2]]$varcomp)
    
    # Rounding of numbers
    decimal_places <- 2  
    for (col in 1:3) {
      dat[[col]] <- round(as.numeric(dat[[col]]), decimal_places)
    }
    
    DT::datatable(dat,  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) 
                  ),
                  class = "display")
  })
  
  # Output for AIC and BIC
  output$aic_bic_out <- DT::renderDataTable({
    dat <- data.frame(button3()[[3]])
    
    # Rounding of numbers
    decimal_places <- 2  
    for (col in 1:2) {
      dat[[col]] <- round(as.numeric(dat[[col]]), decimal_places)
    }
    
    DT::datatable(dat,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Brt',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) 
                  ),
                  class = "display")
  })
  
  # Output for BLUPs - Table
  output$blups_table_out <- DT::renderDataTable({
    dat <- data.frame(button3()[[4]])  
    
    # Rounding of numbers
    decimal_places <- 2  
    for (col in 2) {
      dat[[col]] <- round(as.numeric(dat[[col]]), decimal_places)
    }
    
    DT::datatable(dat,
                  rownames = TRUE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) 
                  ),
                  class = "display")
  })    
  
  # -----------------------------
  
  output$plot_int <- renderPlot({
    
    req(input$plot_ready)
    
    withProgress(message = 'Working:', value = 0, {
      incProgress(0.3, detail = "building graphic...")
      
      dat <- button1()
      GEI <- dat %>%
        group_by(.data[[input$fixed_ef]], .data[[input$random_ef]]) %>%
        summarise(
          Trait = mean(.data[[input$trait]], na.rm = TRUE),
          .groups = "drop"
        )
      print(GEI)
      
      # Ordering
      index <- order(GEI$Trait, decreasing = FALSE)
      GEI <- GEI[index,]
      
      ggplot(GEI, aes(x = .data[[input$fixed_ef]], y = Trait)) +
        geom_line(
          linewidth = 0.8, 
          aes(
            group = .data[[input$random_ef]], 
            color = .data[[input$random_ef]], 
            alpha = ifelse(.data[[input$random_ef]] %in% input$genotypes, 1, 0.7)
          )
        ) +
        labs(
          title = "Graphic Visualization of Interaction",
          x = input$fixed_ef,  
          y = paste("Average", input$trait)
        ) +
        guides(
          color = guide_legend(title = input$random_ef, ncol = 1), 
          alpha = "none"
        ) +
        theme_bw() +
        scale_color_manual(
          values = scales::hue_pal()(length(unique(input$genotypes))),
          limits = input$genotypes,
          breaks = input$genotypes
        )
      
      
    })
  })
  
  # Output Graph
  output$mean_graph_out <- renderPlot({
    dat <- data.frame(button3()[[5]])
    
    observe({
      random_effects <- names(dat)  
      updateSelectInput(session, "random_ef", 
                        choices = random_effects, 
                        selected = NULL)  
    })
    
    observeEvent(input$filter_random_ready, {
      req(input$random_ef)  
      
      gen_var <- input$random_ef
      dat_filtered <- dat[dat[[gen_var]] %in% input$random_ef, ]
      
      x_var <- input$fixed_ef   
      y_var <- input$trait 
      
      dat[[x_var]] <- as.factor(dat[[x_var]])  
      dat[[y_var]] <- as.numeric(dat[[y_var]])  
      
      ggplot(dat, 
             aes_string(x = dat[[x_var]], y = dat[[y_var]])) +
        geom_line(linewidth = 0.8, aes(group = gen_var, color = gen_var, 
                                       alpha = ifelse(dat[[gen_var]] %in% input$selected_vars, 1, 0.7))) +
        labs(title = "Graphic Visualization of Interaction",
             x = input$fixed_ef,  
             y = input$trait) +
        guides(color = guide_legend(title = "Selected Random Effect", ncol = 1), alpha = "none") +
        theme_bw() +
        scale_color_manual(values = colors,
                           limits = input$selected_vars,
                           breaks = input$selected_vars)
    })
  })
  
  # -----------------------------
  
  # Download results
  r_downloadname <- reactive({
    seed <- sample(1:10,1)
    filename <- paste0("mixedmodel","_",seed,".RData")
    return(filename)
  })
  
  r_download <- function() {
    mixedmodel <- button3()[[1]]
    save(mixedmodel, file = r_downloadname())
  }
  
  # download handler
  observeEvent(input$analysis_run, {
    output$download_rdata <- downloadHandler(
      filename = r_downloadname,
      content = function(file) {
        r_download()
        file.copy(r_downloadname(), file, overwrite=T)
        file.remove(r_downloadname())
      }
    )
  })
}


## To be copied in the UI
# mod_MixedModel_ui("MixedModel_ui_1")

## To be copied in the server
# callModule(mod_MixedModel_server, "MixedModel_ui_1")
