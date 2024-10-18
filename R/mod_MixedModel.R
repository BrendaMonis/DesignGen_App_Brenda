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
    fluidRow(style = "height:5000px",
             box(width = 12, 
                 p(HTML("On this page, you can perform analyses using mixed models. Before starting your analysis, you can filter your data to meet your requirements. Once your analysis is complete, you can review or export previous results for future reference.
                 <ul>
                  Please keep the following points in mind:
                 <ul>
                 <li>Follow each step and press the button at the end of each one;</li>
                 <li>If you select something incorrectly or wish to change, you can return to the specific section to modify it and proceed with the subsequent steps;</li>
                 <li>The 'sommer' package performs the analysis, so its syntax should be considered.</li>
                        </ul>"))
             ),
             
             # Choose the experiment design
             box(width = 12,
                 h4("Experiment Design"),
                 p("Please select the design used in your experimental data."), 
                 selectInput(
                   ns("design"), 
                   label = NULL,
                   choices = list("Randomized Complete Block Design" = "block", "Alpha Lattice Design" = "lattice"), 
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
                     uiOutput(ns("data_dynamic_view")),
                 ),
                 
                 # Read the file
                 hr(),
                 actionButton(ns("data_load"), "Load file", icon("file-text")),
                 br(),
                 h6("Click here to proceed to the next step.")
             ),
             
             #Select variables
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select Variables",
                 box(width = 6,
                     radioButtons(ns("trait"), label = p("Choose the traits to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("local"), label = p("Choose the location to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("corte"), label = p("Choose the harvest to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 )
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

                 actionButton(ns("run_analysis"), "Run analysis",icon("refresh")), br(),
                 p("Click here and then expand the 'Results' section to access the analyses.")
             ), hr(),
             
             # Results
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Results:",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Variance Components:",
                     DT::dataTableOutput(ns("varcomp_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "AIC and BIC",
                     DT::dataTableOutput(ns("aic_bic_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "BLUPs",
                    # box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Table Visualization",
                     DT::dataTableOutput(ns("blups_out"))
                 ),
                 # Download
                 #p("Click here to download the complete analysis data in '.RData' format.  
                  # Once you import this into R or RStudio, an object named 'mixedmodel' will be created, enabling you to work with it."),
                 #downloadButton(ns('download_rdata'), "Download .RData", class = "butt") 
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
# server part ----
mod_MixedModel_server <- function(input, output, session){
  ns <- session$ns
  ## download input
  output$data_example <- downloadHandler(
    filename =  function() {
      paste("example_data.csv")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  # Data processing using .txt and .csv files.
  observeEvent(input$read_data1, {
    observeEvent(input$data_input, {
      if (is.null(input$data_input)) {
        output$dataview <- renderTable({
          return(p("Upload your data"))
        })
      } else {
        df <- read.csv(input$data_input$datapath, sep = input$read_data1)
        output$dataview <- renderTable({
          return(head(df))
        })
      }
    })
  })  
  
  
  ## download pedigree
  output$pedigree_example <- downloadHandler(
    filename =  function() {
      paste("pedigree.csv")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_pedigree.csv", package = "StatGenESALQ"), row.names = 1, header = T)
      write.csv(dat, file = file)
    } 
  )
  
  button1 <- eventReactive(input$read_data, {
    if (is.null(input$data_input$datapath)) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
    } else {
      dat <- read.csv(input$data_input$datapath, sep = input$read_data1)
    }
    cat(colnames(dat))
    dat
  })
  
  observe({
    
    if(any(colnames(button1()) %in% "rep"))
      choices_trait_temp <- colnames(button1())[-c(1:4)] else
        choices_trait_temp <- colnames(button1())[-c(1:3)]
      
      #Choose the trait and the location   
      choices_trait <- choices_trait_temp
      names(choices_trait) <- choices_trait_temp
      
      choices_locations_temp <- unique(button1()[,"local"])
      choices_locations <- choices_locations_temp
      names(choices_locations) <- choices_locations_temp
      
      choices_corte_temp <- unique(button1()[,"corte"])
      choices_corte <- choices_corte_temp
      names(choices_corte) <- choices_corte_temp
      
      updateRadioButtons(session, "trait",
                         label="Choose the trait to be evaluated:",
                         choices = choices_trait,
                         selected = unlist(choices_trait)[1])
      
      updateCheckboxGroupInput(session, "local",
                               label="Choose the locations to be evaluated:",
                               choices = choices_locations)
      
      updateCheckboxGroupInput(session, "corte",
                               label="Choose the harvest to be evaluated:",
                               choices = choices_corte)
  })
  
  # defining the model as a factor
  button2 <- eventReactive(input$run_analysis, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button1()
      dat$block <- as.factor(dat$block)
      dat$gen <- as.factor(dat$gen)
      dat$local <- as.factor(dat$local)
      dat$corte <- as.factor(dat$corte)
      dat$peso <- as.double(dat$peso)
      
      if(input$design == "block"){
        if(!all(c("local", "block", "gen", "corte") %in% colnames(dat)) | ("rep" %in% colnames(dat)))
          stop(safeError("Randomized complete block design should have columns 'local', 'block' and 'gen'."))
        dat <- dat %>% select(c("local", "gen", "block", "corte",input$trait)) %>% 
          filter(local %in% input$local) %>% droplevels() #%>% 
         # filter(corte %in% input$corte) %>% droplevels()
        
      } else {
        if(!all(c("local", "block", "gen", "corte") %in% colnames(dat)))
          stop(safeError("Alpha lattice design should have columns 'local', 'block', 'rep', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        
        dat <- dat %>% select(c("local", "gen", "block","corte",input$trait)) %>%
          filter(local %in% input$local) %>% droplevels() #%>% 
         # filter(corte %in% input$corte) %>% droplevels()
        
        dat$local <- as.factor(dat$rep)
        #dat$corte <- as.factor(dat$rep)
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
      
      BLUPs <- data.frame(ID = names(mod$U$gen), BLUPs = mod$U$gen)
      
      incProgress(0.25, detail = paste("Doing part", 2))
      list(mod,summary_mod, aic_bic, BLUPs)
    })
  })
  
  output$varcomp_out <- DT::renderDataTable({
    data <- data.frame(button2()[[2]]$varcomp)
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    # columns_to_round <- c("Sum.Sq", "Mean.Sq", "F.value", "Pr..F.", "outra_coluna1", "outra_coluna2")
    decimal_places1 <- 4  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 1:3) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    # decimal_places1 <- 5
    # for (col in 5) {
    #   data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    # }
    
    # Outputs
    DT::datatable(data,  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  })
  
  output$aic_bic_out <- DT::renderDataTable(
    DT::datatable(data.frame(button2()[[3]]),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Brt',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$blups_out <- DT::renderDataTable(
    DT::datatable(data.frame(button2()[[4]]),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
}

## To be copied in the UI
# mod_MixedModel_ui("MixedModel_ui_1")

## To be copied in the server
# callModule(mod_MixedModel_server, "MixedModel_ui_1")
