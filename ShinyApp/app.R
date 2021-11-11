# BD4QoL - ShinyApp to improve data exploration and resources description
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(readxl)
library(haven)
library(dplyr)
library(tidyselect)
library(scales)
library(table1)
library(Dict)


if(Sys.info()[['sysname']]=="Windows"){
    path_bristol <- "N:/durable/file-import/p1402-member-group/Bristol/"
    path_italy <- "N:/durable/file-import/p1402-member-group/Italy/"
    path_mainz <- "N:/durable/file-import/p1402-member-group/Mainz/"
    addResourcePath(prefix = 'pics', directoryPath = "M:/p1402-mauricim/Documents/bd4qol/app/BD4QoL")
} else{
    path_bristol  <- "/tsd/p1402/data/durable/file-import/p1402-member-group/Bristol/"
    path_italy <-"/tsd/p1402/data/durable/file-import/p1402-member-group/Italy/"
    path_mainz <- "/tsd/p1402/data/durable/file-import/p1402-member-group/Mainz/translated_data/"
    addResourcePath(prefix = 'pics', directoryPath = '/tsd/p1402/home/p1402-mauricim/Documents/bd4qol/app/BD4QoL')
    }



bristol <- 'HN057 - HN5000 V2.6 Dataset 180821.xlsx'
bd4qol <- 'BD4QoL_hn5000_280921.csv'
milan <- 'BD4QoL historical study - INT fully anonimized.xlsx'
umm <- c("umm01_en.sav", "umm02_en.sav", "umm03_en.sav", "umm04_en.sav", "umm05_en.sav", "umm06_en.sav")


datafull <- Dict$new(
    BD4QoL = paste0(path_bristol, bd4qol),
    Bristol = paste(path_bristol, bristol, sep=""),
    Milan = paste(path_italy, milan, sep=""),
    UMM1 = paste(path_mainz, umm[1], sep=""),
    UMM2 = paste(path_mainz, umm[2], sep=""),
    UMM3 = paste(path_mainz, umm[3], sep=""),
    UMM4 = paste(path_mainz, umm[4], sep=""),
    UMM5 = paste(path_mainz, umm[5], sep=""),
    UMM6 = paste(path_mainz, umm[6], sep=""),
    .class = "character",
    .overwrite = TRUE
)

tumorLocation <- Dict$new(
    C00 = "Oral cavity", 
    C02 = "Oral cavity", 
    C03 = "Oral cavity", 
    C04 = "Oral cavity", 
    C05 = "Oral cavity", 
    C06 = "Oral cavity",
    C01 = "Oropharynx", 
    C02.4 = "Oropharynx", 
    C05.1 = "Oropharynx", 
    C05.2 = "Oropharynx", 
    C05.8 = "Oropharynx", 
    C09 = "Oropharynx", 
    C10 = "Oropharynx",
    C11 = "Nasopharynx",
    C12 ="Hypopharynx", 
    C13="Hypopharynx",
    C10.1 = "Larynx", 
    C32.0 = "Larynx",
    C73 = "Thyroid",
    C30 = "Nasal cavity",
    C31 = "Sinuses",
    C07 = "Salivary glands", 
    C08 = "Salivary glands",
    C14.0 = "Other", 
    C30.1 = "Other", 
    C41.1 = "Other", 
    C69.5 = "Other",
    C80 ="Primary of unknown origin",
    .class = "character",
    .overwrite = TRUE
)

figure_size <- Dict$new(
    BD4QoL=800,
    Bristol=800,
    Milan=2400,
    UMM1=2400,
    UMM2=2400,
    UMM3=2400,
    UMM4=2400,
    UMM5=2400,
    UMM6=2400,
    .class = "numeric",
    .overwrite = TRUE
)
datasets <- c("BD4QoL", "Bristol", "Milan", "UMM1", "UMM2", "UMM3", "UMM4", "UMM5", "UMM6")


getTumorLocation <- function(ctn_location){
    
    grouped_loc <- tumorLocation[ctn_location]
    
    if(is.null(grouped_loc)){
        grouped_loc = ctn_location
    }
    
    return(grouped_loc)
}
getTumorLocation <- Vectorize(getTumorLocation)


#covariates <- df %>% select(-not_explanatory) %>% names(.)

#dependent_vars <-  df %>% select(-covariates$covariates) %>% names(.)
#dependent_vars <- data.frame(dependent_vars)

theme_ <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
theme_2 <- theme(panel.grid.major.x = element_line( size=.1, color="black" ), panel.background = element_blank(), axis.line = element_line(colour = "black"))

extractVarNames <- function(tb){
    varNameText <- ""
    for (varName in tb) {
        varNameText  <- paste(varNameText, varName, sep="<br/>")
    }
    
    return(varNameText)
}

count_miss <- function(df){
    
    missingness <- data.frame(matrix(ncol=2, nrow=ncol(df)))
    colnames(missingness) <- c("cov", "missing")
    missingness$cov <- names(df)
    
    
    n <- 1
    for ( column in names(df)){
        missingness[['missing']][n] <- 100*(sum(grepl("Missing",df[[column]])) + sum(grepl("Unknown",df[[column]])) + sum(is.na(df[[column]])))/nrow(df)
        
        #if(class(df[[column]])=="numeric"){
        #    missingness[['missing']][n] <- 100*sum(is.na(df[[column]]))/nrow(df)
        #}
        n <- n + 1
    }
    
    #missingness <- missingness %>% filter(missing>0|missing==100)
    return(missingness)
}







# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    navbarPage(HTML('<img src="pics/logo.jpeg", height="72px" style="float:left"/> '),
               
               tabPanel("Main",
                        
                        
                        
                        # Application title
                        titlePanel(""),
                        
                        helpText("Living after Head and Neck Cancer: Monitor, Involve, Empower"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            
                            
                            sidebarPanel(
                                
                                selectInput(inputId = "dataset",
                                            label = "Select dataset:",
                                            choices = datasets),
                                
                                selectInput(inputId = "variables",
                                            label = "Variables:",
                                            choices = ""),
                                
                                selectInput(inputId = "timepoint", 
                                            label = "Time point:",
                                            choices = c("hn1", "hn2", "hn3", "hn4")),
                                
                                selectInput(inputId = "filter2tp", 
                                            label = "Show data with both hn3 and hn4 c30 scores:",
                                            choices = c("No", "Yes")),
                                
                                
                            ),
                            
                            # Show a plot of the generated distribution
                            
                            mainPanel(
                                uiOutput('variables'),
                                tabsetPanel(
                                    tabPanel("Plot", plotOutput("plot1")), 
                                    tabPanel("Variable summary", htmlOutput("textvar")),
                                    tabPanel("Missingness", plotOutput("plot2")),
                                    tabPanel("PRISMA", imageOutput("prisma"))
                                )
                            )
                            
                        )
               ),
               tabPanel("Cohort Description",
                        HTML("</br> </br>"),
                        htmlOutput("cohort_title"),
                        htmlOutput("cohort_description")
               ),       
               
               tabPanel("Summary",
                        HTML("</br> </br>"),
                        verbatimTextOutput("data_summary")
               ),
               
               tabPanel("Data",
                        HTML("</br> </br>"),
                        DT::dataTableOutput("table"))
    )
    
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    getVariables <- reactive({
        df <- dataInput()
        variables <- df  %>% names(.)
        variables <- data.frame(variables)
        variables < variables$variables
        return(variables)
    })
    
    X <- reactive({
        x <- input$variables
        x
    })
    
    dataInput <- reactive({
        
        if (grepl("UMM",input$dataset)){
            df <- read_sav(datafull[input$dataset])
            df <- haven::as_factor(df)
        }
        else if (grepl("Milan",input$dataset)){
            df <- read_excel(datafull[input$dataset])
            df <- df %>% mutate(hn1_ICD_group_conf = getTumorLocation(ctn_Anatomical_Tumor_Location) )
        }
        else if (grepl("Bristol", input$dataset)) {
            df <- read_excel(datafull[input$dataset])
        
        }
        else{
            df <- read.csv(datafull[input$dataset])
            if(input$filter2tp == "Yes"){
                df <- df %>% filter(!is.na(hn4hn3_dv_c30_summary))
                }
        }
        
        
        return(df)
        
    })
    
    observe({
        updateSelectInput(session, "variables",
                          choices = getVariables()
        )})
    
    
    output$plot1 <-renderPlot({
        df <- dataInput()
        #covariates <- 
        
        
        ggplot(df, 
               aes_string(x=X()))+geom_bar(aes(y = (..count..)/sum(..count..)), fill='#2b5378') +
            scale_y_continuous(labels = percent) + theme_ + theme(text = element_text(size=20), axis.text.x = element_text(angle = 60, hjust=1))  +
            labs(y="%")}, height = 800, width = 800)
    
    output$textvar<- renderUI({df <- dataInput()
    summarytab <- table1(~eval(as.name(X())) , data=df)
    HTML(summarytab)})
    
    output$plot2 <- renderPlot({
        df <- dataInput()
        if(grepl("Bristol", input$dataset) | grepl("BD4QoL", input$dataset) ){
            df <- df %>%select(vars_select(names(df), starts_with(input$timepoint, ignore.case = TRUE)))
        }
        missingness <- count_miss(df)
        ggplot(missingness) + geom_col(aes(x=reorder(cov, missing), y=missing),fill='#2b5378') + coord_flip() + 
            theme_2 + 
            theme(text = element_text(size=20)) +
            labs(x="Variables", y="%")
        
    }, height = 2400, width =800) #2400 for mainz data and others
    
    output$prisma <-   renderImage({
        list(src = "prisma.png",
             contentType = 'image/png',
             width = 600,
             height = 700,
             alt = "This is alternate text") }, deleteFile=FALSE)
    
    output$cohort_title <- renderUI({
        HTML(paste("<h1>", input$dataset, "</h1> </br> </br>", sep=""))
    })
    
    output$cohort_description <- renderTable({
        df <-  dataInput()
        
        if(grepl("Bristol", input$dataset) | grepl("BD4QoL", input$dataset)){
            table1(~ hn1_ICD_group_conf  | hn1_TNM_stage_best, data=df)}
        else if(grepl("Milan", input$dataset)){
            table1(~ hn1_ICD_group_conf  | ctn_TNM_cT_7Edition, data=df)}
        else{
            table1(~ dok32  | dok33, data=df)
        }
        
    }, sanitize.text.function = function(x) x)
    
    output$data_summary <- renderPrint({
        df <-  dataInput()
        summary(df)
    })
    
    output$table <- DT::renderDataTable({
        df <-  dataInput()
        DT::datatable(df)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
