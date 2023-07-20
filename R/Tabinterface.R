
# Fonction ui du module ----
#id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO, summaryfunction= compute_suitability_DENTRO
moduleTabInterface_UI <- function(id, data, interface) {
  
  # Crée une fonction ns qui va coller l'Id du module avec l'Id des inputs/outputs (séparation avec un tiret)
  # Permet d'avoir des Id uniques dans toute l'application pour les inputs/outputs
  # Appliquer la fonction ns à tous les inputId / outputId
  ns <- NS(id)
  
  
  # Il faut encapsuler l'interface dans un tagList
  tagList(
    fluidRow(column(width=6,
                    box(title = "Your site",
                        solidHeader = TRUE,
                        status="danger",
                        width=NULL,
                        uiOutput(ns("dynamicControlsResponse"))
                    )),
             column(width=6,
                    box(title = "Your objectives",
                        solidHeader = TRUE,
                        status="primary",
                        width=NULL,
                        uiOutput(ns("dynamicControlsEffect"))
                    ))
    ),
    fluidRow(wellPanel(
      class = "custom-well-panel5",
      style = "display: flex; justify-content: center;",
      # Fourth panel (full width)
      # box(title="For debugging",
      #     solidHeader = TRUE,
      #     status="danger",
      #     width=NULL,
      #     fluidRow(verbatimTextOutput(ns("controlOutput")))),
      actionButton(inputId = ns("ab_compute"), label="Compare trees !")
      
    )),
    
    box(title = "All trees",
        solidHeader = TRUE,
        status="warning",
        width=NULL,
        fluidRow(
          column(width=12,
                 radioButtons(inputId=ns("orderby"), label="Order By", 
                              choices=c(Adaptation="responsetrait", Efficiency="effecttrait"),
                              selected="responsetrait", inline=TRUE),
                 plotOutput((ns("barplot_suitability")))
                            
                 ),
          column(width=12,
                 DTOutput(outputId = ns("DTSuitability"))
          )
        ))
    
  ) # fin tagList
  
} # fin moduleTabInterface_UI


# Fonction server du module ----
#language is a reactive value from the main app
moduleTabInterface_Server <- function(id, language, data = dataDENTRO, interface= interfaceDENTRO, functionSuitability=compute_suitability_DENTRO) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns # utile si renderUI
      
      ## reactives ----
      
      # Reactive expression to capture control names and values
      controlData <- reactive({
        controls <- reactiveValuesToList(input)
        theoreticalcontrols<-compactcontrols()
        theoreticalcontrols<-theoreticalcontrols[theoreticalcontrols$side %in% c("effecttrait", "responsetrait"), "criteria"]
        names(controls) <- sub(paste0("^", ns("")), "", names(controls))
        controls<-controls[names(controls) %in% theoreticalcontrols]
        controls
      })#controlData() is a named list of the effecttrait and responsetrait controls (TRUE/FALSE for checkboxes, value for SeletInput)
      
      
      #reactive to define the controls of the module (site (=> responsetraits) and objectives (=> effecttraits))
      compactcontrols<-reactive({
        reshapecontrols(interface, language=language())
      }) # compactcontrols() is a dataframe of controls (object type, criteria (for site controls) or BigCriteria (for objective controls) (=object name), and their labels, and the choices (for SelectInput and CheckboxGroup)
      observe(print(str(head(compactcontrols()))))
      
      #reactive value of formatted inputs for computing suitability
      reformattedinputs<-reactive({
        allinputs <- controlData()
        #transform 
        checkboxyes<-sapply(allinputs, function (x) (is.logical(x) & as.logical(x)))
        allinputs<-unlist(allinputs)
        allinputs[checkboxyes]<- names(allinputs)[checkboxyes]
        allinputs<-allinputs[allinputs!="FALSE"]
        return(allinputs)
      })#reformattedinputs() is a named character of choices of the user (with the checkbox name for TRUE checkboxes)
      #observe(print(str(reformattedinputs())))
      # output$controlOutput <- renderPrint({
      #   # Only update controlOutput if the button is clicked
      #   #req(buttonClicked())
      #   reformattedinputs()
      # })
      
      # reactive of radiobutton to reorder the data
      orderby<-reactive(
        return(input$orderby)
      )
      
      # reactive indicating if the compute button has been cliqued
      buttonClicked <- reactiveVal(FALSE)
      observeEvent(input$ab_compute, {
        # Update the buttonClicked reactive value when the button is clicked
        buttonClicked(TRUE)
      })
      
      
      datatoplot<-reactive({
        allinputs<-reformattedinputs() 
        orderby<-input$orderby
        #print(orderby)
        #print(allinputs)
        if(length(allinputs)>0){
          message(paste("computing suitability graph with"), paste(names(allinputs), collapse=" "))
          #dfSuitability<-data.frame(species="icicici", side="responsetrait", value=1, BigCriteria="debugging")
          
          dfSuitability<-functionSuitability(inputsdata=allinputs, interface=interface, database=data,
                                             orderby = orderby)
        } else{
          dfSuitability<-data.frame(species="no data yet", side="responsetrait", value=1, BigCriteria="please describe your site and objectives")
        }
        #order of the bars in ggplot is determined by species (ordered factor), but for the DT, we need to also sort the rows
        #browser()
        dfSuitability<-dfSuitability[order(
          dfSuitability$species, 
          factor(dfSuitability$side, levels=c("responsetrait", "effecttrait")), 
          dfSuitability$BigCriteria),]
        #print(str(dfSuitability))
        return(dfSuitability)
      }) %>% bindEvent(input$ab_compute, input$orderby) # datatoplot() is a data frame of trees, BigCriteria and values

      
      
      
      
      
      ## dynamic ui ----
      
      ### Generate dynamic UI controls only at initiation (in english) ----
      #after that, when just the language is modified, we use update... to simply update the labels without redrawing the whole controls
      output$dynamicControlsResponse <- renderUI({
        message("init dynamicControlsResponse")
        #print(str(compactcontrols()))
        #initcompactcontrols<-reshapecontrols(interface, language="en")
        initcompactcontrols<-compactcontrols()
        responsecontrols<-which(initcompactcontrols$side=="responsetrait")
        #browser()
        #print(responsecontrols)
        controls_list <- lapply(responsecontrols, function(i) {
          control_type <- initcompactcontrols$objecttype[i]
          input_id <- ns(initcompactcontrols$criteria[i])
          #message(paste("creation", input_id))
          choices <- strsplit(initcompactcontrols$choice[i], ",")[[1]]
          labchoices<-strsplit(initcompactcontrols$labelchoice[i], ",")[[1]]
          names(choices)<-labchoices
          #print(choices)
          labelinput<-initcompactcontrols$labelcriteria[i]
          
          control <- switch(
            control_type,
            checkboxInput=checkboxInput(input_id, label = labelinput),
            numericInput=numericInput(input_id, label = labelinput, value = 0),
            selectInput=selectInput(input_id, label = labelinput, choices = choices),
            checkboxGroupInput=checkboxGroupInput(input_id, label = labelinput, choices = choices)
            # Add more control types as needed
          )
          
          column(width = 6, control)
        })
        
        tagList(controls_list)
      }) # end create dynamic controls for user site constraints
      
      output$dynamicControlsEffect <- renderUI({
        message("init dynamicControls Effect")
        #print(str(compactcontrols()))
        initcompactcontrols<-compactcontrols()
        responsecontrols<-which(initcompactcontrols$side=="effecttrait")
        #print(responsecontrols)
        #browser()
        controls_list <- lapply(responsecontrols, function(i) {
          control_type <- initcompactcontrols$objecttype[i]
          input_id <- ns(initcompactcontrols$criteria[i])
          #message(paste("creation", input_id))
          choices <- strsplit(initcompactcontrols$choice[i], ",")[[1]]
          labchoices<-strsplit(initcompactcontrols$labelchoice[i], ",")[[1]]
          names(choices)<-labchoices
          #print(choices)
          labelinput<-initcompactcontrols$labelcriteria[i]
          
          control <- switch(
            control_type,
            checkboxInput=checkboxInput(input_id, label = labelinput),
            numericInput=numericInput(input_id, label = labelinput, value = 0),
            selectInput=selectInput(input_id, label = labelinput, choices = choices),
            checkboxGroupInput=checkboxGroupInput(input_id, label = labelinput, choices = choices)
            # Add more control types as needed
          )
          
          column(width = 6, control)
        })
        #print(str(controls_list))
        tagList(controls_list)
      }) # end create dynamic controls for user objectives
      
      ### update dynamic ui in case of language selection----
      observeEvent(compactcontrols(), {
        message("update dynamicControlsResponse")
        goodtranslations<-compactcontrols()
        responsecontrols<-which(goodtranslations$side=="responsetrait")
        lapply(responsecontrols, function(i) {
          #print(goodtranslations[i,])
          control_type <- goodtranslations$objecttype[i]
          input_id <- ns(goodtranslations$criteria[i])
          current_value<-input[[input_id]]
          choices <- strsplit(goodtranslations$choice[i], ",")[[1]]
          labchoices<-strsplit(goodtranslations$labelchoice[i], ",")[[1]]
          names(choices)<-labchoices
          #print(choices)
          labelinput<-goodtranslations$labelcriteria[i]
          
          
          switch(
            control_type,
            checkboxInput=updateCheckboxInput(session, input_id, label = labelinput, value=current_value),
            numericInput=updateNumericInput(session, input_id, label = labelinput, value=current_value),
            selectInput=updateSelectInput(session, input_id, label = labelinput, choices=choices, selected  = current_value),
            checkboxGroupInput=updateCheckboxGroupInput(session,input_id, label = labelinput, choices = choices)
            # Add more control types as needed
          )
        })
        
        
        message("update dynamicControlsEffect")
        goodtranslations<-compactcontrols()
        responsecontrols<-which(goodtranslations$side=="effecttrait")
        lapply(responsecontrols, function(i) {
          #print(goodtranslations[i,])
          control_type <- goodtranslations$objecttype[i]
          input_id <- ns(goodtranslations$criteria[i])
          current_value<-input[[input_id]]
          choices <- strsplit(goodtranslations$choice[i], ",")[[1]]
          labchoices<-strsplit(goodtranslations$labelchoice[i], ",")[[1]]
          names(choices)<-labchoices
          #print(choices)
          labelinput<-goodtranslations$labelcriteria[i]
          
          
          switch(
            control_type,
            checkboxInput=updateCheckboxInput(session, input_id, label = labelinput, value=current_value),
            numericInput=updateNumericInput(session, input_id, label = labelinput, value=current_value),
            selectInput=updateSelectInput(session, input_id, label = labelinput, choices=choices, selected  = current_value),
            checkboxGroupInput=updateCheckboxGroupInput(session,input_id, label = labelinput, choices = choices)
            # Add more control types as needed
          )
          
        })
      }, ignoreInit=TRUE) #end update dynamic controls according to language
      
      
      
      ## barplot ----
      output$barplot_suitability <- renderPlot({
        print("plot")
        #  setdiff(names(allinputs), c("orderby", "sidebarCollapsed", "ab_compute", "in_language", "sidebarItemExpanded"))]
        #browser()
        plot_Suitability<-ggplot(datatoplot(), aes(x = value, y = species, fill = BigCriteria)) +
          #geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = "#ffcccc", alpha = 0.5) +
          #geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#ccccff", alpha = 0.5) +
          geom_bar(stat = "identity", position = "stack") +
          geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 1.5)+
          #scale_fill_manual(values = colors) +
          theme_minimal() +
          #theme(legend.position = "none" )+
          labs(x = "Adaptation                         Efficiency", y = "Species")
        return(plot_Suitability)
      })
      
      ## DT table----
      output$DTSuitability <- renderDT({
        datatoplot()
      })
      
      
      
      
      
      
      
      
      
      
      
    } # fin function(input, output, session)
    
  ) # fin moduleServer
  return(moduleServer)
} # fin moduleTabInterface_Server
