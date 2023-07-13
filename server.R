#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram



shinyServer(function(input, output, session) {
  
  ## reactives ----
  compactcontrols<-reactive({
    print("reactive compact controls")
    reshapecontrols(interface, language=input$in_language)
  }) # compact controls is a dataframe of controls (object type, criteria (for site controls) or BigCriteria (for objective controls) (=object name), and their labels, and the choices (for SelectInput and CheckboxGroup)
  
  allinputs<-reactive({
    print("reactive allinputs")
    allinputs<-reactiveValuesToList(input)
    allinputs<-allinputs[names(allinputs) %in% unlist(interface[,c("BigCriteria", "criteria", "choice")])]
    #transform 
    checkboxyes<-sapply(allinputs, function (x) (is.logical(x) & as.logical(x)))
    allinputs<-unlist(allinputs)
    allinputs[checkboxyes]<- names(allinputs)[checkboxyes]
    allinputs<-allinputs[allinputs!="FALSE"]
    print(length(allinputs))
    allinputs
  })|> bindEvent(input$ab_compute) #allinputs is a named character of choices of the user
  
  
  datatoplot<-reactive({
    print("reactive datatoplot")
    allinputs<-allinputs()
    if(length(allinputs)>0){
      message(paste("computing suitability graph with"), paste(names(allinputs), collapse=" "))
      dfSuitability<-compute_suitability(inputsdata=allinputs, interface=interface, dataDENTRO=dataDENTRO,
                                orderby = input$orderby)
    } else{
      dfSuitability<-data.frame(species="no data yet", value=1, BigCriteria="please describe your site and objectives")
    }
    #order of the bars in ggplot is determined by species (ordered factor), but for the DT, we need to also sort the rows
    #browser()
    dfSuitability<-dfSuitability[order(
      dfSuitability$species, 
      factor(dfSuitability$side, levels=c("responsetrait", "effecttrait")), 
      dfSuitability$BigCriteria),]
    print(str(dfSuitability))
    dfSuitability
  }) # datatoplot is a data frame of trees, BigCriteria and values
  #observe(print(str(head(compactcontrols()))))
  
  ## dynamic ui ----
  # Generate dynamic UI controls only at initiation (in english)
  #after that, when just the language is modified, we use update... to simply update the labels without redrawing the whole controls
  output$dynamicControlsResponse <- renderUI({
    message("init dynamicControlsResponse")
    #print(str(compactcontrols()))
    initcompactcontrols<-reshapecontrols(interface, language="en")
    responsecontrols<-which(initcompactcontrols$side=="responsetrait")
    #print(responsecontrols)
    controls_list <- lapply(responsecontrols, function(i) {
      control_type <- initcompactcontrols$objecttype[i]
      input_id <- initcompactcontrols$criteria[i]
      message(paste("creation", input_id))
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
    initcompactcontrols<-reshapecontrols(interface, language="en")
    responsecontrols<-which(initcompactcontrols$side=="effecttrait")
    #print(responsecontrols)
    controls_list <- lapply(responsecontrols, function(i) {
      message("creation", initcompactcontrols$criteria[i])
      control_type <- initcompactcontrols$objecttype[i]
      input_id <- initcompactcontrols$criteria[i]
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
  
  ### update dynamic ui ----
  observeEvent(compactcontrols(), {
    message("update dynamicControlsResponse")
    goodtranslations<-compactcontrols()
    responsecontrols<-which(goodtranslations$side=="responsetrait")
    lapply(responsecontrols, function(i) {
      #print(goodtranslations[i,])
      control_type <- goodtranslations$objecttype[i]
      input_id <- goodtranslations$criteria[i]
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
      input_id <- goodtranslations$criteria[i]
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
  }, ignoreInit=TRUE) #end uodate dynamic controls according to language
  
  
  
  ## barplot ----
  
  output$barplot_suitability <- renderPlot({
    print("plot")
    #  setdiff(names(allinputs), c("orderby", "sidebarCollapsed", "ab_compute", "in_language", "sidebarItemExpanded"))]
    datatoplot<-datatoplot()
    #browser()
    plot_Suitability<-ggplot(datatoplot, aes(x = value, y = species, fill = BigCriteria)) +
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
    datatoplot<-datatoplot()
    return(datatoplot)
  })
  
  
})

