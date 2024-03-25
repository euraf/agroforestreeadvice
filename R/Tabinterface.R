
# Fonction ui du module ----
#id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO, summaryfunction= compute_suitability_DENTRO
moduleTabInterface_UI <- function(id, data, interface) {
  
  # Crée une fonction ns qui va coller l'Id du module avec l'Id des inputs/outputs (séparation avec un tiret)
  # Permet d'avoir des Id uniques dans toute l'application pour les inputs/outputs
  # Appliquer la fonction ns à tous les inputId / outputId
  ns <- NS(id)


  # Il faut encapsuler l'interface dans un tagList
  tagList(
    uiOutput(ns("dynamicUI")),
    
    box(title = "All trees",
        solidHeader = TRUE,
        status="warning",
        width=NULL,
        fluidRow(
          column(width=12,
                 fluidRow(
                   div(style="display: inline-block;vertical-align:top; width: 20px;", HTML("<br>")), #because else the orderby radiobuttons start out of the box
                   div(style="display: inline-block;vertical-align:top; width: 300px;",
                       radioButtons(inputId=ns("orderby"), label="Order By", 
                                    choices=c(Adaptation="responsetrait", Efficiency="effecttrait"),
                                    selected="responsetrait", inline=TRUE)),
                   div(style="display: inline-block;vertical-align:top; width: 100px;",
                       HTML("<br>")),
                   div(style="display: inline-block;vertical-align:top; width: 200px;",
                       numericInput(inputId=ns("barplotfrom"), label="Display from the xth", value=1)),
                   div(style="display: inline-block;vertical-align:top; width: 200px;",
                       numericInput(inputId=ns("barplotto"), label="to the yth", value=20))
                 ),
                 plotOutput((ns("barplot_suitability")))
                 
          ),
          column(width=12,
                 DTOutput(outputId = ns("DTSuitability"))
          )
        )),

    if (id == "Czech") {      # Add legislative criteria for Czech tree advice
          box(title = "Legislative criteria for tree selection",
              solidHeader = TRUE,
              status="warning",
              width=NULL,
                column(width=12,
                      DTOutput(outputId = ns("DTSinformations"))
          ))}
            
        
      
  ) # fin tagList
  
} # fin moduleTabInterface_UI


# Fonction server du module ----
#language is a reactive value from the main app
moduleTabInterface_Server <- function(id, language, data = dataDENTRO, interface= interfaceDENTRO, functionSuitability=compute_suitability_DENTRO, compactobjectives=TRUE) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns # utile si renderUI
      
      output$dynamicUI <- renderUI({
      req(language())  # Ensure 'language' is available
      tagList(
        fluidRow(column(width=6,
                        box(title = Site_box_lang(language()),
                            solidHeader = TRUE,
                            status="danger",
                            width=NULL,
                            uiOutput(ns("dynamicControlsResponse"))
                        )),
                 column(width=6,
                        box(title = Objectives_box_lang(language()),
                            solidHeader = TRUE,
                            status="primary",
                            width=NULL,
                            uiOutput(ns("dynamicControlsEffect"))
                        ))
        ),
        fluidRow(wellPanel(
          class = "custom-well-panel5",
          style = "display: flex; justify-content: center;",
          actionButton(inputId = ns("ab_compute"), label=Compare_trees_button(language()))
        ))
      )
    })



      ## reactives ----
      
      # Reactive expression to capture control names and values

      # reactiveValues to keep the order of the species for the second datatable
      speciesOrder <- reactiveValues(order = NULL)   

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
        reshapecontrols(interface, language=language(), compactobjectives=compactobjectives)
      }) # compactcontrols() is a dataframe of controls (object type, criteria (for site controls) or BigCriteria (for objective controls if compactobjectives is true) (=object name), and their labels, and the choices (for SelectInput and CheckboxGroup)
      #observe(print(str(head(compactcontrols()))))
      
      #reactive value of formatted inputs for computing suitability (icicicici : now I regret reformatting: lists are more versatile than character vectors, but for legacy reasons, suitability functions expect character vectors.)
      #to do in the future: move the formatting to inside the suitability functions that need character vector, and allow suitability fnctions to accept lists as inputs
      reformattedinputs<-reactive({
        allinputs <- controlData()
        reformated<-character()
        if(length(allinputs)>0) {
          message("initial inputs:") ; message(str(allinputs)) 
          # List of things (chr for selectInput, 
          #                 chr vector for checkboxGroupInput,
          #                 int for numericInput,
          #                 logical for checkbox)
          #transform checked checkboxes into their name
          checkboxyes<-sapply(allinputs, function (x) all(is.logical(x) & as.logical(x)))
          if(sum(checkboxyes)>0){
            checked<-unlist(allinputs)
            checked[checkboxyes]<- names(checked)[checkboxyes]
            checked<-checked[checked!="FALSE"]
            reformated<-c(reformated, checked)
          }
          #transform numericInput selectInputs and checkboxgroupInputs to character vectors
          notcheckbox<-sapply(allinputs, function (x) !is.logical(x))
          if(sum(notcheckbox)>0){
            reformated<-c(reformated, unlist(allinputs[notcheckbox]))
          }
        }
        message("reformated inputs:"); message(str(reformated))
        return(reformated)

      })#reformattedinputs() is a named character of choices of the user (with the checkbox name for TRUE checkboxes)
      #observe(print(str(reformattedinputs())))
      
      # output$controlOutput <- renderPrint({
      #   # Only update controlOutput if the button is clicked
      #   #req(buttonClicked())
      #   reformattedinputs()
      # })
      
      #for a strange reason, it seems we cannot use the input$whatever values directly, they need to go through a reactive value
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
      
      #reactive keeping the range of species to plot
      rangetoplot<-reactiveValues(from=1, to=20)
      observeEvent(input$barplotfrom, {
        rangetoplot$from<<-input$barplotfrom
      })
      observeEvent(input$barplotto, {
        rangetoplot$to<<-input$barplotto
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

          # filter the trees based on the hard criteria
          # dfSuitability<-Hard_criteria_filter(dfSuitability, allinputs)
        
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

        #write_xlsx(dfSuitability, "01_dfsuitablity.xlsx")
        return(dfSuitability)
      }) %>% bindEvent(input$ab_compute, input$orderby) # datatoplot() is a data frame of trees, BigCriteria and values
      
      
      
      
      
      
      
      ## dynamic ui ----
      
      ### Generate dynamic UI controls only at initiation (in english) ----
      #after that, when just the language is modified, we use update... to simply update the labels without redrawing the whole controls
      output$dynamicControlsResponse <- renderUI({
        message("init dynamicControlsResponse")
        print(str(compactcontrols()))
        #initcompactcontrols<-reshapecontrols(interface, language="en")
        initcompactcontrols<-compactcontrols()
        responsecontrols<-which(initcompactcontrols$side=="responsetrait")
        print(str(responsecontrols))
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
            checkboxGroupInput=checkboxGroupInput(input_id, label = labelinput, choices = choices),
            sliderInput=sliderInput(input_id, label = labelinput, min=min(as.numeric(choices)), max=max(as.numeric(choices)), value=range(as.numeric(choices)))
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
          
          # If the label is "Legislation", return NULL  - make batter, now primitive way to remove the legislation option, but store its data         
          if (labelinput == "Legislation") {
            return(NULL)
          }
  
          control <- switch(
            control_type,
            checkboxInput=checkboxInput(input_id, label = labelinput),
            numericInput=numericInput(input_id, label = labelinput, value = 0),
            selectInput=selectInput(input_id, label = labelinput, choices = choices),
            checkboxGroupInput=checkboxGroupInput(input_id, label = labelinput, choices = choices),
            sliderInput=sliderInput(input_id, label = labelinput, min=min(as.numeric(choices)), max=max(as.numeric(choices)), value=range(as.numeric(choices)))
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
        databis<-datatoplot()
        print(str(databis))
        
        # change BigCriteria names in order to change plot legend language
        # databis <- BigCriteria_lang(language(), databis)

        # reset the as.numeric(databis$species) from one to the number of species - needed if I want to reduce the number of species to plot
        databis$species<-factor(databis$species, levels=speciesOrder$order)

        print(str(as.numeric(databis$species)))
        #select only the range of species to display (user choice, by default 1 to 20), species is an ordered vector
        databis<-databis[as.numeric(databis$species)>=rangetoplot$from 
                         & as.numeric(databis$species)<=rangetoplot$to ,]
        plot_Suitability<-ggplot(databis, aes(x = value, y = species, fill = BigCriteria)) +
          #geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = "#ffcccc", alpha = 0.5) +
          #geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#ccccff", alpha = 0.5) +
          geom_bar(stat = "identity", position = "stack", color = NA) +
          scale_y_discrete(limits=rev) +
          geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 1.5)+
          theme_minimal() +
          labs(y = "Species", x = NULL, fill = Graph_legend_lang(language())) +
          scale_x_continuous(breaks = c(-2, 2), labels = Graph_labels_lang(language())) +        # anchors the descriptions of X axis around the vline, hides X axis values
          theme(axis.text.y = element_text(family = "Arial", size = 14),                         # too small descriptions on some monitors
                axis.text.x = element_text(family = "Arial", size = 14),
                legend.title = element_text(family = "Arial", size = 16),
                legend.text = element_text(family = "Arial", size = 14),
                panel.grid.major.x = element_blank(),                                            # hides major vertical grid lines
                panel.grid.minor.x = element_blank()                                             # hides minor vertical grid lines  
                ) +
          scale_fill_discrete(labels = Plot_legend_lang(language()))      
              
        return(plot_Suitability)
      })
      
      ## DT table----
      output$DTSuitability <- renderDT({
        
        datalong<-datatoplot()
        if(datalong$species[1] != "no data yet") {
          datalong[datalong$side=="responsetrait", "value"]<- -datalong[datalong$side=="responsetrait", "value"]
          if("criteria" %in% names(datalong)) {
            #datalong<-datalong[order(datalong$side, datalong$criteria, datalong$species),]
            datawide<-reshape(datalong[,c("species", "criteria", "value")], direction="wide",
                              v.names="value",
                              timevar="criteria",
                              idvar=c("species")
            )
            crits<-unique(datalong[,c("side", "criteria")])
            crits$criteria<-paste("value", crits$criteria, sep=".")
          } else {
            #datalong<-datalong[order(datalong$side, datalong$BigCriteria, datalong$species),]
            datawide<-reshape(datalong[,c("species", "BigCriteria", "value")], direction="wide",
                              v.names="value",
                              timevar="BigCriteria",
                              idvar=c("species")
            )
            crits<-unique(datalong[,c("side", "BigCriteria")])
            crits$criteria<-paste("value", crits$BigCriteria, sep=".")
          }
          if(any(crits$side=="responsetrait")){
            datawide$adaptation.score<-rowSums(datawide[, crits$criteria[crits$side=="responsetrait"], drop=FALSE],na.rm=TRUE)
          } else datawide$adaptation.score<-0
          if(any(crits$side=="effecttrait")){
            datawide$efficiency.score<-rowSums(datawide[, crits$criteria[crits$side=="effecttrait"], drop=FALSE],na.rm=TRUE)
          } else datawide$efficiency.score<-0
          speciesOrder$order <- unique(datalong$species)
          
          datawide[,c("species", "adaptation.score", "efficiency.score", setdiff(names(datawide), c("species", "adaptation.score", "efficiency.score")))]
        } else {datalong}
        }, options = list(
        scrollX = TRUE,
        order = list(list(1, 'asc')) #order by the species column, which is an ordered factor
      ))
      

      output$DTSinformations <- renderDT({
        # MOST IMPORTANT! select desired criteria it will find them in the interface and also the answers in choice_en, choice_cz, etc.
        legislative_criteria <- c("approval", "endengeredG", "endengeredU", "endengeredY")
        informative_criteria <- c("wood", "food")

        # use informative_criteria to load dataframe from interfaceCzech
        datainfo <- dataCzech[, c("Scientific_name", informative_criteria, legislative_criteria)]

        language_col <- paste("choice", (as.character(language())), sep = "_") # returns choice_en, choice_cz, etc.

        informative_text = list()
        for(i in seq_along(informative_criteria)) {  # finds answers in choice_en, choice_cz, etc. for each informative_criteria                                              
          row <- interfaceCzech[interfaceCzech$criteria == informative_criteria[i], ]
          informative <- row[, language_col]
          informative_text[[length(informative_text) + 1]] <- informative
        }

        legislative_text = list()
        for(i in seq_along(legislative_criteria)) {  # finds answers in choice_en, choice_cz, etc. for each informative_criteria                                              
          row <- interfaceCzech[interfaceCzech$choice == legislative_criteria[i], ]
          legislative <- row[, language_col]
          print("##### legislative")
          print(row)
          legislative_text[[length(legislative_text) + 1]] <- legislative
        }

        # Replace VRAI with the legislative text
        for(i in seq_along(legislative_criteria)) {
          datainfo[[legislative_criteria[i]]][datainfo[[legislative_criteria[i]]] == "VRAI"] <- legislative_text[[i]]
        }

        # Concatenate columns into custom columns
        datainfo$information <- do.call(paste, c(lapply(informative_criteria, function(x) datainfo[[x]]), sep = ", "))
        datainfo$legislation <- do.call(paste, c(lapply(legislative_criteria, function(x) datainfo[[x]]), sep = ", "))
        
        # Replace all occurrences of invalid characters
        datainfo$information <- gsub("FAUX,", "", datainfo$information)
        datainfo$information <- gsub("FAUX", "", datainfo$information)
        datainfo$information <- gsub(",FAUX", "", datainfo$information)
        datainfo$information <- gsub(", $", "", datainfo$information) # remove trailing comma
        datainfo$information <- gsub(",$", "", datainfo$information)  # remove trailing comma
        datainfo$information <- gsub("^, ", "", datainfo$information) # remove leading comma

        datainfo$legislation <- gsub("FAUX,", "", datainfo$legislation)
        datainfo$legislation <- gsub("FAUX", "", datainfo$legislation)
        datainfo$legislation <- gsub(",FAUX", "", datainfo$legislation)
        datainfo$legislation <- gsub(", $", "", datainfo$legislation) # remove trailing comma
        datainfo$legislation <- gsub(",$", "", datainfo$legislation)  # remove trailing comma

        # Order the data frame by species_order of main dataframe
        speciesOrder$order #calls the reactive value
        datainfo <- datainfo[order(match(datainfo$Scientific_name, speciesOrder$order)), ]
        
        # simplify the data frame
        datainfo <- datainfo[, c("Scientific_name", "legislation", "information")]
        
        # change language of column names
        if (as.character(language()) == "cz") {colnames(datainfo) <- c("Vědecký název", "Legislativa", "Informace")}
        if (as.character(language()) == "en") {colnames(datainfo) <- c("Scientific name","Legislation", "Information")}
        if (as.character(language()) == "fr") {colnames(datainfo) <- c("Nom scientifique", "Législation", "Information")}
        if (as.character(language()) == "de") {colnames(datainfo) <- c("Wissenschaftlicher Name", "Gesetzgebung", "Information")}
        if (as.character(language()) == "es") {colnames(datainfo) <- c("Nombre científico", "Legislación", "Información")}
        if (as.character(language()) == "nl") {colnames(datainfo) <- c("Wetenschappelijke naam", "Wetgeving", "Informatie")}
      
        #assign("datainfo", datainfo, envir = .GlobalEnv) # debugging

        # Return the simplified data frame
        datainfo

        }, options = list(
          scrollX = TRUE,
          columnDefs = list(   # auto width is on, but we can partially set the width of the columns
            list(width = '40px', targets = c(0)),
            list(width = '300px', targets = c(1)),
            list(width = '300px', targets = c(2))
          )
          
        ))    
    return(moduleServer)
  })
} # fin moduleTabInterface_Server
