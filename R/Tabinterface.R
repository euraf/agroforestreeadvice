
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

    box(title = i18n$t("Trees suitability"),
        solidHeader = TRUE,
        status="info",
        width=NULL,
        fluidRow(
          column(width=12,
                 fluidRow(
                   div(style="display: inline-block;vertical-align:top; width: 20px;", HTML("<br>")), #because else the orderby radiobuttons start out of the box
                   div(style="display: inline-block;vertical-align:top; width: 300px;",
                       radioButtons(inputId=ns("orderby"), label=i18n$t("Order by"), 
                                    choices=c(Adaptation="responsetrait", Efficiency="effecttrait"),
                                    selected="responsetrait", inline=TRUE)),
                   div(style="display: inline-block;vertical-align:top; width: 100px;",
                       HTML("<br>")),
                   div(style="display: inline-block;vertical-align:top; width: 200px;",
                       numericInput(inputId=ns("barplotfrom"), label=i18n$t("Display from the"), value=1)),
                   div(style="display: inline-block;vertical-align:top; width: 200px;",
                       numericInput(inputId=ns("barplotto"), label=i18n$t("Display to the"), value=20))
                 ),
                 
                 plotOutput(ns("barplot_suitability"),
                            hover = hoverOpts(id =ns("plot_hover"))),
                 uiOutput(ns("tooltip"), style = "pointer-events: none"),
                 #htmlOutput(ns("hover_info"))
                 
          ),
          column(width=12,
                 DTOutput(outputId = ns("DTSuitability"))
          )
        )),

    if (id == "Czech") {      # Add legislative criteria for Czech tree advice
          box(title = i18n$t("Additional information"),
              solidHeader = TRUE,
              status="info",
              width=NULL,
                column(width=12,
                      DTOutput(outputId = ns("DTinformations"))
          ))}
            
        
      
  ) # fin tagList
  
} # fin moduleTabInterface_UI


#### Fonction server du module ----
#language is a reactive value from the main app
moduleTabInterface_Server <- function(id, language, data = dataDENTRO, interface= interfaceDENTRO, functionSuitability=compute_suitability_DENTRO, compactobjectives=TRUE) 
{
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns # utile si renderUI
      
      output$dynamicUI <- renderUI({
      req(language())  # Ensure 'language' is available
      # assign actual language
      actual_lang <<- paste0("choice_",as.character(language()))
      
      # information about the filters, calls modalDialog
      tagList(
        fluidRow(wellPanel(
        style = "padding: 5px; border-radius: 0px",
        class = "custom-class",
        div(
          style = "text-align: right;",
          actionButton(inputId = ns("filter_info"), 
            label = i18n$t("Information"), icon("circle-question"),
            style = "font-weight: bold; background-color: #337ab7; color: white; border: none; padding: 5px 10px;"),
          actionButton(inputId = ns("show_modal"), 
            label = i18n$t("Download"), icon = icon("download"), 
            style = "font-weight: bold; background-color: #337ab7; color: white; border: none; padding: 5px 10px;"),
              
              
              
              ) # end div
            )), 


        fluidRow(column(width=6,
                        box(title = i18n$t("Your site"),
                            solidHeader = TRUE,
                            status="danger",
                            width=NULL,
                            uiOutput(ns("dynamicControlsResponse")),
                            tags$style(HTML("
                              .box-title {
                                font-family: 'Arial', sans-serif;
                                   }")
                                )
                        )),
                 column(width=6,
                        box(title = i18n$t("Your Objectives"),
                            solidHeader = TRUE,
                            status="primary",
                            width=NULL,
                            uiOutput(ns("dynamicControlsEffect")),
                            tags$style(HTML("
                              .box-title {
                                font-family: 'Arial', sans-serif;
                                   }")
                                )
                        ))
        ),
        fluidRow(wellPanel(
          class = "custom-well-panel5",
          style = "display: flex; justify-content: center;",
          actionButton(inputId = ns("ab_compute"), label=i18n$t(c("Compare Trees")),
                        icon("tree"), style=" font-weight: bold; color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
      observe(print(str(head(compactcontrols()))))
      
      #reactive value of formatted inputs for computing suitability (icicicici : now I regret reformatting: lists are more versatile than character vectors, but for legacy reasons, suitability functions expect character vectors.)
      #to do in the future: move the formatting to inside the suitability functions that need character vector, and allow suitability fnctions to accept lists as inputs
      reformattedinputs<-reactive({
        allinputs <- controlData()
        reactive_inputs(allinputs)   # we need to get to the used inputs in downloadhandler
        reformated<-character()
        if(length(allinputs)>0) {
          message("initial inputs:") ; print((allinputs)) 
          # List of things (chr for selectInput, 
          #                 chr for radioButtons
          #                 chr vector for checkboxGroupInput,
          #                 int for numericInput,
          #                 logical for checkbox)
          #transform checked checkboxes into their name
          checkboxyes<-sapply(allinputs, function (x) !is.null(x) & all(is.logical(x) & as.logical(x)))
          if(sum(checkboxyes)>0){
            checked<-unlist(allinputs[checkboxyes])
            checked<- names(checked)
            names(checked)<-checked
            reformated<-c(reformated, checked)
          }
          #transform numericInput radiobuttons, sliderInput, selectInput and checkboxgroupInput to character vectors
          notcheckbox<-sapply(allinputs, function (x) !is.logical(x))
          if(sum(notcheckbox)>0){
            reformated<-c(reformated, unlist(allinputs[notcheckbox]))
          }
        }
        #message("reformated inputs:"); message(str(reformated))
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

      observeEvent(input$show_modal, {
        showModal(modalDialog(
          title = i18n$t("Download selected data"),
          htmlOutput("dataPreview"),  # Display data preview
          footer = tagList(
            div(style = "text-align: left; padding: 5px;",
                downloadButton("downloadExcel", i18n$t("Download as Excel file"), class = "download-button"),
                downloadButton("downloadSVG", i18n$t("Download as SVG file"), class = "download-button")),

            div(style = "text-align: left; padding: 5px;",
                downloadButton("downloadCSV", i18n$t("Download as CSV file"), class = "download-button"),
                downloadButton("downloadPDF", i18n$t("Download as PDF file"), class = "download-button")),

            modalButton(i18n$t("Close"), icon = icon("remove"))
            ),
          size = "l",
          easyClose = TRUE
          ))
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
          #message(paste("computing suitability graph with"), paste(names(allinputs), collapse=" "))
          #dfSuitability<-data.frame(species="icicici", side="responsetrait", value=1, BigCriteria="debugging")
          
          dfSuitability<-functionSuitability(inputsdata=allinputs, interface=interface, database=data,
                                             orderby = orderby)
          #print(str(dfSuitability))
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
      }) %>% bindEvent(input$ab_compute, input$orderby) # datatoplot() is a data frame of trees, BigCriteria and values and tooltipspecies
      
      
      
      
      
      
      ## dynamic ui ----
      
      ### Generate dynamic UI controls only at initiation (in english) ----
      #after that, when just the language is modified, we use update... to simply update the labels without redrawing the whole controls
      output$dynamicControlsResponse <- renderUI({
        message("init dynamicControlsResponse")
        #print(str(compactcontrols()))
        #initcompactcontrols<-reshapecontrols(interface, language="en")
        initcompactcontrols<-compactcontrols()
        responsecontrols<-which(initcompactcontrols$side=="responsetrait")
        #print(str(responsecontrols))
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
            sliderInput=sliderInput(input_id, label = labelinput, min=min(as.numeric(choices)), max=max(as.numeric(choices)), value=range(as.numeric(choices))),
            radioButtons=radioButtons(input_id, label = labelinput,choices = choices)
            # Add more control types as needed
          )
          # tooltip(
          #   control,
          #   "ça marche le tooltip!!" #actually it does not work: tooltip does not get triggered... need to better understand where to add the function.
          # )
          
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
        
        controls_list <- lapply(responsecontrols, function(i) {
          
          control_type <- initcompactcontrols$objecttype[i]
          input_id <- ns(initcompactcontrols$criteria[i])
          #message(paste("creation", input_id))
          choices <- strsplit(initcompactcontrols$choice[i], ",")[[1]]
          labchoices<-strsplit(initcompactcontrols$labelchoice[i], ",")[[1]]
          names(choices)<-labchoices
          labelinput<-initcompactcontrols$labelcriteria[i]
          
          control <- switch(
            control_type,
            checkboxInput=checkboxInput(input_id, label = labelinput),
            numericInput=numericInput(input_id, label = labelinput, value = 0),
            selectInput=selectInput(input_id, label = labelinput, choices = choices),
            checkboxGroupInput=checkboxGroupInput(input_id, label = labelinput, choices = choices),
            sliderInput=sliderInput(input_id, label = labelinput, min=min(as.numeric(choices)), max=max(as.numeric(choices)), value=range(as.numeric(choices))),
            radioButtons=radioButtons(input_id, label = labelinput,choices = choices)
            # Add more control types as needed
          )
          #browser()
          column(width = 6, control)
        })
        #print(str(controls_list))
        tagList(controls_list)
      }) # end create dynamic controls for user objectives
      
      # Update language in session when user selects a new language
      observeEvent(input$selected_language, {
      # This print is just for demonstration
      print(paste("Language change!", input$selected_language))
      # Here is where we update language in session
      store_input <<- input
      shiny.i18n::update_lang(input$selected_language)
      })

      ### update dynamic ui in case of language selection----
      observeEvent(compactcontrols(), {
        message("update dynamicControls (both Response and Effect)")
        goodtranslations<-compactcontrols()
        responsecontrols<-which(goodtranslations$side %in% c("responsetrait","effecttrait"))
        lapply(responsecontrols, function(i) {
          #print(goodtranslations[i,])
          control_type <- goodtranslations$objecttype[i]
          input_id <- ns(goodtranslations$criteria[i])
          current_value<-input[[input_id]]
          #print(paste("values of ", input_id, "=", paste(current_value, collapse=";")))
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
            checkboxGroupInput=updateCheckboxGroupInput(session,input_id, label = labelinput, choices = choices, selected = current_value),
            sliderInput=updateSliderInput(session, input_id, label = labelinput, min=min(as.numeric(choices)), max=max(as.numeric(choices)), value=as.numeric(current_value))
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
        # print(str(databis))
        

        # change BigCriteria names in order to change plot legend language
        # databis <- BigCriteria_lang(language(), databis)

        # reset the as.numeric(databis$species) from one to the number of species - needed if I want to reduce the number of species to plot
        databis$species<-factor(databis$species, levels=speciesOrder$order)

        # print(str(as.numeric(databis$species)))
        #select only the range of species to display (user choice, by default 1 to 20), species is an ordered vector
        databis<-databis[as.numeric(databis$species)>=rangetoplot$from 
                         & as.numeric(databis$species)<=rangetoplot$to ,]

        # drop all rows where "value" is NA or 0 # icicicici Tadeas filtered out the data with value = 0, Marie prefers to keep them
        #databis <- databis %>% filter(!is.na(value) & value != 0)
        databis <- databis %>% filter(!is.na(value))
        #print(paste("there are ", nrow(databis), " rows in databis"))
        if (nrow(databis) == 0) {
          # If there are no data to plot, return an empty plot
          plot_Suitability<-ggplot() + theme_minimal()
          reactive_plotSuitability(plot_Suitability)
          print("Saved reactive plot")
          return(plot_Suitability)
        }
        plot_Suitability<-ggplot(databis, aes(x = value, y = species, fill = BigCriteria)) +
          #geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = "#ffcccc", alpha = 0.5) +
          #geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#ccccff", alpha = 0.5) +
          geom_bar(stat = "identity", position = "stack", color = NA) +
          scale_y_discrete(limits=rev) +
          geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 1.5)+
          theme_minimal() +
          labs(x = NULL, fill = i18n$t("Main Criteria"))+
          ylab(i18n$t("Species")) +
          scale_x_continuous(breaks = c(-2, 2), labels = i18n$t(c("Adaptation", "Efficiency"))) +        # anchors the descriptions of X axis around the vline, hides X axis values
          theme(axis.text.y = element_text(size = 14),                         # too small descriptions on some monitors
                axis.text.x = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                axis.title.y = element_text(size = 16),
                panel.grid.major.x = element_blank(),                                            # hides major vertical grid lines
                panel.grid.minor.x = element_blank()                                             # hides minor vertical grid lines  
                ) +
          scale_fill_discrete(labels = Plot_legend_lang(language()))
        print("Saved reactive plot")
        reactive_plotSuitability(plot_Suitability)     

        return(plot_Suitability)
      })
     
      
      # ## barplot hovered bar info ----
      # output$hover_info <- renderPrint({
      #   if(!is.null(input$plot_hover)){
      #     hoverlist<-input$plot_hover
      #     #str(hoverlist$y)
      #     #browser()
      #     id<-hoverlist$domain$discrete_limits$y[[round(hoverlist$y)]]
      #     print(paste("<b>",id,"</b><br>", data[data$IDAFTA==id, "description"]))
      #     #icicicici works only for SUOMI because the description and ID fields names are hard-coded
      #   }
      # })
      
      output$tooltip <- renderUI({
        hoverlist <- input$plot_hover
        if(!is.null(hoverlist)){
          id<-hoverlist$domain$discrete_limits$y[[round(hoverlist$y)]]
          databis<-datatoplot()
          tooltipgraph<-unique(databis[databis$IDAFTA==id, "tooltipspecies"]) #strangely, even if the data is supposed to be filtered so that IDAFTA is a unique identifier, some trees have their tooltipspecies of length>1
          left_px <- hoverlist$coords_css$x
          top_px <- hoverlist$coords_css$y
          # create style property fot tooltip
          # background color is set so tooltip is a bit transparent
          # z-index is set so we are sure are tooltip will be on top
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:", left_px + 2, "px; top:", top_px + 2, "px;")
          
          # actual tooltip created as wellPanel
          wellPanel(
            style = style,
            p(HTML(paste0("<b> ID: </b>", id, "<br/>",
                          "<b> Info: </b>",tooltipgraph, "<br/>"#,
                          #"<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
                          )))
          )
        }
        
      })
      
      
      ## DT table----
      output$DTSuitability <- renderDT({
        
        datalong<-datatoplot()
        if(datalong$species[1] != "no data yet") { #we have data: this is not the dummy dataset
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
          
          # column datainfo$Scientific_name - delete text after second space (if present) to better fit the table
          # datawide$species <- gsub("^(\\S+\\s+\\S+).*", "\\1", datawide$species)

          # drop value. from the column names
          names(datawide) <- gsub("\\value.", "", names(datawide))

          DataSuitability <- datawide[,c("species", "adaptation.score", "efficiency.score", setdiff(names(datawide), c("species", "adaptation.score", "efficiency.score")))]
          
          reactive_DataSuitability(DataSuitability)

          DataSuitability

        } else {datalong}
      }, options = list(
        scrollX = TRUE,
        language = list(
          search = i18n$t("Search:"),
          lengthMenu = i18n$t("Display _MENU_ entries"),
          info = i18n$t("Showing _START_ to _END_ of _TOTAL_ entries"),
          paginate = list(
            "next" = i18n$t("Next"),
            "previous" = i18n$t("Previous"))
          )
        ))
      
    return(moduleServer)
  })

} # fin moduleTabInterface_Server
