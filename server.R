
server <- function(input, output, session) {
  #http://127.0.0.1:3775/?selected_language=cz&model=Czech this will just take the user to the desired tab, with the desired language
  #127.0.0.1:3775/?model=Czech&soil_water=soil_water_waterlogged&habitus=bush this triggers a modal dialog to download a txt file with the species scores for this particular set of conditions
  reactive_dataSuitability <<- reactiveVal(data.frame(x = numeric(), y = numeric()))
  reactive_plotSuitability <<- reactiveVal(ggplot())

  # Access the datatable - for debug purposes
  access_dataSuitability <- function() {
    print("Suitability data accessed")
    reactive_dataSuitability()
  }

  # Access the plot - for debug purposes
  access_plotSuitability <- function() {
    print("Suitability plot accessed")
    reactive_plotSuitability()
  }


  # Function to create a combined plot with a table for download
  create_combined_plot <- function() {
    plotting <- access_plotSuitability()
    DataSuitability <- access_dataSuitability()
    DataSuitability <- data.frame(as.matrix(DataSuitability))

    # Style the table grob
    table_theme <- ttheme_default(
      core = list(bg_params = list(fill = c(rep(c("white", "grey95"), length.out=20)), col = NA)),
      colhead = list(bg_params = list(fill = "grey80", col = NA)),
      rowhead = list(bg_params = list(fill = "grey80", col = NA))
    )
    table_grob <- tableGrob(head(DataSuitability, 20), theme = table_theme, rows = NULL)

    # Create a headline
    headline <- ggdraw() + 
      draw_label(i18n$t("Report of Tree Suitability by AgroForesTreeAdvice"), fontface = 'bold', size = 20, x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 10, 20, 0))  # Add space below the headline

    # Combine the elements into a single plot
    combined <- plot_grid(
      headline, NULL, plotting, NULL, table_grob, 
      ncol = 1, 
      rel_heights = c(0.08, 0.01, 1, 0.05, 1)  # Adjust heights to add space between elements
    )

    # Wrap the combined plot in a ggdraw to add a bottom margin
    combined_with_margin <- ggdraw(combined) + 
      theme(plot.margin = margin(10, 10, 10, 10))
    return(combined_with_margin)
    }

  # Download handler for svg
  observe({
    output$downloadSVG <- downloadHandler(
      filename = function() {
        paste("plot_and_data-", Sys.Date(), ".svg", sep = "")
      },
      content = function(file) {
        combined <- create_combined_plot()
        svg(file, width = 17, height = 13)
        print(combined)
        dev.off()
      }
    )
  })

  # Download handler for png
  observe({
    output$downloadPNG <- downloadHandler(
      filename = function() {
        paste("plot_and_data-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        combined <- create_combined_plot()
        svg_file <- tempfile(fileext = ".svg")
        svg(svg_file, width = 17, height = 13)
        print(combined)
        dev.off()
        rsvg_png(svg_file, file)
      }
    )
  })

  # Download handler for csv
  observe({
    output$downloadCSV <- downloadHandler(
      filename = function() {
        paste("plot_and_data-", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      csv_data <- data.frame(access_dataSuitability())
      write.csv(csv_data, file, row.names = FALSE)
      }
    )
  })

  # Download handler for excel
  observe({
    output$downloadExcel <- downloadHandler(
      filename = function() {
        paste("plot_and_data-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        excel_data <- data.frame(access_dataSuitability())
        write.xlsx(excel_data, file, rowNames = FALSE)
      }
    )
  })
    
  # use shiny.i18n to update the language and translate the app
  observeEvent(input$selected_language, {
    # Print the selected language to the console
    print(paste("The selected language has changed to:", input$selected_language))
    shiny.i18n::update_lang(input$selected_language)
  })
    
  # Reactive expression for the selected language - still needed for some functions
  language <- reactive({
    input$selected_language
  })


  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['selected_language']])) {
      updateRadioButtons(session, "selected_language", selected = query[['selected_language']])
    }
    
    desiredmodel <- query$model 
    if(is.null(input$sidemenu) || !is.null(desiredmodel) && desiredmodel != input$sidemenu){
      freezeReactiveValue(input, "sidemenu")
      updateTabItems(session, "sidemenu", selected = "tool")
      freezeReactiveValue(input, "toolsTabset")
      # print(desiredmodel)
      updateTabsetPanel(session, "toolsTabset", selected = desiredmodel)
    }
    
    allotherparameters<-setdiff(names(query), c("selected_language", "model"))
    if(length(allotherparameters)>0){ #we passed parameters by URL = we want to get the results as csv
      queryinputs<-unlist(query[allotherparameters]) #icicicic todo : check that all necessary inputs are provided before sending to the suitability function
      print(paste("computing suitability of model", desiredmodel))
      resultdf<-do.call(paste("compute_suitability_", desiredmodel, sep=""), list(
        inputsdata=queryinputs,
        database=get(paste("data", desiredmodel, sep="")), 
        interface=get(paste("interface", desiredmodel, sep="")))
      )
      #print(str(resultCSV))
      # Define a function to generate and return the CSV content
      generatetxtContent <- function(df) {
        m <- as.matrix(df)
        # add column headers
        m <- rbind(dimnames(m)[[2]], m)
        m<-apply(m, 1, paste, collapse="\t")
        return(m)
      }
      
      preview_table <- head(resultdf, n = 5)
      
      # Display data preview in the modal
      output$dataPreview <- renderUI({
        tableOutput("previewTable")
      })
      output$previewTable <- renderTable({
        preview_table
      })
      
      # Handle the download button within the modal
      output$modalDownload <- downloadHandler(
        filename = function() {
          "computed_data.csv"
        },
        content = function(file) {
          txt_content <- generatetxtContent(resultdf)
          writeLines(txt_content, file)
        }
      )

      
      
    }
    
  }) #end managing URL queries
  
  
  
  # Czech tree advice ----
  moduleTabInterface_Server(id = "Czech",
                            language= language,
                            data=dataCzech, interface=interfaceCzech, functionSuitability=compute_suitability_Czech, compactobjectives=FALSE)
  
  
  # Flanders tree advice ----
  moduleTabInterface_Server(id = "DENTRO",
                            language= language,
                            data = dataDENTRO, interface= interfaceDENTRO, functionSuitability=compute_suitability_DENTRO, compactobjectives=TRUE)
  
  # Shade tree advice ----
  moduleTabInterface_Server(id = "STA",
                            language= language,
                            data=dataSTA, interface=interfaceSTA, functionSuitability=compute_suitability_STA, compactobjectives=FALSE)
  
  
  # Deciduous ----
  moduleTabInterface_Server(id = "DECIDUOUS",
                            language= language,
                            data=dataDECIDUOUS, interface=interfaceDECIDUOUS, functionSuitability=compute_suitability_DECIDUOUS, compactobjectives=FALSE)
  
  # Species Climate Suitability Model ----
  moduleTabInterface_Server(id = "SCSM",
                            language= language,
                            data=dataSCSM, interface=interfaceSCSM, functionSuitability=compute_suitability_SCSM, compactobjectives=FALSE)
  
  # Juiste Boom op de Juiste Plek ----
  
  moduleTabInterface_Server(id = "JBOJP",
                            language= language,
                            data=dataJBOJP, interface=interfaceJBOJP, functionSuitability=compute_suitability_JBOJP, compactobjectives=FALSE)
  
  
  # German Hedgerow manager
  moduleTabInterface_Server(id = "DEHM",
                            language= language,
                            data=dataDEHM, interface=interfaceDEHM, functionSuitability=compute_suitability_DEHM, compactobjectives=FALSE)
  
  #Finnish tree suitability
  moduleTabInterface_Server(id = "SUOMI",
                            language= language,
                            data=dataSUOMI, interface=interfaceSUOMI, functionSuitability=compute_suitability_SUOMI, compactobjectives=FALSE)
  
  
  
}

