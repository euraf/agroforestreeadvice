server <- function(input, output, session) {
  #http://127.0.0.1:3775/?selected_language=cz&model=Czech this will just take the user to the desired tab, with the desired language
  #127.0.0.1:3775/?model=Czech&soil_water=soil_water_waterlogged&habitus=bush this triggers a modal dialog to download a txt file with the species scores for this particular set of conditions
  reactive_DataSuitability <<- reactiveVal(data.frame(x = numeric(), y = numeric()))
  reactive_plotSuitability <<- reactiveVal(ggplot())
  reactive_Interface <<- reactiveVal(list())

  reactive_inputs <<- reactiveVal(list())

  # We need to access the data from TabInterface (output$DTSuitability)
  access_DataSuitability <- function() {
    print("Suitability data accessed")
    DataSuitability <- reactive_DataSuitability()
    reactive_DataSuitability()
  }

  # We need to access the plot from TabInterface (output$barplot_suitability)
  access_plotSuitability <- function() {
    print("Suitability plot accessed")
    plotSuitability <- reactive_plotSuitability()
    reactive_plotSuitability()
  }

  # We need to access the interface from global (orderdf)
  access_Interface <- function() {
    print("Interface accessed")
    interface <- reactive_Interface()
    reactive_Interface()
  }

  access_inputs <- function() {
    print("Inputs accessed")
    inputsdata <- reactive_inputs()
    reactive_inputs()
  }

  # Download handler for svg
  observe({
    output$downloadSVG <- downloadHandler(
      filename = function() {
        paste("AGFTadvice_results_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        OutputPlots <- CombinePlotsForDownload(
          interface = req(access_Interface()), 
          language = req(language()), 
          DataSuitability = req(access_DataSuitability()), 
          plotSuitability = req(access_plotSuitability()),
          inputsdata = req(access_inputs())
        )

        
        tempDir <- tempdir()
        tempSVG1 <- file.path(tempDir, "AGFTadvice_results.svg")

        svg(tempSVG1, height = 19, width = 14)
        print(OutputPlots)
        dev.off()
        
        # Create a ZIP file containing the SVG
        oldwd <- setwd(tempDir)
        on.exit(setwd(oldwd), add = TRUE)
        zip::zip(file, files = c("AGFTadvice_results.svg"))

        # Clean up temporary files
        unlink(tempSVG1)
      }
    )
  })

  # Download handler for pdf
  observe({
    output$downloadPDF <- downloadHandler(
      filename = function() {
        paste("AGFTadvice_results_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        OutputPlots <- CombinePlotsForDownload(
          interface = req(access_Interface()), 
          language = req(language()), 
          DataSuitability = req(access_DataSuitability()), 
          plotSuitability = req(access_plotSuitability()),
          inputsdata = req(access_inputs())
        )

        tempDir <- tempdir()
        tempSVG1 <- file.path(tempDir, "AGFTadvice_results.svg")
        tempPDF1 <- file.path(tempDir, "AGFTadvice_results.pdf")


        svg(tempSVG1, height = 19, width = 14)
        print(OutputPlots)
        dev.off()
        rsvg_pdf(tempSVG1, tempPDF1,  height = 3508, width = 2480)  # metrics are in pixels - 1 inch = 96 pixels; A4 is 2480 x 3508 pixels
        # Create a ZIP file containing both SVGs
        oldwd <- setwd(tempDir)
        on.exit(setwd(oldwd), add = TRUE)
        zip::zip(file, files = c("AGFTadvice_results.pdf"))

        # Clean up temporary files
        unlink(tempSVG1)
        unlink(tempPDF1)
      }
    )
  })

  # Download handler for csv
  observe({
    output$downloadCSV <- downloadHandler(
      filename = function() {
        paste("AGFTadvice_csv_results_", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      csv_data <- data.frame(req(access_DataSuitability()))
      write.csv(csv_data, file, row.names = FALSE)
      }
    )
  })

  # Download handler for excel
  observe({
    output$downloadExcel <- downloadHandler(
      filename = function() {
        paste("AGFTadvice_excel_results_-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        excel_data <- data.frame(req(access_DataSuitability()))
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
      print(desiredmodel)
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
  
  
  
  
  
  # For the database page ----
  
  
  # Prepare the map data
  map_data <- reactive({
    data <- prepare_map_data(toolsdata)
    # Filter data based on selected projects
    if (!is.null(input$project_select)) {
      data <- data[data$project %in% input$project_select,]
    }
    return(data)
  })
  
  # Create the map
  output$map <- renderLeaflet({
    # data <- map_data()
    # 
    # # Create a legend with project information
    # projects <- unique(data[, c("project", "color")])
    # 
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Update map markers when selection changes
  observe({
    data <- map_data()
    # Add markers with different icons based on project
    leafletProxy("map") %>%
      clearMarkers()
    # Add markers one project at a time so we can use a different icon for each project
    for (proj in unique(data$project)) {
      proj_data <- data[data$project == proj,]
      icon_index <- proj_data$icon_index[1]
      leafletProxy("map") %>%
        addAwesomeMarkers(
          data = proj_data,
          lng = ~longitude, 
          lat = ~latitude,
          popup = ~paste("<strong>Tool:</strong>", project, "<br>",
                         "<strong>Description:</strong>", Info),
          label = ~project,
          icon = iconespossibles[[icon_index]],
          group = proj
        )
    }
  })
  
  # Add a legend to the map
  observe({
    data <- map_data()
    projects <- unique(data[, c("project", "color")])
    
    # Create HTML for the legend
    legend_html <- "<div style='padding: 6px; background-color: white; border-radius: 4px; border: 1px solid #ccc;'>"
    legend_html <- paste0(legend_html, "<div style='font-weight: bold; margin-bottom: 5px;'>Projects</div>")
    
    # Add each project to the legend
    for (i in 1:nrow(projects)) {
      proj <- projects$project[i]
      color <- projects$color[i]
      
      # Create a colored circle to represent the project
      legend_html <- paste0(
        legend_html, 
        "<div style='margin: 3px 0;'><span style='background-color: ", 
        color, 
        "; width: 12px; height: 12px; border-radius: 50%; display: inline-block; margin-right: 6px;'></span>",
        proj,
        "</div>"
      )
    }
    
    legend_html <- paste0(legend_html, "</div>")
    
    # Add the legend to the map
    leafletProxy("map") %>%
      clearControls() %>%
      addControl(
        html = legend_html,
        position = "bottomright"
      )
  })
  
  
  # Show project table
  output$DTToolComparison <- renderDT({
    selected_data <- toolsdata[toolsdata$project %in% input$project_select,]
    pasmanq<-selected_data$link_reference !=""
    selected_data$link_reference[pasmanq] <- paste0("<a href='",selected_data$link_reference[pasmanq],"' target='_blank'>",selected_data$link_reference[pasmanq],"</a>")
    pasmanq<-selected_data$Link_standalone !=""
    selected_data$Link_standalone[pasmanq] <- paste0("<a href='",selected_data$Link_standalone[pasmanq],"' target='_blank'>",selected_data$Link_standalone[pasmanq],"</a>")
    selected_data
  })
  
  
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
  
  
  # German Hedgerow manager ----
  moduleTabInterface_Server(id = "DEHM",
                            language= language,
                            data=dataDEHM, interface=interfaceDEHM, functionSuitability=compute_suitability_DEHM, compactobjectives=FALSE)
  
  
  # Finnish tree suitability ----
  moduleTabInterface_Server(id = "SUOMI",
                            language= language,
                            data=dataSUOMI, interface=interfaceSUOMI, functionSuitability=compute_suitability_SUOMI, compactobjectives=FALSE)
  
  # UK Guide of agroforestry trees  ----
  moduleTabInterface_Server(id = "UKguide",
                            language= language,
                            data=dataUKguide, interface=interfaceUKguide, functionSuitability=compute_suitability_UKguide, compactobjectives=FALSE)
  
  
  
}

