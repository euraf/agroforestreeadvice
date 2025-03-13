
server <- function(input, output, session) {
  #http://127.0.0.1:3775/?in_language=cz&model=Czech this will just take the user to the desired tab, with the desired language
  #127.0.0.1:3775/?model=Czech&soil_water=soil_water_waterlogged&habitus=bush this triggers a modal dialog to download a txt file with the species scores for this particular set of conditions
  
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['in_language']])) {
      updateRadioButtons(session, "in_language", selected = query[['in_language']])
    }
    
    desiredmodel <- query$model 
    if(is.null(input$sidemenu) || !is.null(desiredmodel) && desiredmodel != input$sidemenu){
      freezeReactiveValue(input, "sidemenu")
      updateTabItems(session, "sidemenu", selected = "tool")
      freezeReactiveValue(input, "toolsTabset")
      print(desiredmodel)
      updateTabsetPanel(session, "toolsTabset", selected = desiredmodel)
    }
    
    allotherparameters<-setdiff(names(query), c("in_language", "model"))
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
      
      showModal(modalDialog(
        title = "Download txt file",
        htmlOutput("dataPreview"),  # Display data preview
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("modalDownload", "Download")
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      
    }
    
  }) #end managing URL queries
  
  
  
  # Reactive expression for the selected language
  language <- reactive({
    input$in_language
  })
  
  
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
  
  moduleTabInterface_Server( # nom de la fonction server du module
    
    id = "DENTRO", # Attention à bien donner le même id que dans ui !
    language= language,
    
    data = dataDENTRO, interface= interfaceDENTRO, functionSuitability=compute_suitability_DENTRO, compactobjectives=TRUE )
  
  
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

