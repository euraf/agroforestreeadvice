
server <- function(input, output, session) {
  #http://127.0.0.1:3775/?selected_language=cz&model=Czech this will just take the user to the desired tab, with the desired language
  #127.0.0.1:3775/?model=Czech&soil_water=soil_water_waterlogged&habitus=bush this triggers a modal dialog to download a txt file with the species scores for this particular set of conditions
  reactive_dataSuitability <<- reactiveVal(data.frame(x = numeric(), y = numeric()))
  reactive_plotSuitability <<- reactiveVal(ggplot())
  reactive_Interface <<- reactiveVal(list())
  reactive_AdditionalInfo <<- reactiveVal(data.frame(x = numeric(), y = numeric()))

  # We need to access the data from TabInterface (output$DTSuitability)
  access_dataSuitability <- function() {
    print("Suitability data accessed")
    reactive_dataSuitability()
  }

  # We need to access the plot from TabInterface (output$barplot_suitability)
  access_plotSuitability <- function() {
    print("Suitability plot accessed")
    reactive_plotSuitability()
  }

  # We need to access the interface from global (orderdf)
  access_Interface <- function() {
    print("Interface accessed")
    reactive_Interface()
  }

  # We need to access the additional info from TabInterface (output$DTinformations)
  access_AdditionalInfo <- function() {
    print("Additional informations accessed")
    reactive_AdditionalInfo()
  }

  # Download handler for svg
  observe({
    output$downloadSVG <- downloadHandler(
      filename = function() {
        paste("plot_and_data-", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        OutputPlots <- CombinePlotsForDownload(interface = req(access_Interface()), language = req(language()), 
          DataSuitability = req(access_dataSuitability()), ComputedPlot = req(access_plotSuitability()))
        OutputAdditionalInfo <- create_dataINFO_plot(datainfo = req(access_AdditionalInfo()))
        
        tempDir <- tempdir()
        tempSVG1 <- file.path(tempDir, "plot_and_data.svg")
        tempSVG2 <- file.path(tempDir, "plot_and_data222.svg")

        svg(tempSVG1, height = 19, width = 14)
        print(OutputPlots)
        dev.off()

        svg(tempSVG2, height = 19, width = 14)
        print(OutputAdditionalInfo)
        dev.off()
        
        # Create a ZIP file containing both SVGs
        oldwd <- setwd(tempDir)
        on.exit(setwd(oldwd), add = TRUE)
        zip::zip(file, files = c("plot_and_data.svg", "plot_and_data222.svg"))

        # Clean up temporary files
        unlink(tempSVG1)
        unlink(tempSVG2)
       }
    )
  })

  # Download handler for pdf
  observe({
    output$downloadPDF <- downloadHandler(
      filename = function() {
        paste("plot_and_data-", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        OutputPlots <- CombinePlotsForDownload(interface = req(access_Interface()), language = req(language()), 
          DataSuitability = req(access_dataSuitability()), ComputedPlot = req(access_plotSuitability()))
        OutputAdditionalInfo <- create_dataINFO_plot(datainfo = req(access_AdditionalInfo()))
        
        tempDir <- tempdir()
        tempSVG1 <- file.path(tempDir, "plot_and_data.svg")
        tempSVG2 <- file.path(tempDir, "plot_and_data222.svg")
        tempPDF1 <- file.path(tempDir, "plot_and_data.pdf")
        tempPDF2 <- file.path(tempDir, "plot_and_data222.pdf")


        svg(tempSVG1, height = 19, width = 14)
        print(OutputPlots)
        dev.off()
        rsvg_pdf(tempSVG1, tempPDF1,  height = 3508, width = 2480)  # metrics are in pixels - 1 inch = 96 pixels; A4 is 2480 x 3508 pixels

        svg(tempSVG2, height = 19, width = 14)
        print(OutputAdditionalInfo)
        dev.off()
        rsvg_pdf(tempSVG2, tempPDF2,  height = 3508, width = 2480)  # metrics are in pixels - 1 inch = 96 pixels; A4 is 2480 x 3508 pixels
        
        # Create a ZIP file containing both SVGs
        oldwd <- setwd(tempDir)
        on.exit(setwd(oldwd), add = TRUE)
        zip::zip(file, files = c("plot_and_data.pdf", "plot_and_data222.pdf"))

        # Clean up temporary files
        unlink(tempSVG1)
        unlink(tempSVG2)
        unlink(tempPDF1)
        unlink(tempPDF2)
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

