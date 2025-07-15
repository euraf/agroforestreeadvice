# Fichier de définition de l'interface utilisateur pour l'application Shiny

# header ----

header <- dashboardHeader(title = "AgroforesTreeAdvice")


# sidebar ----

sidebar <- dashboardSidebar(
  
  shiny.i18n::usei18n(i18n),
  selectInput(inputId = 'selected_language',
              i18n$t("Change language"),
              choices = i18n$get_languages(),
              selected = "en"),

  sidebarMenu(
    id="sidemenu",
    menuItem(i18n$t("Tools"), tabName = "tool"),
    menuItem(i18n$t("Information"), tabName = "Welcome"),
    menuItem(i18n$t("Databases"), tabName = "databases"),
    menuItem(i18n$t("Scoring"), tabName = "Scoring")
  )
)

# Main panel ----

body <- dashboardBody(
  # tags$head(
  #     tags$style(HTML("
  #       /* Custom styles for tabs */
  #       .nav-tabs > li > a {
  #         background-color: #f8f9fa; /* Light grey background */
  #         border: 1px solid #dee2e6;
  #         color: #495057; /* Dark grey text */
  #       }
  #       .nav-tabs > li.active > a {
  #         background-color: #007bff; /* Blue background for active tab */
  #         color: white; /* White text for active tab */
  #       }
  #       .tab-content {
  #         padding: 20px;
  #         border: 1px solid #dee2e6;
  #         border-top: none;
  #       }
  #     "))
  #   ),
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
      }
    "))
  ),
  # style for github link
  tags$style(HTML("
      .spaced-text {
        margin-top: 50px;  /* Adjust the value to add more space */
      }
      .text-and-link {
        margin-bottom: 0px;  /* Remove bottom margin */
      }
    ")),

  tabItems(
    # Welcome page ----
    tabItem(tabName = "Welcome", 
            h1("Welcome! please go to Tool in the menu to find the best tree for you! or to the Databases to learn where all this comes from"),
            h1("This tool is under development (to do: make this welcome page look nicer!) within the DigitAF European project"),
            img(src = "logoDigitAF_HR_500.png"),
            h1("Contributors:"),
            h2("Contributors to code"),
            h2("Marie Gosme; Clément Rigal; Raphael Paut ; Birk Skyum"),
            h2("Contributors to databases"),
            h2("DENTRO: Sarah Carton; Paul Pardon; Bert Reubens"),
            h2("Shade Tree Advice: Clément Rigal; Philippe Vaast; Laurence Jassogne; Just van der Wolf; Gilles Gram; Mathilde LEPINE; Anais CARPENTE; Mai Phuong NGUYEN; Sigrun WAGNER; Sophie GRAEFE; Baptiste CAMUS; Juan Carlos Villalba Malaver"),
            h2("Deciduous: Raphael Paut; François Warlop"),
            h2("SCSM : Birk Skyum Kristoffer Ronn-Anderson"),
            h2("Czech database : Jan Weger (VUKOZ Průhonice); Lubos Úradníček and Antonín Martiník (MENDELU Brno)"),
            h2("Juiste Boom op de Juiste Plek: Jordy van Eijk (St. ReGeneration & Van Eijk : Consultantree); Jade Koop (Jade Reforestry);Euridice Leyequien Abarca (HVHL); Jerke de Vries (HVHL); Evert Prins (Louis Bolk Institute) "),
            h2("GoÖko : Tsonkova, P., C. Böhm, R. Hübner and J. Ehritt (2019). Managing hedgerows to optimise ecosystem services in agroforestry systems. Agroforestry for sustainable agriculture. M. R. Mosquera-Losada and R. Prabhu. Cambridge, Burleigh Dodds Science Publishing. 1: 39-88. DOI 10.19103/as.2018.0041.03 using among others data from Gaida W., Grothe H., 2000. Gehölze: Handbuch für Planung und Ausführung (Woody Species: Handbook for Planning and Planting). Berlin, Hannover: Patzer Verlag: 319"),
            h2("Finnish tree suitability:  Ujula, Juha & Mattila, Iris. (2023). Suomessa menestyviä puu- ja pensaslajeja agrometsäkäyttöön. Teoksessa I. Mattila (toim.). Puustoinen maatalous Suomessa - opas suunnitteluun, katsaus kulttuurinmuutokseen (liite 1). Kilpiän tila. https://doi.org/10.5281/zenodo.8418298"),
            h2("UK Tree Suitability: Staton T, Beauchamp K, Broome A, Breeze T (2024) Tree species guide for UK agroforestry systems https://cdn.forestresearch.gov.uk/2024/11/READING-AGROFORESTY-Accessible.pdf"),
            h2("Swiss tree suitability: Lisa Nilles, Sonja Kay. https://www.haupt.ch/agroforst/"),
            h2("If you want to report bugs, or have suggestions for improvement, or want to contribute, please do so on github: https://github.com/euraf/agroforestreeadvice")
    ),#fin infos
    # Tool page ----
    tabItem(tabName = "tool", 
            tabsetPanel(
              id = "toolsTabset",

              tabPanel("Flanders Tree Advisor (DENTRO)", value="DENTRO", moduleTabInterface_UI(id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO)), 
              
              tabPanel(i18n$t("Shade Tree Advice (coffee and cocoa)"), value="STA", moduleTabInterface_UI(id = "STA", data=dataSTA, interface=interfaceSTA)), 
              
              tabPanel(i18n$t("Deciduous (fruit trees in France)"), value="DECIDUOUS", moduleTabInterface_UI(id = "DECIDUOUS", data=dataDECIDUOUS, interface=interfaceDECIDUOUS)), 

              tabPanel(i18n$t("Czech tree selection tool"), value="Czech", moduleTabInterface_UI(id = "Czech", data = dataCzech, interface= interfaceCzech)),
              
              tabPanel(i18n$t("SCSM (species climate suitability model)"), value="SCSM", moduleTabInterface_UI(id = "SCSM", data=dataSCSM, interface=interfaceSCSM)), 
              
              tabPanel(i18n$t("Juiste Boom op de Juiste Plek"), value="JBOJP", moduleTabInterface_UI(id = "JBOJP", data = dataJBOJP, interface= interfaceJBOJP)),
              
              tabPanel(i18n$t("GoÖko (German Hedgerow manager)"), value="DEHM", moduleTabInterface_UI(id = "DEHM", data = dataDEHM, interface= interfaceDEHM)),
              
              tabPanel(i18n$t("Finnish tree suitability"), value="SUOMI", moduleTabInterface_UI(id = "SUOMI", data = dataSUOMI, interface= interfaceSUOMI)),
              
              tabPanel(i18n$t("UK Guide"), value="UKguide", moduleTabInterface_UI(id = "UKguide", data = dataUKguide, interface= interfaceUKguide)),
              
              tabPanel(i18n$t("Swiss tree guide"), value="CH", moduleTabInterface_UI(id = "CH", data = dataCH, interface= interfaceCH))
              
              
            )
    ),#fin tool
    # databases information page ----
    tabItem(tabName = "databases", 
            #h1("Coming soon! Inventory of all (known to us) tools for tree selection in agroforestry, with:"),
            h1("Origin of the data"),
            h2("Select the tools you want to display"),
            h2("Click on a marker to see more information"),
            h2("...And even more information on the table below the map"),
           # h1("Czech AgroforesTree Selection Tool:"),
            #p("Data and other know-how for this tree selection tool in conditions of the Czech Republic were provided from publication (certified methodology) Practices and components of agroforestry systems recommended for the restoration and strengthening of environmental functions of landscape which was main result of the research project EPSILON TH04030409 of TACR (2019-2022). The input database was updated and adapted for use in the on-line tool AgroforetsTreeAdvice by following authors: Jan Weger, Luboš Úradníček, Antonín Martiník, Tadeáš Staněk and Marie Gosme."),
            #a("Link for the pdf explaining the methodology", href="https://www.vukoz.cz/wp-content/uploads/2023/03/Metodika-ALS-Epsilon-fin-3.pdf"),
            # Make the checkboxGroupInput scrollable and visually compact
            div(
              style = "max-height: 120px; overflow-y: auto; border: 1px solid #ccc; padding: 4px; background: #fafafa; margin-bottom: 10px;",
              checkboxGroupInput("project_select", "Select Tools:", 
                                 choices = toolsdata$project,
                                 selected = toolsdata$project)
            ),
            leafletOutput("map", height = "600px"),
            card(
              full_screen = TRUE,
              card_header("Tool Information"),
              div(
                style = "height: 600px; overflow-y: auto;",
                DTOutput(outputId ="DTToolComparison")
              )
            )
            
            
            
            # A completer: page tutoriel video, page pdf backend
    )#fin databases
  ) #fin pages
    ,tags$style(HTML("
        /* first box, red */
        .box.box-solid.box-danger>.box-header {
                  background-color: #e35a56; 
                  color: white;}
        .box.box-solid.box-danger {border-color: #e35a56;}

        /* second box, green */
        .box.box-solid.box-primary>.box-header {
                  background-color: #6dab57; 
                  color: white;}
        .box.box-solid.box-primary {border-color: #6dab57;}

        /* third box, blue */
        .box.box-solid.box-info>.box-header {
                  background-color: #3c8dbc; 
                  color: white;}
        .box.box-solid.box-info {border-color: #3c8dbc;}
        ")),
    
    # center the image
    tags$head(
      tags$style(HTML("
          .centered-image-container {
            display: flex;
            justify-content: center;
            align-items: center;
          }
      "))
  ),

  # style for the buttons on download modal
  tags$head(
    tags$style(HTML("
      .download-button {
        width: 220px;
      }
    "))
  ),
          
) #fin dashboardbody


# Complete page ----

dashboardPage(
  header,
  sidebar,
  body
)



