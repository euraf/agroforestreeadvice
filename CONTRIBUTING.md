# Welcome to AgroforesTreeAdvice contributing guide <!-- omit in toc -->

Thank you for investing your time in contributing to our project! Any contribution you make will be reflected on [AgroforesTreeAdvice](https://github.com/euraf/agroforestreeadvice) :sparkles:.

## New contributor guide

To get an overview of the project, read the [README](README.md) file. Here are some resources to help you get started with open source contributions:

- [Finding ways to contribute to open source on GitHub](https://docs.github.com/en/get-started/exploring-projects-on-github/finding-ways-to-contribute-to-open-source-on-github)
- [Set up Git](https://docs.github.com/en/get-started/quickstart/set-up-git)
- [GitHub flow](https://docs.github.com/en/get-started/quickstart/github-flow)
- [Collaborating with pull requests](https://docs.github.com/en/github/collaborating-with-pull-requests)

## Getting started
AgroforesTreeAdvice is an aggregator of tree selection tools. Existing tools were broken down into 3 parts: 
1) the description of the criteria used in the tool to collect user inputs (description of their site and description of their objectives)
2) the database of tree characteristics predicting the adaptation to the site and the efficiency at providing the desired products and services
3) the model that allows computing each tree species's score according to the input of the user (site constraints and objectives) and the tree database

AgroforesTreeAdvice can be used either interactively through the shiny app user interface or by querying a model through an URL query.

The UI allows selecting the desired language, the desired tool, enter the user's site and objectives descriptions (according to the info needed by the specific tool), and then visualise the results

The URL query can be used to query one model with a set of parameters, and returns a tab-delimited file of the result

Contributors can:
1) Check/correct translations of each tool in their native language
2) Add a new model
3) Contribute to code

## Check/correct translations
The widgets (checkboxes, drop-down menus, radio buttons...) of each tool have been translated from their native languages into all the languages supported by AgroforesTreeAdvice (currently, english, french, german, dutch and czech) automatically by deepL => there may be some mistakes. If you detect an error, we would appreciate if you could correct it.

The translations are kept in the tab-delimited text file describing the interface of each tool **Warning:** do not include tabs in the text within cells if you use a spreadsheet editor to input the data. These are stored in the ["models" folder](https://github.com/euraf/agroforestreeadvice/tree/main/models), in the form of 2 tab-delimited text files for each tool : one named "data", which contains the tree characteristics, and one named "interface", which contains all the information AgroforesTreeAdvice needs to build the interface for this specific tool.

Tab-delimited text files can be changed directly on github, but since the columns are not aligned, it is very difficult to edit the file on the github web page. So the steps to change the file with a more convenient text editor (or spreadsheet) are:
1) if you are a github user, clone the repository or create a new branch on your computer, if you are not using github, download the file of the model you want to correct on your computer
2) (preferably with libreoffice Calc, because excel does not save as UTF8 even when explicitely asking for it) modify the necessary cells in columns I to W in the "interface" sheet (DO NOT modify columns A to H)
3) if you used a spreadsheet editor and are sending the file by email (see below), highlight these cells in colors so that we can quickly do a manual merge if several people have tried to modify the same file at the same time
4) save the corrected file in **UTF-8** tab-delimited format (this is particularly important because the Czech words contain a lot of unusual characters). to save as tab-delimited in libreoffice select save as..., choose the file type "Text CSV (.csv)"" form the drop down list, uncheck box "Automatic file name extension"" and enter the file name with .txt, check the box "Edit filter settings", click Save to get a dialog for the settings, in the drop-down list "field delimiter"" select Tab, check that the encoding is UTF8 ; to save as UTF-8 in excel, you need to go to menu>saves as... then select .txt (tab-delimited) and on the same screen, click on the button "Tools" then select "Web options..." then go to tab "encoding" and then select Unicode (UTF-8)... but this does not always work (check for characters like ř or ů in Czech translations)
5) if you are a github user, commit your changes and create a pull request, if you are not using github, send the file by email to Marie Gosme (firstname.lastname@inrae.fr) 


## Add a new model
If you are a tool developper, or have access to an (opensource) database of tree characteristics, and whish to include it in agroforestreeadvice, here's how to proceed. To help you, we provide templates for the excel sheet (https://github.com/euraf/agroforestreeadvice/blob/main/models/template.xlsx) and for the scoring function (https://github.com/euraf/agroforestreeadvice/blob/main/R/suitability_template.R).
### step 1: clean the database of tree characteristics
Agroforestreeadvice needs a clean database of tree characteristics with tree species in lines, characteristics of the species in columns. The first line must contain headers, with one header for each column, no space and no special characters in headers. Check that you have at least one column that can serve as a unique tree identifier (ideally this should be the tree latin name and if you have species with several lines (e.g. because of several cultivars), then the unique identifier should include the cultivar). Check that the text cells are correctly spelled.

### step 2: decide which inputs to ask from the user
In agroforestreeadvice, inputs are called "criteria" (because they are the criteria on which the tree species are scored). They can be of different types (corresponding to shiny widgets): checkboxInput, checkboxGroupInput, radioButtons, selectInput (=drop-down menu), numericInput, sliderInput etc... You need to decide, for each criteria, the object type that you want, and indicate it in the objecttype column (column F). 

For criteria that don't have suboptions, you need to write one line per criteria, with criteria name (see below for the choice of name) in both column "criteria" and "choice" (columns E end F). For criteria that need 2 supplementary information (i.e. sliderInput, which need the min and max of the range), you need to have 2 lines for the criteria, with the criteria name in column "criteria", and the min and max in 2 lines in the column "choice" For criteria that have several suboptions (i.e. selectInput, checkboxGroupInput, radioButtons, checkboxGroupInput), you need to add as many lines as necessary, with the criteria name in the "criteria" column, and the different suboptions in the "choice" column.

Criteria name: In order to use the automatic score computation function, each user input should correspond to one column of the tree characteristic database (= with the same name as the column), or to several columns (=each with the name of a choice of a criteria that has suboptions), but this is not mandatory: you can code your own scoring function, and even mix automatic computation for certain criteria, and ad-hoc function for others.

Once your criteria are defined, you need to specify where they should go on the interface, i.e. identify which of the needed inputs correspond to the description of the user's site, and which correspond to the farmer's objectives. 

The "site" inputs correspond to constraints that the user needs to take into account to know what tree species are adapted to their local conditions, in terms of soil, climate, biotic context, constraints at plot scale, at farm scale or at the socio-economic level). They will be placed on the left-hand side of the interface and will be matched with the response traits (response to the environmental conditions) of the trees. Therefore you need to write "responsetrait" in the "side" column (column B) for those.

The "objectives" input correspond to the user axpects from the tree in terms of production of products (e.g. timber, fruits, nuts, fuelwood etc..) and/or ecosystem services (e.g. shade, soil fertility etc...). They will be placed in the right-hand side of the interface and will be matched to the effect traits of the trees (effect on the environment). THerefore you need to write "effecttrait" in the "side" column (column B) for those.

You can change (or you should soon be able to) the order of display of the criteria with coulmn "order".

In order to simplify the visualisaiton of results, you can organise your criteria by "BigCriteria". these are used to give the same color to a group of scores in the graphical output (e.g. criteria describing the soil texture, soil pH, soil depths... can all be grouped in the "soil" BigCriteria. To do so, simply fill the "BigCriteria" column (column C).

Provide the translation of the BigCriteria, criteria and choices in all the languages supported by AgroforesTreeAdvice (currently, english, french, german, dutch and czech) in the corresponding columns (e.g. criteria_en)

Create an excel file with 2 sheets, one named "data", which contains the tree characteristics, and one named "interface", which contains all the information AgroforesTreeAdvice needs to build the interface for this specific tool. The file name should be your model name and it should not have space, underscore nor special characters in it.

### step 3: code the scoring function
You can use the file [suitability_template.R](https://github.com/euraf/agroforestreeadvice/blob/main/R/suitability_template.R) to start from. You just need to add the names of criteria that follow the standard algorithm in line 22, add the computation of other non-standard criteria on line 33 and insert the name of your ID column in line 38.

The standard algorithm works for each criteria that corresponds to a single column of the tree characteristics database (= name of the criteria = name of the column in the database) or to multiple columns of the database (names of columns = names of the different choices within the criteria). In this case, there is a generic function that will compute a score between 0 and 1 for each criteria based on the following rules:

|User input|Database|Score|
|-----------|---------|-------|
|1 item from a drop-down list (or in radio buttons)|Yes/no columns for each possible item|1 if the tree has this feature, 0 otherwise|
|1 item from a drop-down list (or in radio buttons)|1 column containing an item|1 if the tree has this feature, 0 otherwise|
|1 or more items in a set of checkboxes|Yes/no columns for each possible item|(number of items present in tree features, among selected items)/(number of selected items)|
|1 or more items in a set of checkboxes|1 column containing one or more items|(number of items present in tree features, among selected items)/(number of selected items)|
|1 single numerical value|1 column containing a single value|1-abs(feature-value)/(max(features)-min(features))|
|1 single numerical value|1 column containing a range (x-y) |1 if value is within range, 0 if value is outside range|
|range of values|1 column containing a single value|1 if the characteristic is within the input range, 0 if the characteristic is outside it|

save your file as suitability_MODELNAME.R

### step 4: add your tool to agroforestryadvice code
- clone the repository on your computer
- add the excel file with sheets "data" and "interface" in folder "models", with name MODELNAME.xlsx (MODELNAME should be a short version of your model name, without spaces not underscore nor special characters that will be used everywhere in agroforestreeadvice to identify parts that are specific to your tool)
- add the suitability_MODLENAME.R file to the "R" folder
- in global.R file, add the following 2 lines at the beginning:
```
dataMODELNAME<-read.xlsx("models/MODELNAME.xlsx", sheet="data")
interfaceMODELNAME<-read.xlsx("models/MODELNAME.xlsx", sheet="interface")
```
and add the following line at the end to import your suitability function:
```
source("R/suitability_MODELNAME.R")
```

- in ui.R, add the following line in the tool tabItem
```
tabPanel("name to display in tab", value="MODELNAME", moduleTabInterface_UI(id = "MODELNAME", data = dataMODELNAME, interface= interfaceMODELNAME))
```
(you can also add the information about your tool in the "infos" tabItem)
- at the end of the  server.R file, add the following line:
```
moduleTabInterface_Server(id = "MODELNAME",
                            language= language,
                            data=dataMODELNAME, interface=interfaceMODELNAME, functionSuitability=compute_suitability_MODELNAME, compactobjectives=FALSE)
```
NB the compactobjectives argument can be set to TRUE if you want to reduce the number of inputs (only big criteria are diaplyed and all criteria that belong to the selected bigcriteria are selected)


:tada: That's it, you have added your tool! 
## Contribute to code
### To Do: explain structure of code files, structure of shiny app, reactive structure to help contributors
### To Do: comment code to help contributors
