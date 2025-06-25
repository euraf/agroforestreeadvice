# this file is weak point, the most of translations are handled by i18n library, but that is not suitable for
# translation of dynamic tables, hence the need for these custom functions here, which are hard-coded and heavy-handed


Plot_legend_lang <- function(lang) {
  # Load the translation of the graph legend
  BigCriteria_translates <- read.csv("R/translation/BigCriteria_translates.csv", stringsAsFactors = FALSE)

  #return as character vector use en as name and cz as value
  final <- BigCriteria_translates[[as.character(lang)]]
  final <- setNames(final, BigCriteria_translates[["en"]])

  return(final)
}
# test = Plot_legend_lang("cz")