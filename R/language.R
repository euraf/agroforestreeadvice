Plot_legend_lang <- function(lang) {
    # changes names of X axis , actually used:
    # climate, equipment, soil, species, resources, height, production, legislation, growth, understory_tree, food,
    # wood, animals, biomass, timber, fruits, biodiversity, windbreak, shade, fertility, bioticenvironment, cropproduction,
    # economic, firewood, microclimate, otherProducts, pestdiseasecontrol, treegrowth, work, crop, location, size, lifespan
    # default for English
    translations <- c("climate" ,"equipment", "soil", "species", "resources", "height", "production", 
    "legislation", "growth", "understory_tree", "food", "wood", "animals", "biomass", "timber", "fruits", 
    "biodiversity", "windbreak", "shade", "fertility", "bioticenvironment", "cropproduction", "economic", 
    "firewood", "microclimate", "otherProducts", "pestdiseasecontrol", "treegrowth", "work", "crop", 
    "location", "size", "lifespan")

    if (as.character(lang) == "fr") {
        translations <- c("climate" = "climat", "equipment" = "équipement", "soil" = "sol", 
        "species" = "espèces", "resources" = "ressources", "height" = "hauteur", "production" = "production", 
        "legislation" = "législation", "growth" = "croissance", "understory_tree" = "sous-étage", 
        "food" = "nourriture", "wood" = "bois", "animals" = "animaux", "biomass" = "biomasse", 
        "timber" = "bois", "fruits" = "fruits", "biodiversity" = "biodiversité", 
        "windbreak" = "brise-vent", "shade" = "ombre", "fertility" = "fertilité", 
        "bioticenvironment" = "environnement biotique", "cropproduction" = "production de cultures", 
        "economic" = "économique", "firewood" = "bois de chauffage", "microclimate" = "microclimat", 
        "otherProducts" = "autres produits", "pestdiseasecontrol" = "contrôle des ravageurs et des maladies", 
        "treegrowth" = "croissance des arbres", "work" = "travail", "crop" = "culture", "location" = "emplacement", 
        "size" = "taille", "lifespan" = "durée de vie")}

    if (as.character(lang) == "es") {
        translations <- c("climate" = "clima", "equipment" = "equipo", "soil" = "suelo",
        "species" = "especies", "resources" = "recursos", "height" = "altura", "production" = "producción",
        "legislation" = "legislación", "growth" = "crecimiento", "understory_tree" = "sotobosque",
        "food" = "comida", "wood" = "madera", "animals" = "animales", "biomass" = "biomasa",
        "timber" = "madera", "fruits" = "frutas", "biodiversity" = "biodiversidad",
        "windbreak" = "rompevientos", "shade" = "sombra", "fertility" = "fertilidad",
        "bioticenvironment" = "entorno biótico", "cropproduction" = "producción de cultivos",
        "economic" = "económico", "firewood" = "leña", "microclimate" = "microclima",
        "otherProducts" = "otros productos", "pestdiseasecontrol" = "control de plagas y enfermedades",
        "treegrowth" = "crecimiento de árboles", "work" = "trabajo", "crop" = "cultivo", "location" = "ubicación",
        "size" = "tamaño", "lifespan" = "vida útil")}

    if (as.character(lang) == "de") {
        translations <- c("climate" = "Klima", "equipment" = "Ausrüstung", "soil" = "Boden", 
        "species" = "Arten", "resources" = "Ressourcen", "height" = "Höhe", "production" = "Produktion", 
        "legislation" = "Gesetzgebung", "growth" = "Wachstum", "understory_tree" = "Unterholz",
        "food" = "Nahrung", "wood" = "Holz", "animals" = "Tiere", "biomass" = "Biomasse",
        "timber" = "Holz", "fruits" = "Früchte", "biodiversity" = "Biodiversität",
        "windbreak" = "Windschutz", "shade" = "Schatten", "fertility" = "Fruchtbarkeit",
        "bioticenvironment" = "Biotische Umwelt", "cropproduction" = "Ernteerzeugung",
        "economic" = "Wirtschaftlich", "firewood" = "Brennholz", "microclimate" = "Mikroklima",
        "otherProducts" = "andere Produkte", "pestdiseasecontrol" = "Schädlings- und Krankheitsbekämpfung",
        "treegrowth" = "Baumwachstum", "work" = "Arbeit", "crop" = "Ernte", "location" = "Ort",
        "size" = "Größe", "lifespan" = "Lebensdauer")}

    if (as.character(lang) == "cz") {
        translations <- c("climate" = "klima", "equipment" = "vybavení", "soil" = "půda", 
        "species" = "druh", "resources" = "zdroje", "height" = "výška", "production" = "produkce", 
        "legislation" = "legislativa", "growth" = "růst", "understory_tree" = "podrost", 
        "food" = "potrava", "wood" = "dřevo", "animals" = "zvířata", "biomass" = "biomasa", 
        "timber" = "dřevo", "fruits" = "ovoce", "biodiversity" = "biodiverzita", 
        "windbreak" = "protivětrný pás", "shade" = "stín", "fertility" = "plodnost", 
        "bioticenvironment" = "biotické prostředí", "cropproduction" = "plodiny", 
        "economic" = "ekonomické", "firewood" = "palivo", "microclimate" = "mikroklima", 
        "otherProducts" = "jiné produkty", "pestdiseasecontrol" = "kontrola škůdců a nemocí", 
        "treegrowth" = "růst stromů", "work" = "práce", "crop" = "plodina", "location" = "umístění", 
        "size" = "velikost", "lifespan" = "doba života")}

    return(translations)
}
