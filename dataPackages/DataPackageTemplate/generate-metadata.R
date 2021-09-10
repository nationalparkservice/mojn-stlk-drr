###### Step 1. Get everything set up. ###### General Setup Stuff

# remotes::install_github("EDIorg/EMLassemblyline" # Only required if not already installed

pkgList <- c("devtools",
             "EML",
             "EMLassemblyline",
             "lubridate",
             "readtext",
             "zip",
             "readxl",
             "tidyverse",
             "streamsandlakes"
)

inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) {
  install.packages(pkgList[!inst], dep = TRUE,
                   repos = "https://cloud.r-project.org")
}

lapply(pkgList, library, character.only = TRUE, quietly = TRUE)

### The data package will be a zip file containing:
###  1. zip file of csv data
###  2. xml metadata file (EML format)
###  3. data package manifest (data package info and listing of files)

#### Step 1: zip the data ####
# STLK data is stored in a set of .csv files in the data/raw folder.
# Zip these files and save them in data/final (since zipping them is the only processing that occurs)

data_files <- list.files(path = here::here("data", "raw"), pattern = ".csv$")
zip::zip(here::here("data", "final", "data.zip"), files = data_files, root = here::here("data", "raw"))

#### Step 2: generate the metadata file ####
### Step 2a: Review or edit "Easy" template files
# Must be done in MS Excel or a text editor. Files to edit:
# metadata_templates/personnel.txt
# metadata_templates/keywords.txt
# metadata_templates/intellectual_rights.txt (version in the template should generally not be modified)
# metadata_templates/custom_units.txt (not typically needed)

### Step 2b: Generate the attribute tables and the categorical variables tables

## Get attributes
# Columns are attributeName, attributeDefinition, class, unit, dateTimeFormatString, missingValueCode, missingValueCodeExplanation
# Retrieve from data dictionary
data_dict <- readxl::read_xlsx(here::here("data", "dictionary", "DataDictionary_Field_Descriptions.xlsx")) %>%
  dplyr::select(TableName, FieldName, FieldDescription, Unit, dateTimeFormatString, missingValueCode, missingValueCodeExplanation)

# Get R datatypes for STLK data - first, get a list of all the data as dataframes, then use sapply and typeof() to get each column type
stlk_data <- streamsandlakes::GetRawData(path.to.data = here::here("data", "raw"), data.source = "local")
stlk_data$WQStreamXSection_CALCULATED <- StreamWqMedian(path.to.data = here::here("data", "raw"), data.source = "local")
stlk_data$WaterQuality_CALCULATED <- LakeWqMedian(path.to.data = here::here("data", "raw"), data.source = "local")
stlk_data$LakeLevel_CALCULATED <- LakeSurfaceElevation(path.to.data = here::here("data", "raw"), data.source = "local")

stlk_datatypes <- sapply(stlk_data, function(df) {sapply(df, class)})

# Add R datatypes to data dictionary
# There must be a more R way to do this

class_map <- c(
  character = "character", 
  logical = "character", 
  factor = "character",
  integer = "numeric",
  integer64 = "numeric",
  double = "numeric",
  numeric = "numeric",
  Date = "Date",
  POSIXct = "Date",
  POSIXt = "Date")

for (i in 1:nrow(data_dict)) {
  tbl <- data_dict$TableName[i]
  col <- data_dict$FieldName[i]
  data_type <- stlk_datatypes[[tbl]][col]
  data_dict[[i, "storageType"]] <- data_type
}

data_dict %<>% dplyr::mutate(class = unname(class_map[storageType])) %>%
  dplyr::select(-storageType)  # make_eml doesn't like storageType but we may want to include it in our metadata someday

# Rename columns
data_dict %<>% dplyr::rename(attributeName = FieldName, attributeDefinition = FieldDescription, unit = Unit) %>%
  dplyr::mutate(missingValueCode = "Empty string",
                missingValueCodeExplanation = "Not applicable or data not collected")

## Get categorical vars
cat_vars <- readxl::read_xlsx(here::here("data", "dictionary", "STLK_Categories.xlsx"))

# Write attributes and categorical vars to tab separated text files
for (datafile in unique(data_dict$TableName)) {
  # Table-level attributes
  attr <- data_dict %>% 
    dplyr::filter(TableName == datafile) %>%
    dplyr::select(-TableName)
  
  # Table-level categorical vars
  cat <- cat_vars[cat_vars$attributeName %in% attr$attributeName, ]
  
  readr::write_tsv(attr, here::here("dataPackages", "DataPackageTemplate", "metadata_templates", paste0("attributes_", datafile, ".txt")))
  readr::write_tsv(cat, here::here("dataPackages", "DataPackageTemplate", "metadata_templates", paste0("catvars_", datafile, ".txt")))
}

#### Step 3: Set Geographic Coverage ####

geographicDescription <- "Streams and lakes within Great Basin National Park (GRBA)"

# To generate from the data... Template geographic coverage

template_geographic_coverage(
 path = here::here("dataPackages", "DataPackageTemplate", "metadata_templates"),
 data.path = here::here("data", "raw"),
 data.table = 'Site.csv',
 site.col = 'Park',
 lat.col = 'Lat_WGS84',
 lon.col = 'Lon_WGS84'
)

#### Step 4: Set Temporal Coverage ####
beginDate <- min(stlk_data$Visit$VisitDate)
endDate <- max(stlk_data$Visit$VisitDate)

#### Step 5: Create EML File ####
intellectual_rights<-readLines(here::here("dataPackages", "DataPackageTemplate", "metadata_templates","intellectual_rights.txt"),encoding="UTF-8")

themekeywords<-read.delim(here::here("dataPackages", "DataPackageTemplate", "metadata_templates","keywords.txt"))
themekeywords<-paste(themekeywords$keyword, collapse = ', ')

abstract<-params$packageAbstract
writeLines(abstract,here::here("dataPackages", "DataPackageTemplate", "metadata_templates","abstract.txt"))

methodstext<-paste0("Methods for the generation of this data set are documented in Data Release Report ",gsub("—","--",params$reportNumber), ", available at ", "https://irma.nps.gov/DataStore/Reference/Profile/", params$reportRefID,".")
writeLines(methodstext,here::here("dataPackages", "DataPackageTemplate", "metadata_templates","methods.txt"))

make_eml(
  path = here::here("dataPackages", "DataPackageTemplate", "metadata_templates"),
  data.path = here::here("dataPackages", "DataPackageTemplate", "data_objects"),
  data.table = data_files,
  eml.path = here::here("dataPackages", "DataPackageTemplate", "eml"),
  dataset.title = params$dataPackage1Title, 
  temporal.coverage = c(as.Date(beginDate), as.Date(endDate)),
  maintenance.description = 'completed',
  package.id = paste0(params$dataPackage1Description, params$dataPackage1RefID, "-metadata")
)

