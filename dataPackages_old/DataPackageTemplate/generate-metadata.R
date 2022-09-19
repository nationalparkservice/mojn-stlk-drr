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

#### Step 0: knit the DRR
## YOU MUST DO THIS FIRST ##

#### Step 1: zip the data and also copy raw data to data_objects ####
# STLK data is stored in a set of .csv files in the data/raw folder.
# Zip these files and save them in data/final (since zipping them is the only processing that occurs)

# Get table names and descriptions
tables <- read_tsv(here::here("data", "dictionary", "data-dictionary-tables.txt"), lazy = FALSE) %>%
  mutate(File_Name = paste0(Table_Name, ".csv"))

zip::zip(here::here("data", "final", "data.zip"), files = tables$File_Name, root = here::here("data", "raw"))
file.copy(here::here("data", "raw", tables$File_Name),
          here::here("dataPackages", "DataPackageTemplate", "data_objects"), 
          overwrite = TRUE)

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
data_dict <- readr::read_tsv(here::here("data", "dictionary", "data-dictionary-fields.txt"), lazy = FALSE) %>%
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
cat_vars <- readr::read_tsv(here::here("data", "dictionary", "data-dictionary-categories.txt"), lazy = FALSE)

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
  data.table = tables$File_Name,
  data.table.name = tables$Table_Name,
  data.table.description = tables$Table_Description,
  eml.path = here::here("dataPackages", "DataPackageTemplate", "eml"),
  dataset.title = params$dataPackage1Title, 
  temporal.coverage = c(as.Date(beginDate), as.Date(endDate)),
  maintenance.description = 'completed',
  package.id = paste0(params$dataPackage1Description, "_", params$dataPackage1RefID, "_metadata")
)

#### Step 6: Create Manifest File and zip up the package ####

manifestfilename <- here::here("dataPackages", "DataPackageTemplate", "eml", paste0(params$dataPackage1Description, "_", params$dataPackage1RefID, "_manifest.txt"))
metadatafilename <- here::here("dataPackages", "DataPackageTemplate", "eml", paste0(params$dataPackage1Description, "_", params$dataPackage1RefID, "_metadata.xml"))
datafilename <- here::here("dataPackages", "DataPackageTemplate", "eml", paste0(params$dataPackage1Description, "_", params$dataPackage1RefID, "_data.zip"))
datapackagefilename <- here::here("dataPackages", "DataPackageTemplate", "eml", paste0(params$dataPackage1Description, "_", params$dataPackage1RefID, "_datapackage.zip"))

cat("This data package was produced by the National Park Service (NPS) Inventory and Monitoring Division and can be downloaded from the [NPS Data Store](https://irma.nps.gov/DataStore/Reference/Profile/",params$dataPackage1RefID,").",file=manifestfilename,"\n",sep="") 
cat("These data are provided under the Creative Commons CC0 1.0 “No Rights Reserved” (see: https://creativecommons.org/publicdomain/zero/1.0/).",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("DATA PRODUCT INFORMATION",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

# ID
cat("ID: ", params$dataPackage1RefID, " Data Store Code.",file=manifestfilename,"\n",sep="",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

#Title
cat("Title: ",params$dataPackage1Title, file=manifestfilename,"\n",sep="",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

#Description
cat("Description: ",params$dataPackage1Description,file=manifestfilename,"\n",sep="",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

#Abstract
cat("Abstract: ",abstract,file=manifestfilename,"\n",sep="",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

#Brief Study Area Description
cat("Brief Study Area Description: ",geographicDescription,file=manifestfilename,"\n",sep="",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

#Keywords
cat("Keywords:",unlist(themekeywords),file=manifestfilename,"\n",sep=" ",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("Date for Data Publication: ",as.character(today()),file=manifestfilename,"\n",sep="",append=TRUE) 
cat("This zip package was generated on: ",as.character(today()),file=manifestfilename,"\n",sep="",append=TRUE) 
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("DATA PACKAGE CONTENTS",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("This zip package contains the following documentation files:",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("- This readme file: ", manifestfilename,"\n",file=manifestfilename,sep="",append=TRUE)

cat("- Machine-readable metadata file describing the data set(s): ", params$dataPackage1Description, "_", params$dataPackage1RefID, "_metadata.xml. This file uses the Ecological Metadata Language (EML) schema. Learn more about this format at https://knb.ecoinformatics.org/external//emlparser/docs/eml-2.1.1/index.html#N1022A.",file=manifestfilename, "\n",sep="",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("This zip package contains the following data set(s):",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat(params$dataPackage1Description, "_", params$dataPackage1RefID, "_data.zip",file=manifestfilename,sep="",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("ADDITIONAL INFORMATION",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("Primary related products: The following product(s) were created concurrently with this dataset:",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("1. Data Release Report. Document describing the methods and the analysis code used to generate data set. Available at ", "https://irma.nps.gov/DataStore/Reference/Profile/", params$reportRefID, ".", "\n",file=manifestfilename,sep="",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("CHANGE LOG",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("N/A",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("ADDITIONAL REMARKS",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("N/A",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)


cat("NPS DATA POLICY AND CITATION GUIDELINES",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("See NPS Inventory and Monitoring Division's data policy and citation guidelines at https://irma.nps.gov/content/portal/about/.",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)


cat("DATA QUALITY AND VERSIONING",file=manifestfilename,sep="\n",append=TRUE)
cat("------------------------",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)

cat("The data contained in this file are considered Accepted under the IMD Data Certification Guidance (https://www.google.com/url?q=https://irma.nps.gov/DataStore/Reference/Profile/2227397). Updates to the data, QA/QC and/or processing algorithms over time will occur on an as-needed basis.  Please check back to this site for updates tracked in change logs.",file=manifestfilename,sep="\n",append=TRUE)
cat(" ",file=manifestfilename,sep="\n",append=TRUE)



### Zip up file

# Zip the data
zip::zip(datafilename, files = tables$File_Name, root = here::here("data", "raw"))
# Zip the whole package
zip::zipr(datapackagefilename,c(datafilename,manifestfilename,metadatafilename), recurse=FALSE)
