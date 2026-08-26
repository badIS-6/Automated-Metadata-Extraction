############ Automated metadata extraction pipeline

# by: Badis Zammouri



library(data.table)
library(dplyr)
library(stringr)
library(readxl)
library(tools)



#######################################

base_dir <- "xxxxxx"

categories <- c(
  "Agricultural products",
  "Animal production",
  "Dams & Irrigated areas",
  "Forests",
  "Land development",
  "Rainfall"
)

publisher <- "xxx"

pipeline <- "Cleaning and processing by: xxxx"

governorates <- c("Ariana", "Beja", "Béja", "Ben Arous","Bizerte","Gabes","Gabès", "Kef", "Jandouba","Gafsa","Jendouba","Kairouan", "Kasserine","Kebili","Kébili","Silana", "Kef","Le Kef","Mahdia","Manouba","Medenine","Médenine","Monastir","Nabeul","Sfax","Sidi Bouzid","Siliana","Sousse","Tataouine","Tozeur","Tunis","Zaghouan")

#######################################

stopwords <- c(
  "evolution", "of", "with", "by", "number", "distribution", "list", "according",
  "to", "in", "and", "the", "total", "area", "functional", "approved", "active",
  "different", "measures", "for", "achievements", "support", "statistics",
  "monitoring", "created", "zones", "intervention", "program", "regional",
  "a", "an", "at", "on", "from", "into", "over", "under", "between", "among",
  "within", "across", "through", "during", "before", "after", "since", "per",
  "via", "as", "than", "or", "but", "nor", "including", "excluding",
  "data", "dataset", "datasets", "database", "indicator", "indicators",
  "index", "indices", "report", "reports", "survey", "surveys",
  "inventory", "register", "registry", "catalog", "catalogue",
  "average", "averages", "rate", "rates", "share", "shares",
  "percentage", "percent", "proportion", "count", "counts",
  "value", "values", "level", "levels", "status", "type", "types",
  "category", "categories", "classification", "classifications",
  "annual", "yearly", "monthly", "quarterly", "daily", "weekly",
  "current", "historical", "recent", "latest", "trend", "trends",
  "change", "changes", "growth", "development", "developments",
  "national", "local", "global", "international", "municipal",
  "district", "province", "state", "county", "community",
  "public", "private", "government", "administrative", "governorate of",
  "project", "projects", "initiative", "initiatives", "-", "mm",
  ",", "strategy", "strategies", "plan", "plans", "scheme", "schemes",
  "framework", "frameworks", "policy", "policies", "vocation",
  "general", "main", "selected", "available", "associated",
  "related", "based", "defined", "identified", "estimated",
  "observed", "measured", "recorded", "reported", "associations", 
  "basic", "batches", "cells", "centers", "cereal", "cold", "collection", "collective", 
  "crda", "crees", "delegation", "extension", "gdaps", "gdas", "governorate", "groups", 
  "infrastructure", "interest", "june", "outreach", "port", "region", "storage", "technician", "year",
  "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november",
  "december", "hectares", "processing", "summary", "consolidation", "gouv", "intervene", "jandouba",
  "nalles", "nlles", "pno", "product", "protection", "rces", "realist", "supporting", "zone",
  "deep", "exploitation", "million", "overall", "perimeters", "ppis", "processing", "produced", 
  "quantities", "secadenord", "source", "surface", "tables", "gouvernorat", "its", "numbers", "number",
  "Agricultural", "Agricultural","Kef", "Mixed", "Territorial", "Foodstuffs", "Origin", "a", "b", "c", 
  "d", "e", "f", "g", "h", "i", "j","k", "l", "m", "n", "o", "p", "q", "r", "s", "t","u", "v", "w", "x", "y", "z",
  "Productive", "Heads", "Ha", "Fodder", "Industrial", "Zd"
)






clean_text <- function(x) {
  
  x <- as.character(x)
  
  x[is.na(x)] <- ""
  
  x <- str_replace_all(
    x,
    "[_\\r\\n\\t]+",
    " "
  )
  
  x <- str_squish(x)
  
  x
}

normalize_key <- function(x) {
  
  x <- clean_text(x)
  
  x <- iconv(
    x,
    to = "ASCII//TRANSLIT"
  )
  
  x <- tolower(x)
  
  x <- str_replace_all(
    x,
    "[^a-z0-9]+",
    " "
  )
  
  x <- str_squish(x)
  
  x
}

format_list <- function(
    x,
    empty = "Not identified"
) {
  
  x <- clean_text(x)
  
  x <- x[nzchar(x)]
  
  x <- unique(x)
  
  if (length(x) == 0) {
    return(empty)
  }
  
  paste(
    x,
    collapse = ", "
  )
}

get_extension <- function(path) {
  
  ext <- tolower(
    file_ext(path)
  )
  
  if (!nzchar(ext)) {
    return("unknown")
  }
  
  ext
}

dataset_files <- function(category_dir) {
  
  if (!dir.exists(category_dir)) {
    return(character())
  }
  
  files <- list.files(
    category_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
  
  files[
    tolower(file_ext(files)) %in%
      c(
        "csv",
        "xlsx",
        "xls",
        "ods",
        "tsv"
      )
  ]
}

safe_read_table <- function(
    path,
    max_rows = 5000
) {
  
  ext <- get_extension(path)
  
  result <- tryCatch(
    
    {
      
      if (ext == "csv") {
        
        fread(
          file = path,
          nrows = max_rows,
          encoding = "UTF-8",
          showProgress = FALSE,
          data.table = FALSE
        )
        
      } else if (ext == "tsv") {
        
        fread(
          file = path,
          sep = "\t",
          nrows = max_rows,
          encoding = "UTF-8",
          showProgress = FALSE,
          data.table = FALSE
        )
        
      } else if (ext %in% c("xlsx", "xls")) {
        
        sheets <- tryCatch(
          excel_sheets(path),
          error = function(e) character()
        )
        
        if (length(sheets) == 0) {
          return(NULL)
        }
        
        read_excel(
          path,
          sheet = sheets[1],
          n_max = max_rows
        )
        
      } else if (ext == "ods") {
        
        if (!requireNamespace(
          "readODS",
          quietly = TRUE
        )) {
          return(NULL)
        }
        
        readODS::read_ods(
          path,
          sheet = 1,
          n_max = max_rows
        )
        
      } else {
        
        NULL
        
      }
      
    },
    
    error = function(e) {
      NULL
    }
  )
  
  if (is.null(result)) {
    return(NULL)
  }
  
  as.data.frame(
    result,
    stringsAsFactors = FALSE
  )
}

year_values <- function(x) {
  
  x <- clean_text(x)
  
  if (length(x) == 0) {
    return(integer())
  }
  
  extracted <- str_extract_all(
    x,
    "(?<!\\d)(?:19|20)\\d{2}(?!\\d)"
  )
  
  values <- unlist(
    extracted,
    use.names = FALSE
  )
  
  values <- suppressWarnings(
    as.integer(values)
  )
  
  values <- values[
    !is.na(values) &
      values >= 1900 &
      values <= 2100
  ]
  
  sort(
    unique(values)
  )
}

extract_years_from_title <- function(title) {
  
  year_values(title)
}

extract_years_from_data <- function(df) {
  
  if (
    is.null(df) ||
    nrow(df) == 0 ||
    ncol(df) == 0
  ) {
    return(integer())
  }
  
  years <- integer()
  
  for (j in seq_len(ncol(df))) {
    
    column <- df[[j]]
    
    if (
      inherits(
        column,
        c(
          "Date",
          "POSIXct",
          "POSIXlt"
        )
      )
    ) {
      
      extracted <- suppressWarnings(
        as.integer(
          format(
            column,
            "%Y"
          )
        )
      )
      
      years <- c(
        years,
        extracted
      )
      
      next
    }
    
    values <- clean_text(column)
    
    numeric_values <- suppressWarnings(
      as.integer(values)
    )
    
    numeric_years <- numeric_values[
      !is.na(numeric_values) &
        numeric_values >= 1900 &
        numeric_values <= 2100
    ]
    
    years <- c(
      years,
      numeric_years
    )
    
    text_years <- unlist(
      lapply(
        values,
        year_values
      ),
      use.names = FALSE
    )
    
    years <- c(
      years,
      text_years
    )
  }
  
  sort(
    unique(
      years[
        years >= 1900 &
          years <= 2100
      ]
    )
  )
}

extract_governorates <- function(x) {
  
  x <- clean_text(x)
  
  if (length(x) == 0) {
    return(character())
  }
  
  normalized <- normalize_key(x)
  
  found <- character()
  
  canonical <- c(
    "Ariana",
    "Béja",
    "Ben Arous",
    "Bizerte",
    "Gabès",
    "Gafsa",
    "Jendouba",
    "Kairouan",
    "Kasserine",
    "Kébili",
    "Le Kef",
    "Mahdia",
    "Manouba",
    "Médenine",
    "Monastir",
    "Nabeul",
    "Sfax",
    "Sidi Bouzid",
    "Siliana",
    "Sousse",
    "Tataouine",
    "Tozeur",
    "Tunis",
    "Zaghouan"
  )
  
  canonical_normalized <- normalize_key(
    canonical
  )
  
  for (i in seq_along(canonical)) {
    
    governorate_normalized <- canonical_normalized[i]
    
    if (
      length(governorate_normalized) != 1 ||
      is.na(governorate_normalized) ||
      !nzchar(governorate_normalized)
    ) {
      next
    }
    
    pattern <- paste0(
      "(^| )",
      governorate_normalized,
      "($| )"
    )
    
    matches <- str_detect(
      normalized,
      regex(
        pattern,
        ignore_case = TRUE
      )
    )
    
    if (any(
      matches,
      na.rm = TRUE
    )) {
      found <- c(
        found,
        canonical[i]
      )
    }
  }
  
  unique(found)
}

remove_governorates_from_text <- function(text) {
  
  text <- clean_text(text)
  
  if (length(text) == 0) {
    return(character())
  }
  
  normalized <- normalize_key(text)
  
  canonical <- c(
    "Ariana",
    "Béja",
    "Ben Arous",
    "Bizerte",
    "Gabès",
    "Gafsa",
    "Jendouba",
    "Kairouan",
    "Kasserine",
    "Kébili",
    "Le Kef",
    "Mahdia",
    "Manouba",
    "Médenine",
    "Monastir",
    "Nabeul",
    "Sfax",
    "Sidi Bouzid",
    "Siliana",
    "Sousse",
    "Tataouine",
    "Tozeur",
    "Tunis",
    "Zaghouan"
  )
  
  normalized_governorates <- normalize_key(
    canonical
  )
  
  for (g in normalized_governorates) {
    
    if (
      length(g) != 1 ||
      is.na(g) ||
      !nzchar(g)
    ) {
      next
    }
    
    normalized <- str_replace_all(
      normalized,
      regex(
        paste0(
          "\\b",
          g,
          "\\b"
        ),
        ignore_case = TRUE
      ),
      " "
    )
  }
  
  str_squish(normalized)
}


title_clean_for_keywords <- function(title) {
  
  x <- file_path_sans_ext(
    basename(title)
  )
  
  x <- tolower(x)
  
  x <- str_replace_all(
    x,
    "[_]+",
    " "
  )
  
  x <- str_replace_all(
    x,
    "[,;:/|()\\[\\]{}]+",
    " "
  )
  
  x <- str_replace_all(
    x,
    "[0-9]+",
    " "
  )
  
  x <- clean_text(x)
  
  # Normalize accents/case while keeping word order
  x <- normalize_key(x)
  
  # Remove governorates
  x <- remove_governorates_from_text(x)
  
  # Remove stopwords as complete words
  normalized_stopwords <- normalize_key(
    stopwords
  )
  
  for (sw in normalized_stopwords) {
    
    if (
      length(sw) != 1 ||
      is.na(sw) ||
      !nzchar(sw)
    ) {
      next
    }
    
    x <- str_replace_all(
      x,
      regex(
        paste0(
          "\\b",
          sw,
          "\\b"
        ),
        ignore_case = TRUE
      ),
      " "
    )
  }
  
  # Remove additional generic words
  additional_stopwords <- c(
    "de",
    "du",
    "des",
    "la",
    "le",
    "les",
    "un",
    "une",
    "sur",
    "dans",
    "pour",
    "avec",
    "selon",
    "aux",
    "au",
    "en",
    "et",
    "ou",
    "que",
    "qui"
  )
  
  for (sw in additional_stopwords) {
    
    x <- str_replace_all(
      x,
      regex(
        paste0(
          "\\b",
          sw,
          "\\b"
        ),
        ignore_case = TRUE
      ),
      " "
    )
  }
  
  # Collapse gaps created by removed words
  x <- str_squish(x)
  
  x
}


extract_subcategories <- function(titles) {
  
  phrases <- character()
  
  for (title in titles) {
    
    phrase <- title_clean_for_keywords(
      title
    )
    
    if (
      length(phrase) == 1 &&
      !is.na(phrase) &&
      nzchar(phrase)
    ) {
      
      phrases <- c(
        phrases,
        phrase
      )
    }
  }
  
  phrases <- unique(
    phrases
  )
  
  if (length(phrases) == 0) {
    return(character())
  }
  
  # Convert normalized text back to readable title case
  phrases <- str_to_title(
    phrases
  )
  
  sort(
    unique(phrases)
  )
}
unit_dictionary <- c(
  "tonnes" = "tonnes",
  "ton" = "tonnes",
  "tons" = "tonnes",
  "kg" = "kg",
  "kilogram" = "kg",
  "kilograms" = "kg",
  "quintal" = "quintals",
  "quintals" = "quintals",
  "ha" = "ha",
  "hectare" = "ha",
  "hectares" = "ha",
  "km2" = "km²",
  "km²" = "km²",
  "m2" = "m²",
  "m²" = "m²",
  "litre" = "litres",
  "litres" = "litres",
  "liter" = "litres",
  "liters" = "litres",
  "m3" = "m³",
  "m³" = "m³",
  "million m3" = "million m³",
  "millions m3" = "million m³",
  "dinar" = "TND",
  "dinars" = "TND",
  "tnd" = "TND",
  "td" = "TND",
  "%" = "%",
  "percent" = "%",
  "percentage" = "%",
  "number" = "number",
  "units" = "units",
  "unit" = "units",
  "head" = "head",
  "heads" = "head",
  "cubic meter" = "m³",
  "cubic meters" = "m³"
)

extract_units_from_text <- function(text) {
  
  text <- clean_text(text)
  
  if (length(text) == 0) {
    return(character())
  }
  
  original <- paste(
    text,
    collapse = " "
  )
  
  normalized <- normalize_key(
    original
  )
  
  found <- character()
  
  for (u in names(unit_dictionary)) {
    
    pattern <- normalize_key(
      u
    )
    
    if (
      length(pattern) != 1 ||
      is.na(pattern) ||
      !nzchar(pattern)
    ) {
      next
    }
    
    if (
      str_detect(
        normalized,
        regex(
          paste0(
            "\\b",
            pattern,
            "\\b"
          ),
          ignore_case = TRUE
        )
      )
    ) {
      
      found <- c(
        found,
        unname(
          unit_dictionary[[u]]
        )
      )
    }
  }
  
  if (
    str_detect(
      original,
      fixed("%")
    )
  ) {
    found <- c(
      found,
      "%"
    )
  }
  
  if (
    str_detect(
      original,
      regex(
        "m[³3]",
        ignore_case = TRUE
      )
    )
  ) {
    found <- c(
      found,
      "m³"
    )
  }
  
  if (
    str_detect(
      original,
      regex(
        "km[²2]",
        ignore_case = TRUE
      )
    )
  ) {
    found <- c(
      found,
      "km²"
    )
  }
  
  if (
    str_detect(
      original,
      regex(
        "\\bha\\b",
        ignore_case = TRUE
      )
    )
  ) {
    found <- c(
      found,
      "ha"
    )
  }
  
  unique(found)
}

extract_units_from_df <- function(df) {
  
  if (
    is.null(df) ||
    nrow(df) == 0 ||
    ncol(df) == 0
  ) {
    return(character())
  }
  
  units <- character()
  
  units <- c(
    units,
    extract_units_from_text(
      names(df)
    )
  )
  
  for (j in seq_len(ncol(df))) {
    
    column <- df[[j]]
    
    sample_values <- head(
      clean_text(column),
      500
    )
    
    units <- c(
      units,
      extract_units_from_text(
        sample_values
      )
    )
  }
  
  unique(units)
}

identify_main_units <- function(
    df,
    title
) {
  
  if (
    !is.null(df) &&
    ncol(df) > 0
  ) {
    
    column_units <- extract_units_from_text(
      names(df)
    )
    
    if (length(column_units) > 0) {
      return(column_units)
    }
  }
  
  title_units <- extract_units_from_text(
    title
  )
  
  if (length(title_units) > 0) {
    return(title_units)
  }
  
  if (!is.null(df)) {
    
    data_units <- extract_units_from_df(
      df
    )
    
    if (length(data_units) > 0) {
      return(data_units)
    }
  }
  
  "Not identified"
}

dataset_title <- function(path) {
  
  title <- file_path_sans_ext(
    basename(path)
  )
  
  title <- str_replace_all(
    title,
    "[_]+",
    " "
  )
  
  title <- str_replace_all(
    title,
    "\\s+",
    " "
  )
  
  str_squish(title)
}

dataset_metadata <- function(path) {
  
  title <- dataset_title(
    path
  )
  
  df <- safe_read_table(
    path
  )
  
  years_title <- extract_years_from_title(
    title
  )
  
  years_data <- extract_years_from_data(
    df
  )
  
  years <- sort(
    unique(
      c(
        years_title,
        years_data
      )
    )
  )
  
  governorates_title <- extract_governorates(
    title
  )
  
  governorates_data <- character()
  
  if (
    !is.null(df) &&
    nrow(df) > 0 &&
    ncol(df) > 0
  ) {
    
    sample_df <- df[
      seq_len(
        min(
          nrow(df),
          5000
        )
      ),
      ,
      drop = FALSE
    ]
    
    text_sample <- apply(
      as.data.frame(
        lapply(
          sample_df,
          clean_text
        )
      ),
      1,
      paste,
      collapse = " "
    )
    
    governorates_data <- extract_governorates(
      text_sample
    )
  }
  
  governors <- unique(
    c(
      governorates_title,
      governorates_data
    )
  )
  
  units <- identify_main_units(
    df,
    title
  )
  
  list(
    title = title,
    extension = get_extension(path),
    years = years,
    governors = governors,
    units = units
  )
}

collapse_metadata <- function(
    metadata_list
) {
  
  titles <- vapply(
    metadata_list,
    function(x) x$title,
    character(1)
  )
  
  years <- sort(
    unique(
      unlist(
        lapply(
          metadata_list,
          function(x) x$years
        ),
        use.names = FALSE
      )
    )
  )
  
  governors <- unique(
    unlist(
      lapply(
        metadata_list,
        function(x) x$governors
      ),
      use.names = FALSE
    )
  )
  
  units <- unique(
    unlist(
      lapply(
        metadata_list,
        function(x) x$units
      ),
      use.names = FALSE
    )
  )
  
  extensions <- unique(
    unlist(
      lapply(
        metadata_list,
        function(x) x$extension
      ),
      use.names = FALSE
    )
  )
  
  list(
    titles = titles,
    years = years,
    governors = governors,
    units = units,
    extensions = extensions
  )
}

format_years <- function(years) {
  
  years <- sort(
    unique(years)
  )
  
  if (length(years) == 0) {
    return("Not identified")
  }
  
  if (length(years) == 1) {
    return(as.character(years))
  }
  
  # Detect continuous coverage.
  if (
    length(years) ==
    (max(years) - min(years) + 1)
  ) {
    
    return(
      paste0(
        min(years),
        "-",
        max(years)
      )
    )
  }
  
  paste(
    years,
    collapse = ", "
  )
}

format_file_types <- function(
    extensions
) {
  
  if (length(extensions) == 0) {
    return("None")
  }
  
  counts <- table(
    toupper(extensions)
  )
  
  paste(
    paste0(
      as.integer(counts),
      " ",
      names(counts)
    ),
    collapse = ", "
  )
}

category_metadata <- function(
    category
) {
  
  category_dir <- file.path(
    base_dir,
    category,
    "Clean data"
  )
  
  files <- dataset_files(
    category_dir
  )
  
  if (length(files) == 0) {
    
    return(
      data.frame(
        Category = category,
        `Number of datasets (and types)` = "0 datasets",
        Publisher = publisher,
        `Cleaning and processing pipeline` = pipeline,
        `Sub-categories` = "Not identified",
        `Main units` = "Not identified",
        `Governorates involved` = "Not identified",
        `Years covered` = "Not identified",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }
  
  message(
    "Processing category: ",
    category
  )
  
  message(
    "Datasets found: ",
    length(files)
  )
  
  metadata <- lapply(
    files,
    dataset_metadata
  )
  
  collapsed <- collapse_metadata(
    metadata
  )
  
  type_text <- format_file_types(
    collapsed$extensions
  )
  
  subcategories <- extract_subcategories(
    collapsed$titles
  )
  
  data.frame(
    Category = category,
    
    `Number of datasets (and types)` =
      paste0(
        length(files),
        " datasets (",
        type_text,
        ")"
      ),
    
    Publisher = publisher,
    
    `Cleaning and processing pipeline` =
      pipeline,
    
    `Sub-categories` =
      format_list(
        subcategories
      ),
    
    `Main units` =
      format_list(
        collapsed$units
      ),
    
    `Governorates involved` =
      format_list(
        collapsed$governors
      ),
    
    `Years covered` =
      format_years(
        collapsed$years
      ),
    
    check.names = FALSE,
    
    stringsAsFactors = FALSE
  )
}

catalog <- bind_rows(
  lapply(
    categories,
    category_metadata
  )
)

output_dir <- file.path(
  base_dir,
  "Catalog metadata output"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  "Metadata.csv"
)

write.csv(
  catalog,
  file = output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
