library(httr)

# Function to convert a string to a numeric value, or return NA if it fails
tryfloat <- function(v) {
  out <- tryCatch(
    {
      as.numeric(v)
    },
    error = function(e) {
      return(NA)  # Use NA for not-a-number in R
    }
  )
  return(out)
}

# Function to convert a positive number to a negative number
make_neg <- function(v) {
  v <- tryCatch(
    {
      as.numeric(v)
    },
    error = function(e) {
      return(NA)
    }
  )
  if (!is.na(v) && is.numeric(v) && v >= 0) {
    v <- v * -1
  }
  return(v)
}

# Function to read points from a file and return them as a list
read_point_file <- function(fn = "mypoints.csv") {
  fill_list <- c("lat", "lon")
  out_d <- list()
  reader <- read.csv(fn)
  for (i in 1:nrow(reader)) {
    row <- reader[i, ]
    if (grepl("#", row[1]) || trimws(row[1]) == "Name") {
      next
    }
    try({
      name <- trimws(row[1])
      param <- tolower(trimws(row[2]))
      d_or_m <- tolower(trimws(row[3]))
      
      place_name <- paste(name, param, d_or_m, sep = "_")
      out_d[[place_name]] <- list(
        lat = tryfloat(row[4]),
        lon = make_neg(tryfloat(row[5])),
        d_or_m = d_or_m,
        param = param
      )
    }, silent = TRUE)
  }
  return(out_d)
}

# Function to get data for a single place
get_one <- function(d, place) {
  print(paste("Getting data for:", place))
  lat <- d[[place]]$lat
  lon <- d[[place]]$lon
  param <- d[[place]]$param
  fparam <- ifelse(param %in% names(param_dict), param_dict[[param]], param)
  d_or_m <- d[[place]]$d_or_m
  check_name <- paste0(place, "_all_years.csv")
  if (file.exists(check_name)) {
    return(FALSE)
  }
  for (year in seq(first_year, last_year)) {
    print(year)
    if (d_or_m == "daily") {
      this_url <- sprintf(daily_url, year, fparam, fparam, lat, lon, year, year)
    } else {
      this_url <- sprintf(monthly_url, year, fparam, param, lat, lon)
    }
    success <- FALSE
    remaining_download_tries <- 5
    while (remaining_download_tries > 0) {
      tryCatch({
        download.file(this_url, paste0(place, "_", year, ".csv"))
        remaining_download_tries <- 0
        success <- TRUE
      }, error = function(e) {
        cat("Trying again:", year, "\n")
        remaining_download_tries <- remaining_download_tries - 1
        Sys.sleep(3)
      })
    }
    if (!success) break
  }
  return(TRUE)
}

# Function to fix monthly lines
fix_monthly_lines <- function(line, year) {
  line <- gsub("1980-", paste0(year, "-"), line)
  pl <- strsplit(line, ",")[[1]]
  ts <- substr(pl[1], 1, 7)
  pl[1] <- ts
  outline <- paste(pl, collapse = ",")
  return(outline)
}

# Function to merge year files
merge_years <- function(d, place) {
  print(paste("Merging year files for:", place))
  outfilename <- paste0(place, "_all_years.csv")
  outfile <- file(outfilename, "w")
  for (year in seq(first_year, last_year)) {
    infilename <- paste0(place, "_", year, ".csv")
    if (!file.exists(infilename)) next
    infile <- file(infilename, "r")
    line_count <- 1
    while (length(line <- readLines(infile, n = 1)) > 0) {
      if (year != first_year && line_count == 1) {
        line_count <- line_count + 1
        next
      }
      if (grepl("monthly", infilename)) {
        line <- fix_monthly_lines(line, year)
      }
      writeLines(line, outfile)
      line_count <- line_count + 1
    }
    close(infile)
    file.remove(infilename)
  }
  close(outfile)
}

# Main execution
param_dict <- list('aet' = 'AET', 'pet' = 'PET', 'deficit' = 'Deficit')
first_year <- 1980
last_year <- 2019
monthly_url <- 'http://www.yellowstone.solutions/thredds/ncss/daily_or_monthly/v2_historical/monthly/v2_%d_%s_monthly.nc4?var=%s&latitude=%f&longitude=%f&time_start=1980-01-16T05:14:31.916Z&time_end=1980-12-16T00:34:14.059Z&accept=csv'
daily_url <- 'http://www.yellowstone.solutions/thredds/ncss/daily_or_monthly/v2_historical/daily/v2_%d_%s.nc4?var=%s&latitude=%f&longitude=%f&time_start=%d-01-01T12:00:00Z&time_end=%d-12-31T12:00:00Z&accept=csv'

# Read data
d <- read_point_file()

# Iterate over places
for (place in names(d)) {
  new <- get_one(d, place)
  if (new) {
    tryCatch({
      merge_years(d, place)
    }, error = function(e) {
      cat('Error occurred during merging:', e$message, "\n")
    })
  } else {
    cat('Skipping, duplicate:', place, "\n")
  }
}
