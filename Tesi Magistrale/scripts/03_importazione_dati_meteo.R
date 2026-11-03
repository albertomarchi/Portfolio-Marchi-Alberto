install.packages(c("ecmwfr", "lubridate"))
library(ecmwfr)
library(lubridate)


library(ecmwfr)

out_dir <- "era5_europe_monthly"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

years  <- 2010:2024
months <- sprintf("%02d", 1:12)

# Europa: N, W, S, E
area <- c(72, -25, 34, 45)

vars <- c(
  "2m_temperature",
  "10m_u_component_of_wind",
  "10m_v_component_of_wind",
  "surface_solar_radiation_downwards"
)

for (y in years) {
  
  # se esiste già un file per quell'anno, salta (evita riscarichi)
  if (length(list.files(out_dir, pattern = paste0(y, ".*\\.nc$"))) > 0) {
    message("Anno ", y, ": già presente (salto).")
    next
  }
  
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
    product_type = "monthly_averaged_reanalysis",
    variable = vars,
    year = as.character(y),
    month = months,
    time = "00:00",
    area = area,
    data_format = "netcdf",
    download_format = "unarchived"
  )
  
  message("Scarico ERA5 mensile anno ", y, " ...")
  
  wf_request(
    request  = request,
    transfer = TRUE,
    path     = out_dir,
    verbose  = TRUE
  )
}

message("Download completato. File presenti: ", length(list.files(out_dir, pattern="\\.nc$")))

zip_dir <- "C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati/era5_europe_monthly"  # <-- cambia
out_nc  <- "C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati/era5_europe_monthly"    # <-- cambia

dir.create(out_nc, showWarnings = FALSE, recursive = TRUE)

zips <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

for (z in zips) {
  y <- sub("\\.zip$", "", basename(z))  # se si chiamano "2010.zip" -> "2010"
  
  tmp <- file.path(out_nc, paste0("_tmp_", y))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  
  unzip(z, exdir = tmp)
  
  extracted <- list.files(tmp, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  
  if (length(extracted) == 0) {
    message("Nessun .nc in: ", basename(z))
    unlink(tmp, recursive = TRUE, force = TRUE)
    next
  }
  
  for (i in seq_along(extracted)) {
    new_name <- file.path(out_nc, sprintf("era5_%s_part%02d.nc", y, i))
    file.rename(extracted[i], new_name)
  }
  
  unlink(tmp, recursive = TRUE, force = TRUE)
  message("OK ", y, ": estratti ", length(extracted), " .nc")
}

nc_files <- list.files(out_nc, pattern = "^era5_\\d{4}_part\\d+\\.nc$", full.names = TRUE)
length(nc_files)
head(nc_files)


## raggruppamento dati

eu27_iso2 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
               "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
               "PL","PT","RO","SK","SI","ES","SE")

library(sf)
library(dplyr)

shp_path <- "C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati/CNTR_RG_10M_2024_3035.shp"

countries_sf <- st_read(shp_path)

# controllo rapido
names(countries_sf)
nrow(countries_sf)

eu27_sf <- countries_sf %>%
  dplyr::filter(CNTR_ID %in% eu27_iso2) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    iso2 = ifelse(CNTR_ID == "EL", "GR", CNTR_ID)
  ) %>%
  dplyr::transmute(
    iso2 = iso2,
    country = NAME_ENGL,
    geometry = geometry
  )

nrow(eu27_sf)

out_nc <- "C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati/era5_europe_monthly"  # <- dove hai i 30 .nc
nc_files <- list.files(out_nc, pattern="^era5_\\d{4}_part\\d+\\.nc$", full.names=TRUE)

length(nc_files)  # deve essere 30

read_var_from_year <- function(year, varname, out_nc) {
  f1 <- file.path(out_nc, sprintf("era5_%d_part01.nc", year))
  f2 <- file.path(out_nc, sprintf("era5_%d_part02.nc", year))
  
  pick <- function(f) {
    r <- terra::rast(f)
    idx <- which(grepl(paste0("^", varname), names(r)))
    if (length(idx) == 0) return(NULL)
    r[[idx]]
  }
  
  v <- pick(f1)
  if (is.null(v)) v <- pick(f2)
  v
}

annual_grids_for_year <- function(year, out_nc) {
  t2m  <- read_var_from_year(year, "t2m",  out_nc)
  u10  <- read_var_from_year(year, "u10",  out_nc)
  v10  <- read_var_from_year(year, "v10",  out_nc)
  ssrd <- read_var_from_year(year, "ssrd", out_nc)
  
  if (any(sapply(list(t2m,u10,v10,ssrd), is.null))) {
    stop("Manca almeno una variabile nell'anno: ", year,
         ". Controlla i nomi variabili nei NetCDF.")
  }
  
  # media annuale da 12 mesi
  t2m_ann  <- terra::app(t2m,  mean, na.rm = TRUE) - 273.15
  u10_ann  <- terra::app(u10,  mean, na.rm = TRUE)
  v10_ann  <- terra::app(v10,  mean, na.rm = TRUE)
  ssrd_ann <- terra::app(ssrd, mean, na.rm = TRUE)
  
  wind_ann <- sqrt(u10_ann^2 + v10_ann^2)
  
  names(t2m_ann)  <- "temp_c"
  names(wind_ann) <- "wind_ms"
  names(ssrd_ann) <- "solar_wm2"
  
  c(t2m_ann, wind_ann, ssrd_ann)
}

years <- 2010:2024
res_list <- vector("list", length(years))
names(res_list) <- years

for (y in years) {
  message("Processing ERA5 anno ", y, " ...")
  ras <- annual_grids_for_year(y, out_nc)
  
  # area-weighted mean per ogni paese
  temp  <- exactextractr::exact_extract(ras[["temp_c"]],   eu27_sf, "mean")
  wind  <- exactextractr::exact_extract(ras[["wind_ms"]],  eu27_sf, "mean")
  solar <- exactextractr::exact_extract(ras[["solar_wm2"]], eu27_sf, "mean")
  
  res_list[[as.character(y)]] <- tibble(
    iso2 = eu27_sf$iso2,
    country = eu27_sf$country,
    year = y,
    temp_c = temp,
    wind_ms = wind,
    solar_wm2 = solar
  )
}

era5_country_year <- bind_rows(res_list) %>%
  arrange(iso2, year)

dplyr::glimpse(era5_country_year)

era5_country_year <- era5_country_year %>%
  mutate(solar_kwh_m2_year = solar_wm2 * 24 * 365 / 1000)

install.packages("writexl")
library(writexl)

write_xlsx(
  era5_country_year,
  "C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati/era5_country_year.xlsx"
)
