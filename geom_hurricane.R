################# ~~~~~~~~~~~~~~~~~ ######## ~~~~~~~~~~~~~~~~~ #################
##                                                                            ##
##                   Coursera - Software Development with R                   ##
##                          Assignment Week 4 - geom                          ##
##                                                                            ##
##                              Marco R. Morales                              ##
##                                                                            ##
##                                                                            ##
## created: 17.06.2017                                last update: 17.06.2017 ##
################# ~~~~~~~~~~~~~~~~~ ######## ~~~~~~~~~~~~~~~~~ #################
rm(list = ls())
cat("\014")


## set directory ----------------------------------------------------------------
##     
##
setwd("C:/Users/marco/OneDrive/061 Coursera/spec_MasteringSoftwareDevInR/04_RDataVisualization/week4_assignment_geom")
# setwd("C:/Users/mmora/OneDrive/061 Coursera/spec_MasteringSoftwareDevInR/04_RDataVisualization/week4_assignment_geom")

## function: load_hur_data ------------------------------------------------------
##     loads data according to directory and file name
##   

#' Load Hurricane Data 
#' 
#' @importFrom readr read.fwf fwf_widths
#' @param dataDirectory Where the Data is stored
#' @param fileName Name of the file
#' @examples 
#' load_hur_data(dataDirectory = "./010_data/hurricanes_data", fileName = "ebtrk_atlc_1988_2015.txt")
#' 
#' @export
load_hur_data <- function(dataDirectory, fileName) {
        
        ## required:
        require(readr)
        
        ## routine to check if the data is here and to load it accordingly -------------
        filePathSep <- "/"
        ## dataDirectory <- "./010_data/hurricanes_data"
        fileExt <- "txt"
        ## files <- list.files(dataDirectory)  # [1] "ebtrk_atlc_1988_2015.txt"
        
        ## not so nice: if file is not at 1st possition, it might fail
        fullQualifiedFileName <- paste(dataDirectory, fileName, sep = filePathSep) 
        
        ## if the file exists, calculate size and load the file
        if (file.exists(fullQualifiedFileName) == TRUE) {
                fInfo <- file.info(fullQualifiedFileName)
                fileSizeInMb <- paste(round(fInfo$size / 1024 / 1024, 2), "MB")
                print(fileSizeInMb)
                
                ## read the file
                ## this code is given in the instructions
                ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
                ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                                         "hour", "year", "latitude", "longitude",
                                         "max_wind", "min_pressure", "rad_max_wind",
                                         "eye_diameter", "pressure_1", "pressure_2",
                                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                                         "storm_type", "distance_to_land", "final")
                
                ext_tracks <- read_fwf(fullQualifiedFileName, 
                                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                       na = "-99")
                
        } else {
                stop("File not found! #mmor") 
        }
        
        
}


## function: tidy_up_data ------------------------------------------------------
##     clean and tidy data
##   

#' Tidy the hurricane data
#' 
#' @importFrom dplyr mutate select
#' @importFrom tidyr gather spread
#' @importFrom  stringr str_to_title str_extract
#' @param input_data input data
#' 
#' @export
tidy_up_data <- function(input_data) {
        
        require(tidyverse)
        require(stringi)
        
        ## this is the goal here:
        
        #      storm_id                date latitude longitude wind_speed  ne  nw  se
        ## Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         34 200 100 200
        ## Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         50 120  75 120
        ## Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         64  90  60  90
        #   sw
        ## 150
        ##  75
        ##  60
        
        tidyData <- input_data %>%
                dplyr::mutate(storm_id = paste(stringr::str_to_title(storm_name), year, sep = "-"),
                              date     = paste0(year, "-", month, "-", day, " ", hour, ":00:00"), 
                              date     = as.POSIXct(date),
                              wind_speed = max_wind,
                              longitude = - longitude) %>%
                dplyr::select(storm_id, date, latitude, longitude, wind_speed,
                              radius_34_ne, radius_34_se, radius_34_sw, radius_34_nw,
                              radius_50_ne, radius_50_se, radius_50_sw, radius_50_nw,
                              radius_64_ne, radius_64_se, radius_64_sw, radius_64_nw) %>%
                tidyr::gather(variable, value, -storm_id, -date, -latitude, -longitude, -storm_id, -date) %>% 
                #         # A tibble: 6 × 6
                #         storm_id                date latitude longitude   variable value
                # <chr>              <dttm>    <dbl>     <dbl>      <chr> <int>
                # 1 Alberto-1988 1988-08-05 18:00:00     32.0     -77.5 wind_speed    20
                # 2 Alberto-1988 1988-08-06 00:00:00     32.8     -76.2 wind_speed    20
                # 3 Alberto-1988 1988-08-06 06:00:00     34.0     -75.2 wind_speed    20
                # 4 Alberto-1988 1988-08-06 12:00:00     35.2     -74.6 wind_speed    25
                # 5 Alberto-1988 1988-08-06 18:00:00     37.0     -73.5 wind_speed    25
                # 6 Alberto-1988 1988-08-07 00:00:00     38.7     -72.4 wind_speed    25
                mutate(wind_speed = stringr::str_extract(variable, "(34|50|64)"),
                       variable = stringr::str_extract(variable, "(ne|nw|se|sw)")) %>% 
                #         # A tibble: 6 × 7
                #               storm_id                date latitude longitude variable value wind_speed
                #                  <chr>              <dttm>    <dbl>     <dbl>    <chr> <int>      <chr>
                #         1 Alberto-1988 1988-08-05 18:00:00     32.0     -77.5     <NA>    20       <NA>
                #         2 Alberto-1988 1988-08-06 00:00:00     32.8     -76.2     <NA>    20       <NA>
                #         3 Alberto-1988 1988-08-06 06:00:00     34.0     -75.2     <NA>    20       <NA>
                #         4 Alberto-1988 1988-08-06 12:00:00     35.2     -74.6     <NA>    25       <NA>
                #         5 Alberto-1988 1988-08-06 18:00:00     37.0     -73.5     <NA>    25       <NA>
                #         6 Alberto-1988 1988-08-07 00:00:00     38.7     -72.4     <NA>    25       <NA>
                tidyr::spread(variable, value) %>% 
                #         # A tibble: 6 × 10
                #       storm_id                date latitude longitude wind_speed    ne    nw    se    sw `<NA>`
                #          <chr>              <dttm>    <dbl>     <dbl>      <chr> <int> <int> <int> <int>  <int>
                # 1 Alberto-1988 1988-08-05 18:00:00     32.0     -77.5       <NA>    NA    NA    NA    NA     20
                # 2 Alberto-1988 1988-08-06 00:00:00     32.8     -76.2       <NA>    NA    NA    NA    NA     20
                # 3 Alberto-1988 1988-08-06 06:00:00     34.0     -75.2       <NA>    NA    NA    NA    NA     20
                # 4 Alberto-1988 1988-08-06 12:00:00     35.2     -74.6       <NA>    NA    NA    NA    NA     25
                # 5 Alberto-1988 1988-08-06 18:00:00     37.0     -73.5       <NA>    NA    NA    NA    NA     25
                # 6 Alberto-1988 1988-08-07 00:00:00     38.7     -72.4       <NA>    NA    NA    NA    NA     25
                select_(.dots = c('storm_id', 'date', 'latitude', 'longitude', 'wind_speed', 'ne', 'nw', 'se', 'sw'))
        return(tidyData)
}


## function: filter the data ----------------------------------------------------
##     simple filter function

#' Filter the data according to the needs of geom_hurricane
#' 
#' @importFrom dplyr filter
#' @param data input data to be filtered
#' @param hurricane hurricane as character
#' @param observation time to filter the data
#' 
#' @export
filter_data <- function(data, hurricane, observation) {
        data_filtered <- data %>%
                dplyr::filter(storm_id == hurricane & date == observation)
        
        # I am not sure how this NA's came in
        data_filtered <- data_filtered[!is.na(data_filtered$wind_speed), ]
        
        return(data_filtered)
                
        
}


## function: geom_hurricane (building the layer)  --------------------------------
##     quite generic
##     builds the layer based on the geom specifications
##     geom specification is defined in the next function

#' Function which builts the layer for the ggplot
#' 
#' @importFrom ggplot2 layer
#' @param mapping 
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
                geom = hurricane_proto_class, mapping = mapping,  
                data = data, stat = stat, position = position, 
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}

## function: draw_panel_function  ------------------------------------------------
##     function within ggproto
##     I outsourced due to its length

#' Outsourced function which builts the plot
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom dplyr mutate bind_rows rename
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob gpar
#' @param data 
#' @param panel_scales
#' @param coord
#' 
#' @export
draw_panel_function <- function(data, panel_scales, coord) {
        
        ## Transform the data first
        ##     see book chapter 4.7.2
        ##     
        coords <- coord$transform(data, panel_scales)
        
        
        
        ## Transform to meters and add the scale_radii
        ##     wind radii in data are reported in nautical miles
        ##     reference: 
        ##     https://en.wikipedia.org/wiki/Nautical_mile
        data <- data %>%
                mutate(r_ne = r_ne * 1852 * scale_radii,
                       r_se = r_se * 1852 * scale_radii,
                       r_nw = r_nw * 1852 * scale_radii,
                       r_sw = r_sw * 1852 * scale_radii
                )
        ## geosphere::destPoint(p, b, d, a=6378137, f=1/298.257223563, ...)
        ##     Based on starting point (p), direction (b)
        ##     and distance (d), it gives back: destination point
        ##     along shortest path on an ellipsoid (the geodesic)
        ##
        ##     good example - chapter 4:
        ##     https://cran.r-project.org/web/packages/geosphere/vignettes/geosphere.pdf
        
        df_points <- data.frame()
        
        for (i in 1:nrow(data)) {
                ## Example
                ## > head(hur_Ike_data)
                # A tibble: 3 × 9
                
                # storm_id                date latitude longitude wind_speed    ne    nw    se    sw
                #      <chr>              <dttm>    <dbl>     <dbl>      <chr> <int> <int> <int> <int>
                # 1 Ike-2008 2008-09-13 12:00:00     30.3     -95.2         34   125    60   180   125
                # 2 Ike-2008 2008-09-13 12:00:00     30.3     -95.2         50    75    45    90    60
                # 3 Ike-2008 2008-09-13 12:00:00     30.3     -95.2         64    50    20    45    30
                
                
                ## NE / NorthEast Sector
                data_ne <- data.frame(colour = data[i, ]$colour,
                                      fill = data[i, ]$fill, 
                                      geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y),
                                                           ## points along 90° segment
                                                           ##     1 point each degree (°)
                                                           ##     NE: 0° --> 90°
                                                           b = 0:90, 
                                                           d = data[i, ]$r_ne),
                                      group = data[i, ]$group,
                                      PANEL = data[i, ]$PANEL,
                                      alpha = data[i, ]$alpha
                )
                
                ## SE / SouthEast Sector
                data_se <- data.frame(colour = data[i, ]$colour,
                                      fill = data[i, ]$fill, 
                                      geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y),
                                                           ## points along 90° segment
                                                           ##     1 point each degree (°)
                                                           ##     SE: 90° --> 180°
                                                           b = 90:180, 
                                                           d = data[i, ]$r_se),
                                      group = data[i, ]$group,
                                      PANEL = data[i, ]$PANEL,
                                      alpha = data[i, ]$alpha
                )
                
                
                ## NW / NorthWest Sector
                data_nw <- data.frame(colour = data[i, ]$colour,
                                      fill = data[i, ]$fill, 
                                      geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y),
                                                           ## points along 90° segment
                                                           ##     1 point each degree (°)
                                                           ##     NW: 270° --> 360°
                                                           b = 270:360, 
                                                           d = data[i, ]$r_nw),
                                      group = data[i, ]$group,
                                      PANEL = data[i, ]$PANEL,
                                      alpha = data[i, ]$alpha
                )
                
                
                
                ## SW / SouthWest Sector
                data_sw <- data.frame(colour = data[i, ]$colour,
                                      fill = data[i, ]$fill, 
                                      geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y),
                                                           ## points along 90° segment
                                                           ##     1 point each degree (°)
                                                           ##     SW: 180° --> 270°
                                                           b = 180:270, 
                                                           d = data[i, ]$r_sw),
                                      group = data[i, ]$group,
                                      PANEL = data[i, ]$PANEL,
                                      alpha = data[i, ]$alpha
                )
                
                df_points <- dplyr::bind_rows(list(df_points, data_nw, data_ne, data_se, data_sw))

                
        } ## for-loop END
        

        
        ## New names: x and y (old: long and lat)
        df_points <- df_points %>%
                dplyr::rename(x = lon, 
                              y = lat
                              )
        
        
        ## Convert to character
        ##     or else: doesn't read colour correctly
        ##     (it remains black)
        df_points$colour <- base::as.character(df_points$colour)
        df_points$fill <- base::as.character(df_points$fill)
        
        coords_df <- coord$transform(df_points, panel_scales) 
        

        
        cat("==================================== coords_df ====================================\n")
        cat("=================================================================================== \n\n")
        print(unique(coords_df$group))
        print(head(coords_df))
        print(tail(coords_df))
        cat("=================================================================================== \n\n")


                grid::polygonGrob(
                        x = coords_df$x,
                        y = coords_df$y,
                        gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha)
                )


     
} # END draw_panel_function



## function: hurricane_proto_class -----------------------------------------------
##     ggproto() creates new class corresponding to new geom (geom_hurricane)
##     Chapter 4.7.1 in the book: buildinga geom
##   

#' Function creates the new geom (geom_huuricane)
#' draw_panel_function is outsourced due to its length
#' 
#' @param required_aes necessary aes inputs for the geom
#' @param default_aes default values
#' @param draw_key function to draw the legend with the associated geom
#' @param draw_group where the magic is happening
#' 
#' @export
hurricane_proto_class <- ggplot2::ggproto("hurricane_proto_class", Geom, 
                                         
                                          # required_aes = <a character vector of required aesthetics>,
                                          # default_aes = aes(<default values for certain aesthetics>),
                                          # draw_key = <a function used to draw the key in the legend>,
                                          # draw_panel = function(data, panel_scales, coord) {
                                          #         ## Function that returns a grid grob that will 
                                          #         ## be plotted (this is where the real work occurs)
                                          # }
                                          
                                          required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
                                          default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                                          draw_key = draw_key_polygon, 
                                          
                                          ## function: draw_panel_function
                                          ##     is outsourced
                                          ##     due to its length
                                          ##     (!!) draw_panel only takes one colour
                                          ##     (!!) draw_group takes all colours (same shape)
                                          draw_group = draw_panel_function
                                          )


## ------------

## ------------

## ------------


## packages
require(ggmap)
require(tidyverse)
require(stringr)
require(ggmap)
require(geosphere)
require(readr)

## load & tidy & filter the data
hur_Katrina_data <- load_hur_data(dataDirectory = "./010_data/hurricanes_data", fileName = "ebtrk_atlc_1988_2015.txt") %>%
        tidy_up_data() %>%
        filter_data(hurricane = "Katrina-2005", observation = "2005-08-29 12:00:00")

hur_Ike_data <- load_hur_data(dataDirectory = "./010_data/hurricanes_data", fileName = "ebtrk_atlc_1988_2015.txt") %>%
        tidy_up_data() %>%
        filter_data(hurricane = "Ike-2008", observation = "2008-09-13 12:00:00")



## slightly modified code given in the assessment
get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
        ggmap(extent = "device") +
        geom_hurricane(data = hur_Ike_data,
                       aes(x = longitude, y = latitude, 
                           r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                           fill = wind_speed, color = wind_speed, alpha = 0.5, scale_radii = 1)) + 
        guides(alpha = FALSE) +
        theme(legend.justification=c(1,0), legend.position=c(0.97,0.03)) +

                scale_color_manual(name = "Wind speed (kts)", 
                           values = c("red4", "orange4", "yellow4")) + 
        scale_fill_manual(name = "Wind speed (kts)", 
                          values = c("red", "orange", "yellow")) -> hurricane_ike


## save as *.png
png(filename="./020_figures/hurricane_ike.png",  width = 1200, height = 900)
plot(hurricane_ike)
dev.off()
