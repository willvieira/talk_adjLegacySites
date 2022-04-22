
library(tidyverse)
library(spatstat)
library(sf)
library(spsurvey)
library(MBHdesign)

set.seed(0.0)



# parameters

    # Maximum proportion of NA pixels (non-habitat) in a hexagon
    # if 0.8, it means an hexagon has to have at least 20% of habitat pixels
    prop_na = 0.8

    # Percentage of hexagons to cover a region
    sample_effort = 0.02

    # Total sample size for SSU (Main + Over)
    # It must be a even number
    ssu_N = 6

    # Number of replications when running the GRTS
    nb_rep = 15

    # Ecoregions
    eco_sim <- c(
        '7', '28', '30', '31',
        '46', '47', '48', '49',
        '73', '77', '78', '86',
        '72', '74', '75', '76',
        '96', #'96N', '96S',
        '99',
        '100', #'100N', '100S',
        '101', '101N', '101S',
        '102',
        '103', #'103N', '103S',
        '117',
        '216',
        '217'#, '217N', '217S'
    )
    
    # Buffer size (in Km) to adjust sample size given nb of legacy sites
    bufferSize_N = 18

    # Buffer size (in Km) to adjust inclusion probability around legacy sites
    bufferSize_p = 10

    # Distance between SSU centroid (in meters)
    ssu_dist = 294

#



# Prepare hexagons

    hexas <- readRDS('../samplingBMS/data/hexa_complete.RDS') %>%
        filter(propNA <= prop_na) %>%
        filter(ecoregion %in% eco_sim) %>%
        mutate(
            p = (hab_prob * cost_prob) / sum(hab_prob * cost_prob)
        ) %>%
        filter(p != 0)

#



# legacy sites

    # function to transform Latitude & longitude legacy site points in a table
    # with the number of points per hexagon ID (ET_Index)
    import_legacySites <- function(File, lat_name, lon_name)
    {
        hx <- hexas %>%
            st_transform(4326)
        
        # read file
        lg <- read_csv(File) %>%
            rename(
                lat = all_of(lat_name),
                lon = all_of(lon_name)
            ) %>%
            st_as_sf(
                coords = c('lon', 'lat'),
                crs = st_crs(hx)
            )

        # intersect
        nbLegacy <- hx %>%
            st_contains(lg, sparse = FALSE) %>%
            apply(1, sum)

        tibble(
            ET_Index = hx$ET_Index,
            legacyNew = nbLegacy
        ) %>%
        filter(legacyNew > 0)
    }
    

    # load and transform legacy info
    legacySites <- import_legacySites(
        File = '../samplingBMS/data/SitesLegacy_GRTS20220314.csv',
        lat_name = 'Lat_DegDecValide',
        lon_name = 'Long_DegDecValide'
    )

    # merge to hexagons
    hexas <- hexas %>%
        left_join(legacySites) %>%
        mutate(
            legacyNew = replace_na(legacyNew, 0),
            legacy = legacySite + legacyNew
        )

#
