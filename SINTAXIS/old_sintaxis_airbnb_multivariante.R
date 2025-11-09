## ----------------------------------------------------------------------------------------------------------
## ----------------------------------------------- LIBRERÍAS ------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
library(tidyverse)
`%notin%` <- Negate("%in%")

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- BBDD --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## Leemos el id como character, que por defecto lo leía como numeric, el resto por defecto
data_airbnb <- read.csv("./BBDD/listings.csv", encoding = "UTF-8", , colClasses = c("character", rep(NA, 78)))

## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------- DATA CLEANING ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## Analizamos los missings por variable
DataExplorer::plot_missing(data_airbnb)

## Seleccionamos solamente aquellas columnas de interés
variables_interes <- c("host_response_time", "host_response_rate", "host_acceptance_rate", 
                       "host_total_listings_count", "neighbourhood_cleansed", "neighbourhood_group_cleansed",
                       "latitude", "longitude", "room_type", "accommodates", "bathrooms",
                       "bathrooms_text", "bedrooms", "beds", "amenities", 
                       "price", "minimum_nights", "maximum_nights",
                       "availability_60", "number_of_reviews", "estimated_occupancy_l365d", "review_scores_rating", 
                       "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin", 
                       "review_scores_communication", "review_scores_location", 
                       "review_scores_value")

data_airbnb <- data_airbnb %>% 
        select(all_of(c("id", variables_interes)))

## Limpiamos las variables
data_airbnb <- data_airbnb %>%
        mutate(
                host_response_time = factor(ifelse(host_response_time %in% c("", "N/A"), NA, host_response_time)),
                host_response_rate = as.numeric(gsub("%", "", x = host_response_rate)),
                host_acceptance_rate = as.numeric(gsub("%", "", x = host_acceptance_rate)),
                bathrooms_text_num = str_extract(bathrooms_text, "\\d*\\.?\\d+"),  # extrae número decimal o entero
                bathrooms_text_num = as.numeric(bathrooms_text_num),
                bathrooms = if_else(is.na(bathrooms), bathrooms_text_num, bathrooms),
                price = as.numeric(gsub(pattern = "\\$", "", x = price)),
                neighbourhood_cleansed = factor(
                        neighbourhood_cleansed, 
                        levels = sort(unique(neighbourhood_cleansed))
                ),
                neighbourhood_group_cleansed = factor(
                        neighbourhood_group_cleansed, 
                        levels = sort(unique(neighbourhood_group_cleansed))
                ),
                room_type = factor(
                        room_type, 
                        levels = sort(unique(room_type))
                ),
                amenities_clean = str_remove_all(amenities, "\\[|\\]|\""),
                amenities_list = str_split(amenities_clean, ",\\s*")
        ) %>%
        select(-bathrooms_text, -bathrooms_text_num)

amenities_binary <- data_airbnb %>%
        select(id, amenities_list) %>%
        unnest(amenities_list) %>%
        mutate(
                amenities_list = str_trim(amenities_list),
                value = 1
        ) %>%
        filter(amenities_list != "", !is.na(amenities_list)) %>% 
        distinct() %>%
        pivot_wider(names_from = amenities_list, values_from = value, values_fill = 0) %>%
        janitor::clean_names()

## Seleccionamos amenidades de interés (solamente aquellas que aparezcan con cierta frecuencia)
freq <- colMeans(select(amenities_binary, -id))
selected_amenities <- names(freq[freq > 0.10])

freq <- colMeans(select(amenities_binary, -id))

selected_amenities_filtered <- sort(c("bed_linens", "cooking_basics", "wifi", "microwave", 
                                 "coffee_maker", "air_conditioning", "kitchen", "refrigerator",
                                 "tv", "heating", "elevator", "oven", "washer", "pets_allowed", 
                                 "dedicated_workspace", "dryer", "paid_parking_off_premises",
                                 "freezer", "dishwasher", "free_street_parking"))

data_airbnb <- inner_join(data_airbnb, 
                         select(amenities_binary, all_of(c("id", selected_amenities_filtered))), 
                         by = "id") %>%
        select(-amenities, -amenities_clean, -amenities_list)

data_airbnb <- data_airbnb[complete.cases(data_airbnb), ]


## Obtenemos una muestra de tamaño 1000 del dataset (ya que es uno de los requisitos del ejercicio)
set.seed(123)
data_airbnb_sample <- data_airbnb %>%
        group_by(neighbourhood_group_cleansed) %>%
        sample_frac(size = 1000 / nrow(data_airbnb), replace = FALSE) %>%
        ungroup()

## Borramos todo lo que ya no hace falta
rm(amenities_binary, freq, selected_amenities, selected_amenities_filtered)

## Guardamos el archivo limpio
write.csv(data_airbnb_sample, "./BBDD/data_airbnb_sample_clean.csv")



## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- EDA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- PCA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------- MDS/CLUST/ETC -----------------------------------------------
## ----------------------------------------------------------------------------------------------------------