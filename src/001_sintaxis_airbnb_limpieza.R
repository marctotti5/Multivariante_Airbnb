## ----------------------------------------------------------------------------------------------------------
## ----------------------------------------------- LIBRERÍAS ------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(DataExplorer)
`%notin%` <- Negate("%in%")
library(labelled)
library(janitor)
library(gtsummary)
library(gt)
library(corrplot)
library(ggcorrplot)

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- BBDD --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
##
rm(list = ls())

## Leemos el id como character, que por defecto lo leía como numeric, el resto por defecto
data_airbnb <- read.csv(
        "./BBDD/listings.csv",
        encoding = "UTF-8",
        ,
        colClasses = c("character", rep(NA, 78))
)

## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------- DATA CLEANING ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## Analizamos los missings por variable
DataExplorer::plot_missing(data_airbnb)

## Seleccionamos solamente aquellas columnas de interés
variables_interes <- c(
        "host_response_time",
        "host_response_rate",
        "host_acceptance_rate",
        "host_total_listings_count",
        "neighbourhood_cleansed",
        "neighbourhood_group_cleansed",
        "latitude",
        "longitude",
        "room_type",
        "accommodates",
        "bathrooms",
        "bathrooms_text",
        "bedrooms",
        "beds",
        "amenities",
        "price",
        "minimum_nights",
        "maximum_nights",
        "availability_60",
        "number_of_reviews",
        "estimated_occupancy_l365d",
        "review_scores_rating",
        "review_scores_accuracy",
        "review_scores_cleanliness",
        "review_scores_checkin",
        "review_scores_communication",
        "review_scores_location",
        "review_scores_value"
)

data_airbnb <- data_airbnb %>%
        select(all_of(c("id", variables_interes)))


## Limpiamos las variables
data_airbnb <- data_airbnb %>%
        mutate(
                host_response_time = factor(ifelse(
                        host_response_time %in% c("", "N/A"),
                        NA,
                        host_response_time
                )),
                host_response_rate = as.numeric(gsub(
                        "%",
                        "",
                        x = host_response_rate
                )),
                host_acceptance_rate = as.numeric(gsub(
                        "%",
                        "",
                        x = host_acceptance_rate
                )),
                bathrooms_text_num = str_extract(
                        bathrooms_text,
                        "\\d*\\.?\\d+"
                ), # extrae número decimal o entero
                bathrooms_text_num = as.numeric(bathrooms_text_num),
                bathrooms = if_else(
                        is.na(bathrooms),
                        bathrooms_text_num,
                        bathrooms
                ),
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
        pivot_wider(
                names_from = amenities_list,
                values_from = value,
                values_fill = 0
        ) %>%
        janitor::clean_names()

## Seleccionamos amenidades de interés (solamente aquellas que aparezcan con cierta frecuencia)
freq <- colMeans(select(amenities_binary, -id))
selected_amenities <- names(freq[freq > 0.10])

selected_amenities_filtered <- sort(c(
        "bed_linens",
        "cooking_basics",
        "wifi",
        "microwave",
        "coffee_maker",
        "air_conditioning",
        "kitchen",
        "refrigerator",
        "tv",
        "heating",
        "elevator",
        "oven",
        "washer",
        "pets_allowed",
        "dedicated_workspace",
        "dryer",
        "paid_parking_off_premises",
        "freezer",
        "dishwasher",
        "free_street_parking"
))

amenities_binary <- data_airbnb %>%
        select(id, amenities_list) %>%
        unnest(amenities_list) %>%
        mutate(
                amenities_list = str_trim(amenities_list),
                value = "Yes"
        ) %>%
        filter(amenities_list != "", !is.na(amenities_list)) %>%
        distinct() %>%
        pivot_wider(
                names_from = amenities_list,
                values_from = value,
                values_fill = "No"
        ) %>%
        janitor::clean_names() %>%
        select(all_of(colnames(amenities_binary)))

data_airbnb <- inner_join(
        data_airbnb,
        select(amenities_binary, all_of(c("id", selected_amenities_filtered))),
        by = "id"
) %>%
        select(-amenities, -amenities_clean, -amenities_list)

## Quitamos todas las filas con algún missing.
data_airbnb <- data_airbnb[complete.cases(data_airbnb), ]

## Labels
var_label(data_airbnb) <- colnames(data_airbnb) %>%
        str_replace_all("_", " ") %>%
        str_to_title()

## Obtenemos una muestra de tamaño 1000 del dataset (ya que es uno de los requisitos del ejercicio)
set.seed(123)
data_airbnb_sample <- data_airbnb %>%
        group_by(neighbourhood_group_cleansed) %>%
        sample_frac(size = 1000 / nrow(data_airbnb), replace = FALSE) %>%
        ungroup()

## Borramos todo lo que ya no hace falta
rm(amenities_binary, freq, selected_amenities)


## Guardamos el archivo limpio
#write.csv(data_airbnb_sample, "./BBDD/data_airbnb_sample_clean.csv")

## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- EDA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
## Geographic
geographic_variables <- c("longitude", "latitude")

## Continuas: histogramas, densidades
continuous_variables <- data_airbnb_sample %>%
        select_if(is.double) %>%
        select(-latitude, -longitude) %>%
        colnames()


## Discretas
discrete_variables <- data_airbnb_sample %>%
        select_if(is.integer) %>%
        colnames()

## Categóricas
categorical_variables <- data_airbnb_sample %>%
        select(where(~ is.character(.) | is.factor(.))) %>%
        colnames()
categorical_variables <- categorical_variables[categorical_variables != "id"]


## Gráficos
### Histogramas
list_histograms <- vector(mode = "list", length = length(continuous_variables))
names(list_histograms) <- continuous_variables
labels_histograms <- var_label(data_airbnb_sample[, continuous_variables])

for (i in 1:length(continuous_variables)) {
        list_histograms[[i]] <- ggplot(
                data_airbnb_sample,
                aes(
                        x = !!sym(continuous_variables[[i]]),
                        y = after_stat(count / sum(count))
                )
        ) +
                geom_histogram(color = "black", fill = "lightblue") +
                labs(
                        x = labels_histograms[[i]],
                        y = "Relative frequency",
                        title = paste("Histogram for", labels_histograms[[i]])
                ) +
                theme_minimal()
}


### Boxplots: precio por barrio/tipo de habitacion/etc categoricas
create_boxplot <- function(
        categorical_variable = "room_type",
        numerical_variable = "price"
) {
        ggplot(
                data_airbnb_sample,
                aes(
                        x = !!sym(categorical_variable),
                        y = !!sym(numerical_variable),
                        color = !!sym(categorical_variable)
                )
        ) +
                geom_boxplot() +
                geom_jitter(alpha = 0.1) +
                labs(
                        x = var_label(data_airbnb_sample)[[
                                categorical_variable
                        ]],
                        y = var_label(data_airbnb_sample)[[
                                numerical_variable
                        ]],
                        title = paste(
                                var_label(data_airbnb_sample)[[
                                        numerical_variable
                                ]],
                                "by",
                                var_label(data_airbnb_sample)[[
                                        categorical_variable
                                ]]
                        )
                ) +
                theme_minimal() +
                theme(legend.position = "none")
}

create_boxplot(
        categorical_variable = "room_type",
        numerical_variable = "price"
)
create_boxplot(
        categorical_variable = "dedicated_workspace",
        numerical_variable = "price"
)

## Gráficos barras
create_barplot <- function(categorical_variable = "room_type") {
        data_airbnb_sample %>%
                count(!!sym(categorical_variable)) %>%
                mutate(prop = n / sum(n)) %>%
                ggplot(aes(
                        x = reorder(!!sym(categorical_variable), n),
                        y = prop
                )) +
                scale_y_continuous(labels = scales::percent) +
                geom_col(fill = "coral", color = "coral") +
                geom_text(
                        aes(label = scales::percent(prop, accuracy = 0.1)),
                        vjust = -0.5,
                        size = 3
                ) +
                labs(
                        x = var_label(data_airbnb_sample)[[
                                categorical_variable
                        ]],
                        y = "Relative Frequency",
                        title = paste(
                                "Distribution of",
                                var_label(data_airbnb_sample)[[
                                        categorical_variable
                                ]]
                        )
                ) +
                theme_minimal() +
                coord_flip()
}
create_barplot(categorical_variable = "room_type")

### Tablas cruzadas
create_crosstab_heatmap <- function(
        var1 = "room_type",
        var2 = "neighbourhood_group_cleansed"
) {
        data_airbnb_sample %>%
                count(!!sym(var1), !!sym(var2)) %>%
                ggplot(aes(x = !!sym(var1), y = !!sym(var2), fill = n)) +
                geom_tile(color = "white") +
                geom_text(aes(label = n), color = "white") +
                scale_fill_gradient(low = "lightblue", high = "darkblue") +
                labs(
                        x = var_label(data_airbnb_sample)[[var1]],
                        y = var_label(data_airbnb_sample)[[var2]],
                        fill = "Count"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_crosstab_heatmap("room_type", var2 = "neighbourhood_group_cleansed")
create_crosstab_heatmap("room_type", var2 = "neighbourhood_group_cleansed")


amenities_freq <- data_airbnb_sample %>%
        select(all_of(selected_amenities_filtered)) %>%
        summarise(across(everything(), ~ sum(. == "Yes"))) %>%
        pivot_longer(
                everything(),
                names_to = "amenity",
                values_to = "count"
        ) %>%
        mutate(
                prop = count / nrow(data_airbnb_sample),
                amenity_label = unlist(var_label(data_airbnb_sample)[amenity])
        ) %>%
        arrange(desc(count))


grafico_amenities <- ggplot(
        amenities_freq,
        aes(x = reorder(amenity_label, count), y = prop)
) +
        geom_col(fill = "coral") +
        geom_text(
                aes(label = scales::percent(prop, accuracy = 1)),
                hjust = -0.1,
                size = 3
        ) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(
                x = "Amenity",
                y = "Proportion",
                title = "Amenities Availability"
        ) +
        theme_minimal()

## Tabla bivariante en funcion de categóricas

## Mapa: LEAFLET - TODO

## Analisis de correlaciones
correlation_matrix <- cor(data_airbnb_sample[, c(
        continuous_variables,
        discrete_variables
)])
correlation_plot <- ggcorrplot(
        correlation_matrix,
        method = "square",
        type = "upper",
        lab = FALSE,
        lab_size = 0,
        hc.order = TRUE,
        colors = c("#6D9EC1", "white", "#E46726"),
        outline.color = "white",
        title = "Correlation Matrix",
        ggtheme = theme_minimal()
) +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8),
                plot.title = element_text(size = 12),
                legend.text = element_text(size = 8)
        )

## ------------------------------------------ BOX-COX TRANSFORMATIONS ---------------------------------------

## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- PCA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------- MDS/CLUST/ETC -----------------------------------------------
## ----------------------------------------------------------------------------------------------------------
