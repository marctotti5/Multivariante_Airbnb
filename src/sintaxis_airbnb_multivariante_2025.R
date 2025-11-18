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
library(GGally)
library(FactoMineR)
library(factoextra)
library(leaflet)
library(pheatmap)
library(sf)

## ----------------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- BBDD --------------------------------------------------

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
        "host_since",
        #"neighbourhood_cleansed",
        "neighbourhood_group_cleansed",
        "latitude",
        "longitude",
        "room_type",
        "accommodates",
        "bathrooms",
        "bathrooms_text",
        "bedrooms",
        #"beds",
        "amenities",
        "price",
        "minimum_nights",
        #"maximum_nights",
        #"availability_30",
        "number_of_reviews",
        "estimated_occupancy_l365d",
        #"review_scores_rating",
        #"review_scores_accuracy",
        #"review_scores_cleanliness",
        #"review_scores_checkin",
        #"review_scores_communication",
        #"review_scores_location",
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
                host_since = as.Date(host_since),
                # Antigüedad del host (en días desde que se registró)
                host_age_days = as.numeric(Sys.Date() - host_since),
                # En años
                host_age_years = host_age_days / 365.25,
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
                # neighbourhood_cleansed = factor(
                #         neighbourhood_cleansed,
                #         levels = sort(unique(neighbourhood_cleansed))
                # ),
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
        select(
                -bathrooms_text,
                -bathrooms_text_num,
                -host_since,
                -host_age_days
        )

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
        #"bed_linens",
        #"cooking_basics",
        #"wifi",
        #"microwave",
        #"coffee_maker",
        "air_conditioning",
        #"kitchen",
        #"refrigerator",
        #"tv",
        "heating",
        "elevator" #, #,
        #"oven",
        #"washer",
        #"pets_allowed"#,
        #"dedicated_workspace",
        #"dryer",
        #"paid_parking_off_premises",
        #"freezer",
        #"dishwasher" #,
        #"free_street_parking"
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
        select(all_of(colnames(amenities_binary))) %>%
        mutate(across(all_of(colnames(amenities_binary)), as.factor))

data_airbnb <- inner_join(
        data_airbnb,
        select(amenities_binary, all_of(c("id", selected_amenities_filtered))),
        by = "id"
) %>%
        select(-amenities, -amenities_clean, -amenities_list)

## Quitamos todas las filas con algún missing.
data_airbnb <- data_airbnb[complete.cases(data_airbnb), ]
variables_interes_final <- colnames(data_airbnb)

## Labels
var_label(data_airbnb) <- colnames(data_airbnb) %>%
        str_replace_all("_", " ") %>%
        str_to_title()

## Obtenemos una muestra de tamaño 1000 del dataset (ya que es uno de los requisitos del ejercicio)
set.seed(123)
variables_descartar <- c(
        "host_response_time",
        "host_response_rate",
        "host_acceptance_rate"
)

data_airbnb_sample <- data_airbnb %>%
        select(-all_of(variables_descartar)) %>%
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
## Tabla univariante
univariate_table <- data_airbnb_sample %>%
        select(-id) %>%
        tbl_summary(
                statistic = list(
                        all_continuous() ~ "{median} ({IQR})",
                        all_categorical() ~ "{n} / {N} ({p}%)",
                        all_dichotomous() ~ "{n} / {N} ({p}%)"
                ),
                digits = list(
                        all_continuous() ~ 2 # Redondeo a 2 decimales para continuas
                ),
                missing = "no"
        ) %>%
        modify_header(label ~ "**Variables**") %>%
        bold_labels() %>%
        as_gt() %>%
        tab_header(
                title = "Univariate descriptive analysis of the sample"
        ) %>%
        #row_group_order(c("Demographic characteristics", "Pre-surgery variables", "Comorbidities", "Surgery", "Follow-up")) %>%
        tab_options(
                table.border.top.color = "black",
                table.border.top.width = px(2),
                table.border.top.style = "solid",
                heading.border.bottom.color = "transparent",
                column_labels.border.top.color = "black",
                column_labels.border.top.width = px(2),
                column_labels.border.bottom.color = "black",
                column_labels.border.bottom.width = px(2),
                table_body.border.bottom.color = "black", # Añadimos una línea negra al final de la tabla
                table_body.border.bottom.width = px(2), # Grosor de la línea final
                source_notes.border.bottom.color = "transparent", # Elimina la línea bajo la nota
                data_row.padding.horizontal = px(20),
                source_notes.multiline = TRUE
        ) %>%
        tab_style(
                style = cell_text(size = px(16), weight = "bold"), # Ajustar el tamaño de la letra y hacerla negrita
                locations = cells_row_groups()
        ) %>%
        tab_style(
                style = gt::cell_borders(
                        sides = "bottom",
                        weight = px(3),
                        color = "black"
                ), # Hacer más gruesa la línea debajo de los títulos de grupos
                locations = cells_row_groups()
        ) %>%
        tab_style(
                style = list(
                        cell_fill(color = "lightgrey") # Fondo gris
                ),
                locations = cells_row_groups() # Aplica a los grupos de filas
        )


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

## Dispersion
create_scatterplot <- function(
        var_x = "accommodates",
        var_y = "price",
        color_var = NULL,
        add_smooth = TRUE
) {
        # Base plot
        p <- ggplot(
                data_airbnb_sample,
                aes(
                        x = !!sym(var_x),
                        y = !!sym(var_y)
                )
        )

        # Add color aesthetic if provided
        if (!is.null(color_var)) {
                p <- p + aes(color = !!sym(color_var))
        }

        # Add points and smooth line
        p <- p +
                geom_point(alpha = 0.6, size = 2) +
                labs(
                        x = var_label(data_airbnb_sample)[[var_x]],
                        y = var_label(data_airbnb_sample)[[var_y]],
                        title = paste(
                                var_label(data_airbnb_sample)[[var_y]],
                                "vs",
                                var_label(data_airbnb_sample)[[var_x]]
                        )
                ) +
                theme_minimal()

        # Add smooth line if requested
        if (add_smooth) {
                p <- p +
                        geom_smooth(
                                method = "lm",
                                se = TRUE,
                                alpha = 0.2,
                                linewidth = 1
                        )
        }

        # Customize legend if color variable is provided
        if (!is.null(color_var)) {
                p <- p +
                        labs(color = var_label(data_airbnb_sample)[[color_var]])
        }

        return(p)
}

create_scatterplot(var_x = "accommodates", var_y = "price")
create_scatterplot(
        var_x = "accommodates",
        var_y = "price",
        color_var = "room_type"
)
create_scatterplot(
        var_x = "bathrooms",
        var_y = "price",
        color_var = "neighbourhood_group_cleansed",
        add_smooth = FALSE
)

data_airbnb_sample %>%
        select(all_of(c(continuous_variables, discrete_variables))) %>%
        ggpairs(
                lower = list(continuous = "smooth"),
                upper = list(continuous = wrap("cor", size = 3)),
                diag = list(continuous = "barDiag")
        )

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

## Analisis de correlaciones
correlation_matrix <- cor(data_airbnb_sample[, c(
        continuous_variables,
        discrete_variables
)])

rownames(correlation_matrix) <- unlist(var_label(data_airbnb_sample)[rownames(
        correlation_matrix
)])
colnames(correlation_matrix) <- unlist(var_label(data_airbnb_sample)[colnames(
        correlation_matrix
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

## Tabla bivariante en funcion de categóricas

## Mapa: LEAFLET - TODO
distritos_madrid <- st_read("./BBDD/Distritos/DISTRITOS.shp")
distritos_madrid <- st_transform(distritos_madrid, crs = 4326)

#names(distritos_madrid)
#unique(distritos_madrid$NOMBRE) # O la columna que contenga los nombres

# Calcular precio medio por distrito
precio_por_distrito <- data_airbnb_sample %>%
        group_by(neighbourhood_group_cleansed) %>%
        summarise(
                precio_medio = mean(price, na.rm = TRUE),
                precio_mediano = median(price, na.rm = TRUE),
                n = n(),
                .groups = "drop"
        )

# Verificar coincidencia de nombres
#unique(data_airbnb_sample$neighbourhood_group_cleansed)

# Unir con el shapefile
distritos_madrid <- distritos_madrid %>%
        left_join(
                precio_por_distrito,
                by = c("NOMBRE" = "neighbourhood_group_cleansed")
        )

# Crear paleta de colores
pal_distritos <- colorNumeric(
        palette = "YlOrRd",
        domain = distritos_madrid$precio_medio,
        na.color = "lightgray"
)

# Crear mapa con polígonos de distritos
mapa_distritos_precio <- leaflet(distritos_madrid) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
                fillColor = ~ pal_distritos(precio_medio),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                        weight = 4,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.9,
                        bringToFront = TRUE
                ),
                label = ~ paste0(
                        "<b>",
                        NOMBRE,
                        "</b><br>",
                        "Precio medio: €",
                        round(precio_medio, 2),
                        "<br>",
                        "Precio mediano: €",
                        round(precio_mediano, 2),
                        "<br>",
                        "N° listings: ",
                        n
                ) %>%
                        lapply(htmltools::HTML),
                labelOptions = labelOptions(
                        style = list(
                                "font-weight" = "normal",
                                padding = "8px 12px",
                                "font-size" = "14px"
                        ),
                        textsize = "15px",
                        direction = "auto"
                )
        ) %>%
        addLegend(
                pal = pal_distritos,
                values = ~precio_medio,
                opacity = 0.7,
                title = "Precio medio (€)",
                position = "bottomright",
                na.label = "Sin datos"
        ) %>%
        addScaleBar(position = "bottomleft")


## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- PCA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## Análisis PCA
variables_PCA <- data_airbnb_sample %>%
        select_if(is.numeric) %>%
        select(-latitude, -longitude) %>%
        colnames()

pca_object <- PCA(
        data_airbnb_sample[, variables_PCA],
        scale.unit = TRUE,
        ncp = 5,
        graph = F
)

## Screeplots: seleccionamos 3 componentes principales siguiendo el criterio de Kaiser (todos los autovalores > 1)
pca_screeplot_eigenvalues <- fviz_screeplot(
        pca_object,
        addlabels = T,
        choice = "eigenvalue"
) +
        geom_abline(
                intercept = 1,
                slope = 0,
                colour = "red",
                linetype = "dashed",
                linewidth = 0.5
        ) +
        annotate(
                "text",
                x = Inf,
                y = 1,
                label = "Kaiser's criterion",
                hjust = 1.05,
                vjust = -0.5,
                color = "red",
                size = 3.5
        ) +
        geom_abline(
                intercept = 0.7,
                slope = 0,
                colour = "orange",
                linetype = "dotted",
                linewidth = 0.5
        ) +
        annotate(
                "text",
                x = Inf,
                y = 0.7,
                label = "Jollife's criterion",
                hjust = 1.05,
                vjust = -0.5,
                color = "orange",
                size = 3.5
        )

pca_screeplot_variance <- fviz_screeplot(
        pca_object,
        addlabels = T,
        choice = "variance"
)

## Correlation plots between PC's and original variables
pca_correlation_dimensions_1_2 <- fviz_pca_var(
        pca_object,
        axes = c(1, 2)
)

pca_correlation_dimensions_1_2 <- fviz_pca_var(
        pca_object,
        axes = c(1, 2)
)
pca_correlation_dimensions_1_3 <- fviz_pca_var(
        pca_object,
        axes = c(1, 3)
)
pca_correlation_dimensions_2_3 <- fviz_pca_var(
        pca_object,
        axes = c(2, 3)
)

## Contribution plots
pca_contribution_dim1 <- fviz_contrib(
        pca_object,
        choice = "var",
        axes = 1,
        top = 10
)
pca_contribution_dim2 <- fviz_contrib(
        pca_object,
        choice = "var",
        axes = 2,
        top = 10
)
pca_contribution_dim3 <- fviz_contrib(
        pca_object,
        choice = "var",
        axes = 3,
        top = 10
)

cor_matrix_pca <- pca_object$var$coord[, 1:3]
rownames(cor_matrix_pca) <- unlist(var_label(data_airbnb_sample)[rownames(
        cor_matrix_pca
)])

heatmap_correlations_PCA_originalvariables <- pheatmap(
        cor_matrix_pca,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        display_numbers = TRUE,
        color = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlations between original variables and PCs (1-3)",
        fontsize_row = 8
)

## Individuals plots
pca_individuals_neighbourhood <- fviz_pca_ind(
        pca_object,
        label = "none",
        axes = c(1, 2),
        geom = "point",
        habillage = data_airbnb_sample$neighbourhood_group_cleansed,
        pointshape = 19
) +
        xlim(c(-3, 5))

pca_individuals_roomtype <- fviz_pca_ind(
        pca_object,
        label = "none",
        axes = c(1, 2),
        geom = "point",
        habillage = data_airbnb_sample$room_type,
        addEllipses = T,
        pointshape = 19
)

pca_individuals_airconditioning <- fviz_pca_ind(
        pca_object,
        label = "none",
        axes = c(1, 2),
        geom = "point",
        habillage = data_airbnb_sample$air_conditioning,
        addEllipses = T,
        pointshape = 19
)

pca_individuals_heating <- fviz_pca_ind(
        pca_object,
        label = "none",
        axes = c(1, 2),
        geom = "point",
        habillage = data_airbnb_sample$heating,
        addEllipses = T,
        pointshape = 19
)

pca_individuals_elevator <- fviz_pca_ind(
        pca_object,
        label = "none",
        axes = c(1, 2),
        geom = "point",
        habillage = data_airbnb_sample$elevator,
        addEllipses = T,
        pointshape = 19
)

## Estabilidad del PCA: análisis bootstrap
set.seed(123)
n_obs <- nrow(data_airbnb_sample)
n_bootstrap <- 10000 # Número de muestras bootstrap

### Matrices para almacenar loadings de PC1, PC2 y PC3
loadings_pc1_bootstrap <- matrix(
        NA,
        nrow = n_bootstrap,
        ncol = length(variables_PCA)
)
loadings_pc2_bootstrap <- matrix(
        NA,
        nrow = n_bootstrap,
        ncol = length(variables_PCA)
)
loadings_pc3_bootstrap <- matrix(
        NA,
        nrow = n_bootstrap,
        ncol = length(variables_PCA)
)
colnames(loadings_pc1_bootstrap) <- variables_PCA
colnames(loadings_pc2_bootstrap) <- variables_PCA
colnames(loadings_pc3_bootstrap) <- variables_PCA

### Matriz para almacenar eigenvalues
eigenvalues_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = 5)
colnames(eigenvalues_bootstrap) <- paste0("PC", 1:5)

### Bootstrap: remuestreo CON reemplazo
for (i in 1:n_bootstrap) {
        # Muestra bootstrap (con reemplazo)
        indices_boot <- sample(1:n_obs, size = n_obs, replace = TRUE)
        data_bootstrap <- data_airbnb_sample[indices_boot, variables_PCA]

        # PCA en la muestra bootstrap
        pca_boot <- PCA(
                data_bootstrap,
                scale.unit = TRUE,
                ncp = 5,
                graph = FALSE
        )

        # Guardar eigenvalues y loadings
        eigenvalues_bootstrap[i, ] <- pca_boot$eig[1:5, 1]
        loadings_pc1_bootstrap[i, ] <- pca_boot$var$coord[, 1]
        loadings_pc2_bootstrap[i, ] <- pca_boot$var$coord[, 2]
        loadings_pc3_bootstrap[i, ] <- pca_boot$var$coord[, 3]
}

### Gráfico de estabilidad de eigenvalues
eigenvalues_stability <- data.frame(
        PC = paste0("PC", 1:5),
        Original = pca_object$eig[1:5, 1],
        CI_lower = apply(eigenvalues_bootstrap, 2, quantile, probs = 0.025),
        CI_upper = apply(eigenvalues_bootstrap, 2, quantile, probs = 0.975)
)

eigenvalues_bootstrap_long <- as.data.frame(eigenvalues_bootstrap) %>%
        pivot_longer(everything(), names_to = "PC", values_to = "Eigenvalue")

plot_eigenvalues_stability <- ggplot(
        eigenvalues_bootstrap_long,
        aes(x = PC, y = Eigenvalue)
) +
        #geom_violin(fill = "lightblue", alpha = 0.5) +
        geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
        geom_point(
                data = eigenvalues_stability,
                aes(x = PC, y = Original),
                color = "red",
                size = 4,
                shape = 18
        ) +
        geom_errorbar(
                data = eigenvalues_stability,
                aes(x = PC, y = Original, ymin = CI_lower, ymax = CI_upper),
                color = "darkred",
                width = 0.3,
                linewidth = 1
        ) +
        labs(
                title = "Stability of Eigenvalues - Bootstrap Analysis",
                subtitle = "Red diamonds = Original PCA; Error bars = 95% CI",
                x = "Principal Component",
                y = "Eigenvalue"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10)
        )

### Función para crear gráfico de estabilidad de loadings
create_loadings_stability_plot <- function(
        pc_number,
        original_loadings,
        bootstrap_loadings
) {
        loadings_df <- data.frame(
                Variable = unlist(var_label(data_airbnb_sample)[variables_PCA]),
                Original = original_loadings,
                Mean_Bootstrap = colMeans(bootstrap_loadings),
                SD_Bootstrap = apply(bootstrap_loadings, 2, sd),
                CI_lower = apply(
                        bootstrap_loadings,
                        2,
                        quantile,
                        probs = 0.025
                ),
                CI_upper = apply(bootstrap_loadings, 2, quantile, probs = 0.975)
        ) %>%
                arrange(desc(abs(Original)))

        ggplot(loadings_df, aes(x = reorder(Variable, abs(Original)))) +
                geom_point(aes(y = Original), color = "red", size = 3) +
                geom_errorbar(
                        aes(ymin = CI_lower, ymax = CI_upper),
                        width = 0.2,
                        color = "blue",
                        alpha = 0.6,
                        linewidth = 1
                ) +
                geom_hline(
                        yintercept = 0,
                        linetype = "dashed",
                        color = "gray"
                ) +
                coord_flip() +
                labs(
                        title = paste0(
                                "Stability of PC",
                                pc_number,
                                " Loadings - Bootstrap"
                        ),
                        subtitle = "Red = Original; Blue bars = 95% Bootstrap CI",
                        x = "Variable",
                        y = paste0("Loading on PC", pc_number)
                ) +
                theme_minimal() +
                theme(
                        plot.title = element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 9)
                )
}

### Crear gráficos para PC1, PC2 y PC3
plot_loadings_pc1_stability <- create_loadings_stability_plot(
        1,
        pca_object$var$coord[, 1],
        loadings_pc1_bootstrap
)

plot_loadings_pc2_stability <- create_loadings_stability_plot(
        2,
        pca_object$var$coord[, 2],
        loadings_pc2_bootstrap
)

plot_loadings_pc3_stability <- create_loadings_stability_plot(
        3,
        pca_object$var$coord[, 3],
        loadings_pc3_bootstrap
)

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------- MDS/CLUST/ETC -----------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------- GUARDAR RESULTADOS ------------------------------------------
## ----------------------------------------------------------------------------------------------------------
save.image("./BBDD/resultados_airbnb.RData")
