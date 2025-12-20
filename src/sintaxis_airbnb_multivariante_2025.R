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
library(cluster)
library(dbrobust)
library(DescTools)
library(vegan)
library(plotly)
library(geometry) # Para calcular convex hulls

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
missing <- DataExplorer::plot_missing(
  data_airbnb,
  missing_only = T,
  ggtheme = theme_minimal()
)

## Seleccionamos solamente aquellas columnas de interés
variables_interes <- c(
  "host_response_time",
  "host_response_rate",
  "host_acceptance_rate",
  "host_total_listings_count",
  "host_since",
  # "neighbourhood_cleansed",
  "neighbourhood_group_cleansed",
  "latitude",
  "longitude",
  "room_type",
  "accommodates",
  "bathrooms",
  "bathrooms_text",
  "bedrooms",
  # "beds",
  "amenities",
  "price",
  "minimum_nights",
  # "maximum_nights",
  # "availability_30",
  "number_of_reviews",
  "estimated_occupancy_l365d",
  # "review_scores_rating",
  # "review_scores_accuracy",
  # "review_scores_cleanliness",
  # "review_scores_checkin",
  # "review_scores_communication",
  # "review_scores_location",
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
  # "bed_linens",
  # "cooking_basics",
  # "wifi",
  # "microwave",
  # "coffee_maker",
  "air_conditioning",
  # "kitchen",
  # "refrigerator",
  # "tv",
  "heating",
  "elevator" # , #,
  # "oven",
  # "washer",
  # "pets_allowed"#,
  # "dedicated_workspace",
  # "dryer",
  # "paid_parking_off_premises",
  # "freezer",
  # "dishwasher" #,
  # "free_street_parking"
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
# write.csv(data_airbnb_sample, "./BBDD/data_airbnb_sample_clean.csv")

## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- EDA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
## Tabla univariante
univariate_table <- data_airbnb_sample %>%
  select(-id, -latitude, -longitude) %>%
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
  # row_group_order(c("Demographic characteristics", "Pre-surgery variables", "Comorbidities", "Surgery", "Follow-up")) %>%
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

plot_accomodates_price <- create_scatterplot(
  var_x = "accommodates",
  var_y = "price",
  color_var = "room_type"
)
plot_bathrooms_price <- create_scatterplot(
  var_x = "bathrooms",
  var_y = "price",
  color_var = "neighbourhood_group_cleansed",
  add_smooth = FALSE
)
plot_bathrooms_accommodates <- create_scatterplot(
  var_x = "accommodates",
  var_y = "bathrooms",
  color_var = "neighbourhood_group_cleansed",
  add_smooth = FALSE
)

ggpairs_plot <- data_airbnb_sample %>%
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

boxplot_room_type_price <- create_boxplot(
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
bplot <- create_barplot(categorical_variable = "room_type")

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

crosstab <- create_crosstab_heatmap(
  "room_type",
  var2 = "neighbourhood_group_cleansed"
)

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

# names(distritos_madrid)
# unique(distritos_madrid$NOMBRE) # O la columna que contenga los nombres

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
# unique(data_airbnb_sample$neighbourhood_group_cleansed)

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
      "Average Price: €",
      round(precio_medio, 2),
      "<br>",
      "Median Price: €",
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
    title = "Average Price (€)",
    position = "bottomright",
    na.label = "NA"
  ) %>%
  addScaleBar(position = "bottomleft")


## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------------- PCA --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

# Análisis PCA
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


## TODO

## ----------------------------------------------------------------------------------------------------------
## ------------------------------------------- MDS: G-GOWER vs RELMS ----------------------------------------
## ----------------------------------------------------------------------------------------------------------

## ============== 0. MATRICES DE DISTANCIAS (CUMPLIENDO PROPIEDAD EUCLIDEA) ==============
data_airbnb_sample <- data_airbnb_sample %>%
  select(-latitude, -longitude)

numeric_variables_MDS <- data_airbnb_sample %>%
  select_if(is.numeric) %>%
  colnames()
categorical_variables_MDS <- c("neighbourhood_group_cleansed", "room_type")
binary_variables_MDS <- c("air_conditioning", "elevator", "heating")
weight_vector <- rep(1, nrow(data_airbnb_sample))

D_robust_ggower_squared <- robust_distances(
  data = select(data_airbnb_sample, -id),
  cont_vars = numeric_variables_MDS,
  bin_vars = binary_variables_MDS,
  cat_vars = categorical_variables_MDS,
  method = "ggower",
  return_dist = FALSE
)

euclidianize_D_robust_ggower_squared <- make_euclidean(
  D_robust_ggower_squared,
  w = weight_vector
)
D_robust_ggower <- as.dist(sqrt(euclidianize_D_robust_ggower_squared$D_euc))

D_robust_relms_squared <- robust_distances(
  data = select(data_airbnb_sample, -id),
  cont_vars = numeric_variables_MDS,
  bin_vars = binary_variables_MDS,
  cat_vars = categorical_variables_MDS,
  method = "relms",
  return_dist = FALSE
)

euclidianize_D_robust_relms_squared <- make_euclidean(
  D_robust_relms_squared,
  w = weight_vector
)
D_robust_relms <- as.dist(sqrt(euclidianize_D_robust_relms_squared$D_euc))


## ============== 1. CALCULAR AMBOS MDS ==============
numero_eigenvalues <- 10

MDS_ggower <- cmdscale(D_robust_ggower, k = numero_eigenvalues, eig = TRUE)
MDS_ggower_data <- MDS_ggower$points %>% as_data_frame()

MDS_relms <- cmdscale(D_robust_relms, k = numero_eigenvalues, eig = TRUE)
MDS_relms_data <- MDS_relms$points %>% as_data_frame()

## ============== 2. INTERPRETABILIDAD ==============

### 2.1 Varianza explicada
var_ggower <- (MDS_ggower$eig[1:10] / sum(MDS_ggower$eig)) * 100
var_relms <- (MDS_relms$eig[1:10] / sum(MDS_relms$eig)) * 100

comparison_variance <- data.frame(
  Dimension = rep(1:10, 2),
  Method = rep(c("G-Gower", "RelMS"), each = 10),
  Variance = c(var_ggower, var_relms),
  Cumulative = c(cumsum(var_ggower), cumsum(var_relms))
) %>%
  mutate(
    Dimension = as.factor(Dimension)
  )

# Plot comparativo de varianza
plot_variance_comparison <- ggplot(
  comparison_variance,
  aes(x = Dimension, y = Variance, fill = Method)
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = paste0(round(Variance, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 2.5
  ) +
  labs(
    title = "Variance Explained: G-Gower vs RelMS",
    x = "Dimension",
    y = "% Variance Explained"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot de varianza acumulada
plot_cumulative_comparison <- ggplot(
  comparison_variance,
  aes(x = Dimension, y = Cumulative, color = Method, group = Method)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 70,
    linetype = "dashed",
    color = "red"
  ) +
  annotate(
    "text",
    x = 9,
    y = 70,
    label = "70% threshold",
    vjust = -0.5,
    color = "red"
  ) +
  labs(
    title = "Cumulative Variance: G-Gower vs RelMS",
    x = "Dimension",
    y = "Cumulative % Variance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### 2.2 Correlaciones con variables originales (numéricas y categóricas)

# Variables numéricas y categóricas
numeric_vars <- data_airbnb_sample %>%
  select(where(is.numeric)) %>%
  colnames()

categorical_vars <- c(
  "neighbourhood_group_cleansed",
  "room_type",
  "air_conditioning",
  "heating",
  "elevator"
)

## -------- Función para calcular V de Cramér --------
calculate_cramer_v_mds <- function(categorical_var, mds_dimension) {
  # Discretizar la dimensión MDS en 5 categorías (quintiles)
  mds_cat <- cut(
    mds_dimension,
    breaks = quantile(mds_dimension, probs = seq(0, 1, 0.2)),
    include.lowest = TRUE,
    labels = c("Very Low", "Low", "Medium", "High", "Very High")
  )

  # Tabla de contingencia
  contingency_table <- table(categorical_var, mds_cat)

  # V de Cramér
  cramer_v <- CramerV(contingency_table)

  return(cramer_v)
}

## -------- G-Gower: Correlaciones numéricas --------
cor_ggower_numeric <- cor(
  data_airbnb_sample[, numeric_vars],
  MDS_ggower_data[, 1:5],
  method = "spearman"
)
colnames(cor_ggower_numeric) <- paste0("MDS Dim ", 1:5)
rownames(cor_ggower_numeric) <- unlist(var_label(data_airbnb_sample)[
  numeric_vars
])

## -------- G-Gower: V de Cramér para categóricas --------
cramer_ggower <- matrix(
  NA,
  nrow = length(categorical_vars),
  ncol = 5
)

rownames(cramer_ggower) <- unlist(var_label(data_airbnb_sample)[
  categorical_vars
])
colnames(cramer_ggower) <- paste0("MDS Dim ", 1:5)

for (i in 1:length(categorical_vars)) {
  for (j in 1:5) {
    cramer_ggower[i, j] <- calculate_cramer_v_mds(
      data_airbnb_sample[[categorical_vars[i]]],
      MDS_ggower_data[[j]]
    )
  }
}

## -------- G-Gower: Combinar correlaciones y V de Cramér --------
associations_ggower <- rbind(
  #abs(cor_ggower_numeric[, 1:5]),
  cor_ggower_numeric[, 1:5],
  cramer_ggower
)

## -------- RELMS: Correlaciones numéricas --------
cor_relms_numeric <- cor(
  data_airbnb_sample[, numeric_vars],
  MDS_relms_data[, 1:5],
  method = "spearman"
)
colnames(cor_relms_numeric) <- paste0("MDS Dim ", 1:5)
rownames(cor_relms_numeric) <- unlist(var_label(data_airbnb_sample)[
  numeric_vars
])

## -------- RELMS: V de Cramér para categóricas --------
cramer_relms <- matrix(
  NA,
  nrow = length(categorical_vars),
  ncol = 5
)

rownames(cramer_relms) <- unlist(var_label(data_airbnb_sample)[
  categorical_vars
])
colnames(cramer_relms) <- paste0("MDS Dim ", 1:5)

for (i in 1:length(categorical_vars)) {
  for (j in 1:5) {
    cramer_relms[i, j] <- calculate_cramer_v_mds(
      data_airbnb_sample[[categorical_vars[i]]],
      MDS_relms_data[[j]]
    )
  }
}

## -------- RELMS: Combinar correlaciones y V de Cramér --------
associations_relms <- rbind(
  #abs(cor_relms_numeric[, 1:5]),
  cor_relms_numeric[, 1:5],
  cramer_relms
)

## -------- Heatmaps comparativos --------
heatmap_associations_ggower <- pheatmap(
  associations_ggower,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "G-Gower: Associations with MDS dimensions (1-5)",
  fontsize_row = 7,
  fontsize_number = 7,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
  annotation_row = data.frame(
    Type = c(
      rep("Numeric (|Pearson|)", nrow(cor_ggower_numeric)),
      rep("Categorical (Cramér's V)", nrow(cramer_ggower))
    ),
    row.names = rownames(associations_ggower)
  )
)

heatmap_associations_relms <- pheatmap(
  associations_relms,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "RelMS: Associations with MDS dimensions (1-5)",
  fontsize_row = 7,
  fontsize_number = 7,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
  annotation_row = data.frame(
    Type = c(
      rep("Numeric (|Pearson|)", nrow(cor_ggower_numeric)),
      rep("Categorical (Cramér's V)", nrow(cramer_ggower))
    ),
    row.names = rownames(associations_ggower)
  )
)


### 2.3 Visualización de separación por variables categóricas
create_comparison_plots <- function(
  variable,
  dim_x = 1,
  dim_y = 2,
  include_density = FALSE
) {
  # Validar dimensiones
  max_dim <- ncol(MDS_ggower_data)
  if (dim_x < 1 || dim_x > max_dim || dim_y < 1 || dim_y > max_dim) {
    stop(paste("Dimensions must be between 1 and", max_dim))
  }
  if (dim_x == dim_y) {
    stop("dim_x and dim_y must be different")
  }

  # Detectar tipo de variable
  var_data <- data_airbnb_sample[[variable]]
  is_numeric <- is.numeric(var_data)
  is_categorical <- is.factor(var_data) || is.character(var_data)

  # Obtener nombres de columnas para las dimensiones seleccionadas
  col_x <- paste0("V", dim_x)
  col_y <- paste0("V", dim_y)

  # Combinar datos de ambos métodos
  combined_data <- bind_rows(
    MDS_ggower_data %>%
      mutate(
        group = var_data,
        Method = "G-Gower",
        x_coord = .data[[col_x]],
        y_coord = .data[[col_y]]
      ),
    MDS_relms_data %>%
      mutate(
        group = var_data,
        Method = "RelMS",
        x_coord = .data[[col_x]],
        y_coord = .data[[col_y]]
      )
  ) %>%
    mutate(Method = factor(Method, levels = c("G-Gower", "RelMS")))

  # Crear plot base
  base_plot <- ggplot(
    combined_data,
    aes(x = x_coord, y = y_coord)
  )

  # Añadir capa de puntos según tipo de variable
  if (is_numeric) {
    # Variable numérica: escala de color continua
    scatterplot <- base_plot +
      geom_point(aes(color = group), alpha = 0.6, size = 2) +
      scale_color_viridis_c(
        option = "plasma",
        name = var_label(data_airbnb_sample)[[variable]]
      ) +
      facet_grid(. ~ Method, scales = "free") +
      labs(
        title = paste(
          "MDS Comparison:",
          var_label(data_airbnb_sample)[[variable]]
        ),
        subtitle = paste0(
          "G-Gower: Dim ",
          dim_x,
          " (",
          round(var_ggower[dim_x], 1),
          "%) vs Dim ",
          dim_y,
          " (",
          round(var_ggower[dim_y], 1),
          "%) | ",
          "RelMS: Dim ",
          dim_x,
          " (",
          round(var_relms[dim_x], 1),
          "%) vs Dim ",
          dim_y,
          " (",
          round(var_relms[dim_y], 1),
          "%)"
        ),
        x = paste0("Dimension ", dim_x),
        y = paste0("Dimension ", dim_y)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "lightgray", color = "black"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        aspect.ratio = 1
      )
  } else if (is_categorical) {
    # Contar número de categorías únicas
    n_categories <- length(unique(var_data[!is.na(var_data)]))

    # Elegir paleta de colores según número de categorías
    if (n_categories <= 9) {
      # Usar Set1 para ≤9 categorías
      color_scale <- scale_color_brewer(
        palette = "Set1",
        name = var_label(data_airbnb_sample)[[variable]]
      )
    } else if (n_categories <= 12) {
      # Usar Set3 o Paired para 10-12 categorías
      color_scale <- scale_color_brewer(
        palette = "Set3",
        name = var_label(data_airbnb_sample)[[variable]]
      )
    } else {
      # Usar paleta continua interpolada para >12 categorías
      color_scale <- scale_color_manual(
        values = colorRampPalette(
          RColorBrewer::brewer.pal(12, "Set3")
        )(n_categories),
        name = var_label(data_airbnb_sample)[[variable]]
      )
    }

    # Variable categórica: colores discretos
    scatterplot <- base_plot +
      geom_point(aes(color = group), alpha = 0.6, size = 2) +
      color_scale +
      facet_grid(. ~ Method, scales = "free") +
      labs(
        title = paste(
          "MDS Comparison:",
          var_label(data_airbnb_sample)[[variable]]
        ),
        subtitle = paste0(
          "G-Gower: Dim ",
          dim_x,
          " (",
          round(var_ggower[dim_x], 1),
          "%) vs Dim ",
          dim_y,
          " (",
          round(var_ggower[dim_y], 1),
          "%) | ",
          "RelMS: Dim ",
          dim_x,
          " (",
          round(var_relms[dim_x], 1),
          "%) vs Dim ",
          dim_y,
          " (",
          round(var_relms[dim_y], 1),
          "%)"
        ),
        x = paste0("Dimension ", dim_x),
        y = paste0("Dimension ", dim_y)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "lightgray", color = "black"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        aspect.ratio = 1
      )

    # Ajustar leyenda para muchas categorías (≥10)
    if (n_categories >= 10) {
      scatterplot <- scatterplot +
        guides(
          color = guide_legend(
            ncol = ceiling(n_categories / 8), # Múltiples columnas
            byrow = TRUE,
            title.position = "top",
            title.hjust = 0.5
          )
        ) +
        theme(
          legend.text = element_text(size = 7), # Letra más pequeña
          legend.key.size = unit(0.4, "cm") # Iconos más pequeños
        )
    }
  } else {
    stop("Variable type not supported. Must be numeric, factor, or character.")
  }

  return(scatterplot)
}


# Probar con neighbourhood
create_comparison_plots("room_type")
create_comparison_plots("accommodates")
create_comparison_plots("host_total_listings_count")
create_comparison_plots("price")

create_comparison_plots("air_conditioning")
create_comparison_plots("heating")
create_comparison_plots("elevator")
create_comparison_plots("number_of_reviews")

create_comparison_plots("neighbourhood_group_cleansed")
create_comparison_plots("neighbourhood_group_cleansed", dim_x = 1, dim_y = 5)
create_comparison_plots("host_total_listings_count")
create_comparison_plots("host_age_years")
create_comparison_plots("review_scores_value")


## ============== 3. ESTABILIDAD (BOOTSTRAP) ==============
set.seed(123)
n_bootstrap <- 10
n_obs <- nrow(data_airbnb_sample)

# Matrices para guardar coordenadas bootstrap
coords_ggower_boot <- array(NA, dim = c(n_bootstrap, n_obs, 3))
coords_relms_boot <- array(NA, dim = c(n_bootstrap, n_obs, 3))

# Matrices para guardar eigenvalues
eigen_ggower_boot <- matrix(NA, nrow = n_bootstrap, ncol = 10)
eigen_relms_boot <- matrix(NA, nrow = n_bootstrap, ncol = 10)

cat("\nIniciando bootstrap para estabilidad...\n")

for (i in 1:n_bootstrap) {
  if (i %% 50 == 0) {
    cat("  Bootstrap iteration:", i, "/", n_bootstrap, "\n")
  }

  # Muestra bootstrap
  indices_boot <- sample(1:n_obs, size = n_obs, replace = TRUE)
  data_boot <- data_airbnb_sample[indices_boot, ]

  # Recalcular distancias robustas G-Gower
  D_ggower_boot_sq <- robust_distances(
    data = select(data_boot, -id),
    cont_vars = numeric_variables_MDS,
    bin_vars = binary_variables_MDS,
    cat_vars = categorical_variables_MDS,
    method = "ggower",
    return_dist = FALSE
  )

  eucl_ggower_boot <- make_euclidean(D_ggower_boot_sq, w = rep(1, n_obs))
  D_ggower_boot_euc <- pmax(eucl_ggower_boot$D_euc, 0) # Reemplazar negativos por 0
  D_ggower_boot <- as.dist(sqrt(D_ggower_boot_euc))

  # Recalcular distancias robustas RELMS
  D_relms_boot_sq <- robust_distances(
    data = select(data_boot, -id),
    cont_vars = numeric_variables_MDS,
    bin_vars = binary_variables_MDS,
    cat_vars = categorical_variables_MDS,
    method = "relms",
    return_dist = FALSE
  )

  eucl_relms_boot <- make_euclidean(D_relms_boot_sq, w = rep(1, n_obs))
  D_relms_boot_euc <- pmax(eucl_relms_boot$D_euc, 0) # Reemplazar negativos por 0
  D_relms_boot <- as.dist(sqrt(D_relms_boot_euc))

  # MDS bootstrap
  mds_ggower_boot <- cmdscale(D_ggower_boot, k = 10, eig = TRUE)
  mds_relms_boot <- cmdscale(D_relms_boot, k = 10, eig = TRUE)

  # Guardar resultados
  coords_ggower_boot[i, , ] <- mds_ggower_boot$points[, 1:3]
  coords_relms_boot[i, , ] <- mds_relms_boot$points[, 1:3]

  eigen_ggower_boot[i, ] <- mds_ggower_boot$eig[1:10]
  eigen_relms_boot[i, ] <- mds_relms_boot$eig[1:10]
}

### 3.1 Estabilidad de eigenvalues
eigen_stability <- data.frame(
  Dimension = rep(1:10, 2),
  Method = rep(c("G-Gower", "RELMS"), each = 10),
  Original = c(MDS_ggower$eig[1:10], MDS_relms$eig[1:10]),
  Mean_Boot = c(
    colMeans(eigen_ggower_boot),
    colMeans(eigen_relms_boot)
  ),
  SD_Boot = c(
    apply(eigen_ggower_boot, 2, sd),
    apply(eigen_relms_boot, 2, sd)
  ),
  CI_lower = c(
    apply(eigen_ggower_boot, 2, quantile, probs = 0.025),
    apply(eigen_relms_boot, 2, quantile, probs = 0.025)
  ),
  CI_upper = c(
    apply(eigen_ggower_boot, 2, quantile, probs = 0.975),
    apply(eigen_relms_boot, 2, quantile, probs = 0.975)
  )
)

# Plot de estabilidad de eigenvalues
plot_eigen_stability <- ggplot(
  eigen_stability %>% filter(Dimension <= 5),
  aes(x = factor(Dimension), y = Original, color = Method)
) +
  geom_point(size = 4, position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    position = position_dodge(0.3),
    linewidth = 1
  ) +
  labs(
    title = "Eigenvalue Stability: G-Gower vs RelMS",
    subtitle = "Points = Original; Error bars = 95% Bootstrap CI",
    x = "Dimension",
    y = "Eigenvalue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

### 3.2 Coeficiente de variación (CV) de eigenvalues
cv_comparison <- data.frame(
  Dimension = 1:10,
  CV_GGower = (apply(eigen_ggower_boot, 2, sd) /
    colMeans(eigen_ggower_boot)) *
    100,
  CV_RELMS = (apply(eigen_relms_boot, 2, sd) /
    colMeans(eigen_relms_boot)) *
    100
) %>%
  pivot_longer(
    cols = starts_with("CV"),
    names_to = "Method",
    values_to = "CV",
    names_prefix = "CV_"
  )

plot_cv_comparison <- ggplot(
  cv_comparison,
  aes(x = factor(Dimension), y = CV, fill = Method)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Coefficient of Variation: G-Gower vs RELMS",
    subtitle = "Lower CV = More stable",
    x = "Dimension",
    y = "CV (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


summary_comparison <- data.frame(
  Metric = c(
    "Variance Dim 1-3 (%)",
    "Mean CV Eigenvalues (Dim 1-3)",
    "Strongest association (Dim 1)"
  ),
  GGower = c(
    round(sum(var_ggower[1:3]), 2),
    round(
      mean(cv_comparison$CV[
        cv_comparison$Method == "GGower" &
          cv_comparison$Dimension <= 3
      ]),
      2
    ),
    names(sort(associations_ggower[, 1], decreasing = TRUE)[1])
  ),
  RELMS = c(
    round(sum(var_relms[1:3]), 2),
    round(
      mean(cv_comparison$CV[
        cv_comparison$Method == "RELMS" &
          cv_comparison$Dimension <= 3
      ]),
      2
    ),
    names(sort(associations_relms[, 1], decreasing = TRUE)[1])
  )
) %>%
  gt() %>%
  # Título y subtítulo
  tab_header(
    title = md("**MDS Comparison: G-Gower vs RelMS**"),
    subtitle = "Summary of key performance metrics"
  ) %>%
  # Renombrar columnas
  cols_label(
    Metric = md("**Metric**"),
    GGower = md("**G-Gower**"),
    RELMS = md("**RelMS**")
  ) %>%
  # Alinear columnas
  cols_align(
    align = "left",
    columns = Metric
  ) %>%
  cols_align(
    align = "center",
    columns = c(GGower, RELMS)
  ) %>%
  # Añadir nota al pie
  tab_source_note(
    source_note = md(
      "*Lower CV indicates higher stability. Higher variance indicates better dimensionality reduction.*"
    )
  ) %>%
  # Bordes y formato general
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(3),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(3),
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(2),
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2),
    heading.align = "center",
    column_labels.font.weight = "bold",
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14),
    source_notes.font.size = px(11)
  ) %>%
  # Añadir líneas entre filas
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(
      rows = 1:2
    )
  )

cat("\n=== SUMMARY: G-Gower vs RELMS ===\n")
print(summary_comparison)

# Visualizar todos los plots principales
plot_variance_comparison
plot_cumulative_comparison
plot_eigen_stability
plot_cv_comparison


## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- CLUSTERING --------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## CLUSTERING JERARQUICO UTILIZANDO MATRIZ DISTANCIAS RelMS
{
  ## ============== 1. DEFINIR MÉTODOS DE LINKAGE A COMPARAR ==============
  linkage_methods <- c(
    "single",
    "complete",
    "average",
    "ward.D2",
    "mcquitty",
    "median",
    "centroid"
  )

  ## ============== 2. CALCULAR CORRELACIÓN COFENÉTICA PARA CADA MÉTODO ==============
  cophenetic_results <- data.frame(
    Method = linkage_methods,
    Cophenetic_Correlation = NA_real_,
    stringsAsFactors = FALSE
  )

  hierarchical_models <- list()

  cat("\nCalculando correlación cofenética para cada método de linkage...\n")

  for (i in seq_along(linkage_methods)) {
    method <- linkage_methods[i]

    cat("  Método:", method, "... ")

    # Calcular clustering jerárquico
    #hc_model <- hclust(D_robust_relms, method = method)
    hc_model <- hclust(D_robust_ggower, method = method)
    # Calcular matriz de distancias cofenéticas
    coph_dist <- cophenetic(hc_model)

    # Calcular correlación cofenética
    #coph_cor <- cor(D_robust_relms, coph_dist)
    coph_cor <- cor(D_robust_ggower, coph_dist)
    # Guardar resultados
    cophenetic_results$Cophenetic_Correlation[i] <- coph_cor
    hierarchical_models[[method]] <- hc_model

    cat("Correlación =", round(coph_cor, 4), "\n")
  }

  ## ============== 3. ORDENAR Y MOSTRAR RESULTADOS ==============
  cophenetic_results <- cophenetic_results %>%
    arrange(desc(Cophenetic_Correlation))

  ## ============== 4. GRÁFICO DE COMPARACIÓN ==============
  plot_cophenetic_comparison <- ggplot(
    cophenetic_results,
    aes(
      x = reorder(Method, Cophenetic_Correlation),
      y = Cophenetic_Correlation,
      fill = Cophenetic_Correlation
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = round(Cophenetic_Correlation, 3)),
      hjust = -0.1,
      size = 4
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "yellow",
      high = "green",
      midpoint = median(cophenetic_results$Cophenetic_Correlation),
      name = "Correlation"
    ) +
    coord_flip() +
    labs(
      title = "Cophenetic Correlation by Linkage Method",
      subtitle = "Higher correlation = Better representation of original distances",
      x = "Linkage Method",
      y = "Cophenetic Correlation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "none"
    ) +
    ylim(0, 1)

  plot_cophenetic_comparison

  ## ============== 5. SELECCIONAR MEJOR MÉTODO ==============
  best_method <- cophenetic_results$Method[1]
  best_correlation <- cophenetic_results$Cophenetic_Correlation[1]

  library(NbClust)
  nb <- NbClust(
    diss = D_robust_ggower,
    distance = NULL,
    min.nc = 2,
    max.nc = 30,
    method = "average",
    index = "dunn"
  )

  fviz_nbclust(nb)

  ## ============== 6. DENDROGRAMAS PARA LOS TOP 3 MÉTODOS ==============
  top3_methods <- cophenetic_results$Method[1:3]

  cat("\n\nGenerando dendrogramas para los top 3 métodos...\n")

  dendrograms_list <- list()

  for (method in top3_methods) {
    cat("  Generando dendrograma para:", method, "\n")

    hc_model <- hierarchical_models[[method]]

    # Dendrograma básico
    dend_plot <- fviz_dend(
      hc_model,
      k = 4, # Número de clusters a visualizar
      cex = 0.5,
      palette = "jco",
      rect = TRUE,
      rect_border = "jco",
      rect_fill = TRUE,
      main = paste0(
        "Dendrogram - ",
        toupper(method),
        " Linkage\n",
        "Cophenetic Correlation: ",
        round(
          cophenetic_results$Cophenetic_Correlation[
            cophenetic_results$Method == method
          ],
          3
        )
      )
    ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )

    dendrograms_list[[method]] <- dend_plot
  }

  ## ============== 7. MOSTRAR DENDROGRAMAS ==============
  # Dendrograma del mejor método
  dendrograms_list[[best_method]]

  # Comparar los 3 mejores
  gridExtra::grid.arrange(
    dendrograms_list[[top3_methods[1]]],
    dendrograms_list[[top3_methods[2]]],
    dendrograms_list[[top3_methods[3]]],
    ncol = 1
  )

  ## ============== 8. ANÁLISIS DEL MEJOR MÉTODO: DETERMINAR K ÓPTIMO ==============
  cat(
    "\n\nAnalizando número óptimo de clusters para el mejor método:",
    best_method,
    "\n"
  )

  best_hc_model <- hierarchical_models[[best_method]]
  best_hc_model <- hierarchical_models[["complete"]]
  best_method <- "complete"

  # Método del codo (Within-cluster sum of squares)
  wss_values <- numeric(30)
  for (k in 2:30) {
    clusters <- cutree(best_hc_model, k = k)
    wss_values[k] <- sum(
      tapply(
        seq_along(clusters),
        clusters,
        function(idx) {
          if (length(idx) > 1) {
            sum(as.matrix(D_robust_relms)[idx, idx]^2) / (2 * length(idx))
          } else {
            0
          }
        }
      )
    )
  }

  # Plot del codo
  plot_elbow <- data.frame(
    k = 2:30,
    WSS = wss_values[2:30]
  ) %>%
    ggplot(aes(x = k, y = WSS)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    labs(
      title = paste("Elbow Method -", toupper(best_method), "Linkage"),
      x = "Number of Clusters (k)",
      y = "Within-cluster Sum of Squares"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    scale_x_continuous(breaks = 2:30)

  plot_elbow

  # Silhouette para diferentes valores de k
  silhouette_scores <- numeric(28)
  for (k in 2:30) {
    clusters <- cutree(best_hc_model, k = k)
    sil <- silhouette(clusters, D_robust_relms)
    silhouette_scores[k - 1] <- mean(sil[, 3])
  }

  # Plot de Silhouette
  plot_silhouette_k <- data.frame(
    k = 2:30,
    Silhouette = silhouette_scores
  ) %>%
    ggplot(aes(x = k, y = Silhouette)) +
    geom_line(linewidth = 1, color = "darkgreen") +
    geom_point(size = 3, color = "darkgreen") +
    geom_hline(
      yintercept = max(silhouette_scores),
      linetype = "dashed",
      color = "red"
    ) +
    annotate(
      "text",
      x = which.max(silhouette_scores) + 1,
      y = max(silhouette_scores),
      label = paste0("Max at k=", which.max(silhouette_scores) + 1),
      vjust = -0.5,
      color = "red"
    ) +
    labs(
      title = paste(
        "Average Silhouette Width -",
        toupper(best_method),
        "Linkage"
      ),
      x = "Number of Clusters (k)",
      y = "Average Silhouette Width"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    scale_x_continuous(breaks = 2:30)

  plot_silhouette_k

  # K óptimo según Silhouette
  optimal_k <- which.max(silhouette_scores) + 1
  optimal_k <- 23
  cat("\n=== K ÓPTIMO ===\n")
  cat("Según Silhouette: k =", optimal_k, "\n")
  cat("Silhouette promedio:", round(max(silhouette_scores), 4), "\n")

  ## ============== 9. CLUSTERING FINAL CON K ÓPTIMO ==============
  final_clusters <- cutree(best_hc_model, k = optimal_k)

  # Resumen de clusters
  cluster_summary <- data.frame(
    Cluster = 1:optimal_k,
    Size = as.numeric(table(final_clusters))
  ) %>%
    mutate(
      Percentage = round(Size / sum(Size) * 100, 2)
    )

  cat("\n=== RESUMEN DE CLUSTERS FINALES ===\n")
  print(cluster_summary)

  # Silhouette plot final
  final_silhouette <- silhouette(final_clusters, D_robust_relms)
  plot_final_silhouette <- fviz_silhouette(
    final_silhouette,
    print.summary = FALSE
  ) +
    labs(
      title = paste0(
        "Silhouette Plot - ",
        toupper(best_method),
        " Linkage (k=",
        optimal_k,
        ")"
      ),
      subtitle = paste0(
        "Average Silhouette Width: ",
        round(mean(final_silhouette[, 3]), 3)
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )

  plot_final_silhouette

  ## ============== 10. VISUALIZACIÓN EN MDS ==============
  # Añadir clusters a los datos MDS
  MDS_relms_clustered <- MDS_relms_data %>%
    mutate(Cluster = as.factor(final_clusters))

  # Plot MDS con clusters
  plot_mds_clusters <- ggplot(
    MDS_relms_clustered,
    aes(x = V1, y = V2, color = Cluster)
  ) +
    geom_point(alpha = 0.6, size = 2) +
    stat_ellipse(level = 0.95, linewidth = 1) +
    labs(
      title = paste0(
        "Hierarchical Clustering (",
        toupper(best_method),
        ") - MDS Visualization"
      ),
      subtitle = paste0("k = ", optimal_k, " clusters"),
      x = paste0("MDS Dimension 1 (", round(var_relms[1], 1), "%)"),
      y = paste0("MDS Dimension 2 (", round(var_relms[2], 1), "%)")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    coord_fixed()

  plot_mds_clusters

  ## ============== 11. GUARDAR RESULTADOS ==============
  hierarchical_clustering_results <- list(
    cophenetic_comparison = cophenetic_results,
    best_method = best_method,
    best_correlation = best_correlation,
    optimal_k = optimal_k,
    final_clusters = final_clusters,
    cluster_summary = cluster_summary,
    hierarchical_models = hierarchical_models,
    plots = list(
      cophenetic_comparison = plot_cophenetic_comparison,
      dendrograms = dendrograms_list,
      elbow = plot_elbow,
      silhouette_k = plot_silhouette_k,
      final_silhouette = plot_final_silhouette,
      mds_clusters = plot_mds_clusters
    )
  )
}


## CLUSTERING NO JERARQUICO (NEW)

## ============== 0. PREPARAR DATOS CON CRITERIO DE VARIANZA ==============

# Umbral de varianza explicada acumulada
variance_threshold <- 70 # Puedes cambiar este valor (50, 60, 70, etc.)

cat("Criterio de selección de componentes:\n")
cat("  Varianza explicada acumulada ≥", variance_threshold, "%\n\n")

## -------- PCA: Selección de componentes --------
# Varianza explicada por cada PC
pca_variance <- pca_object$eig[, "percentage of variance"]
pca_cumulative <- cumsum(pca_variance)

# Número de PCs necesarios para alcanzar el umbral
n_pcs <- which(pca_cumulative >= variance_threshold)[1]

cat("=== PCA ===\n")
cat("Componentes seleccionadas:", n_pcs, "\n")
cat("Varianza explicada acumulada:", round(pca_cumulative[n_pcs], 2), "%\n")
cat("Detalle por componente:\n")
for (i in 1:n_pcs) {
  cat(sprintf(
    "  PC%d: %.2f%% (Acum: %.2f%%)\n",
    i,
    pca_variance[i],
    pca_cumulative[i]
  ))
}

# Extraer coordenadas PCA y CONVERTIR A DATA.FRAME ESTÁNDAR
PCA_coordinates <- pca_object$ind$coord[, 1:n_pcs] %>%
  as.data.frame() # ← CAMBIO AQUÍ: as.data.frame() en lugar de as_data_frame()

colnames(PCA_coordinates) <- paste0("PC", 1:n_pcs)

## -------- MDS: Selección de dimensiones --------
# Varianza explicada por cada dimensión MDS (ya calculada previamente)
mds_cumulative_ggower <- cumsum(var_ggower)
mds_cumulative_relms <- cumsum(var_relms)

# Número de dimensiones necesarias para G-Gower
n_mds_ggower <- which(mds_cumulative_ggower >= variance_threshold)[1]

# Número de dimensiones necesarias para RelMS
n_mds_relms <- which(mds_cumulative_relms >= variance_threshold)[1]

cat("\n=== MDS G-Gower ===\n")
cat("Dimensiones seleccionadas:", n_mds_ggower, "\n")
cat(
  "Varianza explicada acumulada:",
  round(mds_cumulative_ggower[n_mds_ggower], 2),
  "%\n"
)
cat("Detalle por dimensión:\n")
for (i in 1:n_mds_ggower) {
  cat(sprintf(
    "  Dim%d: %.2f%% (Acum: %.2f%%)\n",
    i,
    var_ggower[i],
    mds_cumulative_ggower[i]
  ))
}

cat("\n=== MDS RelMS ===\n")
cat("Dimensiones seleccionadas:", n_mds_relms, "\n")
cat(
  "Varianza explicada acumulada:",
  round(mds_cumulative_relms[n_mds_relms], 2),
  "%\n"
)
cat("Detalle por dimensión:\n")
for (i in 1:n_mds_relms) {
  cat(sprintf(
    "  Dim%d: %.2f%% (Acum: %.2f%%)\n",
    i,
    var_relms[i],
    mds_cumulative_relms[i]
  ))
}

# Usar el MDS con más componentes
n_mds <- max(n_mds_ggower, n_mds_relms)
cat(
  "\n>>> Usando",
  n_mds,
  "dimensiones MDS (máximo entre G-Gower y RelMS)\n"
)

# Extraer coordenadas MDS y CONVERTIR A DATA.FRAME ESTÁNDAR
MDS_coordinates <- MDS_relms_data[, 1:n_mds] %>%
  as.data.frame() # ← CAMBIO AQUÍ: as.data.frame() en lugar de tibble

colnames(MDS_coordinates) <- paste0("MDS", 1:n_mds)

## ============== 1. FUNCIÓN DE COMPARACIÓN (SIN CAMBIOS) ==============
compare_clustering_methods <- function(
  data_pca,
  data_mds,
  k_values = 2:15,
  seed = 123
) {
  set.seed(seed)

  # Calcular distancias
  cat("\nCalculando matrices de distancias...\n")
  D_pca <- dist(data_pca, method = "euclidean")
  D_mds <- dist(data_mds, method = "euclidean")

  # Listas para guardar resultados
  resultados <- list()
  resumen_tabla <- list()

  # Configuraciones a probar
  configuraciones <- list(
    list(
      name = "PCA",
      data = data_pca,
      dist = D_pca,
      methods = c("PAM", "K-means")
    ),
    list(
      name = "MDS",
      data = data_mds,
      dist = D_mds,
      methods = c("PAM", "K-means")
    )
  )

  ## ============== 2. CLUSTERING PARA CADA CONFIGURACIÓN ==============
  for (config in configuraciones) {
    for (method in config$methods) {
      for (k in k_values) {
        cat(sprintf("\n>>> %s + %s (k=%d)\n", config$name, method, k))

        # Aplicar clustering
        if (method == "PAM") {
          model <- pam(config$dist, k = k)
          clusters <- model$clustering
        } else {
          # K-means
          model <- kmeans(config$data, centers = k, nstart = 25)
          clusters <- model$cluster
        }

        # Métricas de calidad
        ## Silhouette
        sil_obj <- silhouette(clusters, config$dist)
        sil_mean <- mean(sil_obj[, 3])

        ## Calinski-Harabasz
        ch_index <- cluster.stats(config$dist, clusters)$ch

        ## Dunn Index
        dunn_index <- cluster.stats(config$dist, clusters)$dunn

        ## Within-cluster sum of squares
        wss <- sum(cluster.stats(config$dist, clusters)$within.cluster.ss)

        # Tamaños de clusters
        sizes <- table(clusters)
        sizes_str <- paste(sizes, collapse = "-")

        # Balance de clusters (coef. variación de tamaños)
        cv_sizes <- sd(as.numeric(sizes)) / mean(as.numeric(sizes))

        # Silhouette plot
        sil_plot <- fviz_silhouette(sil_obj, print.summary = FALSE) +
          labs(
            title = sprintf("%s + %s (k=%d)", config$name, method, k),
            subtitle = sprintf(
              "Avg Silhouette: %.3f | CH: %.1f",
              sil_mean,
              ch_index
            )
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)
          )

        # Guardar resultados
        key <- sprintf(
          "%s_%s_k%d",
          tolower(config$name),
          tolower(gsub("-", "", method)),
          k
        )

        resultados[[key]] <- list(
          space = config$name,
          method = method,
          k = k,
          clusters = clusters,
          silhouette = sil_mean,
          ch_index = ch_index,
          dunn_index = dunn_index,
          wss = wss,
          cluster_sizes = sizes,
          cv_sizes = cv_sizes,
          silhouette_plot = sil_plot,
          model = model
        )

        # Agregar a tabla resumen
        resumen_tabla[[length(resumen_tabla) + 1]] <- tibble(
          Space = config$name,
          Method = method,
          k = k,
          Silhouette = round(sil_mean, 4),
          CH_Index = round(ch_index, 2),
          Dunn_Index = round(dunn_index, 4),
          WSS = round(wss, 2),
          CV_Sizes = round(cv_sizes, 3),
          Cluster_Sizes = sizes_str
        )
      }
    }
  }

  # Convertir a dataframe y ordenar
  resumen_df <- bind_rows(resumen_tabla) %>%
    mutate(
      Config = paste(Space, Method, sep = " + "),
      # Puntaje combinado (normalizado 0-1)
      Score = ((Silhouette - min(Silhouette)) /
        (max(Silhouette) - min(Silhouette)) *
        0.4 +
        (CH_Index - min(CH_Index)) / (max(CH_Index) - min(CH_Index)) * 0.3 +
        (Dunn_Index - min(Dunn_Index)) /
          (max(Dunn_Index) - min(Dunn_Index)) *
          0.2 +
        (1 - CV_Sizes / max(CV_Sizes)) * 0.1)
    ) %>%
    arrange(desc(Score))

  ## ============== 3. GRÁFICOS COMPARATIVOS ==============

  # Silhouette vs k
  plot_silhouette <- ggplot(
    resumen_df,
    aes(x = k, y = Silhouette, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Average Silhouette Width by Method and k",
      x = "Number of Clusters (k)",
      y = "Average Silhouette Width",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # Calinski-Harabasz vs k
  plot_ch <- ggplot(
    resumen_df,
    aes(x = k, y = CH_Index, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Calinski-Harabasz Index by Method and k",
      x = "Number of Clusters (k)",
      y = "CH Index",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # Dunn Index vs k
  plot_dunn <- ggplot(
    resumen_df,
    aes(x = k, y = Dunn_Index, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Dunn Index by Method and k",
      x = "Number of Clusters (k)",
      y = "Dunn Index",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # Heatmap de métricas
  heatmap_data <- resumen_df %>%
    select(Config, k, Silhouette, CH_Index, Dunn_Index) %>%
    pivot_longer(
      cols = c(Silhouette, CH_Index, Dunn_Index),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    group_by(Metric) %>%
    mutate(
      Value_Normalized = (Value - min(Value)) / (max(Value) - min(Value))
    ) %>%
    ungroup()

  plot_heatmap <- ggplot(
    heatmap_data,
    aes(x = factor(k), y = Config, fill = Value_Normalized)
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Value, 2)), color = "black", size = 2.5) +
    scale_fill_gradient2(
      low = "red",
      mid = "yellow",
      high = "green",
      midpoint = 0.5,
      name = "Normalized\nValue"
    ) +
    facet_wrap(~Metric, ncol = 1, scales = "free_y") +
    labs(
      title = "Quality Metrics Heatmap",
      x = "Number of Clusters (k)",
      y = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      axis.text.x = element_text(angle = 0)
    )

  # Retornar resultados
  return(list(
    resultados = resultados,
    resumen = resumen_df,
    plots = list(
      silhouette = plot_silhouette,
      ch = plot_ch,
      dunn = plot_dunn,
      heatmap = plot_heatmap
    ),
    n_components = list(
      pca = ncol(data_pca),
      mds = ncol(data_mds)
    )
  ))
}


compare_clustering_methods <- function(
  data_pca,
  data_mds,
  k_values = 2:15,
  seed = 123
) {
  set.seed(seed)

  # Calcular distancias
  cat("\nCalculando matrices de distancias...\n")
  D_pca_euclidean <- dist(data_pca, method = "euclidean")
  D_mds_euclidean <- dist(data_mds, method = "euclidean")

  # NUEVO: Calcular distancias de Mahalanobis
  cat("Calculando distancias de Mahalanobis...\n")
  library(biotools)

  D_pca_mahalanobis <- D2.dist(
    data_pca,
    cov = var(data_pca)
  ) %>%
    sqrt() %>%
    as.dist()

  D_mds_mahalanobis <- D2.dist(
    data_mds,
    cov = var(data_mds)
  ) %>%
    sqrt() %>%
    as.dist()

  # Listas para guardar resultados
  resultados <- list()
  resumen_tabla <- list()

  # Configuraciones a probar (ACTUALIZADO con Mahalanobis)
  configuraciones <- list(
    list(
      name = "PCA",
      data = data_pca,
      dist_euclidean = D_pca_euclidean,
      dist_mahalanobis = D_pca_mahalanobis,
      methods = c("PAM", "K-means")
    ),
    list(
      name = "MDS",
      data = data_mds,
      dist_euclidean = D_mds_euclidean,
      dist_mahalanobis = D_mds_mahalanobis,
      methods = c("PAM", "K-means")
    )
  )

  ## ============== 2. CLUSTERING PARA CADA CONFIGURACIÓN ==============
  for (config in configuraciones) {
    for (method in config$methods) {
      for (k in k_values) {
        # PAM con Euclidean
        cat(sprintf(
          "\n>>> %s + %s (Euclidean, k=%d)\n",
          config$name,
          method,
          k
        ))

        if (method == "PAM") {
          model <- pam(config$dist_euclidean, k = k)
          clusters <- model$clustering
          dist_used <- config$dist_euclidean
          dist_name <- "Euclidean"
        } else {
          # K-means
          model <- kmeans(config$data, centers = k, nstart = 25)
          clusters <- model$cluster
          dist_used <- config$dist_euclidean
          dist_name <- "Euclidean"
        }

        # Métricas
        sil_obj <- silhouette(clusters, dist_used)
        sil_mean <- mean(sil_obj[, 3])
        ch_index <- cluster.stats(dist_used, clusters)$ch
        dunn_index <- cluster.stats(dist_used, clusters)$dunn
        wss <- sum(cluster.stats(dist_used, clusters)$within.cluster.ss)

        sizes <- table(clusters)
        sizes_str <- paste(sizes, collapse = "-")
        cv_sizes <- sd(as.numeric(sizes)) / mean(as.numeric(sizes))

        sil_plot <- fviz_silhouette(sil_obj, print.summary = FALSE) +
          labs(
            title = sprintf(
              "%s + %s (%s, k=%d)",
              config$name,
              method,
              dist_name,
              k
            ),
            subtitle = sprintf(
              "Avg Silhouette: %.3f | CH: %.1f",
              sil_mean,
              ch_index
            )
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)
          )

        key <- sprintf(
          "%s_%s_%s_k%d",
          tolower(config$name),
          tolower(gsub("-", "", method)),
          tolower(dist_name),
          k
        )

        resultados[[key]] <- list(
          space = config$name,
          method = method,
          distance = dist_name,
          k = k,
          clusters = clusters,
          silhouette = sil_mean,
          ch_index = ch_index,
          dunn_index = dunn_index,
          wss = wss,
          cluster_sizes = sizes,
          cv_sizes = cv_sizes,
          silhouette_plot = sil_plot,
          model = model
        )

        resumen_tabla[[length(resumen_tabla) + 1]] <- tibble(
          Space = config$name,
          Method = method,
          Distance = dist_name,
          k = k,
          Silhouette = round(sil_mean, 4),
          CH_Index = round(ch_index, 2),
          Dunn_Index = round(dunn_index, 4),
          WSS = round(wss, 2),
          CV_Sizes = round(cv_sizes, 3),
          Cluster_Sizes = sizes_str
        )

        # NUEVO: PAM con Mahalanobis
        if (method == "PAM") {
          cat(sprintf("\n>>> %s + PAM (Mahalanobis, k=%d)\n", config$name, k))

          model_maha <- pam(config$dist_mahalanobis, k = k)
          clusters_maha <- model_maha$clustering

          sil_obj_maha <- silhouette(clusters_maha, config$dist_mahalanobis)
          sil_mean_maha <- mean(sil_obj_maha[, 3])
          ch_index_maha <- cluster.stats(
            config$dist_mahalanobis,
            clusters_maha
          )$ch
          dunn_index_maha <- cluster.stats(
            config$dist_mahalanobis,
            clusters_maha
          )$dunn
          wss_maha <- sum(
            cluster.stats(
              config$dist_mahalanobis,
              clusters_maha
            )$within.cluster.ss
          )

          sizes_maha <- table(clusters_maha)
          sizes_str_maha <- paste(sizes_maha, collapse = "-")
          cv_sizes_maha <- sd(as.numeric(sizes_maha)) /
            mean(as.numeric(sizes_maha))

          sil_plot_maha <- fviz_silhouette(
            sil_obj_maha,
            print.summary = FALSE
          ) +
            labs(
              title = sprintf("%s + PAM (Mahalanobis, k=%d)", config$name, k),
              subtitle = sprintf(
                "Avg Silhouette: %.3f | CH: %.1f",
                sil_mean_maha,
                ch_index_maha
              )
            ) +
            theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5)
            )

          key_maha <- sprintf(
            "%s_pam_mahalanobis_k%d",
            tolower(config$name),
            k
          )

          resultados[[key_maha]] <- list(
            space = config$name,
            method = "PAM",
            distance = "Mahalanobis",
            k = k,
            clusters = clusters_maha,
            silhouette = sil_mean_maha,
            ch_index = ch_index_maha,
            dunn_index = dunn_index_maha,
            wss = wss_maha,
            cluster_sizes = sizes_maha,
            cv_sizes = cv_sizes_maha,
            silhouette_plot = sil_plot_maha,
            model = model_maha
          )

          resumen_tabla[[length(resumen_tabla) + 1]] <- tibble(
            Space = config$name,
            Method = "PAM",
            Distance = "Mahalanobis",
            k = k,
            Silhouette = round(sil_mean_maha, 4),
            CH_Index = round(ch_index_maha, 2),
            Dunn_Index = round(dunn_index_maha, 4),
            WSS = round(wss_maha, 2),
            CV_Sizes = round(cv_sizes_maha, 3),
            Cluster_Sizes = sizes_str_maha
          )
        }
      }
    }
  }

  # Convertir a dataframe y ordenar
  resumen_df <- bind_rows(resumen_tabla) %>%
    mutate(
      Config = paste(Space, Method, Distance, sep = " + "),
      Score = ((Silhouette - min(Silhouette)) /
        (max(Silhouette) - min(Silhouette)) *
        0.4 +
        (CH_Index - min(CH_Index)) / (max(CH_Index) - min(CH_Index)) * 0.3 +
        (Dunn_Index - min(Dunn_Index)) /
          (max(Dunn_Index) - min(Dunn_Index)) *
          0.2 +
        (1 - CV_Sizes / max(CV_Sizes)) * 0.1)
    ) %>%
    arrange(desc(Score))

  ## ============== 3. GRÁFICOS COMPARATIVOS ==============

  # Silhouette vs k
  plot_silhouette <- ggplot(
    resumen_df,
    aes(x = k, y = Silhouette, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Average Silhouette Width by Method and k",
      x = "Number of Clusters (k)",
      y = "Average Silhouette Width",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # NUEVO: Elbow plot (WSS)
  plot_elbow <- ggplot(
    resumen_df,
    aes(x = k, y = WSS, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Elbow Method: Within-cluster Sum of Squares",
      x = "Number of Clusters (k)",
      y = "WSS",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # Calinski-Harabasz vs k
  plot_ch <- ggplot(
    resumen_df,
    aes(x = k, y = CH_Index, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Calinski-Harabasz Index by Method and k",
      x = "Number of Clusters (k)",
      y = "CH Index",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # Dunn Index vs k
  plot_dunn <- ggplot(
    resumen_df,
    aes(x = k, y = Dunn_Index, color = Config, group = Config)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Dunn Index by Method and k",
      x = "Number of Clusters (k)",
      y = "Dunn Index",
      color = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = k_values)

  # Heatmap de métricas
  heatmap_data <- resumen_df %>%
    dplyr::select(Config, k, Silhouette, CH_Index, Dunn_Index) %>%
    pivot_longer(
      cols = c(Silhouette, CH_Index, Dunn_Index),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    group_by(Metric) %>%
    mutate(
      Value_Normalized = (Value - min(Value)) / (max(Value) - min(Value))
    ) %>%
    ungroup()

  plot_heatmap <- ggplot(
    heatmap_data,
    aes(x = factor(k), y = Config, fill = Value_Normalized)
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Value, 2)), color = "black", size = 2.5) +
    scale_fill_gradient2(
      low = "red",
      mid = "yellow",
      high = "green",
      midpoint = 0.5,
      name = "Normalized\nValue"
    ) +
    facet_wrap(~Metric, ncol = 1, scales = "free_y") +
    labs(
      title = "Quality Metrics Heatmap",
      x = "Number of Clusters (k)",
      y = "Configuration"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      axis.text.x = element_text(angle = 0)
    )

  # Retornar resultados
  return(list(
    resultados = resultados,
    resumen = resumen_df,
    plots = list(
      silhouette = plot_silhouette,
      elbow = plot_elbow, # NUEVO
      ch = plot_ch,
      dunn = plot_dunn,
      heatmap = plot_heatmap
    ),
    n_components = list(
      pca = ncol(data_pca),
      mds = ncol(data_mds)
    )
  ))
}

## ============== 4. EJECUTAR COMPARACIÓN ==============
clustering_results <- compare_clustering_methods(
  data_pca = PCA_coordinates,
  data_mds = MDS_coordinates,
  k_values = 3:12,
  seed = 123
)

## ============== 5. VISUALIZAR RESULTADOS ==============
# Ver todos los gráficos
clustering_results$plots$silhouette
clustering_results$plots$elbow # NUEVO: Elbow plot
clustering_results$plots$ch
clustering_results$plots$dunn
clustering_results$plots$heatmap

# Ver resumen con todas las configuraciones (incluyendo Mahalanobis)
clustering_results$resumen %>%
  dplyr::select(
    Space,
    Method,
    Distance,
    k,
    Silhouette,
    CH_Index,
    Dunn_Index,
    WSS,
    Score
  ) %>%
  arrange(desc(Score)) %>%
  head(15)

# 🎓 Justificación para el informe:
# "Se seleccionaron dos configuraciones finales de clustering:

# PCA + K-means (k=4) con distancia Euclidiana, que obtuvo el mejor Score combinado (0.718) y presenta 4 clusters interpretables diferenciados principalmente por variables de tamaño y precio (loadings de PC1-PC2).

# MDS + K-means (k=8) con distancia Euclidiana, que captura segmentos más finos considerando variables mixtas (numéricas + categóricas). Aunque k=11 tenía Score ligeramente superior, k=8 se prefirió por su balance entre separación (Dunn=0.134) y parsimonia interpretativa.

# Ambos modelos utilizan K-means sobre espacios reducidos que explican >70% de varianza, garantizando eficiencia computacional y robustez estadística."

## ============== MODELOS FINALES SELECCIONADOS ==============

# PCA: K-means k=4
modelo_pca_final <- clustering_results$resultados$pca_kmeans_euclidean_k4

# MDS: K-means k=8
modelo_mds_final <- clustering_results$resultados$mds_kmeans_euclidean_k7
#modelo_mds_final <- clustering_results$resultados$mds_kmeans_euclidean_k11

## ============== VISUALIZACIÓN COMPARATIVA ==============
library(gridExtra)

grid.arrange(
  modelo_pca_final$silhouette_plot,
  modelo_mds_final$silhouette_plot,
  ncol = 2
)

## ============== AÑADIR CLUSTERS A LOS DATOS ==============
data_airbnb_sample_clustered <- data_airbnb_sample %>%
  mutate(
    Cluster_PCA = factor(modelo_pca_final$clusters),
    Cluster_MDS = factor(modelo_mds_final$clusters)
  )

## ============== PLOTS EN ESPACIOS REDUCIDOS ==============

# PCA
PCA_clustered <- PCA_coordinates %>%
  mutate(Cluster = factor(modelo_pca_final$clusters))

plot_pca_k4 <- ggplot(
  PCA_clustered,
  aes(x = PC1, y = PC2, color = Cluster)
) +
  geom_point(alpha = 0.6, size = 2.5) +
  stat_ellipse(level = 0.95, linewidth = 1) +
  labs(
    title = "PCA + K-means (k=4)",
    subtitle = "Silhouette: 0.254 | CH: 265 | Score: 0.718",
    x = paste0("PC1 (", round(pca_variance[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_variance[2], 1), "%)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set1")

# MDS
MDS_clustered <- MDS_coordinates %>%
  mutate(Cluster = factor(modelo_mds_final$clusters))

plot_mds_k8 <- ggplot(
  MDS_clustered,
  aes(x = MDS1, y = MDS2, color = Cluster)
) +
  geom_point(alpha = 0.6, size = 2.5) +
  stat_ellipse(level = 0.95, linewidth = 1) +
  labs(
    title = "MDS + K-means (k=8)",
    subtitle = "Silhouette: 0.238 | CH: 141 | Dunn: 0.134 | Score: 0.687",
    x = paste0("MDS1 (", round(var_relms[1], 1), "%)"),
    y = paste0("MDS2 (", round(var_relms[2], 1), "%)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2")

grid.arrange(plot_pca_k4, plot_mds_k8, ncol = 2)


## -------- PCA 3D --------
# Preparar datos con 3 componentes
PCA_clustered_3d <- data.frame(
  PC1 = PCA_coordinates$PC1,
  PC2 = PCA_coordinates$PC2,
  PC3 = PCA_coordinates$PC3,
  Cluster = factor(modelo_pca_final$clusters)
)

# Gráfico 3D interactivo PCA
plot_pca_3d <- plot_ly(
  data = PCA_clustered_3d,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  color = ~Cluster,
  colors = RColorBrewer::brewer.pal(4, "Set1"),
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 4,
    opacity = 0.7,
    line = list(color = "white", width = 0.5)
  ),
  text = ~ paste0(
    "Cluster: ",
    Cluster,
    "<br>PC1: ",
    round(PC1, 2),
    "<br>PC2: ",
    round(PC2, 2),
    "<br>PC3: ",
    round(PC3, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = paste0(
        "<b>PCA 3D: K-means (k=4)</b><br>",
        "<sup>Silhouette: 0.254 | CH: 265 | Score: 0.718</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    scene = list(
      xaxis = list(title = paste0("PC1 (", round(pca_variance[1], 1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(pca_variance[2], 1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(pca_variance[3], 1), "%)"))
    ),
    legend = list(
      title = list(text = "<b>Cluster</b>"),
      orientation = "v",
      x = 1.02,
      y = 0.5
    )
  )

plot_pca_3d

## -------- MDS 3D --------
# Preparar datos con 3 dimensiones MDS
MDS_clustered_3d <- data.frame(
  MDS1 = MDS_coordinates$MDS1,
  MDS2 = MDS_coordinates$MDS2,
  MDS3 = MDS_coordinates$MDS3,
  Cluster = factor(modelo_mds_final$clusters)
)

# Gráfico 3D interactivo MDS
plot_mds_3d <- plot_ly(
  data = MDS_clustered_3d,
  x = ~MDS1,
  y = ~MDS2,
  z = ~MDS3,
  color = ~Cluster,
  colors = RColorBrewer::brewer.pal(8, "Set2"),
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 4,
    opacity = 0.85,
    line = list(color = "white", width = 0.5)
  ),
  text = ~ paste0(
    "Cluster: ",
    Cluster,
    "<br>MDS1: ",
    round(MDS1, 2),
    "<br>MDS2: ",
    round(MDS2, 2),
    "<br>MDS3: ",
    round(MDS3, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = paste0(
        "<b>MDS 3D: K-means (k=8)</b><br>",
        "<sup>Silhouette: 0.238 | CH: 141 | Dunn: 0.134 | Score: 0.687</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    scene = list(
      xaxis = list(title = paste0("MDS1 (", round(var_relms[1], 1), "%)")),
      yaxis = list(title = paste0("MDS2 (", round(var_relms[2], 1), "%)")),
      zaxis = list(title = paste0("MDS3 (", round(var_relms[3], 1), "%)"))
    ),
    legend = list(
      title = list(text = "<b>Cluster</b>"),
      orientation = "v",
      x = 1.02,
      y = 0.5
    )
  )

plot_mds_3d


## ============== GRÁFICOS 3D CON ESFERAS ENVOLVENTES ==============

## -------- Función para crear mesh 3D de un cluster --------
create_cluster_mesh <- function(points, color, opacity = 0.15) {
  # Calcular convex hull
  hull <- convhulln(points, options = "FA")

  # Extraer coordenadas de los vértices del hull
  vertices <- hull$hull

  # Crear mesh3d
  mesh <- list(
    type = "mesh3d",
    x = points[, 1],
    y = points[, 2],
    z = points[, 3],
    i = vertices[, 1] - 1, # Plotly usa índices 0-based
    j = vertices[, 2] - 1,
    k = vertices[, 3] - 1,
    opacity = opacity,
    color = color,
    showlegend = FALSE,
    hoverinfo = "skip"
  )

  return(mesh)
}

## -------- PCA 3D CON ESFERAS --------
# Paleta de colores
colors_pca <- RColorBrewer::brewer.pal(4, "Set1")

# Crear plot base
plot_pca_3d_spheres <- plot_ly()

# Añadir mesh para cada cluster
for (i in 1:4) {
  cluster_data <- PCA_clustered_3d %>%
    filter(Cluster == i) %>%
    select(PC1, PC2, PC3) %>%
    as.matrix()

  # Solo crear hull si hay suficientes puntos (mínimo 4)
  if (nrow(cluster_data) >= 4) {
    mesh <- create_cluster_mesh(
      cluster_data,
      color = colors_pca[i],
      opacity = 0.15
    )

    plot_pca_3d_spheres <- plot_pca_3d_spheres %>%
      add_trace(
        type = mesh$type,
        x = mesh$x,
        y = mesh$y,
        z = mesh$z,
        i = mesh$i,
        j = mesh$j,
        k = mesh$k,
        opacity = mesh$opacity,
        color = mesh$color,
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }
}

# Añadir puntos encima de las esferas
plot_pca_3d_spheres <- plot_pca_3d_spheres %>%
  add_trace(
    data = PCA_clustered_3d,
    x = ~PC1,
    y = ~PC2,
    z = ~PC3,
    color = ~Cluster,
    colors = colors_pca,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 4,
      opacity = 0.8,
      line = list(color = "white", width = 0.5)
    ),
    text = ~ paste0(
      "Cluster: ",
      Cluster,
      "<br>PC1: ",
      round(PC1, 2),
      "<br>PC2: ",
      round(PC2, 2),
      "<br>PC3: ",
      round(PC3, 2)
    ),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = paste0(
        "<b>PCA 3D: K-means (k=4) - Con regiones</b><br>",
        "<sup>Silhouette: 0.254 | CH: 265 | Score: 0.718</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    scene = list(
      xaxis = list(title = paste0("PC1 (", round(pca_variance[1], 1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(pca_variance[2], 1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(pca_variance[3], 1), "%)"))
    ),
    legend = list(
      title = list(text = "<b>Cluster</b>"),
      orientation = "v",
      x = 1.02,
      y = 0.5
    )
  )

plot_pca_3d_spheres

## -------- MDS 3D CON ESFERAS --------
colors_mds <- RColorBrewer::brewer.pal(8, "Set3")

plot_mds_3d_spheres <- plot_ly()

# Añadir mesh para cada cluster
for (i in 1:8) {
  cluster_data <- MDS_clustered_3d %>%
    filter(Cluster == i) %>%
    select(MDS1, MDS2, MDS3) %>%
    as.matrix()

  if (nrow(cluster_data) >= 4) {
    mesh <- create_cluster_mesh(
      cluster_data,
      color = colors_mds[i],
      opacity = 0.12 # Más transparente para MDS (más clusters)
    )

    plot_mds_3d_spheres <- plot_mds_3d_spheres %>%
      add_trace(
        type = mesh$type,
        x = mesh$x,
        y = mesh$y,
        z = mesh$z,
        i = mesh$i,
        j = mesh$j,
        k = mesh$k,
        opacity = mesh$opacity,
        color = mesh$color,
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }
}

# Añadir puntos
plot_mds_3d_spheres <- plot_mds_3d_spheres %>%
  add_trace(
    data = MDS_clustered_3d,
    x = ~MDS1,
    y = ~MDS2,
    z = ~MDS3,
    color = ~Cluster,
    colors = colors_mds,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 4,
      opacity = 0.8,
      line = list(color = "white", width = 0.5)
    ),
    text = ~ paste0(
      "Cluster: ",
      Cluster,
      "<br>MDS1: ",
      round(MDS1, 2),
      "<br>MDS2: ",
      round(MDS2, 2),
      "<br>MDS3: ",
      round(MDS3, 2)
    ),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = paste0(
        "<b>MDS 3D: K-means (k=8) - Con regiones</b><br>",
        "<sup>Silhouette: 0.238 | CH: 141 | Dunn: 0.134 | Score: 0.687</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    scene = list(
      xaxis = list(title = paste0("MDS1 (", round(var_relms[1], 1), "%)")),
      yaxis = list(title = paste0("MDS2 (", round(var_relms[2], 1), "%)")),
      zaxis = list(title = paste0("MDS3 (", round(var_relms[3], 1), "%)"))
    ),
    legend = list(
      title = list(text = "<b>Cluster</b>"),
      orientation = "v",
      x = 1.02,
      y = 0.5
    )
  )

plot_mds_3d_spheres


## ============== TABLAS DE CARACTERIZACIÓN DE CLUSTERS ==============
# Función para crear tabla de caracterización por cluster
create_cluster_table <- function(
  data_with_clusters, # ← CAMBIO: Ya incluye los clusters
  cluster_var, # ← Nombre de la columna de cluster
  title = "Cluster Characterization"
) {
  # Crear tabla bivariante
  tabla_clusters <- data_with_clusters %>%
    dplyr::select(-id) %>% # Quitar ID
    tbl_summary(
      by = all_of(cluster_var), # ← Usar columna existente
      type = list(
        "bedrooms" ~ "continuous"
      ),
      statistic = list(
        all_continuous() ~ "{median} ({IQR})", # Mediana (IQR)
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(
        all_continuous() ~ 2,
        all_categorical() ~ c(0, 1)
      ),
      missing = "no"
    ) %>%
    add_p(
      test = list(
        all_continuous() ~ "kruskal.test", # Kruskal-Wallis para continuas
        all_categorical() ~ "chisq.test" # Chi-cuadrado para categóricas
      ),
      pvalue_fun = function(x) style_pvalue(x, digits = 3)
    ) %>%
    add_overall(last = TRUE) %>% # Añadir columna "Overall"
    modify_header(
      label ~ "**Variable**",
      all_stat_cols() ~ "**{level}**<br>N = {n}"
    ) %>%
    modify_spanning_header(
      all_stat_cols() ~ paste0("**", cluster_var, "**")
    ) %>%
    bold_labels() %>%
    bold_p(t = 0.05) %>% # Resaltar p < 0.05
    as_gt() %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = paste("N =", nrow(data_with_clusters), "observations")
    ) %>%
    tab_options(
      table.border.top.color = "black",
      table.border.top.width = px(3),
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3),
      heading.border.bottom.color = "black",
      heading.border.bottom.width = px(2),
      column_labels.border.top.color = "black",
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(2),
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(2),
      heading.align = "center",
      column_labels.font.weight = "bold",
      table.font.size = px(12),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(12)
    ) %>%
    tab_footnote(
      footnote = md(
        "*Continuous variables: Median (IQR). Test: Kruskal-Wallis.*"
      ),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_footnote(
      footnote = md(
        "*Categorical variables: N (%). Test: Chi-squared. **Bold p < 0.05**.*"
      ),
      locations = cells_title(groups = "subtitle")
    )

  return(tabla_clusters)
}


# Tabla PCA (usando Cluster_PCA que ya existe en data_airbnb_sample_clustered)
tabla_clusters_pca <- create_cluster_table(
  data_with_clusters = data_airbnb_sample_clustered,
  cluster_var = "Cluster_PCA",
  title = "PCA Clustering Characterization (k=4)"
)

# Tabla MDS (usando Cluster_MDS que ya existe en data_airbnb_sample_clustered)
tabla_clusters_mds <- create_cluster_table(
  data_with_clusters = data_airbnb_sample_clustered,
  cluster_var = "Cluster_MDS",
  title = "MDS Clustering Characterization (k=8)"
)


## ============== ADJUSTED RAND INDEX ==============
library(mclust)

rand_index <- adjustedRandIndex(
  data_airbnb_sample_clustered$Cluster_PCA,
  data_airbnb_sample_clustered$Cluster_MDS
)


## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------- GUARDAR RESULTADOS ------------------------------------------
## ----------------------------------------------------------------------------------------------------------
save.image("./BBDD/resultados_airbnb.RData")
