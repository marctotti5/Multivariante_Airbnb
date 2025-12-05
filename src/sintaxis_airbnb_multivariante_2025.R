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
## -------------------------------------------------- MDS ---------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## MDS classic

data_airbnb_sample <- data_airbnb_sample %>%
  select(-latitude, -longitude)

D_gower <- daisy(
  select(data_airbnb_sample, -id),
  metric = "gower"
)

## Initial try
D_matrix <- as.matrix(D_gower)
n <- nrow(D_matrix)
identity_matrix <- diag(rep(1, n))
H_matrix <- identity_matrix - (1 / n) * matrix(1, nrow = n, ncol = n)
G_matrix <- -(1 / 2) * (H_matrix %*% D_matrix^2 %*% H_matrix)

eigenvalues_G <- eigen(G_matrix, only.values = TRUE)$values

if (any(eigenvalues_G < 0)) {
  print("Gram Matrix is Negative definite")
}

constant_eigen_correction <- 2 * abs(min(eigenvalues_G)) + 1e-6
D_squared_matrix_modified <- (D_matrix^2 + constant_eigen_correction)

## Modification of distance matrix
for (i in 1:nrow(D_squared_matrix_modified)) {
  for (j in 1:ncol(D_squared_matrix_modified)) {
    if (i == j) {
      D_squared_matrix_modified[i, j] = 0
    }
  }
}

D_matrix_modified <- sqrt(D_squared_matrix_modified)
G_matrix_modified <- -(1 / 2) *
  (H_matrix %*% D_squared_matrix_modified %*% H_matrix)

## Comprobacion de G otra vez
eigenvalues_G_modified <- eigen(G_matrix_modified, only.values = TRUE)$values


## -------------------------------- MDS classic --------------------------------------
numero_eigenvalues <- 10
MDS_classic <- cmdscale(D_gower, k = numero_eigenvalues, add = TRUE, eig = TRUE)
#MDS_classic <- cmdscale(as.dist(D_matrix_modified), k = numero_eigenvalues, eig = TRUE)
MDS_classic_data <- MDS_classic$points %>% as_data_frame()


# Scree plot de autovalores del MDS
eigenvalues_mds <- data.frame(
  Dimension = factor(1:numero_eigenvalues),
  Eigenvalue = MDS_classic$eig[1:numero_eigenvalues],
  Variance_Explained = (MDS_classic$eig[1:numero_eigenvalues] /
    sum(MDS_classic$eig)) *
    100,
  Cumulative_Variance = cumsum(
    (MDS_classic$eig[1:numero_eigenvalues] / sum(MDS_classic$eig)) * 100
  )
)

# Scree plot de autovalores
screeplot_mds_eigenvalues <- ggplot(
  eigenvalues_mds,
  aes(x = factor(Dimension), y = Eigenvalue)
) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_line(aes(group = 1), color = "red", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  geom_text(
    aes(label = round(Eigenvalue, 2)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "MDS Scree Plot - Eigenvalues",
    x = "Dimension",
    y = "Eigenvalue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Scree plot de varianza explicada
screeplot_mds_variance <- ggplot(
  eigenvalues_mds,
  aes(x = Dimension, y = Variance_Explained)
) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.8) +
  geom_line(aes(group = 1), color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_text(
    aes(label = paste0(round(Variance_Explained, 1), "%")),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "MDS Scree Plot - Variance Explained",
    x = "Dimension",
    y = "% Variance Explained"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Correlations with original variables
numeric_vars <- data_airbnb_sample %>%
  select(where(is.numeric)) %>%
  colnames()

cor_matrix_mds <- cor(
  data_airbnb_sample[, numeric_vars],
  MDS_classic_data[, 1:3] # Primeras 3 dimensiones
)
colnames(cor_matrix_mds) <- paste0("MDS Dim ", 1:3)
rownames(cor_matrix_mds) <- unlist(var_label(data_airbnb_sample)[numeric_vars])

heatmap_correlations_MDS_originalvariables <- pheatmap(
  cor_matrix_mds,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlations between original variables and MDS dimensions (1-3)",
  fontsize_row = 8,
  fontsize_number = 8,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.5, 0, 0.5, 1),
  legend_labels = c("-1", "-0.5", "0", "0.5", "1")
)

## TODO: Comparar/correlar PC's con variables categoricas

# Scree plot de varianza acumulada
screeplot_mds_cumulative <- ggplot(
  eigenvalues_mds,
  aes(x = Dimension, y = Cumulative_Variance)
) +
  geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.8) +
  geom_line(aes(group = 1), color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(
    aes(label = paste0(round(Cumulative_Variance, 1), "%")),
    vjust = -0.5,
    size = 3
  ) +
  geom_hline(
    yintercept = 80,
    linetype = "dashed",
    color = "red",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = 9,
    y = 80,
    label = "80% threshold",
    vjust = -0.5,
    color = "red",
    size = 3
  ) +
  labs(
    title = "MDS Scree Plot - Cumulative Variance",
    x = "Dimension",
    y = "Cumulative % Variance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

## Diagrama de dispersión MDS - 2 primeras dimensiones
mds_plot_dim1_dim2 <- ggplot(
  MDS_classic_data,
  aes(x = V1, y = V2)
) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  labs(
    title = "MDS - First Two Dimensions",
    subtitle = paste0(
      "Variance explained: ",
      round(eigenvalues_mds$Variance_Explained[1], 1),
      "% (Dim1) + ",
      round(eigenvalues_mds$Variance_Explained[2], 1),
      "% (Dim2) = ",
      round(sum(eigenvalues_mds$Variance_Explained[1:2]), 1),
      "%"
    ),
    x = paste0(
      "Dimension 1 (",
      round(eigenvalues_mds$Variance_Explained[1], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      round(eigenvalues_mds$Variance_Explained[2], 1),
      "%)"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  ) +
  coord_fixed()

# Con colores por grupo (ejemplo: room_type)
mds_plot_roomtype <- ggplot(
  MDS_classic_data %>% mutate(room_type = data_airbnb_sample$room_type),
  aes(x = V1, y = V2, color = room_type)
) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "MDS - Colored by Room Type",
    subtitle = paste0(
      "Total variance: ",
      round(sum(eigenvalues_mds$Variance_Explained[1:2]), 1),
      "%"
    ),
    x = paste0(
      "Dimension 1 (",
      round(eigenvalues_mds$Variance_Explained[1], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      round(eigenvalues_mds$Variance_Explained[2], 1),
      "%)"
    ),
    color = "Room Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  ) +
  coord_fixed()

# Con colores por distrito
mds_plot_neighbourhood <- ggplot(
  MDS_classic_data %>%
    mutate(
      neighbourhood = data_airbnb_sample$neighbourhood_group_cleansed
    ),
  aes(x = V1, y = V2, color = neighbourhood)
) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "MDS - Colored by District",
    subtitle = paste0(
      "Total variance: ",
      round(sum(eigenvalues_mds$Variance_Explained[1:2]), 1),
      "%"
    ),
    x = paste0(
      "Dimension 1 (",
      round(eigenvalues_mds$Variance_Explained[1], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      round(eigenvalues_mds$Variance_Explained[2], 1),
      "%)"
    ),
    color = "District"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "right"
  ) +
  coord_fixed()


## TODO: -------------------------------- MDS modificado --------------------------------------

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- CLUSTERING --------------------------------------------
## ----------------------------------------------------------------------------------------------------------

PCA_coordinates <- pca_object$ind$coord[, 1:3] %>%
  as_data_frame() %>%
  rename(
    "PC1" = "Dim.1",
    "PC2" = "Dim.2",
    "PC3" = "Dim.3",
  )
PCA_variables <- colnames(PCA_coordinates)

data_clustering_PCA <- PCA_coordinates


library(biotools)
library(fpc)
mahalanobis_dist <- D2.dist(data_clustering_PCA, var(data_clustering_PCA)) %>%
  sqrt()

compare_clustering_methods <- function(
  data_original = data_airbnb_sample, # Dataset original (con variables mixtas)
  data_pca = data_clustering_PCA, # Coordenadas PCA (ya calculadas)
  data_mds, # Coordenadas MDS (ya calculadas)
  k_values = 2:6, # Valores de k a probar
  B = 1000, # Iteraciones bootstrap
  seed = 1234,
  compute_bootstrap = TRUE
) {
  set.seed(seed)
  library(biotools)
  library(fpc)

  # Inicializar lista de resultados
  resultados <- list()
  resumen_tabla <- list()

  # ============== DISTANCIAS ==============
  cat("Calculando matrices de distancias...\n")

  # 1. Distancia de Gower (variables originales - mixtas)
  cat("  Calculando distancia de Gower...\n")
  D_gower <- daisy(data_original, metric = "gower")

  # 2. Distancia Euclidiana (PCA)
  cat("  Calculando distancia Euclidiana (PCA)...\n")
  D_euclidean_pca <- dist(data_pca, method = "euclidean")

  # 3. Distancia Euclidiana (MDS)
  cat("  Calculando distancia Euclidiana (MDS)...\n")
  D_euclidean_mds <- dist(data_mds, method = "euclidean")

  # 4. Distancia de Mahalanobis (PCA)
  cat("  Calculando distancia de Mahalanobis (PCA)...\n")
  D_mahalanobis_pca <- D2.dist(
    data_pca,
    cov = var(data_pca)
  ) %>%
    sqrt()

  # 5. Distancia de Mahalanobis (MDS)
  cat("  Calculando distancia de Mahalanobis (MDS)...\n")
  D_mahalanobis_mds <- D2.dist(
    data_mds,
    cov = var(data_mds)
  ) %>%
    sqrt()

  # Lista de configuraciones a probar
  configuraciones <- list(
    # Original + Gower solo con PAM (K-means no funciona con Gower)
    list(
      name = "Original + Gower",
      dist = D_gower,
      type = "gower",
      data = NULL,
      methods = c("PAM")
    ),
    # PCA con ambas distancias y ambos métodos
    list(
      name = "PCA + Euclidean",
      dist = D_euclidean_pca,
      type = "euclidean",
      data = data_pca,
      methods = c("PAM", "K-means")
    ),
    list(
      name = "PCA + Mahalanobis",
      dist = D_mahalanobis_pca,
      type = "mahalanobis",
      data = data_pca,
      methods = c("PAM")
    ),
    # MDS con ambas distancias y ambos métodos
    list(
      name = "MDS + Euclidean",
      dist = D_euclidean_mds,
      type = "euclidean",
      data = data_mds,
      methods = c("PAM", "K-means")
    ),
    list(
      name = "MDS + Mahalanobis",
      dist = D_mahalanobis_mds,
      type = "mahalanobis",
      data = data_mds,
      methods = c("PAM")
    )
  )

  # ============== CLUSTERING ==============
  for (config in configuraciones) {
    for (clust_method in config$methods) {
      for (k_val in k_values) {
        cat("\n===========================\n")
        cat(
          "Método:",
          config$name,
          "-",
          clust_method,
          "- k =",
          k_val,
          "\n"
        )

        # Aplicar clustering según el método
        if (clust_method == "PAM") {
          # PAM clustering
          model <- pam(config$dist, k = k_val)
          clustering_result <- model$clustering
        } else if (clust_method == "K-means") {
          # K-means clustering (requiere datos, no distancias)
          model <- kmeans(config$data, centers = k_val, nstart = 25)
          clustering_result <- model$cluster
        }

        # Silhouette
        silhouette_obj <- silhouette(clustering_result, config$dist)
        sil_mean <- mean(silhouette_obj[, 3])

        # Silhouette plot
        sil_plot <- fviz_silhouette(
          silhouette_obj,
          print.summary = FALSE
        ) +
          labs(title = paste(config$name, "-", clust_method, "- k =", k_val))

        # Calinski-Harabasz
        ch_index <- cluster.stats(
          config$dist,
          clustering_result
        )$ch

        # Bootstrap (opcional)
        if (compute_bootstrap) {
          cat("Ejecutando bootstrap...\n")

          if (clust_method == "PAM") {
            boot_result <- clusterboot(
              data = config$dist,
              B = B,
              clustermethod = pamkCBI,
              k = k_val,
              seed = seed
            )
          } else if (clust_method == "K-means") {
            boot_result <- clusterboot(
              data = config$data,
              B = B,
              clustermethod = kmeansCBI,
              krange = k_val,
              seed = seed
            )
          }

          jaccard <- boot_result$bootmean
          jaccard_mean <- mean(jaccard)
          jaccard_min <- min(jaccard)
          jaccard_max <- max(jaccard)
        } else {
          jaccard <- NA
          jaccard_mean <- NA
          jaccard_min <- NA
          jaccard_max <- NA
        }

        # Tamaños de clusters
        sizes <- table(clustering_result)
        sizes_str <- paste(sizes, collapse = "-")

        # Guardar resultados
        key <- paste0(
          gsub(" ", "_", gsub("\\+", "", tolower(config$name))),
          "_",
          tolower(clust_method),
          "_k",
          k_val
        )

        resultados[[key]] <- list(
          metodo = config$name,
          clustering_method = clust_method,
          k = k_val,
          distance_type = config$type,
          cluster = clustering_result,
          silhouette = sil_mean,
          ch_index = ch_index,
          jaccard_mean = jaccard_mean,
          jaccard_clusters = jaccard,
          jaccard_min = jaccard_min,
          jaccard_max = jaccard_max,
          cluster_sizes = sizes,
          silhouette_plot = sil_plot,
          model = model
        )

        # Agregar a tabla resumen
        resumen_tabla[[length(resumen_tabla) + 1]] <- tibble(
          metodo = config$name,
          clustering_method = clust_method,
          distance = config$type,
          k = k_val,
          silhouette = sil_mean,
          ch_index = ch_index,
          jaccard_mean = jaccard_mean,
          jaccard_min = jaccard_min,
          jaccard_max = jaccard_max,
          cluster_sizes = sizes_str
        )
      }
    }
  }

  # Convertir resumen a dataframe
  resumen_df <- bind_rows(resumen_tabla) %>%
    arrange(desc(silhouette))

  # Crear gráficos comparativos
  plot_silhouette <- ggplot(
    resumen_df,
    aes(
      x = factor(k),
      y = silhouette,
      color = paste(metodo, clustering_method),
      group = paste(metodo, clustering_method)
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      title = "Silhouette Coefficient by Method and k",
      x = "Number of Clusters (k)",
      y = "Average Silhouette",
      color = "Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )

  plot_ch <- ggplot(
    resumen_df,
    aes(
      x = factor(k),
      y = ch_index,
      color = paste(metodo, clustering_method),
      group = paste(metodo, clustering_method)
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      title = "Calinski-Harabasz Index by Method and k",
      x = "Number of Clusters (k)",
      y = "CH Index",
      color = "Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )

  if (compute_bootstrap) {
    plot_jaccard <- ggplot(
      resumen_df %>% filter(!is.na(jaccard_mean)),
      aes(
        x = factor(k),
        y = jaccard_mean,
        color = paste(metodo, clustering_method),
        group = paste(metodo, clustering_method)
      )
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_errorbar(
        aes(ymin = jaccard_min, ymax = jaccard_max),
        width = 0.2,
        alpha = 0.5
      ) +
      labs(
        title = "Bootstrap Stability (Jaccard) by Method and k",
        x = "Number of Clusters (k)",
        y = "Mean Jaccard Coefficient",
        color = "Method"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      )
  } else {
    plot_jaccard <- NULL
  }

  # Retornar resultados
  return(list(
    resultados = resultados,
    resumen = resumen_df,
    plot_silhouette = plot_silhouette,
    plot_ch = plot_ch,
    plot_jaccard = plot_jaccard,
    distances = list(
      gower = D_gower,
      euclidean_pca = D_euclidean_pca,
      euclidean_mds = D_euclidean_mds,
      mahalanobis_pca = D_mahalanobis_pca,
      mahalanobis_mds = D_mahalanobis_mds
    )
  ))
}

clustering_comparison <- compare_clustering_methods(
  data_original = data_airbnb_sample %>% dplyr::select(-id),
  data_pca = PCA_coordinates,
  data_mds = MDS_classic_data[, 1:3],
  k_values = 2:10,
  B = 100,
  seed = 1234,
  compute_bootstrap = TRUE # Cambiar a TRUE para bootstrap completo
)

# Ver gráficos comparativos
clustering_comparison$plot_silhouette
clustering_comparison$plot_ch

# Mejor configuración por Silhouette
best_silhouette <- clustering_comparison$resumen %>%
  arrange(desc(silhouette)) %>%
  head(5)

cat("\n=== Top 5 configuraciones por Silhouette ===\n")
print(best_silhouette)

# Mejor configuración por CH Index
best_ch <- clustering_comparison$resumen %>%
  arrange(desc(ch_index))


##
compare_clustering_methods <- function(
  data_original = data_airbnb_sample, # Dataset original (con variables mixtas)
  data_pca = data_clustering_PCA, # Coordenadas PCA (ya calculadas)
  data_mds, # Coordenadas MDS (ya calculadas)
  k_values = 2:6, # Valores de k a probar
  B = 1000, # Iteraciones bootstrap
  seed = 1234,
  compute_bootstrap = TRUE,
  save_plots = FALSE, # Nuevo parámetro para guardar plots
  plots_dir = "./plots" # Directorio donde guardar los plots
) {
  set.seed(seed)
  library(biotools)
  library(fpc)

  # Crear directorio para plots si se solicita guardarlos
  if (save_plots && !dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
    cat("Directorio creado:", plots_dir, "\n")
  }

  # Inicializar lista de resultados
  resultados <- list()
  resumen_tabla <- list()

  # ============== DISTANCIAS ==============
  cat("Calculando matrices de distancias...\n")

  # 1. Distancia de Gower (variables originales - mixtas)
  cat("  Calculando distancia de Gower...\n")
  D_gower <- daisy(data_original, metric = "gower")

  # 2. Distancia Euclidiana (PCA)
  cat("  Calculando distancia Euclidiana (PCA)...\n")
  D_euclidean_pca <- dist(data_pca, method = "euclidean")

  # 3. Distancia Euclidiana (MDS)
  cat("  Calculando distancia Euclidiana (MDS)...\n")
  D_euclidean_mds <- dist(data_mds, method = "euclidean")

  # 4. Distancia de Mahalanobis (PCA)
  cat("  Calculando distancia de Mahalanobis (PCA)...\n")
  D_mahalanobis_pca <- D2.dist(
    data_pca,
    cov = var(data_pca)
  ) %>%
    sqrt()

  # 5. Distancia de Mahalanobis (MDS)
  cat("  Calculando distancia de Mahalanobis (MDS)...\n")
  D_mahalanobis_mds <- D2.dist(
    data_mds,
    cov = var(data_mds)
  ) %>%
    sqrt()

  # Lista de configuraciones a probar
  configuraciones <- list(
    # Original + Gower solo con PAM (K-means no funciona con Gower)
    list(
      name = "Original + Gower",
      dist = D_gower,
      type = "gower",
      data = NULL,
      methods = c("PAM")
    ),
    # PCA con ambas distancias y ambos métodos
    list(
      name = "PCA + Euclidean",
      dist = D_euclidean_pca,
      type = "euclidean",
      data = data_pca,
      methods = c("PAM", "K-means")
    ),
    list(
      name = "PCA + Mahalanobis",
      dist = D_mahalanobis_pca,
      type = "mahalanobis",
      data = data_pca,
      methods = c("PAM")
    ),
    # MDS con ambas distancias y ambos métodos
    list(
      name = "MDS + Euclidean",
      dist = D_euclidean_mds,
      type = "euclidean",
      data = data_mds,
      methods = c("PAM", "K-means")
    ),
    list(
      name = "MDS + Mahalanobis",
      dist = D_mahalanobis_mds,
      type = "mahalanobis",
      data = data_mds,
      methods = c("PAM")
    )
  )

  # ============== CLUSTERING ==============
  for (config in configuraciones) {
    for (clust_method in config$methods) {
      for (k_val in k_values) {
        cat("\n===========================\n")
        cat(
          "Método:",
          config$name,
          "-",
          clust_method,
          "- k =",
          k_val,
          "\n"
        )

        # Aplicar clustering según el método
        if (clust_method == "PAM") {
          # PAM clustering
          model <- pam(config$dist, k = k_val)
          clustering_result <- model$clustering
        } else if (clust_method == "K-means") {
          # K-means clustering (requiere datos, no distancias)
          model <- kmeans(config$data, centers = k_val, nstart = 25)
          clustering_result <- model$cluster
        }

        # Silhouette
        silhouette_obj <- silhouette(clustering_result, config$dist)
        sil_mean <- mean(silhouette_obj[, 3])

        # Silhouette plot
        sil_plot <- fviz_silhouette(
          silhouette_obj,
          print.summary = FALSE
        ) +
          labs(
            title = paste(config$name, "-", clust_method, "- k =", k_val),
            subtitle = paste0(
              "Average Silhouette Width: ",
              round(sil_mean, 3)
            )
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)
          )

        # Calinski-Harabasz
        ch_index <- cluster.stats(
          config$dist,
          clustering_result
        )$ch

        # Bootstrap (opcional)
        if (compute_bootstrap) {
          cat("Ejecutando bootstrap...\n")

          if (clust_method == "PAM") {
            boot_result <- clusterboot(
              data = config$dist,
              B = B,
              clustermethod = pamkCBI,
              k = k_val,
              seed = seed
            )
          } else if (clust_method == "K-means") {
            boot_result <- clusterboot(
              data = config$data,
              B = B,
              clustermethod = kmeansCBI,
              krange = k_val,
              seed = seed
            )
          }

          jaccard <- boot_result$bootmean
          jaccard_mean <- mean(jaccard)
          jaccard_min <- min(jaccard)
          jaccard_max <- max(jaccard)
        } else {
          jaccard <- NA
          jaccard_mean <- NA
          jaccard_min <- NA
          jaccard_max <- NA
        }

        # Tamaños de clusters
        sizes <- table(clustering_result)
        sizes_str <- paste(sizes, collapse = "-")

        # Crear key para guardar
        key <- paste0(
          gsub(" ", "_", gsub("\\+", "", tolower(config$name))),
          "_",
          tolower(clust_method),
          "_k",
          k_val
        )

        # Guardar plot si se solicita
        if (save_plots) {
          plot_filename <- file.path(
            plots_dir,
            paste0("silhouette_", key, ".png")
          )
          ggsave(
            filename = plot_filename,
            plot = sil_plot,
            width = 10,
            height = 6,
            dpi = 300
          )
          cat("  Plot guardado:", plot_filename, "\n")
        }

        # Guardar resultados
        resultados[[key]] <- list(
          metodo = config$name,
          clustering_method = clust_method,
          k = k_val,
          distance_type = config$type,
          cluster = clustering_result,
          silhouette = sil_mean,
          ch_index = ch_index,
          jaccard_mean = jaccard_mean,
          jaccard_clusters = jaccard,
          jaccard_min = jaccard_min,
          jaccard_max = jaccard_max,
          cluster_sizes = sizes,
          silhouette_plot = sil_plot,
          model = model
        )

        # Agregar a tabla resumen
        resumen_tabla[[length(resumen_tabla) + 1]] <- tibble(
          metodo = config$name,
          clustering_method = clust_method,
          distance = config$type,
          k = k_val,
          silhouette = sil_mean,
          ch_index = ch_index,
          jaccard_mean = jaccard_mean,
          jaccard_min = jaccard_min,
          jaccard_max = jaccard_max,
          cluster_sizes = sizes_str
        )
      }
    }
  }

  # Convertir resumen a dataframe
  resumen_df <- bind_rows(resumen_tabla) %>%
    arrange(desc(silhouette))

  # Crear gráficos comparativos
  plot_silhouette <- ggplot(
    resumen_df,
    aes(
      x = factor(k),
      y = silhouette,
      color = paste(metodo, clustering_method),
      group = paste(metodo, clustering_method)
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      title = "Silhouette Coefficient by Method and k",
      x = "Number of Clusters (k)",
      y = "Average Silhouette",
      color = "Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )

  plot_ch <- ggplot(
    resumen_df,
    aes(
      x = factor(k),
      y = ch_index,
      color = paste(metodo, clustering_method),
      group = paste(metodo, clustering_method)
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      title = "Calinski-Harabasz Index by Method and k",
      x = "Number of Clusters (k)",
      y = "CH Index",
      color = "Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )

  if (compute_bootstrap) {
    plot_jaccard <- ggplot(
      resumen_df %>% filter(!is.na(jaccard_mean)),
      aes(
        x = factor(k),
        y = jaccard_mean,
        color = paste(metodo, clustering_method),
        group = paste(metodo, clustering_method)
      )
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_errorbar(
        aes(ymin = jaccard_min, ymax = jaccard_max),
        width = 0.2,
        alpha = 0.5
      ) +
      labs(
        title = "Bootstrap Stability (Jaccard) by Method and k",
        x = "Number of Clusters (k)",
        y = "Mean Jaccard Coefficient",
        color = "Method"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      )
  } else {
    plot_jaccard <- NULL
  }

  # Función helper para extraer plots
  get_silhouette_plot <- function(metodo, clustering_method, k) {
    key <- paste0(
      gsub(" ", "_", gsub("\\+", "", tolower(metodo))),
      "_",
      tolower(clustering_method),
      "_k",
      k
    )
    if (key %in% names(resultados)) {
      return(resultados[[key]]$silhouette_plot)
    } else {
      warning("Configuración no encontrada: ", key)
      return(NULL)
    }
  }

  # Mensaje final
  cat("\n\n========================================\n")
  cat("COMPARACIÓN COMPLETADA\n")
  cat("========================================\n")
  cat("Total configuraciones evaluadas:", length(resultados), "\n")
  if (save_plots) {
    cat("Plots guardados en:", plots_dir, "\n")
  }
  cat(
    "Acceso a plots individuales: clustering_comparison$resultados$<config>$silhouette_plot\n"
  )
  cat("O usa: get_silhouette_plot(metodo, clustering_method, k)\n")

  # Retornar resultados
  return(list(
    resultados = resultados,
    resumen = resumen_df,
    plot_silhouette = plot_silhouette,
    plot_ch = plot_ch,
    plot_jaccard = plot_jaccard,
    distances = list(
      gower = D_gower,
      euclidean_pca = D_euclidean_pca,
      euclidean_mds = D_euclidean_mds,
      mahalanobis_pca = D_mahalanobis_pca,
      mahalanobis_mds = D_mahalanobis_mds
    ),
    get_plot = get_silhouette_plot # Función helper incluida
  ))
}

# ============== EJECUTAR COMPARACIÓN ==============
cat("\n\n========== INICIANDO COMPARACIÓN DE CLUSTERING ==========\n\n")

clustering_comparison <- compare_clustering_methods(
  data_original = data_airbnb_sample %>% dplyr::select(-id),
  data_pca = PCA_coordinates,
  data_mds = MDS_classic_data[, 1:3],
  k_values = 2:20,
  B = 1000,
  seed = 1234,
  compute_bootstrap = FALSE,
  save_plots = TRUE, # Guardar todos los plots automáticamente
  plots_dir = "./plots/silhouette_comparison"
)

# Ver resumen
clustering_comparison$resumen

# Ver gráficos comparativos
clustering_comparison$plot_silhouette
clustering_comparison$plot_ch
clustering_comparison$plot_jaccard

# Mejor configuración por Silhouette
best_silhouette <- clustering_comparison$resumen %>%
  arrange(desc(silhouette)) %>%
  head(5)

cat("\n=== Top 5 configuraciones por Silhouette ===\n")
print(best_silhouette)

# Extraer plot del ganador usando la función helper
best_plot <- clustering_comparison$get_plot(
  metodo = best_silhouette$metodo[1],
  clustering_method = best_silhouette$clustering_method[1],
  k = best_silhouette$k[1]
)
best_plot

# O acceso directo
names(clustering_comparison$resultados) # Ver todos los nombres disponibles


## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------- GUARDAR RESULTADOS ------------------------------------------
## ----------------------------------------------------------------------------------------------------------
save.image("./BBDD/resultados_airbnb.RData")
