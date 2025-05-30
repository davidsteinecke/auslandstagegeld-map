# ========================================================
# Auslandstagegeld Interactive Map Visualization (ARVVwV 2025)
# Author: [Your Name]
# Description: Visualizing official German government per diem 
# and accommodation allowances for international travel.
# ========================================================

# ---------------------------
# 1. Install and Load Packages
# ---------------------------
install.packages(c(
  "leaflet", "dplyr", "readr", "tidyr",
  "rnaturalearth", "rnaturalearthdata",
  "sf", "htmltools", "htmlwidgets", "stringr",
  "pdftools", "ggplot2", "ggrepel" 
))

library(leaflet)
library(dplyr)
library(readr)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(htmltools)
library(stringr)
library(htmlwidgets)
library(ggplot2)
library(ggrepel)

# ---------------------------
# 2. Load and Prepare Data
# ---------------------------

table_data <- pdftools::pdf_text("C:/Users/David/Downloads/arvvwv_2025_pdf.pdf") %>% 
  tibble(.name_repair = ~ c("text")) %>% 
  mutate(page = row_number(),
         text = text %>% str_split("\\n")) %>% 
  unnest(cols = c(text)) %>% 
  mutate(line = row_number()) %>% 
  filter(line > 58,
         line < 362,
         !str_detect(text, regex("Auslands|Ort|Euro")),
         nchar(text) > 0) %>% 
  separate_wider_delim(text, regex("\\s{6,}"), 
                       names = c("land_ort", "auslandstagegeld", "auslandsuebernachtungsgeld", "trash"), 
                       too_few = "align_start") %>% 
  filter(nchar(land_ort) > 0) %>% 
  select(land_ort:auslandsuebernachtungsgeld) %>% 
  separate_wider_delim(land_ort, regex("\\s{2,}"), names = c("land", "ort"), too_few = "align_start") %>% 
  mutate(land = ifelse(nchar(land) == 0, NA_character_, land),
         land = case_when(land == "Philippinen (3)" ~ "Philippinen",
                          land == "Trinidad und Tobago (4)" ~ "Trinidad und Tobago",
                          TRUE ~ land),
         ort = case_when(is.na(ort) |
                           ort == "im Übrigen" ~ "generell",
                         ort == "Rom (2)" ~ "Rom",
                         ort == "Paris sowie die Départements der Île de France (1)" ~ "Paris",
                         TRUE ~ ort),
         auslandsuebernachtungsgeld = auslandsuebernachtungsgeld %>% as.numeric(),
         auslandstagegeld = auslandstagegeld %>% as.numeric()) %>% 
  fill(land, .direction = "down") %>% 
  drop_na()


footnote_data <- table_data %>% 
  filter(land %in% c("Philippinen", "Trinidad und Tobago") |
         ort == "Rom") %>% 
  slice(1:3, rep(3, 6)) %>% 
  mutate(ort = "generell",
         land = c("Vatikanstadt", "Mikronesien", "Antigua und Barbuda", 
         "Dominica", "Grenada", "Guyana", "St. Kitts und Nevis", "St. Lucia", 
         "St. Vincent und Grenadinen"))


world <- ne_countries(scale = "medium", returnclass = "sf")
cities <- ne_download(type = "populated_places", scale = "medium", returnclass = "sf")
subunits <- ne_download(type = "map_subunits", scale = "medium", returnclass = "sf")


clean_data <- bind_rows(table_data, footnote_data) %>% 
  mutate(type = ifelse(ort == "generell", "Land", "Stadt"))


country_data <- clean_data %>% 
  filter(ort == "generell") %>%
  mutate(land_merge = case_when(land == "Botsuana" ~ "Botswana",
                                land == "China" ~ "Volksrepublik China",
                                land == "Côte d'Ivoire" ~ "Elfenbeinküste",
                                land == "Kongo, Demokratische Republik" ~ "Demokratische Republik Kongo",
                                land == "Kongo, Republik" ~ "Republik Kongo",
                                land == "Korea, Demokratische Volksrepublik" ~ "Nordkorea",
                                land == "Korea, Republik" ~ "Südkorea",
                                land == "Marshall Inseln" ~ "Marshallinseln",
                                land == "Moldau, Republik" ~ "Republik Moldau",
                                land == "Russische Föderation" ~ "Russland",
                                land == "Sao Tomé und Principe" ~ "São Tomé und Príncipe",
                                land == "Saudi Arabien" ~ "Saudi-Arabien",
                                land == "Slowakische Republik" ~ "Slowakei",
                                land == "Taiwan" ~ "Republik China",
                                land == "Tschechische Republik" ~ "Tschechien",
                                land == "Vereinigte Staaten von Amerika (USA)" ~ "Vereinigte Staaten",
                                land == "Vereinigtes Königreich von Großbritannien und Nordirland" ~ "Vereinigtes Königreich",
                                land == "Zypern" ~ "Republik Zypern",
                                land == "Mikronesien" ~ "Föderierte Staaten von Mikronesien",
                                land == "St. Vincent und Grenadinen" ~ "St. Vincent und die Grenadinen",
                                TRUE ~ land))


city_data <- clean_data %>% 
  filter(ort != "generell") %>%
  mutate(city_merge = case_when(ort == "Brasilia" ~ "Brasília",
                                ort == "Sao Paulo" ~ "São Paulo",
                                ort == "Kanton" ~ "Guangzhou",
                                ort == "Bangalore" ~ "Bengaluru",
                                ort == "Neu Delhi" ~ "Delhi",
                                ort == "Osaka" ~ "Ōsaka",
                                #ort == "Breslau" ~ "",
                                ort == "St. Petersburg" ~ "Sankt Petersburg",
                                ort == "Djidda" ~ "Dschidda",
                                ort == "Washington, D. C." ~ "Washington",
                                TRUE ~ ort))


subunit_data <- clean_data %>% 
  filter(ort == "Kanarische Inseln"|
           ort == "Palma de Mallorca") %>%
  mutate(subunit_merge = case_when(ort == "Kanarische Inseln" ~ "Kanarische Inseln",
                                ort == "Palma de Mallorca" ~ "Balearische Inseln",
                                TRUE ~ ort))



# ---------------------------
# 3. Merge and Prepare Map Data
# ---------------------------
world_data <- world %>% left_join(country_data, by = c("name_de" = "land_merge"))
cities_data <- cities %>% left_join(city_data, by = c("NAME_DE" = "city_merge")) %>% 
  filter(!is.na(ort))
subunits_data <- subunits %>% left_join(subunit_data, by = c("NAME_DE" = "subunit_merge")) %>% 
  filter(!is.na(ort))

# Define color bins for the choropleth map
bins <- c(0, 20, 30, 40, 50, 60, 70, 100)
pal_bin <- colorBin(
  palette = "YlOrRd",
  domain  = world_data$auslandstagegeld,
  bins    = bins,
  pretty  = FALSE
)



# ---------------------------
# 6. Build and Save Interactive Map
# ---------------------------
map_object <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data        = world_data,
    fillColor   = ~pal_bin(auslandstagegeld),
    weight      = 1,
    color       = "white",
    fillOpacity = 0.7,
    label       = lapply(
      paste0(
        "<strong>", world_data$name_de, "</strong><br/>",
        "Tagegeld: €", world_data$auslandstagegeld, "<br/>",
        "Übernachtungsgeld: €", world_data$auslandsuebernachtungsgeld
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(color = "black", weight = 2)
  ) %>%
  addPolygons(
    data        = subunits_data,
    fillColor   = ~pal_bin(auslandstagegeld),
    weight      = 1,
    color       = "white",
    fillOpacity = 0.7,
    label       = lapply(
      paste0(
        "<strong>", subunits_data$NAME_DE, "</strong><br/>",
        "Tagegeld: €", subunits_data$auslandstagegeld, "<br/>",
        "Übernachtungsgeld: €", subunits_data$auslandsuebernachtungsgeld
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(color = "black", weight = 2)
  ) %>%
  addCircleMarkers(
    data      = cities_data,
    radius    = 6,
    fillColor = "blue",
    fillOpacity = 0.8,
    stroke    = FALSE,
    label     = lapply(
      paste0(
        "<strong>", cities_data$NAME_DE, "</strong><br/>",
        "Tagegeld: €", cities_data$auslandstagegeld, "<br/>",
        "Übernachtungsgeld: €", cities_data$auslandsuebernachtungsgeld
      ),
      htmltools::HTML
    )
  )  %>%
  addLegend(
    pal       = pal_bin,
    values    = world_data$auslandstagegeld,
    title     = "Auslandstagegeld (€)",
    opacity   = 0.7,
    labFormat = labelFormat(prefix = "€")
  )




# Export as self-contained HTML for GitHub pages hosting
saveWidget(map_object, "index.html", selfcontained = TRUE)




# ================================
# Auslandstagegeld Visualization/Plot Generation:
## Scatter and Faceted Plots for Countries With +1 Cities/Städte
# ================================

# ========================
# 1. Plot 1: Overall Scatter Plot
# ========================
p1 <- ggplot(clean_data, aes(
  x = auslandstagegeld,
  y = auslandsuebernachtungsgeld,
  colour = type,
  shape  = type
)) +
  geom_point(size = 3, alpha = 0.8) +
  # Add the non-overlapping city labels
  geom_text_repel(
    data = clean_data %>% filter(type == "Stadt"),
    aes(label = ort), 
    size = 3.5, 
    color = "dodgerblue4",
    max.overlaps = 55,
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  scale_colour_manual(values = c(Land = "brown4", Stadt = "dodgerblue4")) +
  scale_shape_manual(values = c(Land = 19, Stadt = 17)) +
  labs(
    x = "Tagesgeld (€)",
    y = "Übernachtungsgeld (€)",
    colour = "",
    shape  = "",
    title = "Tages- vs. Übernachtungsgeld\nLänder (Braun/Kreis) vs. Städte (Blau/Dreieck)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p1
ggsave("p1.jpeg", width = 28, height = 20, units = "cm", dpi = 800)

# ========================
# 2. Plot 2: Faceted Scatter Plot with Country-Specific Colors
# ========================

p2 <- ggplot(
  clean_data %>% add_count(land) %>% filter(n > 1) %>% 
    mutate(land = case_when(land == "Vereinigte Staaten von Amerika (USA)" ~ "Vereinigte Staaten",
                            land == "Vereinigtes Königreich von Großbritannien und Nordirland" ~ "Vereinigtes Königreich",
                            TRUE ~ land)),
  aes(
    x = auslandstagegeld,
    y = auslandsuebernachtungsgeld,
    colour = land,
    shape  = type
  )
) +
  geom_point(size = 2) +
  
  # Add city labels for cities/Städte only
  geom_text_repel(
    data = . %>% filter(type == "Stadt"),
    aes(label = ort),  
    size = 2.5,
    box.padding = 0.15,
    point.padding = 0.15,
    segment.size = 0.2,
    segment.alpha = 0.5,
    max.overlaps = 50,
    min.segment.length = 0,
    force = 0.3,
    force_pull = 0.5
  ) +
  
  scale_shape_manual(values = c(Land = 19, Stadt = 17)) +
  
  # Standardized axis limits across facets
  facet_wrap(~ land, scales = "fixed", ncol = 4) +
  
  labs(
    x = "Tagesgeld (€)",
    y = "Übernachtungsgeld (€)",
    title = "Tages- vs. Übernachtungsgeld pro Land und seinen Städten"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold")
  )

p2
ggsave("p2.jpeg", width = 28, height = 20, units = "cm", dpi = 800)
