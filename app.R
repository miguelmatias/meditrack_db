# MEDITRACK Mediterranean lake archive — v1.
# Read-only: no downloadHandler, no table export, Plotly mode bar off.
# Time series live in data/series (not www/). Overlay PNGs are served at runtime.

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(jsonlite)
library(htmltools)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.atomic(a) && is.na(a[[1]]))) b else a
}

DATA_DIR <- "data"
SITES_PATH <- file.path(DATA_DIR, "sites.geojson")
META_PATH <- file.path(DATA_DIR, "meta.json")
OVERLAY_JSON <- file.path(DATA_DIR, "overlays", "overlays.json")
SERIES_DIR <- file.path(DATA_DIR, "series")

RECORD_COLORS <- c(
  pollen_only = "#5d6d7e",
  aquatic = "#1ABC9C",
  paired = "#C0392B"
)
RECORD_LABELS <- c(
  pollen_only = "Pollen only",
  aquatic = "Aquatic proxies",
  paired = "Paired climate–ecology"
)
GAIN_COLORS <- c(
  gain = "#E74C3C",
  persist_med = "#E8C9A4",
  loss = "#1A5276",
  persist_non = "#D6DCE2"
)
GAIN_LABELS <- c(
  gain = "Became dry",
  persist_med = "Stayed dry",
  loss = "Became humid",
  persist_non = "Stayed humid"
)

read_json <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

load_sites <- function() {
  raw <- jsonlite::fromJSON(SITES_PATH, simplifyDataFrame = TRUE)
  df <- raw$features$properties
  coords <- raw$features$geometry$coordinates
  df$longitude <- vapply(coords, function(xy) as.numeric(xy[[1]]), numeric(1))
  df$latitude <- vapply(coords, function(xy) as.numeric(xy[[2]]), numeric(1))
  df$record_class <- ifelse(df$record_class %in% names(RECORD_COLORS), df$record_class, "pollen_only")
  df$peninsula <- ifelse(is.na(df$peninsula) | df$peninsula == "", "Other", df$peninsula)
  df$candidate_status <- ifelse(is.na(df$candidate_status), "", as.character(df$candidate_status))
  for (col in c("has_trace", "has_roc", "has_erb", "has_chironomid")) {
    if (!col %in% names(df)) df[[col]] <- FALSE
    df[[col]] <- df[[col]] %in% c(TRUE, "TRUE", "true", 1)
  }
  df
}

load_series <- function(name) {
  path <- file.path(SERIES_DIR, paste0(name, ".csv.gz"))
  if (!file.exists(path)) return(NULL)
  utils::read.csv(gzfile(path), stringsAsFactors = FALSE)
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == "" | x %in% c("NA", "null")] <- NA_character_
  x
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x[1])) return("—")
  format(round(as.numeric(x[1]), digits), nsmall = digits, trim = TRUE)
}

meta <- read_json(META_PATH, list(n_sites = NA, n_cores = NA, freeze_date = ""))
if (!is.null(meta$record_colors)) {
  RECORD_COLORS[names(meta$record_colors)] <- unlist(meta$record_colors)
}
if (!is.null(meta$record_labels)) {
  RECORD_LABELS[names(meta$record_labels)] <- unlist(meta$record_labels)
}
if (!is.null(meta$gain_colors)) {
  GAIN_COLORS[names(meta$gain_colors)] <- unlist(meta$gain_colors)
}
if (!is.null(meta$gain_labels)) {
  GAIN_LABELS[names(meta$gain_labels)] <- unlist(meta$gain_labels)
}

sites <- if (file.exists(SITES_PATH)) load_sites() else data.frame()
overlays <- if (file.exists(OVERLAY_JSON)) jsonlite::fromJSON(OVERLAY_JSON, simplifyDataFrame = TRUE) else data.frame()
if (nrow(overlays) && !"label" %in% names(overlays)) overlays$label <- overlays$id

series <- list(
  trace = load_series("trace"),
  roc = load_series("roc"),
  erb = load_series("erb"),
  chironomid = load_series("chironomid")
)

overlay_choices <- c("None" = "none")
if (nrow(overlays)) {
  labs <- overlays$label
  names(labs) <- NULL
  overlay_choices <- c(overlay_choices, setNames(overlays$id, labs))
}

peninsula_levels <- c("Iberia", "Italy", "Balkans", "Pyrenees", "Alps", "Other")
peninsula_present <- intersect(peninsula_levels, unique(sites$peninsula))
if (!length(peninsula_present)) peninsula_present <- sort(unique(sites$peninsula))

addResourcePath("overlays", file.path(DATA_DIR, "overlays"))

MED_WEST <- -10
MED_EAST <- 45
MED_SOUTH <- 27
MED_NORTH <- 48
if (nrow(overlays)) {
  hum <- if ("group" %in% names(overlays)) {
    overlays[overlays$group == "humidity", , drop = FALSE]
  } else {
    overlays
  }
  if (!nrow(hum)) hum <- overlays
  ov_west <- min(hum$west, na.rm = TRUE)
  ov_east <- max(hum$east, na.rm = TRUE)
  ov_south <- min(hum$south, na.rm = TRUE)
  ov_north <- max(hum$north, na.rm = TRUE)
} else {
  ov_west <- MED_WEST
  ov_east <- MED_EAST
  ov_south <- MED_SOUTH
  ov_north <- MED_NORTH
}

overlay_payload <- list()
if (nrow(overlays)) {
  overlay_payload <- lapply(seq_len(nrow(overlays)), function(i) {
    list(
      id = overlays$id[[i]],
      url = paste0("overlays/", overlays$file[[i]]),
      south = overlays$south[[i]],
      west = overlays$west[[i]],
      north = overlays$north[[i]],
      east = overlays$east[[i]]
    )
  })
}
map_payload <- list(
  overlays = overlay_payload,
  bounds = list(south = ov_south, west = ov_west, north = ov_north, east = ov_east)
)

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1c2833"
)

overlay_js <- HTML(paste(
  "window.mtOverlays = window.mtOverlays || {};",
  "Shiny.addCustomMessageHandler('mtSetOverlay', function(msg) {",
  "  function apply() {",
  "    var map = window.mtMap;",
  "    if (!map || !window.mtOverlays) { setTimeout(apply, 80); return; }",
  "    Object.keys(window.mtOverlays).forEach(function(k) {",
  "      if (map.hasLayer(window.mtOverlays[k])) map.removeLayer(window.mtOverlays[k]);",
  "    });",
  "    if (msg.id && msg.id !== 'none' && window.mtOverlays[msg.id]) {",
  "      window.mtOverlays[msg.id].setOpacity(msg.opacity);",
  "      window.mtOverlays[msg.id].addTo(map);",
  "    }",
  "  }",
  "  apply();",
  "});",
  sep = "\n"
))

legend_record <- tags$div(
  class = "legend-block",
  tags$div(class = "legend-title", "Sites"),
  lapply(names(RECORD_LABELS), function(k) {
    tags$div(
      class = "legend-row",
      tags$span(class = "swatch", style = paste0("background:", RECORD_COLORS[[k]], ";")),
      RECORD_LABELS[[k]]
    )
  }),
  tags$div(
    class = "legend-row",
    tags$span(class = "swatch ring gold"),
    "MEDITRACK candidate"
  )
)

legend_gain <- tags$div(
  class = "legend-block",
  tags$div(class = "legend-title", "I = 30 classes"),
  lapply(names(GAIN_LABELS), function(k) {
    tags$div(
      class = "legend-row",
      tags$span(class = "swatch", style = paste0("background:", GAIN_COLORS[[k]], ";")),
      GAIN_LABELS[[k]]
    )
  })
)

legend_i <- tags$div(
  class = "legend-block",
  tags$div(class = "legend-title", "De Martonne I"),
  tags$div(
    class = "ramp",
    style = "background:linear-gradient(90deg,#8C510A,#D8B365,#F6E8C3,#C7EAE5,#5AB4AC,#01665E);"
  ),
  tags$div(class = "ramp-labels", tags$span("0 dry"), tags$span("80 humid"))
)

ui <- page_sidebar(
  title = NULL,
  theme = theme,
  fillable = TRUE,
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(overlay_js)
  ),
  sidebar = sidebar(
    width = 380,
    bg = "#f7f4ee",
    tags$div(
      class = "brand",
      tags$div(class = "brand-kicker", "MEDITRACK v1"),
      tags$h1("Mediterranean lake archive"),
      tags$p(
        class = "counts",
        sprintf(
          "%s sites / %s cores  ·  freeze %s",
          meta$n_sites %||% nrow(sites),
          meta$n_cores %||% "—",
          meta$freeze_date %||% ""
        )
      )
    ),
    selectInput(
      "overlay",
      "Climate overlay",
      choices = overlay_choices,
      selected = if ("trace_i_0ka" %in% overlay_choices) "trace_i_0ka" else overlay_choices[[1]]
    ),
    sliderInput("opacity", "Overlay opacity", min = 0.25, max = 0.9, value = 0.72, step = 0.05),
    checkboxGroupInput(
      "record_class",
      "Record class",
      choices = setNames(names(RECORD_LABELS), RECORD_LABELS),
      selected = names(RECORD_LABELS)
    ),
    selectInput(
      "peninsula",
      "Region",
      choices = c("All" = "all", setNames(peninsula_present, peninsula_present)),
      selected = "all"
    ),
    checkboxInput("candidates_only", "MEDITRACK candidate lakes only", FALSE),
    uiOutput("legend_ui")
  ),
  div(
    class = "main-stack",
    div(
      class = "map-pane",
      leafletOutput("map", width = "100%", height = "100%")
    ),
    div(
      class = "meta-pane",
      div(
        class = "meta-grid",
        div(class = "meta-facts", uiOutput("site_card")),
        div(class = "meta-series", uiOutput("series_ui"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    df <- sites
    if (!nrow(df)) return(df)
    if (length(input$record_class)) {
      df <- df[df$record_class %in% input$record_class, , drop = FALSE]
    } else {
      df <- df[0, , drop = FALSE]
    }
    if (!is.null(input$peninsula) && input$peninsula != "all") {
      df <- df[df$peninsula == input$peninsula, , drop = FALSE]
    }
    if (isTRUE(input$candidates_only)) {
      df <- df[df$candidate_status %in% c("selected", "alternative"), , drop = FALSE]
    }
    df
  })

  output$legend_ui <- renderUI({
    id <- input$overlay
    bits <- list(legend_record)
    if (!is.null(id) && nrow(overlays) && id %in% overlays$id) {
      kind <- overlays$kind[overlays$id == id][[1]]
      bits <- c(bits, list(if (identical(kind, "classes")) legend_gain else legend_i))
      cap <- overlays$caption[overlays$id == id][[1]]
      bits <- c(bits, list(tags$p(class = "caption-note", cap)))
    }
    do.call(tagList, bits)
  })

  output$map <- renderLeaflet({
    map <- leaflet(options = leafletOptions(
      minZoom = 5,
      maxZoom = 10,
      zoomSnap = 0.25,
      zoomDelta = 0.5,
      worldCopyJump = FALSE,
      maxBounds = list(c(ov_south, ov_west), c(ov_north, ov_east)),
      maxBoundsViscosity = 1.0
    )) %>%
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
        options = providerTileOptions(attribution = "Tiles © Esri")
      ) %>%
      fitBounds(lng1 = ov_west, lat1 = ov_south, lng2 = ov_east, lat2 = ov_north) %>%
      htmlwidgets::onRender(
        "function(el, x, data) {
           var map = this;
           window.mtMap = map;
           map.createPane('climatePane');
           map.getPane('climatePane').style.zIndex = 350;
           map.getPane('climatePane').style.pointerEvents = 'none';
           window.mtOverlays = {};
           var overlays = (data && data.overlays) ? data.overlays : [];
           var b = (data && data.bounds) ? data.bounds : {south: 27, west: -10, north: 48, east: 45};
           overlays.forEach(function(o) {
             window.mtOverlays[o.id] = L.imageOverlay(
               o.url,
               [[o.south, o.west], [o.north, o.east]],
               {pane: 'climatePane', opacity: 0.72}
             );
           });
           var llb = L.latLngBounds([[b.south, b.west], [b.north, b.east]]);
           map.setMaxBounds(llb);
           map.options.maxBoundsViscosity = 1.0;
           function lockExtent() {
             map.invalidateSize();
             map.setMinZoom(3);
             var zFill = map.getBoundsZoom(llb, true);
             if (!isFinite(zFill)) zFill = 5;
             map.setMinZoom(zFill);
             if (map.getZoom() < zFill) map.setZoom(zFill, {animate: false});
           }
           setTimeout(function() {
             lockExtent();
             map.fitBounds(llb, {padding: [0, 0], animate: false, maxZoom: map.getMinZoom()});
           }, 150);
           map.on('resize', lockExtent);
         }",
        data = map_payload
      )
    map
  })

  observe({
    session$sendCustomMessage(
      "mtSetOverlay",
      list(id = input$overlay %||% "none", opacity = input$opacity %||% 0.72)
    )
  })

  observe({
    df <- filtered()
    proxy <- leafletProxy("map", data = df) %>% clearMarkers() %>% clearGroup("sites")
    if (!nrow(df)) return()
    cols <- unname(RECORD_COLORS[df$record_class])
    weight <- ifelse(df$candidate_status == "selected", 2.4, ifelse(df$candidate_status == "alternative", 1.8, 0.6))
    stroke <- ifelse(df$candidate_status %in% c("selected", "alternative"), "#F1C40F", "#1c2833")
    radius <- ifelse(df$candidate_status == "selected", 8, ifelse(df$candidate_status == "alternative", 7, 5))
    proxy %>% addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      layerId = ~slug,
      group = "sites",
      radius = radius,
      color = stroke,
      weight = weight,
      opacity = 1,
      fillColor = cols,
      fillOpacity = 0.92,
      label = ~site_name,
      labelOptions = labelOptions(textsize = "12px")
    )
  })

  observeEvent(input$peninsula, {
    proxy <- leafletProxy("map")
    if (is.null(input$peninsula) || identical(input$peninsula, "all")) {
      proxy %>% flyToBounds(ov_west, ov_south, ov_east, ov_north)
      return()
    }
    df <- sites[sites$peninsula == input$peninsula, , drop = FALSE]
    if (!nrow(df)) return()
    if (nrow(df) < 2) {
      proxy %>% flyTo(lng = df$longitude[[1]], lat = df$latitude[[1]], zoom = 7.5)
      return()
    }
    lngs <- df$longitude
    lats <- df$latitude
    pad_lng <- max(0.35, 0.08 * (max(lngs, na.rm = TRUE) - min(lngs, na.rm = TRUE)))
    pad_lat <- max(0.25, 0.08 * (max(lats, na.rm = TRUE) - min(lats, na.rm = TRUE)))
    proxy %>% flyToBounds(
      lng1 = min(lngs, na.rm = TRUE) - pad_lng,
      lat1 = min(lats, na.rm = TRUE) - pad_lat,
      lng2 = max(lngs, na.rm = TRUE) + pad_lng,
      lat2 = max(lats, na.rm = TRUE) + pad_lat,
      options = list(maxZoom = 8, padding = c(24, 24))
    )
  }, ignoreInit = TRUE)

  selected <- reactiveVal(NULL)

  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    hit <- sites[sites$slug == click$id, , drop = FALSE]
    if (nrow(hit)) selected(hit[1, , drop = FALSE])
  })

  output$site_card <- renderUI({
    row <- selected()
    if (is.null(row) || !nrow(row)) {
      return(tags$div(class = "site-card muted", "Click a site for proxies and time series."))
    }
    doi <- blank_to_na(row$doi_url)
    cand <- blank_to_na(row$candidate_status)
    badge <- NULL
    if (!is.na(cand)) {
      badge <- tags$span(
        class = paste("badge", cand),
        paste(
          "Candidate:",
          cand,
          blank_to_na(row$candidate_region) %||% "",
          blank_to_na(row$candidate_gradient) %||% ""
        )
      )
    }
    doi_link <- if (!is.na(doi)) tags$a(href = doi, target = "_blank", rel = "noopener", "Paper DOI") else NULL
    tags$div(
      class = "site-card",
      tags$h2(row$site_name),
      badge,
      tags$ul(
        class = "facts",
        tags$li(tags$b("Country "), blank_to_na(row$country) %||% "—"),
        tags$li(tags$b("Region "), paste(blank_to_na(row$peninsula) %||% "—", blank_to_na(row$geo_climate), sep = " · ")),
        tags$li(tags$b("Elevation "), paste0(fmt_num(row$elevation_m, 0), " m")),
        tags$li(tags$b("Cores "), row$n_cores %||% "—"),
        tags$li(tags$b("Time span "), blank_to_na(row$core_time_span) %||% "—"),
        tags$li(tags$b("Class "), blank_to_na(row$record_label) %||% RECORD_LABELS[[row$record_class]] %||% "—"),
        tags$li(tags$b("Proxies "), blank_to_na(row$subclass_list) %||% "—"),
        tags$li(tags$b("TraCE I / BIO1 "), paste(fmt_num(row$I, 1), "/", paste0(fmt_num(row$BIO1, 1), " °C"))),
        tags$li(tags$b("Reference "), blank_to_na(row$key_reference) %||% "—")
      ),
      doi_link,
      if (!is.na(cand)) tags$p(class = "note", blank_to_na(row$candidate_note) %||% NULL)
    )
  })

  series_for <- function(name, slug) {
    df <- series[[name]]
    if (is.null(df) || !nrow(df)) return(NULL)
    hit <- df[df$slug == slug, , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    hit
  }

  pal_plot <- function(df, x, y, ylab) {
    df$age_ka <- df[[x]] / 1000
    df$yval <- df[[y]]
    plot_ly(
      df,
      x = ~age_ka,
      y = ~yval,
      type = "scatter",
      mode = "lines",
      hoverinfo = "x+y",
      line = list(color = "#1c2833", width = 1.4)
    ) %>%
      layout(
        margin = list(l = 48, r = 12, t = 6, b = 36),
        xaxis = list(title = "Age (ka BP)", autorange = "reversed", zeroline = FALSE),
        yaxis = list(title = ylab, zeroline = FALSE),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        font = list(family = "sans-serif", size = 11, color = "#1c2833")
      ) %>%
      config(displayModeBar = FALSE, staticPlot = FALSE)
  }

  output$series_ui <- renderUI({
    row <- selected()
    if (is.null(row) || !nrow(row)) return(NULL)
    slug <- row$slug[[1]]
    tabs <- list()
    if (isTRUE(row$has_trace) && !is.null(series_for("trace", slug))) {
      tabs <- c(tabs, list(tabPanel("TraCE climate", plotlyOutput("plot_trace", height = 170))))
    }
    if (isTRUE(row$has_roc) && !is.null(series_for("roc", slug))) {
      tabs <- c(tabs, list(tabPanel("Pollen RoC", plotlyOutput("plot_roc", height = 170))))
    }
    if (isTRUE(row$has_erb) && !is.null(series_for("erb", slug))) {
      tabs <- c(tabs, list(tabPanel("Erb T", plotlyOutput("plot_erb", height = 170))))
    }
    if (isTRUE(row$has_chironomid) && !is.null(series_for("chironomid", slug))) {
      tabs <- c(tabs, list(tabPanel("Chironomid T", plotlyOutput("plot_chiro", height = 170))))
    }
    if (!length(tabs)) {
      return(tags$p(class = "muted", "No compiled time series for this site yet."))
    }
    do.call(tabsetPanel, c(list(id = "series_tabs", type = "pills"), tabs))
  })

  output$plot_trace <- renderPlotly({
    row <- selected()
    req(row)
    df <- series_for("trace", row$slug[[1]])
    req(df)
    pal_plot(df, "age_cal_bp", "july_tas_c", "July T (°C)")
  })
  output$plot_roc <- renderPlotly({
    row <- selected()
    req(row)
    df <- series_for("roc", row$slug[[1]])
    req(df)
    pal_plot(df, "age_cal_bp", "roc", "Pollen RoC")
  })
  output$plot_erb <- renderPlotly({
    row <- selected()
    req(row)
    df <- series_for("erb", row$slug[[1]])
    req(df)
    pal_plot(df, "age_cal_bp", "erb_annual_anom_c", "Erb annual T anomaly (°C)")
  })
  output$plot_chiro <- renderPlotly({
    row <- selected()
    req(row)
    df <- series_for("chironomid", row$slug[[1]])
    req(df)
    y <- if ("t_july_c" %in% names(df)) "t_july_c" else "t_july_anom_c"
    pal_plot(df, "age_cal_bp", y, "Chironomid July T (°C)")
  })
}

shinyApp(ui, server)
