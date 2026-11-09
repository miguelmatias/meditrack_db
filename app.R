# MEDITRACK Mediterranean lake archive — v1.
# Read-only explorer: compiled TraCE / RoC / Erb / chironomid series are not
# downloadable. Catalog CSV (names, coords, public fields) is allowed.

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(jsonlite)
library(htmltools)
library(DT)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.atomic(a) && is.na(a[[1]]))) b else a
}

DATA_DIR <- "data"
SITES_PATH <- file.path(DATA_DIR, "sites.geojson")
META_PATH <- file.path(DATA_DIR, "meta.json")
OVERLAY_JSON <- file.path(DATA_DIR, "overlays", "overlays.json")
SERIES_DIR <- file.path(DATA_DIR, "series")

RECORD_COLORS <- c(
  pollen_only = "#5D6D7E",
  aquatic = "#1ABC9C",
  paired = "#2E5A88"
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
PENINSULA_GROUP <- c("Iberia", "Italy", "Balkans")
CATALOG_COLS <- c(
  "site_name", "country", "peninsula", "record_label", "subclass_list",
  "n_cores", "span_display", "latitude", "longitude", "doi_url"
)

read_json <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == "" | x %in% c("NA", "null", "None")] <- NA_character_
  x
}

as_flag <- function(x) {
  x %in% c(TRUE, "TRUE", "true", 1, "1")
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x[1])) return("—")
  format(round(as.numeric(x[1]), digits), nsmall = digits, trim = TRUE)
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
  for (col in c("has_trace", "has_roc", "has_erb", "has_chironomid", "span_parsed")) {
    if (!col %in% names(df)) df[[col]] <- FALSE
    df[[col]] <- as_flag(df[[col]])
  }
  for (col in c("span_ka_old", "span_ka_young", "series_ka_old", "series_ka_young")) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  for (col in c("span_display", "span_notes", "dataset_dois", "neotoma_site_ids", "repository_datasets")) {
    if (!col %in% names(df)) df[[col]] <- ""
    df[[col]] <- ifelse(is.na(df[[col]]), "", as.character(df[[col]]))
  }
  use_span <- df$span_parsed & !is.na(df$span_ka_old) & !is.na(df$span_ka_young)
  df$filter_ka_old <- ifelse(use_span, df$span_ka_old, df$series_ka_old)
  df$filter_ka_young <- ifelse(use_span, df$span_ka_young, df$series_ka_young)
  df
}

load_series <- function(name) {
  path <- file.path(SERIES_DIR, paste0(name, ".csv.gz"))
  if (!file.exists(path)) return(NULL)
  utils::read.csv(gzfile(path), stringsAsFactors = FALSE)
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
region_choices <- c(
  "All" = "all",
  "Peninsulas" = "peninsulas",
  setNames(peninsula_present, peninsula_present)
)

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
  bg = "#ffffff",
  fg = "#1c2833",
  primary = "#1c2833"
)

head_js <- HTML(paste(
  "(function(){",
  "  var q = new URLSearchParams(window.location.search);",
  "  if (q.get('site') || q.get('view') === 'app') {",
  "    document.documentElement.classList.add('mt-app-mode');",
  "  }",
  "})();",
  "window.mtOverlays = window.mtOverlays || {};",
  "function mtEnter() {",
  "  document.documentElement.classList.add('mt-app-mode');",
  "  var url = new URL(window.location.href);",
  "  if (!url.searchParams.get('site')) url.searchParams.set('view', 'app');",
  "  history.replaceState({}, '', url);",
  "  setTimeout(function() { if (window.mtMap) window.mtMap.invalidateSize(); }, 80);",
  "}",
  "Shiny.addCustomMessageHandler('mtEnter', function(msg) { mtEnter(); });",
  "Shiny.addCustomMessageHandler('mtSetSiteUrl', function(msg) {",
  "  var url = new URL(window.location.href);",
  "  if (msg.slug) {",
  "    url.searchParams.set('site', msg.slug);",
  "    url.searchParams.delete('view');",
  "  }",
  "  history.replaceState({}, '', url);",
  "});",
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
  "$(document).on('click', '#enter_app', function() { mtEnter(); });",
  "$(document).on('keydown', function(e) {",
  "  if (e.key === 'Enter' && !document.documentElement.classList.contains('mt-app-mode')) {",
  "    if (document.activeElement && document.activeElement.tagName === 'INPUT') return;",
  "    $('#enter_app').click();",
  "  }",
  "});",
  sep = "\n"
))

svg_data_uri <- function(shape, fill, stroke, size) {
  stroke_w <- if (identical(shape, "diamond")) 2.2 else 1.6
  inner <- switch(
    shape,
    star = sprintf(
      '<path d="M12 2.2l2.7 5.6 6.2.9-4.5 4.4 1.1 6.2L12 16.6 6.5 19.3l1.1-6.2-4.5-4.4 6.2-.9z" fill="%s" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
      fill, stroke, stroke_w
    ),
    diamond = sprintf(
      '<polygon points="12,2.5 21.5,12 12,21.5 2.5,12" fill="%s" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
      fill, stroke, stroke_w
    ),
    sprintf(
      '<circle cx="12" cy="12" r="6.2" fill="%s" stroke="%s" stroke-width="%s"/>',
      fill, stroke, stroke_w
    )
  )
  svg <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 24 24">%s</svg>',
    size, size, inner
  )
  paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
}

candidate_line <- function(row) {
  status <- blank_to_na(row$candidate_status[[1]])
  if (is.na(status) || !status %in% c("selected", "alternative")) return(NA_character_)
  region <- blank_to_na(row$candidate_region[[1]]) %||% ""
  grad <- blank_to_na(row$candidate_gradient[[1]]) %||% ""
  bits <- c(region, grad)
  bits <- bits[nzchar(bits)]
  prefix <- if (length(bits)) paste(bits, collapse = " ") else "Candidate"
  paste(prefix, status, sep = " · ")
}

marker_label <- function(row) {
  name <- htmltools::htmlEscape(as.character(row$site_name[[1]]))
  extra <- candidate_line(row)
  if (is.na(extra)) return(htmltools::HTML(paste0("<b>", name, "</b>")))
  htmltools::HTML(paste0("<b>", name, "</b><br/>", htmltools::htmlEscape(extra)))
}

extract_dois <- function(...) {
  text <- paste(c(...), collapse = " ")
  if (!nzchar(trimws(text))) return(character())
  unique(unlist(regmatches(
    text,
    gregexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", text, perl = TRUE)
  )))
}

dataset_links <- function(row) {
  bits <- list()
  paper <- blank_to_na(row$doi_url)
  paper_doi <- NA_character_
  if (!is.na(paper)) {
    bits <- c(bits, list(tags$a(href = paper, target = "_blank", rel = "noopener", "Paper DOI")))
    m <- regmatches(paper, regexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", paper, perl = TRUE))
    if (length(m) && nzchar(m)) paper_doi <- m
  }
  dois <- extract_dois(row$dataset_dois, row$repository_datasets)
  dois <- setdiff(dois, paper_doi)
  if (length(dois)) {
    bits <- c(bits, lapply(seq_along(dois), function(i) {
      tags$a(
        href = paste0("https://doi.org/", dois[[i]]),
        target = "_blank",
        rel = "noopener",
        if (length(dois) == 1) "Dataset DOI" else paste("Dataset", i)
      )
    }))
  }
  neo <- blank_to_na(row$neotoma_site_ids)
  if (!is.na(neo)) {
    ids <- unique(unlist(strsplit(neo, "[,;[:space:]]+")))
    ids <- ids[grepl("^[0-9]+$", ids)]
    if (length(ids)) {
      bits <- c(bits, lapply(ids, function(id) {
        tags$a(
          href = paste0("https://apps.neotomadb.org/explorer/?siteids=", id),
          target = "_blank",
          rel = "noopener",
          paste("Neotoma", id)
        )
      }))
    }
  }
  repo <- blank_to_na(row$repository_datasets)
  if (!is.na(repo)) {
    noaa <- unique(unlist(regmatches(repo, gregexpr("NOAA\\s*[#:]?\\s*(\\d{5,})", repo, perl = TRUE, ignore.case = TRUE))))
    nums <- unique(unlist(regmatches(paste(noaa, collapse = " "), gregexpr("\\d{5,}", paste(noaa, collapse = " ")))))
    if (length(nums)) {
      bits <- c(bits, lapply(nums, function(id) {
        tags$a(
          href = paste0("https://www.ncei.noaa.gov/access/paleo-search/study/", id),
          target = "_blank",
          rel = "noopener",
          paste("NOAA", id)
        )
      }))
    }
  }
  if (!length(bits)) return(NULL)
  tags$p(class = "links", bits)
}

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
    tags$span(class = "swatch star gold"),
    "Candidate · selected"
  ),
  tags$div(
    class = "legend-row",
    tags$span(class = "swatch diamond gold"),
    "Candidate · alternative"
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

n_sites_live <- meta$n_sites %||% nrow(sites)
n_cores_live <- meta$n_cores %||% "—"

about_ui <- tags$div(
  class = "about-copy",
  tags$h2("About MEDITRACK"),
  tags$p(
    "A circum-Mediterranean downcore lake archive for the MEDITRACK project: ",
    "pollen, aquatic proxies, and paired climate–ecology records, with TraCE / LGM / CMIP6 humidity overlays."
  ),
  tags$p(tags$b("Version "), "v1  ·  ", tags$b("Sites "), n_sites_live, " / ", n_cores_live, " cores",
         "  ·  freeze ", meta$freeze_date %||% ""),
  tags$h3("Citation"),
  tags$p("MEDITRACK project / Miguel Matias (viewer and catalog). Climate layers: CHELSA-TraCE21k, Karger et al. 2023; WorldClim; Erb et al. 2022 globally resolved temperature."),
  tags$h3("Licence"),
  tags$p("This viewer is a draft. Compiled TraCE, pollen RoC, Erb and chironomid series are not redistributable from this app. Catalog names and coordinates come from published papers."),
  tags$h3("Data access"),
  tags$p("No bulk download. Paper and dataset DOIs (and Neotoma / NOAA links when known) are listed on each site. An optional CSV of the public catalog table (names, coordinates, class, proxies) is available on the Table tab."),
  tags$h3("Time-coverage filter"),
  tags$p("Best-effort parse of the published time-span text: numbers with ka / cal BP / yr, plus named periods (Holocene ≈ 12–0 ka, Lateglacial, LGM, …). Coring years such as 2011 count as 0 ka. If the text cannot be parsed, the site stays visible unless “Hide sites with unknown time span” is on; compiled series ages are used as a fallback when present. Mid-Holocene 8–3.5 ka is a one-click preset."),
  tags$h3("Contact"),
  tags$p(
    "Miguel Matias  ·  ",
    tags$a(href = "https://github.com/miguelmatias/meditrack_db", target = "_blank", rel = "noopener", "github.com/miguelmatias/meditrack_db")
  )
)

splash_ui <- div(
  id = "mt-splash",
  tags$img(
    src = "meditrack_logo.png",
    alt = "MEDITRACK — Mediterranean climate and lake ecosystems",
    width = "280",
    class = "splash-logo"
  ),
  actionButton("enter_app", "Enter", class = "btn-dark btn-lg splash-enter")
)

map_panel <- div(
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

table_panel <- div(
  class = "table-pane",
  tags$p(
    class = "table-note",
    "Public catalog fields only — not the compiled climate or pollen series. Sort and search here; the same sidebar filters apply."
  ),
  downloadButton("catalog_csv", "Download catalog CSV", class = "btn-sm btn-outline-dark"),
  DTOutput("catalog")
)

ui <- tagList(
  tags$head(
    tags$script(HTML("(function(){var q=new URLSearchParams(location.search);if(q.get('site')||q.get('view')==='app')document.documentElement.classList.add('mt-app-mode');})();")),
    tags$title("MEDITRACK · Mediterranean lake archive"),
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", href = "favicon.png"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(head_js)
  ),
  splash_ui,
  page_sidebar(
    title = NULL,
    theme = theme,
    fillable = TRUE,
    sidebar = sidebar(
      width = 380,
      bg = "#f7f4ee",
      tags$div(
        class = "brand",
        tags$img(
          src = "meditrack_logo.png",
          alt = "MEDITRACK — Mediterranean climate and lake ecosystems",
          class = "brand-logo",
          width = "280"
        ),
        tags$div(class = "brand-kicker", "v1"),
        tags$h1("Mediterranean lake archive"),
        tags$p(
          class = "counts",
          sprintf(
            "%s sites / %s cores  ·  freeze %s",
            n_sites_live,
            n_cores_live,
            meta$freeze_date %||% ""
          )
        )
      ),
      textInput("site_search", "Search sites", placeholder = "Name contains…"),
      selectInput(
        "overlay",
        "Climate overlay",
        choices = overlay_choices,
        selected = if ("trace_i_0ka" %in% overlay_choices) "trace_i_0ka" else overlay_choices[[1]]
      ),
      uiOutput("overlay_caption"),
      sliderInput(
        "opacity",
        "Overlay opacity",
        min = 0.25,
        max = 0.9,
        value = 0.72,
        step = 0.05,
        ticks = FALSE
      ),
      checkboxGroupInput(
        "record_class",
        "Record class",
        choices = setNames(names(RECORD_LABELS), RECORD_LABELS),
        selected = names(RECORD_LABELS)
      ),
      selectInput(
        "peninsula",
        "Region",
        choices = region_choices,
        selected = "all"
      ),
      checkboxInput("candidates_only", "MEDITRACK candidate lakes only", FALSE),
      checkboxInput("age_filter", "Filter by time coverage", FALSE),
      sliderInput(
        "age_window",
        "Covers ages (ka BP)",
        min = 0,
        max = 20,
        value = c(0, 12),
        step = 0.1,
        ticks = FALSE
      ),
      actionButton("age_midhol", "Mid-Holocene 8–3.5 ka", class = "btn-sm btn-outline-dark"),
      checkboxInput("age_strict", "Hide sites with unknown time span", FALSE),
      uiOutput("legend_ui")
    ),
    navset_tab(
      id = "main_nav",
      nav_panel("Map", value = "map", map_panel),
      nav_panel("Table", value = "table", table_panel),
      nav_panel("About", value = "about", about_ui)
    )
  )
)

server <- function(input, output, session) {
  default_slug <- "laguna_de_zonar"
  default_row <- if (nrow(sites)) {
    hit <- sites[sites$slug %in% c(default_slug, "laguna_de_zo_ar"), , drop = FALSE]
    if (!nrow(hit)) {
      hit <- sites[grepl("zonar|zo\u00f1ar", sites$site_name, ignore.case = TRUE), , drop = FALSE]
    }
    if (nrow(hit)) hit[1, , drop = FALSE] else NULL
  } else {
    NULL
  }
  selected <- reactiveVal(default_row)

  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search %||% "")
    slug <- q[["site"]]
    if (!is.null(slug) && nzchar(slug)) {
      hit <- sites[sites$slug == slug, , drop = FALSE]
      if (nrow(hit)) selected(hit[1, , drop = FALSE])
    }
  }, once = TRUE, ignoreNULL = FALSE)

  observeEvent(input$enter_app, {
    session$sendCustomMessage("mtEnter", list())
  })

  observeEvent(input$age_midhol, {
    updateCheckboxInput(session, "age_filter", value = TRUE)
    updateSliderInput(session, "age_window", value = c(3.5, 8))
  })

  region_match <- function(df, region) {
    if (is.null(region) || identical(region, "all")) return(df)
    if (identical(region, "peninsulas")) {
      return(df[df$peninsula %in% PENINSULA_GROUP, , drop = FALSE])
    }
    df[df$peninsula == region, , drop = FALSE]
  }

  apply_time <- function(df) {
    if (!nrow(df)) return(df)
    known <- !is.na(df$filter_ka_old) & !is.na(df$filter_ka_young)
    if (isTRUE(input$age_filter) && length(input$age_window) == 2) {
      lo <- min(input$age_window)
      hi <- max(input$age_window)
      overlap <- known & (df$filter_ka_old >= lo) & (df$filter_ka_young <= hi)
      if (isTRUE(input$age_strict)) {
        df <- df[overlap, , drop = FALSE]
      } else {
        df <- df[overlap | !known, , drop = FALSE]
      }
    } else if (isTRUE(input$age_strict)) {
      df <- df[known, , drop = FALSE]
    }
    df
  }

  filtered_pre_class <- reactive({
    df <- sites
    if (!nrow(df)) return(df)
    q <- trimws(input$site_search %||% "")
    if (nzchar(q)) {
      df <- df[grepl(q, df$site_name, ignore.case = TRUE), , drop = FALSE]
    }
    df <- region_match(df, input$peninsula)
    if (isTRUE(input$candidates_only)) {
      df <- df[df$candidate_status %in% c("selected", "alternative"), , drop = FALSE]
    }
    apply_time(df)
  })

  filtered <- reactive({
    df <- filtered_pre_class()
    if (!nrow(df)) return(df)
    if (length(input$record_class)) {
      df <- df[df$record_class %in% input$record_class, , drop = FALSE]
    } else {
      df <- df[0, , drop = FALSE]
    }
    df
  })

  observe({
    df <- filtered_pre_class()
    counts <- vapply(names(RECORD_LABELS), function(k) sum(df$record_class == k, na.rm = TRUE), integer(1))
    labs <- sprintf("%s (%d)", unname(RECORD_LABELS[names(RECORD_LABELS)]), counts)
    selected_cls <- isolate(input$record_class)
    if (is.null(selected_cls)) selected_cls <- names(RECORD_LABELS)
    updateCheckboxGroupInput(
      session,
      "record_class",
      choiceNames = labs,
      choiceValues = names(RECORD_LABELS),
      selected = selected_cls
    )
  })

  output$overlay_caption <- renderUI({
    id <- input$overlay
    if (is.null(id) || identical(id, "none") || !nrow(overlays) || !id %in% overlays$id) {
      return(tags$p(class = "overlay-caption", "No climate overlay."))
    }
    cap <- overlays$caption[overlays$id == id][[1]]
    tags$p(class = "overlay-caption", cap)
  })

  output$legend_ui <- renderUI({
    id <- input$overlay
    bits <- list(legend_record)
    if (!is.null(id) && nrow(overlays) && id %in% overlays$id) {
      row <- overlays[overlays$id == id, , drop = FALSE]
      kind <- as.character(row$kind[[1]])
      group <- if ("group" %in% names(row)) as.character(row$group[[1]]) else ""
      if (identical(kind, "classes") || identical(group, "gainloss")) {
        bits <- c(bits, list(legend_gain))
      } else {
        bits <- c(bits, list(legend_i))
      }
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
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearMarkerClusters()
    if (!nrow(df)) return()
    sel_slug <- {
      row <- selected()
      if (is.null(row) || !nrow(row)) "" else as.character(row$slug[[1]])
    }
    is_sel <- df$slug == sel_slug
    cand <- df$candidate_status
    shape <- ifelse(cand == "selected", "star", ifelse(cand == "alternative", "diamond", "circle"))
    fill <- ifelse(cand == "selected", "#F1C40F", unname(RECORD_COLORS[df$record_class]))
    stroke <- ifelse(
      is_sel,
      "#1c2833",
      ifelse(cand %in% c("selected", "alternative"), "#F1C40F", "#1c2833")
    )
    size <- ifelse(
      is_sel,
      ifelse(cand == "selected", 28, 24),
      ifelse(cand == "selected", 22, ifelse(cand == "alternative", 18, 14))
    )
    urls <- mapply(svg_data_uri, shape, fill, stroke, size, USE.NAMES = FALSE)
    labels <- lapply(seq_len(nrow(df)), function(i) marker_label(df[i, , drop = FALSE]))
    proxy %>% addMarkers(
      lng = df$longitude,
      lat = df$latitude,
      layerId = df$slug,
      icon = icons(
        iconUrl = urls,
        iconWidth = size,
        iconHeight = size,
        iconAnchorX = size / 2,
        iconAnchorY = size / 2
      ),
      label = labels,
      labelOptions = labelOptions(textsize = "12px", direction = "auto"),
      clusterOptions = markerClusterOptions(
        disableClusteringAtZoom = 8,
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = FALSE
      )
    )
  })

  observeEvent(input$peninsula, {
    proxy <- leafletProxy("map")
    if (is.null(input$peninsula) || identical(input$peninsula, "all")) {
      proxy %>% flyToBounds(ov_west, ov_south, ov_east, ov_north)
      return()
    }
    df <- region_match(sites, input$peninsula)
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

  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    hit <- sites[sites$slug == click$id, , drop = FALSE]
    if (nrow(hit)) {
      selected(hit[1, , drop = FALSE])
      session$sendCustomMessage("mtSetSiteUrl", list(slug = click$id))
    }
  })

  observeEvent(selected(), {
    row <- selected()
    proxy <- leafletProxy("map") %>% clearPopups()
    if (is.null(row) || !nrow(row)) return()
    extra <- candidate_line(row)
    pop <- htmltools::htmlEscape(as.character(row$site_name[[1]]))
    if (!is.na(extra)) {
      pop <- paste0(pop, "<br/><span class='popup-cand'>", htmltools::htmlEscape(extra), "</span>")
    }
    proxy %>% addPopups(
      lng = as.numeric(row$longitude[[1]]),
      lat = as.numeric(row$latitude[[1]]),
      popup = pop,
      options = popupOptions(closeButton = TRUE, autoPan = FALSE)
    )
  }, ignoreNULL = TRUE)

  time_span_ui <- function(row) {
    disp <- blank_to_na(row$span_display)
    notes <- blank_to_na(row$span_notes)
    if (!is.na(disp)) {
      tagList(
        disp,
        if (!is.na(notes)) tags$div(class = "span-notes", notes)
      )
    } else {
      blank_to_na(row$core_time_span) %||% "—"
    }
  }

  output$site_card <- renderUI({
    row <- selected()
    if (is.null(row) || !nrow(row)) {
      return(tags$div(class = "site-card muted", "Click a site for proxies and time series."))
    }
    cand <- blank_to_na(row$candidate_status)
    badge <- NULL
    extra <- candidate_line(row)
    if (!is.na(extra)) {
      badge <- tags$span(class = paste("badge", cand), extra)
    }
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
        tags$li(tags$b("Time span "), time_span_ui(row)),
        tags$li(tags$b("Class "), blank_to_na(row$record_label) %||% RECORD_LABELS[[row$record_class]] %||% "—"),
        tags$li(tags$b("Proxies "), blank_to_na(row$subclass_list) %||% "—"),
        tags$li(tags$b("TraCE I / BIO1 "), paste(fmt_num(row$I, 1), "/", paste0(fmt_num(row$BIO1, 1), " °C"))),
        tags$li(tags$b("Reference "), blank_to_na(row$key_reference) %||% "—")
      ),
      dataset_links(row),
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

  series_tab <- function(title, caption, plot_id) {
    tabPanel(
      title,
      tags$p(class = "series-caption", caption),
      plotlyOutput(plot_id, height = 155)
    )
  }

  output$series_ui <- renderUI({
    row <- selected()
    if (is.null(row) || !nrow(row)) return(NULL)
    slug <- row$slug[[1]]
    tabs <- list()
    if (isTRUE(row$has_trace) && !is.null(series_for("trace", slug))) {
      tabs <- c(tabs, list(series_tab(
        "TraCE climate",
        "CHELSA-TraCE21k July T at the lake pixel.",
        "plot_trace"
      )))
    }
    if (isTRUE(row$has_roc) && !is.null(series_for("roc", slug))) {
      tabs <- c(tabs, list(series_tab(
        "Pollen RoC",
        "Mottl-style pollen rate of change.",
        "plot_roc"
      )))
    }
    if (isTRUE(row$has_erb) && !is.null(series_for("erb", slug))) {
      tabs <- c(tabs, list(series_tab(
        "Erb T",
        "Erb et al. 2022 globally resolved temperature; annual anomaly vs native 3–5 ka reference.",
        "plot_erb"
      )))
    }
    if (isTRUE(row$has_chironomid) && !is.null(series_for("chironomid", slug))) {
      tabs <- c(tabs, list(series_tab(
        "Chironomid T",
        "Published July T reconstructions.",
        "plot_chiro"
      )))
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

  catalog_view <- reactive({
    df <- filtered()
    if (!nrow(df)) return(df)
    out <- data.frame(
      Name = df$site_name,
      Country = df$country,
      Peninsula = df$peninsula,
      Class = df$record_label,
      Proxies = df$subclass_list,
      Cores = df$n_cores,
      `Time span` = ifelse(nzchar(df$span_display), df$span_display, df$core_time_span),
      Lat = round(df$latitude, 3),
      Lon = round(df$longitude, 3),
      DOI = df$doi_url,
      slug = df$slug,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out
  })

  output$catalog <- renderDT({
    df <- catalog_view()
    show <- df[, setdiff(names(df), "slug"), drop = FALSE]
    if ("DOI" %in% names(show)) {
      show$DOI <- vapply(show$DOI, function(u) {
        if (is.na(u) || !nzchar(u)) return("")
        sprintf(
          "<a href=\"%s\" target=\"_blank\" rel=\"noopener\">DOI</a>",
          htmltools::htmlEscape(u, attribute = TRUE)
        )
      }, character(1))
    }
    datatable(
      show,
      rownames = FALSE,
      escape = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        deferRender = TRUE,
        dom = "frtip"
      )
    )
  })

  observeEvent(input$catalog_rows_selected, {
    idx <- input$catalog_rows_selected
    df <- catalog_view()
    if (!length(idx) || idx < 1 || idx > nrow(df)) return()
    slug <- df$slug[[idx]]
    hit <- sites[sites$slug == slug, , drop = FALSE]
    if (nrow(hit)) {
      selected(hit[1, , drop = FALSE])
      session$sendCustomMessage("mtSetSiteUrl", list(slug = slug))
      updateTabsetPanel(session, "main_nav", selected = "map")
    }
  })

  output$catalog_csv <- downloadHandler(
    filename = function() "meditrack_catalog.csv",
    content = function(file) {
      df <- filtered()
      keep <- intersect(
        c(
          "site_name", "country", "peninsula", "record_label", "subclass_list",
          "n_cores", "span_display", "span_notes", "latitude", "longitude",
          "doi_url", "dataset_dois", "neotoma_site_ids"
        ),
        names(df)
      )
      utils::write.csv(df[, keep, drop = FALSE], file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
