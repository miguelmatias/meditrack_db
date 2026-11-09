# Deploy the MEDITRACK v1 explorer to shinyapps.io.
# Requires a Posit account: rsconnect::setAccountInfo(...)
# During panel review, use Starter or Basic so the 25-hour free tier is not exhausted.
#
# data/series/ is gitignored (no public dump) but MUST be uploaded. We pass
# appFiles explicitly so rsconnect does not honour .gitignore for that folder.

app_files <- c(
  "app.R",
  "README.md",
  "www/styles.css",
  "www/meditrack_logo.png",
  "data/sites.geojson",
  "data/meta.json",
  list.files("data/overlays", full.names = TRUE),
  list.files("data/series", full.names = TRUE)
)
app_files <- app_files[file.exists(app_files)]

rsconnect::deployApp(
  appDir = ".",
  appName = "meditrack_db",
  appTitle = "MEDITRACK v1 lake archive",
  appFiles = app_files,
  forceUpdate = TRUE,
  lint = FALSE
)
