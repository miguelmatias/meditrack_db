# Deploy the MEDITRACK v1 explorer to shinyapps.io.
#
# The public hostname IS the shinyapps account name:
#   https://<account>.shinyapps.io/<appName>/
# Deploying as appName "meditrack" on account "miguelmatias" still yields
#   https://miguelmatias.shinyapps.io/meditrack/
# so a branded URL requires an account named "meditrack".
#
# shinyapps.io does not redirect old URLs. A second account can host the same
# app (hours count separately). Account rename (dashboard → Account → Settings)
# also changes every existing app URL on that account, including
# azores-lakes-explorer.
#
# One-time setup (Miguel; email + captcha — cannot be automated):
#   1. https://www.shinyapps.io/admin/#/login
#   2. Register a new account with username **meditrack** if the handle is free
#      (or Rename the existing account, knowing all current URLs will break).
#   3. Tokens (avatar menu) → Show → copy:
#        rsconnect::setAccountInfo(name = "meditrack", token = "...", secret = "...")
#   4. Rscript deploy.R  →  https://meditrack.shinyapps.io/archive/
#
# During panel review, use Starter or Basic so the 25-hour free tier is not
# exhausted.
#
# data/series/ is gitignored (no public dump) but MUST be uploaded. We pass
# appFiles explicitly so rsconnect does not honour .gitignore for that folder.

accounts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
account_names <- if (is.null(accounts)) character() else accounts$name

if (!"meditrack" %in% account_names) {
  stop(
    "No shinyapps.io account named 'meditrack' is configured on this machine.\n",
    "Configured accounts: ",
    if (length(account_names)) paste(account_names, collapse = ", ") else "(none)",
    "\n\n",
    "The subdomain is the account name, so this app cannot appear at\n",
    "https://meditrack.shinyapps.io/... until that account exists.\n\n",
    "Do this once, then re-run Rscript deploy.R:\n",
    "  1. Open https://www.shinyapps.io/admin/#/login\n",
    "  2. Sign up with username meditrack (if available) using a different\n",
    "     login than miguelmatias, OR Account → Settings → Rename on the\n",
    "     existing account (that also rewrites azores-lakes-explorer).\n",
    "  3. Tokens → copy and run:\n",
    "       rsconnect::setAccountInfo(name = 'meditrack', token = '...', secret = '...')\n",
    call. = FALSE
  )
}

app_files <- c(
  "app.R",
  "README.md",
  "www/styles.css",
  "www/meditrack_logo.png",
  "www/favicon.png",
  "www/favicon.ico",
  "data/sites.geojson",
  "data/meta.json",
  list.files("data/overlays", full.names = TRUE),
  list.files("data/series", full.names = TRUE)
)
app_files <- app_files[file.exists(app_files)]

rsconnect::deployApp(
  appDir = ".",
  account = "meditrack",
  appName = "archive",
  appTitle = "MEDITRACK v1 lake archive",
  appFiles = app_files,
  forceUpdate = TRUE,
  lint = FALSE
)
