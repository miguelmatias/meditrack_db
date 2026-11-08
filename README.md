# MEDITRACK lake archive (v1)

Read-only Shiny map of the Mediterranean downcore lake catalog used in the MEDITRACK proposal. Click a site for proxy coverage and, when compiled, TraCE climate, pollen rate-of-change, Erb temperature, or chironomid July T. Climate overlays are the same TraCE / LGM / CMIP6 layers discussed in the proposal. **The compiled database cannot be downloaded.**

Live URL: https://miguelmatias.shinyapps.io/meditrack_db/

Source: https://github.com/miguelmatias/meditrack_db

## Proposal caption

An interactive map of the Mediterranean lake archive (live count from MASTER) and TraCE / LGM / CMIP6 hydroclimate overlays is at https://miguelmatias.shinyapps.io/meditrack_db/. Click a site for proxy coverage and available time series. The compiled database is not downloadable.

## What is shown

- One marker per Med-box MASTER site (Macaronesia dropped), coloured as pollen-only / aquatic / paired climate–ecology.
- Optional highlight of MEDITRACK candidate lakes.
- Overlays (one at a time): LGM De Martonne I; TraCE I at 0, 6 and 9 ka; I = 30 gain/loss for peak-wettest Holocene → 0 ka, 6 ka → 0 ka, 8.3 ka → 5 ka; SSP2-4.5 and SSP5-8.5 (2081–2100 vs 1970–2000).

This is v1 of the viewer, not a data release.

## Climate data credit

- CHELSA-TraCE21k: Karger et al. (2023), *Climate of the Past*; EnviDat [10.16904/envidat.211](https://www.envidat.ch/dataset/chelsa_trace).
- WorldClim 1.4 LGM (CCSM) and WorldClim 2.1 CMIP6 (MPI-ESM1-2-HR): [worldclim.org](https://www.worldclim.org/).
- Erb et al. (2022) globally resolved temperature; chironomid July T from published Swiss–Norwegian reconstructions.

Overlay PNGs are derived visualisation products. Source rasters are not redistributed here.

## Rebuild the bundle

From the analysis repo `ms_map_iberian_sites`:

```bash
PYTHONPATH=scripts .venv-stratigram/bin/python \
  scripts/meditrack_app/export_app_bundle.py --out ../meditrack_db
```

That writes `data/sites.geojson`, `data/meta.json`, `data/overlays/`, and `data/series/` (gzipped time series). Series files are gitignored and are uploaded only when you deploy.

## Deploy

1. Install packages: `shiny`, `bslib`, `leaflet`, `plotly`, `jsonlite`, `htmltools`, `dplyr`.
2. Log in to [shinyapps.io](https://www.shinyapps.io/) and run `rsconnect::setAccountInfo(...)` once.
3. From this directory: `Rscript deploy.R`.

shinyapps.io **free** is 25 hours/month and sleeps after idle. The first click after sleep is slow, and a review panel can burn the quota. For the review window use **Starter or Basic**. If the app is asleep, wait ~30 s; if hours are exhausted the link will fail until the next month or an upgrade.

GitHub cannot run Shiny. This repo holds the app source and the public site catalog. A static GitHub Pages snapshot (or shinylive) is the fallback if shinyapps hours run out.

## No-download policy

- No download buttons, no CSV/Excel export, Plotly mode bar off.
- Time series are not in `www/` and are not committed.
- Site names, coordinates and proxy flags in `data/sites.geojson` are the public face of the archive (already in the papers). Screenshots cannot be prevented.
