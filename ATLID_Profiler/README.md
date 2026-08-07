# 🛰️ ATLID Profiler

A lightweight, standalone tool to quickly inspect **EarthCARE/ATLID** curtain
profiles for any longitude/latitude/date of interest — e.g. to check on a
volcanic plume, dust outbreak, or wildfire smoke within minutes.

It was originally derived from the [PACE EarthCARE Fusion (PEF)](https://github.com/atsikerdekis/PaceEarthcareFusion)
comparison codebase, but **PACE/SPEXone is not used at all** and this tool has
no runtime dependency on that repository. Only ATLID Level-2a products are
downloaded and processed.

For each requested day, the tool:
1. Downloads the ATLID **EBD** (Extinction, Backscatter, Depolarization) and
   **TC** (Target Classification) granules via the ESA OADS downloader.
2. Finds the orbit point(s) closest to the target longitude/latitude.
3. Extracts a **100 km wide along-track window** centered on that point.
4. Saves a PNG profile (height vs. along-track distance) for:
   - **Aerosol extinction** at 355 nm (medium resolution), filtered to
     aerosol-classified, cloud-free columns.
   - **Target classification** (cloud/aerosol/clear/etc. per pixel).

## 📁 Structure

```
ATLID_Profiler/
├── start.R              # Control script (CLI entry point)
├── R/
│   ├── config.R         # Paths, product codes, tunable constants
│   ├── geo_utils.R       # Haversine distance, closest-point & window selection
│   ├── download.R        # OADS download + local granule listing/matching
│   ├── read.R             # HDF5 granule reading (geolocation + variables)
│   ├── process.R          # Quality filtering, unit conversion, TC recoding
│   └── plot.R              # Profile rendering & PNG export
├── environment/
│   └── ATLID_Profiler_environment.yml
├── data/                 # Downloaded granules (created at runtime)
├── temp/                 # Scratch space for unzipped files (created at runtime)
└── output/                # Saved profile PNGs (created at runtime)
```

## 🛠 Environment

1. Install Miniforge3: https://github.com/conda-forge/miniforge
2. Create the environment:
   ```
   mamba env create -f environment/ATLID_Profiler_environment.yml
   mamba activate ATLID_Profiler
   ```
3. Install the ESA OADS downloader (`oads_download.py`):
   https://earth.esa.int/eogateway/tools/oads-download

## ⚙️ Configuration

Set these environment variables (or edit `R/config.R` directly):

| Variable                | Purpose                                              | Default            |
|-------------------------|-------------------------------------------------------|---------------------|
| `ATLID_OADS_PYTHON`     | Python interpreter used to run `oads_download.py`      | `python`            |
| `ATLID_OADS_SCRIPT`     | Path to `oads_download.py`                             | `../oads-download-main/oads_download.py` |
| `ATLID_VERSION`         | ATLID processing baseline, e.g. `EXAG`, `EXBA`         | `EXBA`              |
| `ATLID_PROFILER_HOME`   | Base directory for `data/`, `temp/`, `output/`         | current directory   |

## ▶️ Usage

Run from within the `ATLID_Profiler/` directory:

```
Rscript start.R <longitude> <latitude> <start_date:YYYYMMDD> <end_date:YYYYMMDD>
```

### Example: Ambae (Aoba) volcano eruption, Vanuatu, March 2026

```
Rscript start.R 167.84 -15.39 20260307 20260317
```

This downloads all ATLID granules between 2026-03-07 and 2026-03-17, finds
every orbit passing within 500 km of Ambae, and saves an extinction profile
and a target classification profile (each a 100 km window centered on the
closest approach) for every matching orbit/day under `output/`.

## 🔧 Notes

- `max_search_km` (default 500 km) and `window_km` (default 100 km) can be
  tuned in `R/config.R`.
- Granules are matched between the EBD and TC products using the shared
  orbit+frame identifier embedded in ESA's ATLID filenames.
- If no `oads_download.py` installation is available, granules already
  present under `data/` will still be used (the download step logs a warning
  and continues).

## 📜 Attribution

- Originally derived from [PACE EarthCARE Fusion (PEF)](https://github.com/atsikerdekis/PaceEarthcareFusion)
- Data attribution for EarthCARE (ATLID): [van Zadelhoff et al. (2023)](https://doi.org/10.5194/amt-16-3631-2023) and [Donovan et al. (2024)](https://doi.org/10.5194/amt-17-5301-2024)
