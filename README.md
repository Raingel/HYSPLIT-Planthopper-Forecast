# HYSPLIT-Planthopper-Forecast

Predicting planthopper migration with HYSPLIT and GFS forecast data.

## Forecast pipeline

`predictor/run_forecast.py` is now the single entrypoint for the whole forecast job.
It downloads the recent GFS forecast binaries, runs the HYSPLIT ensemble frequency
workflow, and writes the daily GeoJSON plus `*_max_freq.csv` files into `prediction/`.

The GitHub Actions workflow in `.github/workflows/planthopper-forecast.yml` runs this
pipeline on GitHub-hosted Linux runners and commits refreshed prediction data back to
the repository.

## Local development

Install Python dependencies with:

```bash
pip install -r predictor/requirements.txt
```

GitHub Actions uses the Linux HYSPLIT binaries already vendored under
`predictor/HYSPLIT/exec`.

On Windows, point `HYSPLIT_HOME` at a Windows HYSPLIT build that contains
`hyts_ens(.exe)`, `trajfreq(.exe)`, and `con2asc(.exe)`, then run:

```bash
python predictor/run_forecast.py run --cycle-hour 12 --download-count 5
```

Transient downloads and working files are written under `predictor/runtime/` and are
ignored by git.