from __future__ import annotations

import argparse
import json
import logging
import os
import random
import re
import shutil
import subprocess
import time
import zipfile
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Iterable, Sequence

from dateutil.parser import parse as parse_datetime

import matplotlib

matplotlib.use("Agg")

import geojsoncontour
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import requests
from scipy.interpolate import griddata

REPO_ROOT = Path(__file__).resolve().parent.parent
PREDICTOR_ROOT = Path(__file__).resolve().parent
DEFAULT_HYSPLIT_HOME = Path(os.environ.get("HYSPLIT_HOME", PREDICTOR_ROOT / "HYSPLIT"))
DEFAULT_RUNTIME_ROOT = Path(os.environ.get("FORECAST_RUNTIME_ROOT", PREDICTOR_ROOT / "runtime"))
DEFAULT_GFS_DIR = DEFAULT_RUNTIME_ROOT / "gfs_forecast"
DEFAULT_WORKING_DIR = DEFAULT_RUNTIME_ROOT / "working_forecast"
DEFAULT_PREDICTION_DIR = REPO_ROOT / "prediction"
DEFAULT_SOURCE_CSV = PREDICTOR_ROOT / "plant_hopper_source.csv"

READY_BASE_URL = "https://www.ready.noaa.gov"
READY_CYCLE_URL = f"{READY_BASE_URL}/ready2-bin/extract/extractfcyc.pl"
READY_SPAN_URL = f"{READY_BASE_URL}/ready2-bin/extract/extract1f.pl"
READY_EXTRACT_URL = f"{READY_BASE_URL}/ready2-bin/extract/extract3f.pl"
READY_RESULTS_URL = f"{READY_BASE_URL}/ready2-bin/extract/results.pl"
READY_SPAN_PATTERN = re.compile(
    r"contains data from <u>(.*?) </u>\s+to\s+<u>(.*?) </u>\.</h3>",
    re.IGNORECASE | re.DOTALL,
)
READY_FILE_PATTERN = re.compile(
    r'(/extractout/[^\s"<>]+?\.zip)',
    re.IGNORECASE | re.DOTALL,
)
THERMAL_CUTOFF_K = 16.5 + 273.0
GRID_Y_VALUES = np.arange(8.0, 25.0, 0.5)
GRID_X_VALUES = np.arange(100.0, 125.0, 0.5)
CONTOUR_LEVELS = [0, 10, 20, 30, 40, 50, 999]
CONTOUR_COLORS = [
    (1.0, 1.0, 1.0, 0.0),
    (0.70196, 1.0, 1.0),
    (0.8, 1.0, 0.8),
    (1.0, 1.0, 0.6),
    (1.0, 0.4, 0.4),
    (1.0, 0.4, 0.8),
]
TRANSIENT_FILES = {
    "ASCDATA.CFG",
    "CONTROL",
    "INFILE",
    "LABELS.CFG",
    "MESSAGE",
    "SETUP.CFG",
    "STARTUP",
    "WARNING",
    "tfreq.bin",
    "tfreq.bin.txt",
    "trajplot.html",
}


@dataclass(frozen=True)
class PipelineConfig:
    hysplit_home: Path
    runtime_root: Path
    gfs_dir: Path
    working_dir: Path
    prediction_dir: Path
    source_csv: Path
    download_count: int
    forecast_days_before: int
    forecast_days_after: int
    keep_downloads: int
    cycle_hour: int | None
    archive_raw_outputs: bool
    force_download: bool


def build_config(args: argparse.Namespace) -> PipelineConfig:
    return PipelineConfig(
        hysplit_home=Path(args.hysplit_home).resolve(),
        runtime_root=Path(args.runtime_root).resolve(),
        gfs_dir=Path(args.gfs_dir).resolve(),
        working_dir=Path(args.working_dir).resolve(),
        prediction_dir=Path(args.prediction_dir).resolve(),
        source_csv=Path(args.source_csv).resolve(),
        download_count=args.download_count,
        forecast_days_before=args.forecast_days_before,
        forecast_days_after=args.forecast_days_after,
        keep_downloads=max(args.keep_downloads, args.download_count),
        cycle_hour=args.cycle_hour,
        archive_raw_outputs=args.archive_raw_outputs,
        force_download=args.force_download,
    )


def configure_logging(level_name: str) -> None:
    level = getattr(logging, level_name.upper(), logging.INFO)
    logging.basicConfig(level=level, format="%(asctime)s %(levelname)s %(message)s")


def ensure_directories(config: PipelineConfig) -> None:
    config.runtime_root.mkdir(parents=True, exist_ok=True)
    config.gfs_dir.mkdir(parents=True, exist_ok=True)
    config.working_dir.mkdir(parents=True, exist_ok=True)
    config.prediction_dir.mkdir(parents=True, exist_ok=True)


def create_session() -> requests.Session:
    session = requests.Session()
    session.headers.update({"User-Agent": "HYSPLIT-Planthopper-Forecast/1.0"})
    return session


def parse_cycle_string(value: str) -> datetime:
    return datetime.strptime(value.strip(), "%H %Y%m%d")


def parse_cycle_from_path(path: Path) -> datetime:
    return datetime.strptime(path.stem, "%Y%m%d%H%M")


def fetch_available_cycles(session: requests.Session) -> list[datetime]:
    response = session.post(
        READY_CYCLE_URL,
        data={"metdata": "GFS", "xtype": "3"},
        timeout=30,
    )
    response.raise_for_status()
    cycle_values = re.findall(r'<option value="(.*?)">', response.text)
    if not cycle_values:
        raise RuntimeError("Could not find any READY GFS forecast cycles.")
    return [parse_cycle_string(value) for value in cycle_values]


def select_cycles(
    available_cycles: Sequence[datetime],
    count: int,
    cycle_hour: int | None,
) -> list[datetime]:
    if not available_cycles:
        raise RuntimeError("No GFS forecast cycles are available.")

    preferred_hour = available_cycles[0].hour if cycle_hour is None else cycle_hour
    preferred_cycles = [cycle for cycle in available_cycles if cycle.hour == preferred_hour]
    selected = preferred_cycles[:count]

    if len(selected) < count:
        seen = set(selected)
        for cycle in available_cycles:
            if cycle in seen:
                continue
            selected.append(cycle)
            seen.add(cycle)
            if len(selected) == count:
                break

    if len(selected) < count:
        raise RuntimeError(f"Only found {len(selected)} usable cycles, need {count}.")

    selected = sorted(selected)
    logging.info(
        "Selected %s GFS cycles: %s",
        len(selected),
        ", ".join(cycle.strftime("%Y-%m-%d %HZ") for cycle in selected),
    )
    return selected


def parse_ready_datetime(value: str) -> datetime:
    value = value.strip()
    legacy_formats = ["%H %d %b %Y", "%B %d, %Y at %H UTC"]
    for legacy_format in legacy_formats:
        try:
            return datetime.strptime(value, legacy_format)
        except ValueError:
            continue
    return parse_datetime(value).replace(tzinfo=None)


def fetch_cycle_span(session: requests.Session, cycle: datetime) -> tuple[datetime, datetime]:
    response = session.post(
        READY_SPAN_URL,
        data={
            "metdata": "GFS",
            "xtype": "3",
            "metext": "gfsf",
            "metcyc": cycle.strftime("%H %Y%m%d"),
        },
        timeout=30,
    )
    response.raise_for_status()
    span_match = READY_SPAN_PATTERN.search(response.text)
    if span_match is None:
        raise RuntimeError(f"Could not parse the data span for GFS cycle {cycle:%Y-%m-%d %H}.")
    start = parse_ready_datetime(span_match.group(1))
    end = parse_ready_datetime(span_match.group(2))
    return start, end


def request_cycle_extract(
    session: requests.Session,
    cycle: datetime,
    start: datetime,
    end: datetime,
) -> int:
    proc = random.randint(1000, 9999)
    response = session.post(
        READY_EXTRACT_URL,
        data={
            "metdata": "GFS",
            "mdatacfg": "GFSNH",
            "metdir1": f"/pub/forecast/{cycle:%Y%m%d}/",
            "metfil1": f"hysplit.t{cycle:%H}z.gfsf",
            "xtype": "3",
            "proc": proc,
            "cyc": cycle.strftime("%H"),
            "syr": start.strftime("%y"),
            "smn": start.strftime("%m"),
            "sdy": start.strftime("%d"),
            "shr": start.strftime("%H"),
            "eyr": end.strftime("%y"),
            "emn": end.strftime("%m"),
            "edy": end.strftime("%d"),
            "ehr": end.strftime("%H"),
            "latL": "0",
            "lonL": "80",
            "latR": "60",
            "lonR": "160",
        },
        timeout=30,
    )
    response.raise_for_status()
    return proc


def wait_for_extract_file(session: requests.Session, proc: int) -> str:
    max_attempts = 40
    for attempt in range(max_attempts):
        response = session.get(
            READY_RESULTS_URL,
            params={"proc": proc},
            timeout=30,
        )
        response.raise_for_status()
        file_match = READY_FILE_PATTERN.search(response.text)
        if file_match is not None:
            return file_match.group(1)

        sleep_seconds = min(3 * (2**attempt), 30)
        logging.info("Waiting for READY extract %s (%s/%s)...", proc, attempt + 1, max_attempts)
        time.sleep(sleep_seconds)

    raise TimeoutError(f"READY extract {proc} never produced a downloadable file.")


def extract_zip_to_bin(zip_path: Path, output_path: Path) -> None:
    if not zipfile.is_zipfile(zip_path):
        snippet = zip_path.read_text(encoding="utf-8", errors="ignore")[:500]
        raise RuntimeError(f"READY returned a non-zip payload: {snippet}")

    with zipfile.ZipFile(zip_path, "r") as archive:
        members = [name for name in archive.namelist() if name.endswith(".bin")]
        if not members:
            raise RuntimeError(f"No .bin payload found in {zip_path}.")
        with archive.open(members[0], "r") as source, output_path.open("wb") as target:
            shutil.copyfileobj(source, target)


def download_cycle(
    session: requests.Session,
    cycle: datetime,
    output_dir: Path,
    force_download: bool,
) -> Path:
    output_path = output_dir / f"{cycle:%Y%m%d%H00}.bin"
    if output_path.exists() and not force_download:
        logging.info("Reusing existing GFS file %s", output_path.name)
        return output_path

    start, end = fetch_cycle_span(session, cycle)
    proc = request_cycle_extract(session, cycle, start, end)
    relative_zip_url = wait_for_extract_file(session, proc)
    zip_url = f"{READY_BASE_URL}/{relative_zip_url.lstrip('/')}"
    temp_zip_path = output_dir / f"{proc}.zip"

    logging.info("Downloading %s to %s", zip_url, output_path.name)
    with session.get(zip_url, stream=True, timeout=60) as response:
        response.raise_for_status()
        with temp_zip_path.open("wb") as zip_file:
            for chunk in response.iter_content(chunk_size=8192):
                if chunk:
                    zip_file.write(chunk)

    try:
        extract_zip_to_bin(temp_zip_path, output_path)
    finally:
        if temp_zip_path.exists():
            temp_zip_path.unlink()

    return output_path


def prune_old_downloads(gfs_dir: Path, keep_count: int) -> None:
    meteorology_files = sorted(gfs_dir.glob("*.bin"), key=parse_cycle_from_path)
    obsolete_files = meteorology_files[:-keep_count]
    for file_path in obsolete_files:
        logging.info("Removing old GFS file %s", file_path.name)
        file_path.unlink()


def download_latest_gfs(config: PipelineConfig) -> list[Path]:
    ensure_directories(config)
    with create_session() as session:
        available_cycles = fetch_available_cycles(session)
        selected_cycles = select_cycles(available_cycles, config.download_count, config.cycle_hour)
        downloaded_files = [
            download_cycle(session, cycle, config.gfs_dir, config.force_download)
            for cycle in selected_cycles
        ]
    prune_old_downloads(config.gfs_dir, config.keep_downloads)
    return sorted(downloaded_files, key=parse_cycle_from_path)


def normalize_hysplit_dir(path: Path) -> str:
    return f"{path.resolve().as_posix()}/"


def resolve_hysplit_executable(hysplit_home: Path, executable_name: str) -> Path:
    exec_dir = hysplit_home / "exec"
    for candidate_name in (executable_name, f"{executable_name}.exe"):
        candidate = exec_dir / candidate_name
        if candidate.exists():
            return candidate
    raise FileNotFoundError(
        f"Could not find {executable_name} under {exec_dir}. "
        "Set HYSPLIT_HOME to a compatible HYSPLIT build."
    )


def run_command(command: Sequence[str], cwd: Path) -> None:
    logging.debug("Running %s in %s", " ".join(str(part) for part in command), cwd)
    subprocess.run(command, cwd=cwd, check=True)


def write_text_file(path: Path, content: str) -> None:
    path.write_text(content, encoding="utf-8", newline="\n")


def prepare_working_directory(working_dir: Path) -> None:
    if working_dir.exists():
        shutil.rmtree(working_dir)
    working_dir.mkdir(parents=True, exist_ok=True)


def remove_matching_files(working_dir: Path, patterns: Iterable[str]) -> None:
    for pattern in patterns:
        for file_path in working_dir.glob(pattern):
            if file_path.is_file():
                file_path.unlink()


def build_ascdata_content(hysplit_home: Path) -> str:
    lines = [
        "-90.0   -180.0   lat/lon of lower left corner",
        "1.0 1.0   lat/lon spacing in degrees",
        "180 360   lat/lon number of data points",
        "2   default land use category",
        "0.2   default roughness length (m)",
        normalize_hysplit_dir(hysplit_home / "bdyfiles"),
    ]
    return "\n".join(lines) + "\n"


def build_setup_content() -> str:
    lines = [
        "&SETUP",
        "tratio = 0.75,",
        "mgmin = 10,",
        "khmax = 9999,",
        "kmixd = 0,",
        "kmsl = 0,",
        "nstr = 0,",
        "mhrs = 9999,",
        "nver = 0,",
        "tout = 60,",
        "tm_tpot = 0,",
        "tm_tamb = 1,",
        "tm_rain = 0,",
        "tm_mixd = 0,",
        "tm_relh = 0,",
        "tm_sphu = 0,",
        "tm_mixr = 0,",
        "tm_dswf = 0,",
        "tm_terr = 0,",
        "dxf = 1.0,",
        "dyf = 1.0,",
        "dzf = 0.01,",
        "wvert = .TRUE.,",
        "/",
    ]
    return "\n".join(lines) + "\n"


def build_control_content(
    start_date: datetime,
    lat: float,
    lon: float,
    met_files: Sequence[Path],
    output_name: str,
) -> str:
    lines = [
        start_date.strftime("%y %m %d %H"),
        "1",
        f"{lat} {lon} 0.0",
        "36",
        "0",
        "10000.0",
        str(len(met_files)),
    ]
    for met_file in met_files:
        lines.append(normalize_hysplit_dir(met_file.parent))
        lines.append(met_file.name)
    lines.append("./")
    lines.append(output_name)
    return "\n".join(lines) + "\n"


def build_labels_content(output_name: str) -> str:
    lines = [
        f"TITLE, ### {output_name} ###",
        "MAPID, Values",
        "UNITS, %",
        "VOLUM,",
    ]
    return "\n".join(lines) + "\n"


def format_trajectory_row(row: tuple[object, ...]) -> str:
    values = [
        1,
        int(row[1]),
        int(row[2]),
        int(row[3]),
        int(row[4]),
        int(row[5]),
        int(row[6]),
        int(row[7]),
        row[8],
        row[9],
        row[10],
        row[11],
        row[12],
        row[13],
    ]
    widths = [6, 6, 6, 6, 6, 6, 6, 6, 8, 9, 9, 9, 9, 9]
    return "".join(str(value).rjust(width, " ") for value, width in zip(values, widths)) + "\n"


def parse_tdump_output(output_path: Path, met_file_count: int) -> tuple[pd.DataFrame, str]:
    tdump_frame = pd.read_csv(
        output_path,
        header=None,
        sep=r"\s+",
        skiprows=30 + met_file_count,
    )
    lines = output_path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    header = "".join(lines[: 1 + met_file_count])
    header += "     1 FORWARD  OMEGA  \n"
    header += lines[3 + met_file_count]
    header += "     2 PRESSURE AIR_TEMP\n"
    return tdump_frame, header


def exec_ensemble(
    start_date: datetime,
    lat: float,
    lon: float,
    met_files: Sequence[Path],
    config: PipelineConfig,
) -> pd.DataFrame:
    output_name = f"{lat}_{lon}_{start_date:%y%m%d%H}"
    working_dir = config.working_dir

    remove_matching_files(working_dir, ["ens_*", "tfreq.bin*"])
    write_text_file(working_dir / "ASCDATA.CFG", build_ascdata_content(config.hysplit_home))
    write_text_file(working_dir / "CONTROL", build_control_content(start_date, lat, lon, met_files, output_name))
    write_text_file(working_dir / "SETUP.CFG", build_setup_content())

    hyts_ens = resolve_hysplit_executable(config.hysplit_home, "hyts_ens")
    trajfreq = resolve_hysplit_executable(config.hysplit_home, "trajfreq")
    con2asc = resolve_hysplit_executable(config.hysplit_home, "con2asc")

    run_command([str(hyts_ens)], cwd=working_dir)

    output_path = working_dir / output_name
    if not output_path.exists():
        raise RuntimeError(f"HYSPLIT did not create the trajectory output {output_name}.")

    tdump_frame, header = parse_tdump_output(output_path, len(met_files))
    ensemble_ids = sorted(int(value) for value in tdump_frame[0].dropna().unique())
    ensemble_file_names: list[str] = []

    for ensemble_id in ensemble_ids:
        trajectory_lines = [header]
        for row in tdump_frame[tdump_frame[0] == ensemble_id].itertuples(index=False, name=None):
            if float(row[13]) < THERMAL_CUTOFF_K:
                break
            trajectory_lines.append(format_trajectory_row(row))

        ensemble_file_name = f"ens_{output_name}_{ensemble_id:02d}"
        write_text_file(working_dir / ensemble_file_name, "".join(trajectory_lines))
        ensemble_file_names.append(ensemble_file_name)

    write_text_file(working_dir / "INFILE", "\n".join(ensemble_file_names) + "\n")
    run_command(
        [str(trajfreq), "-ftfreq.bin", "-g0.5", "-iINFILE", "-r0", "-s0:99999"],
        cwd=working_dir,
    )
    write_text_file(working_dir / "LABELS.CFG", build_labels_content(output_name))
    run_command([str(con2asc), "-itfreq.bin", "-s"], cwd=working_dir)

    tfreq_path = working_dir / "tfreq.bin.txt"
    if not tfreq_path.exists():
        raise RuntimeError("con2asc did not create tfreq.bin.txt.")

    frequency_frame = pd.read_csv(tfreq_path, sep=r"\s+")

    if config.archive_raw_outputs:
        archive_dir = working_dir / "archive"
        archive_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(output_path, archive_dir / output_name)
        shutil.copy2(tfreq_path, archive_dir / f"{output_name}tfreq.bin.txt")

    removable = list(TRANSIENT_FILES) + ensemble_file_names + [output_name]
    remove_matching_files(working_dir, removable)
    return frequency_frame


def fool_griddata(df: pd.DataFrame, x_point_num: int, y_point_num: int) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    x_min, x_max = df["x"].min() - 0.5, df["x"].max() + 0.5
    y_min, y_max = df["y"].min() - 0.5, df["y"].max() + 0.5
    new_x_coord = np.linspace(x_min, x_max, x_point_num)
    new_y_coord = np.linspace(y_min, y_max, y_point_num)
    xx, yy = np.meshgrid(new_x_coord, new_y_coord)
    result = griddata(
        points=df[["x", "y"]].values,
        values=df["value"].values,
        xi=(xx, yy),
        method="linear",
    )
    return xx, yy, result


def calc_geojson(day: datetime, met_files: Sequence[Path], config: PipelineConfig) -> tuple[str | None, pd.DataFrame | None]:
    source_points = pd.read_csv(config.source_csv)[["LAT", "LON"]].values
    frequency_frames: list[pd.DataFrame] = []

    for lat, lon in source_points:
        for hour in (11, 20):
            try:
                frequency_frames.append(
                    exec_ensemble(
                        datetime(day.year, day.month, day.day, hour),
                        float(lat),
                        float(lon),
                        met_files,
                        config,
                    )
                )
            except Exception as exc:  # noqa: BLE001
                logging.warning(
                    "Failed to run %s-%02d for source (%s, %s): %s",
                    day.strftime("%Y-%m-%d"),
                    hour,
                    lat,
                    lon,
                    exc,
                )

    if not frequency_frames:
        return None, None

    frequency_ascii = pd.concat(frequency_frames, ignore_index=True)
    max_frequency = (
        frequency_ascii.groupby(["LAT", "LON"], as_index=False)["Freq99999"]
        .max()
        .rename(columns={"LAT": "y", "LON": "x", "Freq99999": "value"})
    )

    full_grid = pd.MultiIndex.from_product(
        [GRID_Y_VALUES, GRID_X_VALUES],
        names=["y", "x"],
    ).to_frame(index=False)
    max_frequency_grid = full_grid.merge(max_frequency, on=["y", "x"], how="left")
    max_frequency_grid["value"] = max_frequency_grid["value"].fillna(0.0)

    xx, yy, interpolated = fool_griddata(max_frequency_grid, 25, 25)
    interpolated = np.nan_to_num(interpolated)

    figure = plt.figure(figsize=(20, 20))
    axis = figure.add_subplot(111)
    axis.set_aspect("equal", adjustable="box")
    contourf = axis.contourf(
        xx,
        yy,
        interpolated,
        levels=CONTOUR_LEVELS,
        colors=CONTOUR_COLORS,
    )
    geojson_text = geojsoncontour.contourf_to_geojson(
        contourf=contourf,
        ndigits=3,
        stroke_width=0,
        fill_opacity=0.7,
    )
    plt.close(figure)

    geojson_text = geojson_text.replace(
        '"fill":"#ffffff","fill-opacity":0.7',
        '"fill":"#ffffff","fill-opacity":0',
    )
    return geojson_text, max_frequency_grid


def newest_meteorology_files(gfs_dir: Path, count: int) -> list[Path]:
    meteorology_files = sorted(gfs_dir.glob("*.bin"), key=parse_cycle_from_path)
    if len(meteorology_files) < count:
        raise RuntimeError(
            f"Need at least {count} GFS files in {gfs_dir}, found {len(meteorology_files)}."
        )
    return meteorology_files[-count:]


def run_prediction(config: PipelineConfig) -> list[str]:
    ensure_directories(config)
    prepare_working_directory(config.working_dir)
    meteorology_files = newest_meteorology_files(config.gfs_dir, config.download_count)
    latest_cycle = parse_cycle_from_path(meteorology_files[-1])
    logging.info(
        "Using %s newest GFS files ending at %s",
        len(meteorology_files),
        latest_cycle.strftime("%Y-%m-%d %H:%M"),
    )

    available_days: list[str] = []
    forecast_start = latest_cycle - timedelta(days=config.forecast_days_before)
    forecast_end = latest_cycle + timedelta(days=config.forecast_days_after)

    for day in pd.date_range(forecast_start.date(), forecast_end.date(), freq="D"):
        current_day = day.to_pydatetime()
        logging.info("Generating forecast for %s", current_day.strftime("%Y-%m-%d"))
        geojson_output, max_frequency_grid = calc_geojson(current_day, meteorology_files, config)
        if geojson_output is None or max_frequency_grid is None:
            logging.warning("No usable forecast output for %s", current_day.strftime("%Y-%m-%d"))
            continue

        day_key = current_day.strftime("%Y%m%d")
        max_frequency_grid.to_csv(
            config.prediction_dir / f"{day_key}_max_freq.csv",
            index=False,
        )
        write_text_file(config.prediction_dir / f"{day_key}.json", geojson_output)
        available_days.append(current_day.strftime("%Y-%m-%d"))

    write_text_file(
        config.prediction_dir / "geojson_span.json",
        json.dumps(available_days, ensure_ascii=False),
    )
    logging.info("Finished writing %s forecast day(s).", len(available_days))
    return available_days


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run the planthopper forecast pipeline inside this repository.",
    )
    parser.add_argument("--log-level", default="INFO", help="Logging level (default: INFO).")

    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--hysplit-home", default=str(DEFAULT_HYSPLIT_HOME))
    common.add_argument("--runtime-root", default=str(DEFAULT_RUNTIME_ROOT))
    common.add_argument("--gfs-dir", default=str(DEFAULT_GFS_DIR))
    common.add_argument("--working-dir", default=str(DEFAULT_WORKING_DIR))
    common.add_argument("--prediction-dir", default=str(DEFAULT_PREDICTION_DIR))
    common.add_argument("--source-csv", default=str(DEFAULT_SOURCE_CSV))
    common.add_argument("--download-count", type=int, default=5)
    common.add_argument("--forecast-days-before", type=int, default=3)
    common.add_argument("--forecast-days-after", type=int, default=9)
    common.add_argument("--keep-downloads", type=int, default=7)
    common.add_argument("--cycle-hour", type=int, choices=range(0, 24), default=None)
    common.add_argument("--archive-raw-outputs", action="store_true")
    common.add_argument("--force-download", action="store_true")

    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("download", parents=[common], help="Download the recent GFS files.")
    subparsers.add_parser("predict", parents=[common], help="Generate prediction/*.json and CSV files.")
    subparsers.add_parser("run", parents=[common], help="Download GFS data and then generate predictions.")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    configure_logging(args.log_level)
    config = build_config(args)

    try:
        if args.command == "download":
            download_latest_gfs(config)
        elif args.command == "predict":
            run_prediction(config)
        elif args.command == "run":
            download_latest_gfs(config)
            run_prediction(config)
        else:
            parser.error(f"Unsupported command: {args.command}")
    except Exception as exc:  # noqa: BLE001
        logging.exception("Forecast pipeline failed: %s", exc)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())