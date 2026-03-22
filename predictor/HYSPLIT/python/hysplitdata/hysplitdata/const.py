# ---------------------------------------------------------------------------
# NOAA Air Resources Laboratory
#
# const.py
#
# Declares constants used by this package.
# ---------------------------------------------------------------------------


class HeightUnit:
    METERS = 0
    FEET = 1


class VerticalCoordinate:
    NOT_SET = -1
    PRESSURE = 0
    ABOVE_GROUND_LEVEL = 1
    THETA = 2
    METEO = 3
    NONE = 4
