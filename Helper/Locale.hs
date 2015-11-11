module Helper.Locale
    ( belgiumLocale
    , TimeLocale (..)
    ) where

import Data.Time.Format

belgiumLocale :: TimeLocale
belgiumLocale = TimeLocale {
    wDays  = [("zondag",    "zo"),  ("maandag",  "ma"),
              ("dinsdag",   "ma"),  ("woensdag", "wo"),
              ("donderdag", "di"),  ("vrijdag",  "vr"),
              ("zaterdag",  "za")],

    months = [("januari",   "jan"), ("februari",  "feb"),
              ("maart",     "mrt"), ("april",     "apr"),
              ("mei",       "mei"), ("juni",      "jun"),
              ("juli",      "jul"), ("augustus",  "aug"),
              ("september", "sep"), ("oktober",   "okt"),
              ("november",  "nov"), ("december",  "dec")],

    amPm = ("AM", "PM"),
    dateTimeFmt = "%a %e %b %H:%M %Y",
    dateFmt = "%A %e %b",
    timeFmt = "%H:%M",
    time12Fmt = "%I:%M:%S %p",
    knownTimeZones  = []
    }
