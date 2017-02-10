Readme - bci_manual_evapcl

- The file bci_manual_evapcl.txt contains evapotranspiration data collected with an atmometer at the BCi clearing station.

- The atmometer values are read daily between 8:00 and 9:00 a.m.

- No values are collected on the weekends and holidays.

- evap_raw is the evapotranspiration value read from the instrument.
  -999 means that the instrument was not read - no value collected.

- reset is the reading after a refill of the instrument. When the water column is nearly empty the atmometer has to be refilled.
  -999 means no refill.

- Daily evapotranspiration is calculated by subtracting yesterday's evap_raw value from today's value.
  If there was refill then the reset value is subtracted from today's value.  

- This file provides both the original estimated ET, as well as prorated ET that backfills the ET 
  for days when the instrument was not read - the ET is re-distributed (divided) among the days with no data.
  
- Evapotranspiration readings and the calculated evapcl are in centimeters (cm).     


