Readme - bci_clear_daily_ra.txt

- The file bci_cl_ra_man.txt contains daily rainfall data collected by STRI at the BCI clearing station beginning in 1972.
- The file bci_acp_ra_1929_1971.txt contains daily rainfall data collected by the Panama Canal Authority at the BCI clearing station 
  from 1929 to 1971.

Regarding the STRI data...

- pr_ra is prorated (see the meaning below) daily rainfall total in millimeters (mm)
- man_ra is raw daily total rainfall collected with manual gauges in millimeters (mm)

Manual rainfall is collected with a National Weather Service standard type rain gauge and it is read by
a technician during most weekdays (except weekends and holidays) Usually between 8:00 and 9:00 a.m., but sometimes as late as 2-3pm if it has been raining in the morning. On average, the gauge is read at 8:55am.

Electronically recorded tipping bucket data are made at the same location. 

At different times one, two and even three rain gauges have been used - on the late seventies an eighties there were
three rain gauges at the BCI clearing. An average of the daily total rainfall from all available manual gauges is used as 
the value for manual daily rainfall.

The file that you have received contains both the raw average daily rainfall total, as well as prorated daily rainfall total that uses 
tipping bucket data to fill in the missing manual data.

The tipping bucket rainfall data are used to proportionately distribute the manual rain gauge data for those days when readings were not made, i.e. manual rainfall data is allocated according to the distribution of the tipping bucket rainfall during the same time intervals that, on average, the manual readings would have been made (from 9:00am on the previous day to 8:55am on the current day). 

The prorated rainfall is always exactly equal to the rainfall collected by the rain gauge.

E.g.

Date(yyyy-mm-dd hh:mm),prorated rainfall(mm),raw rainfall(mm)
2015-06-17 08:19,0.000,0.000
2015-06-18 08:35,5.969,5.969
2015-06-19 08:50,0.000,-9
2015-06-20 08:50,0.000,-9
2015-06-21 08:50,0.529,-9
2015-06-22 09:10,2.646,3.175
2015-06-23 08:45,0.000,0.000
2015-06-24 08:36,0.000,0.000
2015-06-25 08:44,0.000,0.000
2015-06-26 08:32,0.000,0.000