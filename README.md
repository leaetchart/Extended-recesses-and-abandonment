# Extended-recesses-and-abandonment

Read_me: Original databasis: df.
•	nest_id: unique combined ID of the nest, composed of the first four letters of the species name (SAND), the year, and the field nest ID.
•	fate: fate of each nest, i.e., ‘H’ for hatched nests producing young at the end of the incubation period, or ‘A’ for abandoned nests.
•	date_on: beginning of each recorded day YY-MM-DD HH:MM:SS.
•	date_off: end of recorded day YY-MM-DD HH:MM:SS.
•	TDR_extended: Total duration of extended recesses (≥ 120 min) per day (in minutes).
•	TDR_short: Total duration of short recesses (< 120 min) per day (in minutes) 
•	NR_extended: Number of extended recesses (≥ 120 min) performed for each recorded day.
•	NR_short: Number of short recesses (< 120 min) performed for each recorded day.
•	nest_age: incubation stage. The first day of incubation is assigned day 0, the second day of incubation is assigned day 1, third day 3… to keep track of the incubation advancement.
•	number: number of days that each nest is monitored.
•	TDR_hour: Total duration of extended recesses (≥ 120 min) per day (in hours).

Additional columns in df1: 
•	std_TDR: Standardised total duration of extended recesses (TDR_hour) divided by the number of days monitored (number).

Additional columns in df2:
•	day_to_fate: day of the fate is coded as the first day (1), the day before the fate is coded as the second day (2) etc, to have the incubation stage but from the end.
