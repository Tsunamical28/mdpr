# Constants Built after Utils ---------------------------------------------

# Default dates based on date that package is attached
c_close_date <- Sys.Date()
c_close_date <- as.Date(dbQuery(paste0(
        "SELECT CalendarDate
        FROM [Architect].[dbo].[CAL_BusDay]
        WHERE NextBusinessDate = '", c_close_date, "'")))
assign("c_close_date", c_close_date, mdpr_globals)
c_as_of_datetime <- Sys.time()
lubridate::hour(c_as_of_datetime) <- 23;
lubridate::minute(c_as_of_datetime) <- 59;
lubridate::second(c_as_of_datetime) <- 59
assign("c_as_of_datetime", c_as_of_datetime, mdpr_globals)
attach(mdpr_globals)