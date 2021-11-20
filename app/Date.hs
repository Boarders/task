module Date where


-- chronos
import Chronos

dateToday :: IO Date
dateToday = dayToDate <$> today
