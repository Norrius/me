{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Calendar where

import Import
import Data.Time.Calendar (Day, gregorianMonthLength, addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Data.Aeson.Types (withObject, parseMaybe)

trd :: (a, b, c) -> c
trd (_, _, x) = x

-- | Lists all available calendars. Like the rest of the handlers here,
-- handles auth and returns demo calendar to non-authenticated users.
getCalendarsR :: Handler Html
getCalendarsR = do
    maybeUserId <- maybeAuthId
    calendars <- runDB $ case maybeUserId of
        Just userId -> selectList [CalendarUserId ==. userId] [Asc CalendarName]
        Nothing -> selectList [CalendarId ==. demoCalendar] [] -- hardcoded demo calendar
    defaultLayout $ do
        setTitle "Calendars"
        $(widgetFile "calendars")

-- | Helper function that extracts a calendar from the DB or returns 404,
-- including demo calendar for non-authenticated users.
getCalendar :: CalendarId -> Handler Calendar
getCalendar calendarId = do
    maybeUserId <- maybeAuthId
    maybeCalendar <- runDB $ case maybeUserId of
        Just userId -> selectFirst [CalendarId ==. calendarId,
                                    CalendarUserId ==. userId] []
        Nothing -> selectFirst [CalendarId ==. calendarId,-- hardcoded demo calendar
                                CalendarId ==. demoCalendar] [] -- hackity hack
    case maybeCalendar of
        Just (Entity _ record) -> return record
        Nothing -> notFound

-- | Produces a calendar view for a specific month.
getCalendarMonthR :: CalendarId -> Integer -> Int -> Handler Html
getCalendarMonthR calendarId year month = do
    calendar <- getCalendar calendarId

    -- get calendar events
    events <- runDB $
        selectList [CalendarEventCalendar ==. calendarId,
                    CalendarEventDate >=. (fromGregorian year month 1),
                    CalendarEventDate <=. (fromGregorian year month 31)] [Asc CalendarEventDate]
    let eventDays = [trd . toGregorian . calendarEventDate $ event
                    | Entity _ event <- events]

    -- setup the values needed to build a calendar
    let firstDay = fromGregorian year month 1
    let (_, _, firstDOW) = toWeekDate firstDay
    let firstDayOffset = firstDOW - 1
    let firstDayOffsetL = [1..firstDayOffset]  -- apparently you cannot iterate 0..n in Hamlet
    let monthLengthL = [1..(gregorianMonthLength year month)] -- but you can iterate through a list
    let prevMonth = addGregorianMonthsClip (-1) firstDay
    let nextMonth = addGregorianMonthsClip 1 firstDay

    -- get today
    today <- liftIO getToday
    let (cYear, cMonth, cDay) = toGregorian today
    let eventClass = eventClass' eventDays cYear cMonth cDay year month

    defaultLayout $ do
        setTitle "Calendar"
        $(widgetFile "calendar")

-- | CSS class for an event's calendar cell
eventClass' :: [Int] -> Integer -> Int -> Int -> Integer -> Int -> Int -> String
eventClass' eventDays cYear cMonth cDay year month day
    | (cYear, cMonth, cDay) == (year, month, day) = eventType ++ " today"
    | otherwise = eventType
    where
        eventType = if elem day eventDays then "event-1" else "event-0"

-- | Redirects to month view.
getCalendarR :: CalendarId -> Handler Html
getCalendarR calendarId = do
    (year, month, _) <- liftIO getToday >>= return . toGregorian
    redirect (CalendarMonthR calendarId year month)

-- | Toggles an event for a calendar-date pair.
-- Accepts POST body in the form {"date": "2018-12-31"}
postCalendarR :: CalendarId -> Handler Value
postCalendarR calendarId = do
    -- requireJsonBody parses the body to Value or returns a 400 status code.
    value <- requireJsonBody
    let maybeDate = do
            let parser = withObject "request" $ \o -> o .: "date"
            strDate <- parseMaybe parser value :: Maybe String
            parseDay $ unpack strDate
    date <- case maybeDate of
        Just x -> return x
        _ -> invalidArgs ["`date` must be YYYY-MM-DD"]

    _ <- getCalendar calendarId  -- access check

    let requestEvent = CalendarEvent calendarId date
    maybeDbEvent <- runDB $
        selectFirst [CalendarEventCalendar ==. (calendarEventCalendar requestEvent),
                     CalendarEventDate ==. (calendarEventDate requestEvent)] []

    runDB $ case maybeDbEvent of
        Just dbEvent -> delete (entityKey dbEvent)
        Nothing -> insert_ requestEvent

    return $ toJSON ("ok"::Text)

-- https://github.com/yesodweb/yesod/wiki/Formatting-dates
formatDay :: Day -> String
formatDay day = formatTime defaultTimeLocale "%d.%m.%Y" $ UTCTime day 0

formatMonth :: Day -> String
formatMonth day = formatTime defaultTimeLocale "%b %Y" $ UTCTime day 0

parseDay :: String -> Maybe Day
parseDay = parseTime defaultTimeLocale "%F"

-- https://wiki.haskell.org/Getting_the_current_date
-- today :: IO (Integer,Int,Int) -- :: (year,month,day)
-- today = getCurrentTime >>= return . toGregorian . utctDay

-- https://techoverflow.net/2014/06/13/get-current-year-month-day-in-haskell/
getToday :: IO Day
getToday = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let localNow = utcToLocalTime timezone now
    return $ localDay localNow
