{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Calendar where

import Import
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Database.Persist.Sql (fromSqlKey)

trd :: (a, b, c) -> c
trd (_, _, x) = x

getCalendarsR :: Handler Html
getCalendarsR = do
    calendars <- runDB $ selectList ([]::[Filter Calendar]) []
    defaultLayout $ do
        setTitle "Calendars"
        $(widgetFile "calendars")

getCalendarNoMonthR :: CalendarId -> Handler Html
getCalendarNoMonthR calendarId = do
    (year, month, _) <- liftIO getToday >>= return . toGregorian
    redirect (CalendarR calendarId year month)

getCalendarR :: CalendarId -> Integer -> Int -> Handler Html
getCalendarR calendarId year month = do
    calendar <- runDB $ get404 calendarId
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

-- Toggles an event for a calendar-date pair.
postCalendarsR :: Handler Value
postCalendarsR = do
    -- requireJsonBody will parse the request body into the appropriate type,
    -- or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    requestEvent <- (requireJsonBody :: Handler CalendarEvent)
    maybeDbEvent <- runDB $
        selectFirst [CalendarEventCalendar ==. (calendarEventCalendar requestEvent),
                     CalendarEventDate ==. (calendarEventDate requestEvent)] []

    runDB $ case maybeDbEvent of
        Just dbEvent -> delete (entityKey dbEvent)
        Nothing -> insert_ requestEvent

    return $ toJSON ("ok"::String)

-- CSS class for an event's calendar cell
eventClass' :: [Int] -> Integer -> Int -> Int -> Integer -> Int -> Int -> String
eventClass' eventDays cYear cMonth cDay year month day
    | (cYear, cMonth, cDay) == (year, month, day) = eventType ++ " today"
    | otherwise = eventType
    where
        eventType = if elem day eventDays then "event-1" else "event-0"

-- https://github.com/yesodweb/yesod/wiki/Formatting-dates
formatDay :: Day -> String
formatDay day = formatTime defaultTimeLocale "%d.%m.%Y" $ UTCTime day 0

formatMonth :: Day -> String
formatMonth day = formatTime defaultTimeLocale "%b %Y" $ UTCTime day 0

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
