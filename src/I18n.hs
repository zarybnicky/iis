{-# LANGUAGE OverloadedStrings #-}
module I18n where

import Data.Text (Text, pack)
import Data.Time (utc)
import Data.Time.Format (TimeLocale(..), defaultTimeLocale)
import Data.Time.Format.Human (HumanTimeLocale(..))
import Message (AppMessage(..))

-- | Provide 'HumanTimeLocale' according to given language list.
selectHumanTimeLocale :: [Text]
                               -> (AppMessage -> String)
                               -> HumanTimeLocale
selectHumanTimeLocale ("nl":_) = dutchHumanTimeLocale
selectHumanTimeLocale ("it":_) = italianHumanTimeLocale
selectHumanTimeLocale ("ru":_) = russianHumanTimeLocale
selectHumanTimeLocale (_:rest) = selectHumanTimeLocale rest
selectHumanTimeLocale _        = defaultHumanTimeLocale

defaultHumanTimeLocale :: (AppMessage -> String) -> HumanTimeLocale
defaultHumanTimeLocale render =
  HumanTimeLocale
  { justNow = render MsgTimeJustNow
  , secondsAgo = \_ x -> render . MsgTimeSecondsAgo $ pack x
  , oneMinuteAgo = \_ -> render MsgTimeOneMinuteAgo
  , minutesAgo = \_ x -> render . MsgTimeMinutesAgo $ pack x
  , oneHourAgo = \_ -> render MsgTimeOneHourAgo
  , aboutHoursAgo = \_ x -> render . MsgTimeAboutHoursAgo $ pack x
  , at = \_ x -> render . MsgTimeAt $ pack x
  , daysAgo = \_ x -> render . MsgTimeDaysAgo $ pack x
  , weekAgo = \_ x -> render . MsgTimeWeekAgo $ pack x
  , weeksAgo = \_ x -> render . MsgTimeWeeksAgo $ pack x
  , onYear = render . MsgTimeOnYear . pack
  , locale = defaultTimeLocale
  , dayOfWeekFmt = render MsgDayOfWeekFmt
  , thisYearFmt = "%b %e"
  , prevYearFmt = "%b %e, %Y"
  , timeZone = utc
  }

italianHumanTimeLocale :: (AppMessage -> String) -> HumanTimeLocale
italianHumanTimeLocale r =
  (defaultHumanTimeLocale r)
  { locale =
      TimeLocale
      { wDays =
          [ ("Domenica", "Do")
          , ("Lunedì", "Lu")
          , ("Martedì", "Ma")
          , ("Mercoledì", "Me")
          , ("Giovedì", "Gi")
          , ("Venerdì", "Ve")
          , ("Sabato", "Sa")
          ]
      , months =
          [ ("Gennaio", "Gen")
          , ("Febbraio", "Feb")
          , ("Marzo", "Mar")
          , ("Aprile", "Apr")
          , ("Maggio", "Mag")
          , ("Giugno", "Giu")
          , ("Luglio", "Lug")
          , ("Agosto", "Ago")
          , ("Settembre", "Set")
          , ("Ottobre", "Ott")
          , ("Novembre", "Nov")
          , ("Dicembre", "Dic")
          ]
      , amPm = ("AM", "PM")
      , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
      , dateFmt = "%d-%m-%y"
      , timeFmt = "%H:%M:%S"
      , time12Fmt = "%H:%M:%S"
      , knownTimeZones = []
      }
  }

russianHumanTimeLocale :: (AppMessage -> String) -> HumanTimeLocale
russianHumanTimeLocale r =
  (defaultHumanTimeLocale r)
  { thisYearFmt = "%e %b"
  , prevYearFmt = "%e %b %Y"
  , locale =
      TimeLocale
      { wDays =
          [ ("Воскресенье", "Вс")
          , ("Понедельник", "Пн")
          , ("Вторник", "Вт")
          , ("Среда", "Ср")
          , ("Четверг", "Чт")
          , ("Пятница", "Пт")
          , ("Суббота", "Сб")
          ]
      , months =
          [ ("Январь", "Янв")
          , ("Февраль", "Фев")
          , ("Март", "Мар")
          , ("Апрель", "Апр")
          , ("Май", "Май")
          , ("Июнь", "Июн")
          , ("Июль", "Июл")
          , ("Август", "Авг")
          , ("Сентябрь", "Сен")
          , ("Октябрь", "Окт")
          , ("Ноябрь", "Ноя")
          , ("Декабрь", "Дек")
          ]
      , amPm = ("am", "pm")
      , dateTimeFmt = "%a, %e %b %Y %H:%M:%S %Z"
      , dateFmt = "%d.%m.%y"
      , timeFmt = "%H:%M:%S"
      , time12Fmt = "%H:%M:%S"
      , knownTimeZones = []
      }
  }

dutchHumanTimeLocale :: (AppMessage -> String) -> HumanTimeLocale
dutchHumanTimeLocale r =
  (defaultHumanTimeLocale r)
  { locale =
      TimeLocale
      { wDays =
          [ ("Zondag", "Zo")
          , ("Maandag", "Ma")
          , ("Dinsdag", "Di")
          , ("Woensdag", "Wo")
          , ("Donderdag", "Do")
          , ("Vrijdag", "Vr")
          , ("Zaterdag", "Za")
          ]
      , months =
          [ ("Januari", "Jan")
          , ("Februari", "Feb")
          , ("Maart", "Mar")
          , ("April", "Apr")
          , ("Mei", "Mei")
          , ("Juni", "Jun")
          , ("Juli", "Jul")
          , ("Augustus", "Aug")
          , ("September", "Sep")
          , ("Oktober", "Okt")
          , ("November", "Nov")
          , ("December", "Dec")
          ]
      , amPm = ("am", "pm")
      , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
      , dateFmt = "%d-%m-%y"
      , timeFmt = "%H:%M:%S"
      , time12Fmt = "%H:%M:%S"
      , knownTimeZones = []
      }
  }
