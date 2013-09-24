module Common where


import Text.ParserCombinators.Parsec
import Text.JSON
import System.Locale
import Data.Time
import Data.Time.Format


newtype Nodename = Nodename [String]


shortMonth :: GenParser Char st String
shortMonth = string "Jan"
         <|> string "Feb"
         <|> string "Mar"
         <|> string "Apr"
         <|> string "May"
         <|> string "Jun"
         <|> string "Jul"
         <|> string "Aug"
         <|> string "Sep"
         <|> string "Oct"
         <|> string "Nov"
         <|> string "Dec"


data DateFormatItem = DateFormatYear
                    | DateFormatMonth
                    | DateFormatShortMonth
                    | DateFormatDay
                    | DateFormatHour
                    | DateFormatMinute
                    | DateFormatSecond
                    | DateFormatString String
                      deriving Show

parseDateFormat :: String -> [DateFormatItem]
parseDateFormat ['%'] = error "unexpected end of format string"
parseDateFormat [] = []
parseDateFormat (x:y:xs) | x == '%' = selectFormatChar y : parseDateFormat xs
    where selectFormatChar c =
            case c of
              'Y' -> DateFormatYear
              'm' -> DateFormatMonth
              'b' -> DateFormatShortMonth
              'd' -> DateFormatDay
              'H' -> DateFormatHour
              'M' -> DateFormatMinute
              'S' -> DateFormatSecond
              '%' -> DateFormatString "%"
              _ -> error $ "bad date format char: " ++ [c]
parseDateFormat (x:xs) = (DateFormatString [x]) : parseDateFormat xs

optimizeDateFormat :: [DateFormatItem] -> [DateFormatItem]
optimizeDateFormat [] = []
optimizeDateFormat ((DateFormatString x):(DateFormatString y):xs) =
  optimizeDateFormat $ (DateFormatString (x ++ y)) : xs
optimizeDateFormat (x:xs) = x : optimizeDateFormat xs

dateFormat = optimizeDateFormat . parseDateFormat

clearDateFormat :: [DateFormatItem] -> [DateFormatItem]
clearDateFormat [] = []
clearDateFormat (DateFormatString _ : xs) = clearDateFormat xs
clearDateFormat (x : xs) = x : clearDateFormat xs

dateFormatToString :: [DateFormatItem] -> String
dateFormatToString [] = ""
dateFormatToString (x:xs) = show' x ++ dateFormatToString xs
    where show' DateFormatYear = "%Y"
          show' DateFormatMonth = "%m"
          show' DateFormatShortMonth = "%b"
          show' DateFormatDay = "%d"
          show' DateFormatHour = "%H"
          show' DateFormatMinute = "%M"
          show' DateFormatSecond = "%S"
          show' (DateFormatString s) = s


date :: String -> GenParser Char st UTCTime
date format = do
  let fmt = dateFormat format
  parse "" fmt fmt
  where parse value fmt [] = return $
          readTime defaultTimeLocale (dateFormatToString (clearDateFormat fmt)) value
        parse value fmt (fitem : tail_) = do
          parsed <- case fitem of
            DateFormatYear -> count 4 digit <?> "year, eg. 2012"
            DateFormatMonth -> count 2 digit <?> "month, eg. 09"
            DateFormatShortMonth -> shortMonth <?> "short month, eg. Seb"
            DateFormatDay -> count 2 digit <?> "day, eg. 24"
            DateFormatHour -> count 2 digit <?> "hour, eg. 11"
            DateFormatMinute -> count 2 digit <?> "minute, eg. 23"
            DateFormatSecond -> count 2 digit <?> "second, eg. 54"
            (DateFormatString s) -> string s >> return ""
          parse (value ++ parsed) fmt tail_


-- RFC 1123 - http://tools.ietf.org/html/rfc1123
hostnameLabelChar = digit
                <|> letter
                <|> char '-' -- hyphen

hostnameLabel :: GenParser Char st String
hostnameLabel = many1 hostnameLabelChar

hostname :: GenParser Char st Nodename
hostname =  sepBy1 hostnameLabel (char '.') >>= return . Nodename


ubuntuRsyslogDateTime :: GenParser Char st UTCTime
ubuntuRsyslogDateTime = date "%b %d %H:%M:%S"


pythonComponentName :: GenParser Char st String
pythonComponentName = do
    char '('
    name <- many1 (noneOf ")")
    char ')'
    return name


component :: GenParser Char st String
component = many1 (noneOf " \t\r\n")


pythonLogLevel :: GenParser Char st String
pythonLogLevel = string "DEBUG"
             <|> string "INFO"
             <|> string "WARNING"
             <|> string "ERROR"
             <|> string "CRITICAL"
             <|> string "AUDIT"  -- nova specific


pythonAsctime :: GenParser Char st UTCTime
pythonAsctime = do
    dt <- date "%Y-%m-%d %H:%M:%S"
    try (char ',') <|> char '.'
    ms <- count 3 digit <?> "milliseconds, eg. 234"
    return dt


apacheDateTime :: GenParser Char st UTCTime
apacheDateTime = date "%d/%b/%Y:%H:%M:%S"


timezone :: GenParser Char st String
timezone = do
  m <- optionMaybe (char '-')
  zone <- count 4 digit
  case m of
    Nothing -> return zone
    Just m -> return (m : zone)


message :: GenParser Char st String
message = many (noneOf "\r\n")


pid :: GenParser Char st (Maybe Int)
pid = do
  optionMaybe pid'
  where pid' = do
          char '['
          pid <- many1 digit
          char ']'
          return $ read pid


tag :: GenParser Char st String
tag = many1 (noneOf ":[ \r\n")


data LogEntry = LogEntry { logTag :: String,
                           logLocalDate :: UTCTime,
                           logRemoteDate :: UTCTime,
                           logLevel :: String,
                           logPid :: Maybe Int,
                           logPythonComponent :: Maybe String,
                           logMessage :: String }
              | BadEntry String String
                deriving Show


parseLogEntry :: (GenParser Char () LogEntry) -> String -> LogEntry
parseLogEntry parser line =
    case parse parser "(line)" line of
        Right entry -> entry
        Left e -> BadEntry line (show e)


instance JSON LogEntry where
  showJSON (BadEntry entry error) = JSObject $ toJSObject [
    ("entry", JSString $ toJSString entry),
    ("error", JSString $ toJSString error) ]

  showJSON le = JSObject $ toJSObject $ [
    ("tag", JSString $ toJSString (logTag le)),
    ("localDate", JSString $ toJSString (show $ logLocalDate le)),
    ("remoteDate", JSString $ toJSString (show $ logRemoteDate le)),
    ("level", JSString $ toJSString (logLevel le)),
    ("message", JSString $ toJSString (logMessage le))
    ]
    ++ case logPythonComponent le of
           Just component -> [("component", JSString $ toJSString component)]
           Nothing -> []
    ++ case  logPid le of
           Just pid -> [("pid", JSString $ toJSString (show pid))]
           Nothing -> []


ipAddress :: GenParser Char st (String,String,String,String)
ipAddress = do
  p1 <- two_or_one
  char '.'
  p2 <- two_or_one
  char '.'
  p3 <- two_or_one
  char '.'
  p4 <- two_or_one
  return (p1, p2, p3, p4)
  where
    two_or_one = try (count 2 digit) <|> count 1 digit


apacheDate :: GenParser Char st UTCTime
apacheDate = do
  char '['
  dt <- date "%d/%b/%Y:%H:%M:%S"
  optionMaybe (char ' ' >> timezone)
  char ']'
  return dt