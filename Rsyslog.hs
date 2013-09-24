module Main where

import Data.Time
import Text.JSON
import Text.ParserCombinators.Parsec

import Common


apache :: String -> UTCTime -> (Maybe Int) -> GenParser Char st LogEntry
apache tag localDate pid = do
  ip <- ipAddress
  string " - - "
  remoteDate <- apacheDate
  char ' ' >> char '"'
  -- drop " from tail
  msg <- message >>= \s -> return $ take (length s - 1) s
  return $ LogEntry { logTag = tag,
                      logLocalDate =  localDate,
                      logRemoteDate = remoteDate,
                      logLevel = "INFO",
                      logPid = pid,
                      logPythonComponent = Nothing,
                      logMessage = msg }


nova :: String -> UTCTime -> (Maybe Int) -> GenParser Char st LogEntry
nova tag localDate pid = do
  remoteDate <- pythonAsctime
  char ' '
  level <- pythonLogLevel
  char ' '
  c <- component
  msg <- message
  return $ LogEntry { logTag = tag,
                      logLocalDate =  localDate,
                      logRemoteDate = remoteDate,
                      logLevel = level,
                      logPid = pid,
                      logPythonComponent = Just c,
                      logMessage = msg }


cinder :: String -> UTCTime -> (Maybe Int) -> GenParser Char st LogEntry
cinder tag localDate pid = do
  remoteDate <- date "%Y-%m-%d %H:%M:%S"
  skipMany (char ' ')
  level <- pythonLogLevel
  char ' '
  c <- cinderComponent
  char ' '
  msg <- message
  return $ LogEntry { logTag = tag,
                      logLocalDate = localDate,
                      logRemoteDate = remoteDate,
                      logLevel = level,
                      logPid = pid,
                      logPythonComponent = Just c,
                      logMessage = msg }
  where cinderComponent = do
          char '['
          c <- many1 (noneOf "]")
          char ']'
          return c



rsyslogEntry :: GenParser Char st LogEntry
rsyslogEntry = do
  localDate <- ubuntuRsyslogDateTime
  char ' '
  hn <- hostname <?> "hostname"
  char ' '
  t <- tag <?> "rsyslog tag: logger -t parameter"
  p <- pid <?> "pid"
  char ':' >> char ' '
  case t of
    "httpd" -> apache t localDate p
    "nova-api" -> nova t localDate p
    "nova-conductor" -> nova t localDate p
    "nova-compute" -> nova t localDate p
    "nova-scheduler" -> nova t localDate p
    "cinder-api" -> cinder t localDate p
    "cinder-scheduler" -> cinder t localDate p
    "cinder-volume" -> cinder t localDate p
    _ -> fail $ "unexpected rsyslog tag: " ++ t


shescape :: GenParser Char st ()
shescape = do
  char '#' <?> "start of escape sequence: #"
  count 3 digit
  char '['
  count 2 digit
  optionMaybe (char ';' >> count 2 digit)
  char 'm'
  return ()


skipShEscapes :: GenParser Char st String
skipShEscapes =
  try (eof >> return "") <|>
  try (shescape >> skipShEscapes) <|>
    (anyChar >>= \c -> (skipShEscapes >>= \t -> return (c : t)))


cleanLine line =
  case parse skipShEscapes "line" line of
    Right s -> s
    Left e -> line


main = do
  log <- getContents >>= return . lines
  putStrLn $ encode $ map (parseLogEntry rsyslogEntry . cleanLine) log
  return ()