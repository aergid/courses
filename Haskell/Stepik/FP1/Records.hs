import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {
        timestamp  :: UTCTime,
        logLevel :: LogLevel,
        message :: String
    }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString = do
    time <- timeToString . timestamp
    lvl <- logLevelToString . logLevel
    msg <- message
    return $ time ++ ": " ++ lvl ++ ": " ++ msg


data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

abbrFirstName :: Person -> Person
abbrFirstName person@(Person {firstName = name}) =  person {firstName = newName} where
    newName = if (length name < 2) then name else (head name) : "."
