{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}

module UserDB
    ( User(..), getSingleUser, getAllUsers, addUser, updateUser)
    where

import Control.Exception
import Data.Text (Text)
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (close)
import Database.SQLite.Simple as SQLite
-- import Database.SQLite.Simple.Types
import Data.Typeable
-- import Text.RawString.QQ
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Data.String (fromString)
import Data.Aeson

data User =
    User {
        userId :: Integer
        , username :: Text
        , shell :: Text
        , homeDirectory :: Text
        , realName :: Text
        , phone :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance ToRow User where
    toRow (User id_ uname sh hDir rName ph) =
        toRow (id_, uname, sh, hDir, rName, ph)

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User 0
        <$> v .: "username"
        <*> v .: "shell"
        <*> v .: "homeDirectory"
        <*> v .: "realName"
        <*> v .: "phone"

-- createUsers :: Query
-- createUsers = [r|
-- CREATE TABLE IF NOT EXISTS users
--     (id INTEGER PRIMARY KEY AUTOINCREMENT,
--     username TEXT UNIQUE,
--     shell TEXT, homeDirectory TEXT,
--     realName TEXT, phone TEXT)
-- |]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

-- type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn uname = do
    results <- query conn getUserQuery (Only uname)
    case results of
        [] -> return $ Nothing
        [user] -> return $ Just user
        _ -> throwIO DuplicateData

addUser :: User -> IO Bool
addUser u = do
    conn <- getConnection
    n <- query conn hasUser (Only $ username u) :: IO [Only Integer]
    b <- case n of
        [] -> (execute conn insertUser $ toRow u) >> return True
        _ -> return False
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
    return b

-- createDatabase :: IO ()
-- createDatabase = do
--     conn <- getConnection
--     execute_ conn createUsers
--     SQLite.close conn

getConnection :: IO Connection
getConnection = open "finger.db"

getSingleUser :: Text -> IO (Maybe User)
getSingleUser name = do
    conn <- getConnection
    result <- getUser conn name
    SQLite.close conn
    return result

getAllUsers :: IO [User]
getAllUsers = do
    conn <- getConnection
    rows <- query_ conn allUsers
    SQLite.close conn
    return rows

hasUser :: Query
hasUser = "SELECT 1 FROM users where username = ?"

updateUser :: String
           -> Maybe String
           -> Maybe String
           -> Maybe String
           -> Maybe String
           -> IO Bool
updateUser name sh hDir rName ph = do
    let xs = [sh, hDir, rName, ph]
        columns = zip ["shell", "homeDirectory", "realName", "phone"] xs
        newData = catMaybes $ fmap sequenceA columns
    case newData of
        [] -> return True
        ys -> do
            let pairs = fmap (\(a,b) -> a ++ "=\"" ++ b ++ "\"" ) ys
                part = concat $ intersperse "," pairs
                full = "update users set " ++ part
                        ++ " where username = \"" ++ name ++ "\""
            conn <- getConnection
            n <- query conn hasUser (Only name) :: IO [Only Integer]
            b <- case n of
                [] -> return False
                _ ->  execute conn (fromString full) (Only name) >> return True
            SQLite.close conn
            return b

-- updateU :: User -> IO ()
-- update