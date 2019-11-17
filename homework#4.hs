

data Author = Author {         
                        name_a :: String,
                        tracks_a :: [Track],
                        albums_a :: [Album]
                     } deriving(Show)

data Track = Track { 
                    name_t :: String,
                    author_t :: String,
                    album_t :: String,
                    duration :: [Int]
                   } deriving (Show)

data Album = Album {
                    name_al :: String,
                    tracks_al :: [Track],
                    year_al :: Int
                     } deriving(Show)

data User = User {
                   id_u :: Int,
                   favourites_u :: [Track],
                   favouriteAuthors_u :: [Author],
                   last_listened :: [Track],
                   playlists :: [Album]
                   } deriving(Show)


findTrack :: String -> [Author] -> [Track] -> [Track]
findTrack _ [] acc = acc
findTrack name (x:list) acc = findTrack name list ((findTrackInAuthor x []) ++ acc) where
    findTrackInAuthor (Author _ tracks _ ) ac = filter (\t -> (name_t) t == name) tracks

addAuthor :: User -> Author -> User
addAuthor (User id fav favA ll pl) author = User id fav (author:favA) ll pl

addAlbum :: User -> Album -> User
addAlbum (User id fav favA ll pl) album = User id fav favA ll (album:pl)

addTrack :: User -> Track -> User
addTrack (User id fav favA ll pl) track = User id (track:fav) favA ll pl




--aut = Author "Lil Pump" [lil, pump] [lil_ep]

--lil_ep = Album "Lil_ep" [lil, pump] 1920

--lil = Track "D rose" "Lil Pump" "Lil_EP" [2, 30, 33]
--pump = Track "Gucci Gang" "Lil Pump" "Lil_EP" [2, 2, 20]
