box::use(R/routes)

# requires Bing API key from https://www.microsoft.com/en-us/maps/choose-your-bing-maps-api

# just trying things out
api_key <- readLines("~/.cache/api_keys/bing_maps.txt")

map <- routes$routes(c("G26QQ", "EH129EB"), api_key) 

map <- routes$routes(c("G26QQ", "EH129EB", "FK95LF", "G630JE", "FK70LJ"), api_key)

# Shortest Distance Route
map[map$dist == min(map$dist),]

# Shortest Time Route
map[map$dura == min(map$dura),]
