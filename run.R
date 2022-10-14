options(box.path = getwd())
box::use(R/routes)



# just trying things out
api_key <- readLines("~/.cache/api_keys/bing_maps.txt")

map <- routes$routes(c("G26QQ", "EH129EB"), api_key) # TODO test works with only 2 locations

map <- routes$routes(c("G26QQ", "EH129EB", "FK95LF", "G630JE", "FK70LJ"), api_key)

# Shortest Distance Route
map[map$dist == min(map$dist),]

# Shortest Time Route
map[map$dura == min(map$dura),]
