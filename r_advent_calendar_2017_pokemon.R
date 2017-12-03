library(rvest)

pokemon_ranking <- read_html("https://yakkun.com/sm/status_list.htm")

html_structure(pokemon_ranking)
as_list(pokemon_ranking)


# class属性がtdタグ
title_nodes <- html_nodes(pokemon_ranking, "td")

length(title_nodes)

text <- html_text(title_nodes)
print(text)


pokemon_data <- matrix(text,ncol = 9,byrow = TRUE)



