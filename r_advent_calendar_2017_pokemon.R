library(rvest)
library(tidyverse)
library(magrittr)

# htmlソースコードを読み込む
pokemon_ranking <- read_html("https://yakkun.com/sm/status_list.htm")

# class属性がtdタグのノードを抽出
node_extracted <- html_nodes(pokemon_ranking, "td")

# ノードからテキストを抽出して行列にする
pokemon_data <- data.frame(matrix(html_text(node_extracted),
                       ncol = 9,byrow = TRUE))

# 列名をつける
pokemon_data <- pokemon_data %>% 
                set_colnames(c('id', 'name', 'Hit_Points', 'Attack',
                               'Defense', 'Special_Attack',
                               'Special_Defense', 'Speed', 'Total'))





