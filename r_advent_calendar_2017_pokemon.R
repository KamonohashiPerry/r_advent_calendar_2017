library(rvest)
library(tidyverse)
library(magrittr)
library(reshape2)

# htmlソースコードを読み込む
pokemon_ranking <- read_html("https://yakkun.com/sm/status_list.htm")

# class属性がtdタグのノードを抽出
node_extracted <- html_nodes(pokemon_ranking, "td")

# ノードからテキストを抽出して行列にして、名前以外を数値に型変換して、変数名を変更する。
pokemon_data <- data.frame(matrix(html_text(node_extracted),
                       ncol = 9,byrow = TRUE),stringsAsFactors = FALSE) %>% 
                set_colnames(c('id', 'name', 'Hit_Points', 'Attack',
                               'Defense', 'Special_Attack',
                               'Special_Defense', 'Speed', 'Total')) %>%
                mutate_at(vars(-name), as.numeric)


# ポケモン別のページを取得するためのURLの取得
pokemon_link <- pokemon_ranking %>% html_nodes("td") %>% html_nodes('a') %>% html_attr('href')
pokemon_link <- gsub(x = pokemon_link,
                     pattern = './zukan',
                     replacement = "https://yakkun.com/sm/zukan")
# ポケモン別のURLを先ほどのデータに加える
pokemon_data <- pokemon_data %>% mutate(url = pokemon_link)


# ポケモンの種族値ランキング
pokemon_data_ranking <- pokemon_data %>% arrange(desc(Total))
pokemon_data_ranking <- pokemon_data_ranking %>% mutate(ranking = 1:n())


# 集計
pokemon_data_melt <- melt(pokemon_data, id.vars = 'name')
pokemon_data_melt %>% 
    group_by(variable) %>% 
    summarise(mean = mean(value),
              median = median(value),
              sd = sd(value),
              max = max(value),
              min = min(value),
              cv = sd/mean)

# Box-Plotを描く
ggplot(data = pokemon_data_melt %>% filter(!(variable %in% c('id','Total'))),
       aes(x = variable, y = value)) +
   geom_boxplot() + ggtitle("Tribal value") + 
   theme(plot.title = element_text(hjust = 0.5))

# Violin-Plotを描く
ggplot(data = pokemon_data_melt %>% filter(!(variable %in% c('id','Total'))),
       aes(x = variable, y = value)) +
  geom_violin() + geom_boxplot(width=0.1,color="red") + ggtitle("Tribal value") + 
  theme(plot.title = element_text(hjust = 0.5))



pokemon_data_standarized <- pokemon_data
pokemon_data_standarized <- pokemon_data_standarized %>% 
                              mutate_at(vars(Hit_Points,
                                             Attack,
                                             Defense,
                                             Special_Attack,
                                             Special_Defense,
                                             Speed),funs(scale(.) %>% as.vector))

pokemon_data_standarized <- pokemon_data_standarized %>% 
                              mutate(Total = rowSums(select(.,c(3:8))))

pokemon_data_standarized_ranking <- pokemon_data_standarized %>% arrange(desc(Total))
pokemon_data_standarized_ranking <- pokemon_data_standarized_ranking %>% mutate(standarized_ranking = 1:n())


pokemon_data_standarized_melt <- melt(pokemon_data_standarized, id.vars = 'name')
pokemon_data_standarized_melt %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value),
            cv = sd/mean)


# Box-Plotを描く
ggplot(data = pokemon_data_standarized_melt %>% filter(!(variable %in% c('id','Total','Total_standarized'))),
       aes(x = variable, y = value)) +
  geom_boxplot() + ggtitle("Tribal value") + 
  theme(plot.title = element_text(hjust = 0.5))


ranking_gap <- pokemon_data_ranking %>% 
                select(id,name,ranking) %>% 
                left_join(pokemon_data_standarized_ranking %>% 
                  select(name,standarized_ranking),by='name') %>% 
                  mutate(gap = abs(ranking - standarized_ranking))
                  

pokemon_detail_database <- data.frame(url = as.character(),
                                      name = as.character(),
                                      rarity = as.integer(),
                                      experience = as.integer())


# ポケモン別のURLからゲットしやすさなどを抽出するための関数
Pokemon_Detail_Get <- function(pokemon_url){
  pokemon_detail <- read_html(pokemon_url)
  
  # XPathで名前とゲットしやすさと経験値タイプを取得
  node_extracted_pokemon_name <- pokemon_detail %>% html_nodes(xpath="//tr[1]") %>% html_text()
  node_extracted_pokemon_name <- node_extracted_pokemon_name[1]
  
  node_extracted_pokemon_get <- pokemon_detail %>% html_nodes(xpath="//tr[24]/td[2]") %>% html_text()
  node_extracted_pokemon_get <- as.integer(gsub(x = node_extracted_pokemon_get[1], pattern = "\u00A0", replacement = ""))
  
  node_extracted_pokemon_exp <- pokemon_detail %>% html_nodes(xpath="//tr[26]/td[2]") %>% html_text()
  node_extracted_pokemon_exp <- as.integer(gsub(x = node_extracted_pokemon_exp[1], pattern = "万", replacement = "0000"))
  
  pokemon_detail_data <- data.frame(url = pokemon_url,
                                    name = node_extracted_pokemon_name,
                                    rarity = node_extracted_pokemon_get,
                                    experience = node_extracted_pokemon_exp)
  return(pokemon_detail_data)
  
  Sys.sleep(20)
}


# ポケモン別のページをスクレイピングする
pokemon_detail_database <- map_dfr(pokemon_link ,
                               ~Pokemon_Detail_Get(.))

# 重複したURLを削除する
pokemon_detail_database <- pokemon_detail_database %>% distinct(url, .keep_all = TRUE)

# 種族値のデータとゲットしやすさなどのデータを繋ぎこむ
pokemon_data <- pokemon_data %>% left_join(pokemon_detail_database %>% select(-name), by ="url")

# ゲットしやすさのヒストグラム
ggplot(data = pokemon_data, aes(x = rarity)) + geom_histogram() 

# 経験値のヒストグラム
ggplot(data = pokemon_data, aes(x = experience)) + geom_histogram() 

ggplot(data = pokemon_data, aes(x = experience, y = Total)) + geom_point()


