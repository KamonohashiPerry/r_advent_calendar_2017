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
                  


