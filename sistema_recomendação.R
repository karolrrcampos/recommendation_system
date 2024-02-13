# Carregando pacotes 
library(plotly)
library(ggthemes)
library(paletteer)
library(lubridate)
library(dplyr)
library(zipcodeR)
library(DescTools)
library(ggplot2)
library(dplyr)
library(recommenderlab)
library(data.table)
library(purrr)
library(tibble)
library(ggplot2)
library(forcats)
library(tidyr)

# # Carregando os datasets
data <- read.table("../input/ml-100k/u.data", 
                   header = F, 
                   col.names = c("user_id", "movie_id", "rating", "timestamp"))
usuario <- read.csv("../input/ml-100k/u.user", 
                    sep = "|", 
                    header = F, 
                    col.names = c("user_id", "age", "sex", "occupation", "zipcode"))
item <- read.csv("../input/ml-100k/u.item", 
                 sep = "|", 
                 header = F, 
                 col.names = c("movie_id", "movie_title" ,"release_date","video_release_date", "IMDb_URL", "unknown", "action", "adventure","animation", "children\"s", "comedy", "crime", "documentary", "drama", "fantasy","film-Noir", "horror", "musical", "mystery", "romance", "sci-fi", "thriller", "war", "western"))

# Unindo os datasets
dados <- full_join(full_join(data, usuario, by = "user_id"), item, by = "movie_id")

# ---------------------------
# Pré-Processamento dos Dados

# As informações secundárias mais comumente usadas incluem TIMESTAMP, que registram os momentos exatos em que os usuários interagem com um item. Como informações contextuais importantes, os TIMESTAMP podem ser usados para modelar os interesses dinâmicos dos usuários e para lidar com a evolução das preferências do usuário. 
dados$datetime <- as_datetime(dados$timestamp)

# Separar o dadostime em informações mais detalhadas
dados$years <- year(dados$datetime)
dados$months <- month(dados$datetime, label = TRUE, abbr = FALSE)
dados$wday <- wday(dados$datetime, label = TRUE, abbr = FALSE)
dados$period <- case_when(
  hour(dados$datetime) >= 00 & hour(dados$datetime) < 06 ~ "madrugada",
  hour(dados$datetime) >= 06 & hour(dados$datetime) < 12 ~ "manhã",
  hour(dados$datetime) >= 12 & hour(dados$datetime) < 18 ~ "tarde",
  hour(dados$datetime) >= 18 ~ "noite"
)
# Transformando zipcode em localidades
city_state <- reverse_zipcode(dados$zipcode)
city_state <- city_state[ ,-c(2,3,5:24)]
city_state <- city_state %>%
  separate(post_office_city, c("city", "state_abb"), sep = ",")
city_state$state_abb <- trimws(city_state$state_abb)
city_state$state <- state.name[sapply(city_state$state_abb, \(x) which(x == state.abb)[1])]

# Muitas localidades não foram encontradas pelo zipcodeR. Sendo assim, irei pesquisar online e como não tenho muitas informações sobre sobre os usuários, pesquisarei os códigos postais com filtro nos estados unidos.
# fonte: https://zipcodebase.com/
city_state$state[city_state$zipcode == "00000"] <- "Missouri"
city_state$state[city_state$zipcode == "17604"] <- "Pennsylvania"
city_state$state[city_state$zipcode == "28814"] <- "North Carolina"
city_state$state[city_state$zipcode == "32067"] <- "Florida"
city_state$state[city_state$zipcode == "37901"] <- "Tennessee"
city_state$state[city_state$zipcode == "58644"] <- "North Dakota"
city_state$state[city_state$zipcode == "59717"] <- "Montana"
city_state$state[city_state$zipcode == "84408"] <- "Utah"
city_state$state[city_state$zipcode == "90840"] <- "California"
city_state$state[city_state$zipcode == "94143"] <- "California"
city_state$state[city_state$zipcode == "95161"] <- "California"
city_state$state[city_state$zipcode == "99687"] <- "Alaska"
city_state <- city_state[ ,-c(2,3)]
city_state <- city_state %>%
  distinct(zipcode, .keep_all = TRUE)
dados <- dados %>%
  left_join(city_state,
            by = 'zipcode',
            multiple = "all"
  ) %>%
  relocate(state, .after = occupation)

dados <- dados %>%
  mutate(age_Range = case_when(
    age <= 12 ~ "Crianças",
    (age >= 12 & age <= 18) ~ "Adolescentes",
    (age >= 19 & age <= 59) ~ "Adultos",
    age >= 60 ~ "Idosos"
  )) %>%
  relocate(age_Range, .after = age)

# Separar release_date em dia, mês e ano
dados <- separate(dados, col = release_date, into = c("release_day", "release_month", "release_year"), sep ="-")

# Remover variáveis que não serão usadas
dados <- dados[ ,-c(4,10,12,15,16,36)]

# reorganizando os dados
dados <- dados[ ,c(1,4,5,6,7,8,3,31,32,33,34,2,9,10,11,12:30)]
# Verificando a ocorrência de NAs em cada variável
print(colSums(is.na(dados)))

dados[is.na(dados)] <- ""
dados[dados == ""] <- "Unknown"
summary(dados)

# ---------------------------
# Análise Exploratória

Mode(dados$movie_title)

Mode(dados$user_id, na.rm=TRUE)

sum(unique(dados$movie_id))

sum(unique(dados$user_id), na.rm=TRUE)

## Análise dos usuários
# Distribuição de Gêneros
table(dados$sex)

#  Média de avaliação por gênero (sexo)
tapply(dados$rating, dados$sex, mean)

# Contagem de faixa etária por gênero (sexo)
table(dados$age_Range, dados$sex)

# Filmes mais bem avaliados
bemAvaliados <- dados[, c("rating", "movie_title")] %>%
  group_by(movie_title) %>%
  summarise(mean_rating = round(mean(rating), 2),
            count = n()) %>%
  mutate(ratio = round(prop.table(count) * 100, 2)) %>%
  arrange(desc(count))

bemAvaliados %>%
  filter(count >= 350) %>%
  ggplot(aes(x = reorder(movie_title, mean_rating), y = count)) +
  geom_bar(stat = "identity", fill = "lightcyan2") +
  coord_flip() +
  geom_text(aes(label = mean_rating), vjust = 0.5, size = 3.5, col = "darkslategray") +
  labs(title = "Total Avaliações e Média das Notas por Filmes",
       x = NULL, 
       y = "Contagem de Avaliações Por Filme") +
  scale_fill_manual(values = color) +
  geom_text(aes(label = count), y = 15, size = 3.5, col = "#00688B") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  theme(panel.grid = element_line(color = "gray96")) 

# Contagem e frequencia de gêneros dos filmes
freq_abs <- lapply(dados[, 16:34], function(x) sum(x == 1))
freq_rel <- lapply(freq_abs, function(x) paste0(round((x / sum(dados[, 16:34] == 1)) * 100, 3), "%"))

nome_generos <- colnames(dados[, 16:34])

frequencia <- tibble(
  Gêneros = rep(nome_generos, each = length(freq_abs[[1]])),
  freq_absoluta = as.numeric(unlist(freq_abs)),
  freq_relativa = (unlist(freq_rel))
) %>%
  arrange(desc(freq_absoluta))

color <- paletteer::paletteer_dynamic("cartography::pink.pal", 19, direction = 1)

ggplot(data = frequencia, aes(x = reorder(Gêneros, freq_absoluta), y = freq_absoluta, fill = freq_relativa)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color) +
  geom_text(aes(label = freq_relativa), vjust = -0.5, size = 3.5, col = "deeppink4") +
  labs(title = "Frequências Absolutas e Relativas por Gêneros Cinematográficos",
       x = NULL,
       y = NULL) +
  theme_minimal()+
  theme(legend.position = "none") +
  theme(panel.grid = element_line(color = "gray96")) 

# Por avaliação - Distribuição de Avaliações
dados_agregados <- dados %>%
  group_by(rating) %>%
  summarise(total_rating = sum(rating)) %>%
  arrange(total_rating)

color <- paletteer::paletteer_c("ggthemes::Purple", 7)

ggplot(dados_agregados, aes(x = as.factor(rating), y = total_rating, fill = as.factor(rating))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = total_rating), vjust = -0.5, size = 3.5, col = "deeppink4") + 
  labs(title = "Distribuição das Notas Atribuídas pelos Usuários",
       x = NULL, 
       y = NULL) +
  scale_fill_manual(values = color) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),
        panel.grid = element_line(color = "gray94"))

# Correlação entre gêneros dos filmes
corGenero <- cor(dados[, 16:34])
corrplot(corGenero, 
         order = 'hclust', 
         addrect = 2, 
         col = COL2("PiYG", 30), 
         tl.cex = 0.8, 
         tl.srt = 45, 
         tl.col = "black")

# Contagem das avaliações de filmes por dia da semana
dados_agregados <- dados %>%
  group_by(wday) %>%
  summarise(count = n()) %>%
  arrange(count)

color <- paletteer::paletteer_d("ggsci::cyan_material", 7)

ggplot(dados_agregados, aes(x = reorder(factor(wday), count), y = count, fill = factor(wday))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5, color = "darkslategray") + 
  labs(title = "Frequência Diária das Avaliações",
       x = NULL, 
       y = NULL) +
  scale_fill_manual(values = color) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),
        panel.grid = element_line(color = "gray94"))

# Contagem avaliações mês a mês
dados_agregados <- dados %>%
  group_by(months) %>%
  summarise(count = n()) %>%
  arrange(count)  

color <- paletteer::paletteer_d("ggsci::teal_material", 8)

ggplot(dados_agregados, aes(x = factor(months, levels = months), y = count, fill = factor(months))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5, col = "#004D40") +
  labs(title = "Frequência Mensal das Avaliações",
       x = NULL, 
       y = NULL) +
  scale_fill_manual(values = color) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_line(color = "gray94"))

# Contagem de lançamentos ao longo do tempo
dados_agregados <- dados %>%
  group_by(release_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

color <- paletteer::paletteer_dynamic("cartography::wine.pal", 20)

ggplot(dados_agregados, aes(x = as.factor(release_year), y = count, fill = as.factor(release_year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5, col = "red4") +
  labs(title = "Frequência Anual de Lançamentos Cinematográficos",
       x = NULL, 
       y = NULL) +
  scale_fill_manual(values = color) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_line(color = "gray94"))

# Avaliações por faixa etária e gênero:
dados_agregados <- dados[, c("rating", "age_Range", "sex")] %>%
  group_by(age_Range, sex) %>%
  summarise(mean_rating = round(mean(rating), 2), .groups = "keep") %>%
  arrange(desc(mean_rating))

ggplot(dados_agregados, aes(x = age_Range, y = mean_rating, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, color = "black",linewidth = 0.4) +
  geom_text(aes(label = mean_rating), position = position_dodge(width = 0.5), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("M" = "skyblue3", "F" = "palevioletred4")) + 
  labs(title = "Pontuação Média dos Filmes, considerando Gênero e Faixa Etária dos Usuários",
       x = NULL,
       y = NULL,
       fill = "Sexo") +
  theme_minimal()+
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  theme(panel.grid = element_line(color = "gray96")) 

# Avaliações por profissão dos usuários
dados_agregados <- dados %>%
  group_by(rating, occupation) %>%
  summarise(total_rating = sum(rating), .groups = "keep")%>%
  arrange(desc(total_rating)) %>%
  head(20)

ggplot(dados_agregados, aes(x = occupation, y = total_rating, fill = factor(rating))) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = total_rating), position = position_stack(vjust = 0.5), size = 3.5, color = "black") +  
  scale_fill_manual(values = c("3" = "snow2", "4" = "snow3", "5" = "snow4")) +
  labs(title = "Frequência das Avaliações Segundo a Profissão dos Usuários",
       x = NULL,
       y = NULL,
       fill = "Avaliação") +
  theme_minimal()+
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_blank(),  
        axis.title.y = element_blank()) +
  theme(panel.grid = element_line(color = "gray96")) 

# Identificação de gêneros mais assistidos por localidade - top 3 por localidade
genero_localidade <- dados[ ,c(6,16:34)] %>%
  group_by(state) %>%
  summarise_all(sum) %>%
  pivot_longer(cols = -state, names_to = "nome_generos", values_to = "contagem") %>%
  arrange(state, desc(contagem)) %>%
  group_by(state) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 1) %>%
  select(state, nome_generos, contagem)%>%
  arrange(desc(contagem))

states_map <- map_data("state")%>% 
  mutate(region = tools::toTitleCase(region))

merged_data <- merge(states_map, genero_localidade, by.x = "region", by.y = "state")

color <- paletteer::paletteer_c("ggthemes::Classic Red-Blue", 30)

p <- ggplot(merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(stat = "identity",
               color = "black", 
               linewidth = 0.2,
               aes(fill = contagem, text = paste("Estado:", region,
                                                 "<br>",
                                                 "Gênero:", nome_generos,
                                                 "<br>",
                                                 "Contagem:", contagem)),
               size = 3) + 
  scale_fill_gradientn(colors = color) +
  guides(fill = FALSE) +
  labs(title = "Popularidade dos Gêneros Cinematográficos por Localidade",
       x = NULL,
       y = NULL,
       fill = "Gênero") +
  theme_minimal()

ggplotly(p, tooltip = "text")

# Avaliações por gênero e período do dia
dados_agregados <- dados[, c("rating", "period", "sex")] %>%
  group_by(period, sex) %>%
  summarise(mean_rating = round(mean(rating), 2), .groups = "keep") %>%
  arrange(desc(mean_rating))

ggplot(dados_agregados, aes(x = period, y = mean_rating, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, color = "black",linewidth = 0.4) +
  geom_text(aes(label = mean_rating), position = position_dodge(width = 0.5), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("M" = "skyblue3", "F" = "palevioletred4")) + 
  labs(title = "Média das Avaliações por Gênero dos Usuários e Horário das Avaliações",
       x = NULL,
       y = "Média Avaliações",
       fill = "Sexo") +
  theme_minimal()+
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  theme(panel.grid = element_line(color = "gray96")) 

# Contagem de gênero dos filmes assistidos por ocupação do usuário
dados_agregados <- dados[ ,c(5,16:34)] %>%
  group_by(occupation) %>%
  summarise_all(sum) %>%
  pivot_longer(cols = -occupation, names_to = "nome_generos", values_to = "contagem") %>%
  arrange(occupation, desc(contagem)) %>%
  group_by(occupation) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 2) %>%
  select(occupation, nome_generos, contagem)

ggplot(dados_agregados, aes(x = contagem, y = occupation, fill = nome_generos)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = contagem), position = position_dodge(width = 0.9), hjust = -0.3, vjust = 0.6, size = 3.5, col = "red4") +
  scale_fill_manual(values = c("action" = "deepskyblue4", "comedy" = "orange", "drama" = "snow3", "thriller" = "firebrick"))+
  labs(title = "Gêneros dos Filmes Mais Populares Entre as Diferentes Profissões",
       x = NULL,
       y = NULL,
       fill = "Gêneros") +  
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(0, 8000, 1000)) +
  theme(panel.grid = element_line(color = "gray96")) 

# ---------------------------
# Pré-Tratamento Para Recomendação

# Remover dados duplicados
dados[,c(1,5,6,8,9,10,12,13,14,15)] <- lapply(dados[,c(1,5,6,8,9,10,12,13,14,15)], as.character)
dados[,c(3,4,11)] <- lapply(dados[,c(3,4,11)], as.factor)
dados[,c(2,7)] <- lapply(dados[,c(2,7)], as.numeric)
dados[,c(16:34)] <- lapply(dados[,c(16:34)], as.logical)

movie <- dados %>%
  group_by(user_id, movie_id) %>%
  summarise(rating = mean(rating), .groups = "keep")

movie$movie_id %>% 
  unique() %>%
  length #1682

movie$user_id %>% 
  unique() %>%
  length #943

as(as.data.frame(movie), "realRatingMatrix") -> rating_mtx

scheme <- evaluationScheme(rating_mtx, 
                           method="cross-validation", 
                           k = 5, 
                           given=-1,
                           goodRating = 4)

models_to_evaluate <- list(
  IBCF_Cosine = list(name = "IBCF", param = list(method = "Cosine")), 
  IBCF_Pearson = list(name = "IBCF", param = list(method = "Pearson")), 
  IBCF_Euclidean = list(name = "IBCF", param = list(method = "Euclidean")),
  
  
  Random  = list(name = "Random" ), 
  Popular = list(name = "Popular"),
  
  
  SVD = list(name = "SVD"),
  ALS = list(name = "ALS")
)

n_recommendations <- c(1,3,5,10,20)

resultado <- evaluate(scheme, 
                      method = models_to_evaluate, 
                      n =n_recommendations)

avg_conf_matr <- function(resultado) {
  tmp <- resultado %>%
    getConfusionMatrix() %>%
    as.list()
  as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5, 10, 20)) %>%
    select("n", "precision", "recall", "TPR", "FPR") 
}

resultados <- resultado %>%
  map(avg_conf_matr) %>%
  enframe() %>%
  unnest(cols = "value")

colnames(resultados) <- c('algorithm', 'n', 'Precision', 'Recall', 'Sensitivity', 'Specificity')

# Curva ROC
resultados %>%
  ggplot(aes(Specificity, Sensitivity, colour = fct_reorder2(as.factor(algorithm), Specificity, Sensitivity))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# Curva precisão x recall
resultados %>%
  ggplot(aes(Recall, Precision, 
             colour = fct_reorder2(as.factor(algorithm), Specificity, Sensitivity))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# ---------------------------
# Sistema de Filtragem Colaborativa e Construção do Sistema de Recomendação

# Dividindo os dados em conjunto de treino e de teste
set.seed(123)
sampled_data <- sample(x = c(TRUE, FALSE),
                       size = nrow(rating_mtx),
                       replace = TRUE,
                       prob = c(0.8, 0.2))
training_data <- rating_mtx[sampled_data, ]
testing_data <- rating_mtx[!sampled_data, ]

modelo_recomendacao <- Recommender(data = training_data,
                                   method = "POPULAR",
                                   param = list(k = 30))
modelo_recomendacao

predicao <- predict(object = modelo_recomendacao,
                    newdata = testing_data,
                    n = 5)

user1 <- predicao@items

recommendation_matrix <- sapply(predicao@items,
                                function(x) { as.integer(colnames(rating_mtx)[x]) })

# Transpor a matriz e adicionar a coluna id_user
transposed_matrix <- t(recommendation_matrix)
transposed_matrix <- as.data.frame(transposed_matrix)
transposed_matrix$id_user <- as.integer(row.names(transposed_matrix)) 

# Reorganizar as colunas para ter 'id_user' como a primeira coluna
transposed_matrix <- transposed_matrix[, c(ncol(transposed_matrix), 1:(ncol(transposed_matrix)-1))]

rownames(transposed_matrix) <- NULL

# Extrair todos os items de predicao
predicao <- predict(object = modelo_recomendacao,
                    newdata = testing_data,
                    n = 5)

user1 <- predicao@items
all_movies <- list()

# Iterar sobre os vetores de itens de cada usuário
for (usuario in user1) {
  movies_user <- predicao@itemLabels[usuario]
  all_movies <-c(all_movies, list(movies_user))
}

df <- do.call(rbind, all_movies)
df <- as.data.frame(df)
user_id <- testing_data@data@Dimnames[[1]]
df <- mutate(df, user_id) %>%
  relocate(user_id, .before = V1)

df <- df %>%
  rename("movie_recomm_1" = V1,
         "movie_recomm_2" = V2,
         "movie_recomm_3" = V3,
         "movie_recomm_4" = V4,
         "movie_recomm_5" = V5)

id_to_movies <- setNames(dados$movie_title, dados$movie_id)
df[ ,c(2:6)] <- lapply(df[ ,c(2:6)], function(ids){
  valid_ids <- ids[ids %in% names(id_to_movies)]
  if (length(valid_ids) > 0) {
    id_to_movies[ids]
  } else {
    character(0)
  }
})