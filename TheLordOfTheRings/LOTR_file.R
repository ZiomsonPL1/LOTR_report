library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(stringr)
library(visNetwork)

LOTR_characters <- read.csv("C:/Users/Paawel/Desktop/TheLordOfTheRings/lotr_characters.csv")
LOTR_scripts <- read.csv("C:/Users/Paawel/Desktop/TheLordOfTheRings/lotr_scripts.csv")


LOTR_characters <- LOTR_characters %>% arrange(name)
LOTR_scripts <- LOTR_scripts %>% arrange(char)

LOTR_characters_liczba_postaci <- LOTR_characters %>% 
  group_by(race, gender) %>% 
  summarise(liczba_postaci = n()) %>%
  mutate(gender = str_replace(gender, "Most likely male", "Male"),
         gender = str_replace(gender, "Males", "Male"),
         gender = str_to_upper(gender)) %>%
  filter(race != "", gender != "")

plot1 <- ggplot(LOTR_characters_liczba_postaci, aes(x = race, y = liczba_postaci, color = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Liczba postaci ze względu na rasę i płeć",
       x = "Rasa",
       y = "Liczba") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 25))
plot1


spouse_edges <- LOTR_characters %>% 
  filter(!str_detect(spouse, regex("unknown|none|unnamed|^$|loved", ignore_case = TRUE))) %>%
  select(from = name, to = spouse)

nodes <- spouse_edges %>%
  select(label = from) %>%
  bind_rows(spouse_edges %>% select(label = to)) %>%
  distinct() %>%
  mutate(id = row_number()) %>%
  distinct(id, .keep_all = TRUE)

edges <- spouse_edges %>%
  left_join(nodes, by = c("from" = "label")) %>%
  rename(from_id = id) %>%
  left_join(nodes, by = c("to" = "label")) %>%
  rename(to_id = id) %>%
  select(from = from_id, to = to_id) %>%
  filter(!is.na(from), !is.na(to))

nodes <- nodes %>% mutate(title = label)

visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123) %>%
  visPhysics(stabilization = TRUE) %>%
  visInteraction(hover = TRUE) %>%
  visNodes(shape = "dot", size = 10, color = list(background = "pink"))







