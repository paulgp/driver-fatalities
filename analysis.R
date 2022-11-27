library(readxl)
library(ggplot2) 
library(dplyr)
ByState <- read_excel("ByState.xlsx")

max_state = ByState %>% 
  filter(Year == 2020) %>%
  filter(`Fatality Rate per 100 Million VMT` == max(`Fatality Rate per 100 Million VMT`) ) %>%
  pull(STATE)
min_state = ByState %>% 
  filter(Year == 2020) %>%
  filter(`Fatality Rate per 100 Million VMT` == min(`Fatality Rate per 100 Million VMT`) ) %>%
  pull(STATE)
ggplot(data = ByState,
       aes(y = `Fatality Rate per 100 Million VMT`, 
           x = Year, 
           group = STATE)) +
  geom_line(data = ByState %>% filter(STATE != "USA"), alpha = 0.1) +
  geom_line(data = ByState %>% filter(STATE == "USA"), alpha = 1) +
  geom_line(data = ByState %>% filter(STATE == max_state), alpha = 1, color = "red") +
  geom_line(data = ByState %>% filter(STATE == min_state), alpha = 1, color = "blue") +
  annotate("text", x = 2020, y = 2.1, hjust = 1, label = max_state, color = "red") +
  annotate("text", x = 2020, y = 0.35, hjust = 1, label = min_state, color = "blue")  +
  annotate("text", x = 2020, y = 1.4, hjust = 1, label = "USA", color = "black")  +
  labs(x = "Year", y = "", subtitle = "Fatality Rate Per 100 Million Vehicle Miles Traveled") +
  scale_y_continuous(limits = c(0, NA))

max_state = ByState %>% 
  filter(Year == 2020) %>%
  filter(`Fatality Rate per 100,000 Population` == max(`Fatality Rate per 100,000 Population`) ) %>%
  pull(STATE)
min_state = ByState %>% 
  filter(Year == 2020) %>%
  filter(`Fatality Rate per 100,000 Population` == min(`Fatality Rate per 100,000 Population`) ) %>%
  pull(STATE)
ggplot(data = ByState,
       aes(y = `Fatality Rate per 100,000 Population`*10, 
           x = Year, 
           group = STATE)) +
  geom_line(data = ByState %>% filter(STATE != "USA"), alpha = 0.1) +
  geom_line(data = ByState %>% filter(STATE == "USA"), alpha = 1) +
  geom_line(data = ByState %>% filter(STATE == max_state), alpha = 1, color = "red") +
  geom_line(data = ByState %>% filter(STATE == min_state), alpha = 1, color = "blue") +
  annotate("text", x = 2020, y = 270, hjust = 1, label = max_state, color = "red") +
  annotate("text", x = 2020, y = 80, hjust = 1, label = min_state, color = "blue")  +
  annotate("text", x = 2020, y = 150, hjust = 1, label = "USA", color = "black")  +
  labs(x = "Year", y = "", subtitle = "Fatality Rate per Million Population") +
  scale_y_continuous(limits = c(0, 250))


ggplot(data = ByState  %>% filter(Year == 2020) %>% mutate(STATE = factor(STATE)),
       aes(y = `Fatality Rate per 100,000 Population`*10, 
           x = forcats::fct_reorder(STATE, `Fatality Rate per 100,000 Population`))) +
  geom_col() +
  geom_col(data = ByState  %>% filter(Year == 2020 & STATE == "USA") %>% mutate(STATE = factor(STATE)),
           aes(y = `Fatality Rate per 100,000 Population`*10, 
               x = STATE), fill = "blue") +
  coord_flip() +
  labs(x = "", y = "", subtitle = "Fatality Rate per Million Population") +
  scale_y_continuous(limits = c(0, NA))
  