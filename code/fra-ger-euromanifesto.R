needs(tidyverse)

# Load the data
fra <- read_csv("data/euromanifesto_sentences_cap_classified_france.csv") |> 
  select(-c(1, file, election)) |> 
  rename("manifesto" = doc,
         "text" = sentence) |> 
  mutate(country = "FRA")

ger <- read_csv("data/euromanifesto_sentences_cap_classified_germany.csv") |> 
  select(-1) |> 
  mutate(country = "GER") 

eu <- rbind(fra, ger) |> 
  mutate(label = case_when(
    predicted == 1 ~ "Macroecnomics",
    predicted == 2 ~ "Civil Rights",
    predicted == 3 ~ "Health",
    predicted == 4 ~ "Agriculture",
    predicted == 5 ~ "Labor",
    predicted == 6 ~ "Education",
    predicted == 7 ~ "Environment",
    predicted == 8 ~ "Energy",
    predicted == 9 ~ "Immigration",
    predicted == 10 ~ "Transportation",
    predicted == 12 ~ "Law and Crime",
    predicted == 13 ~ "Social Welfare",
    predicted == 14 ~ "Housing",
    predicted == 15 ~ "Domestic Commerce",
    predicted == 16 ~ "Defense",
    predicted == 17 ~ "Technology",
    predicted == 18 ~ "Foreign Trade",
    predicted == 19 ~ "International Affairs",
    predicted == 20 ~ "Government Operations",
    predicted == 21 ~ "Public Lands",
    predicted == 22 ~ "Culture",
    predicted == 23 ~ "No Policy Content"
  ),
  party = str_to_upper(party),
  year = as.character(year))

# Plot per country

eu |> 
  group_by(year, country) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  ggplot(aes(x = fct_reorder(label, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~country) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of issues per election",
       x = "Issue",
       y = "Percentage of sentences",
       fill = "Year")


# far-right

eu |> 
  filter(party %in% c("RN", "AFD", "BSW", "REC") & 
           label %in% c("Environment", "Labor", "Immigration", "Defense")) |> 
  group_by(year, party) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  ggplot(aes(x = fct_reorder(label, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  geom_text(aes(label = sprintf("%.1f", percent)), 
            position = position_dodge2(width = 0.9), 
            vjust = 0.5, 
            hjust = -0.5, 
            size = 2) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~party) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of issues per election for far-right parties",
       x = "Issue",
       y = "Percentage of sentences",
       fill = "Year")

eu |> 
  group_by(year, party) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(party %in% c("PS", "SPD", "LR", "CDUCSU") & 
           label %in% c("Environment", "Labor", "Immigration", "Defense")) |> 
  ggplot(aes(x = fct_reorder(label, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  geom_text(aes(label = sprintf("%.1f", percent)), 
            position = position_dodge2(width = 0.9), 
            vjust = 0.5, 
            hjust = -0.5, 
            size = 2) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~party) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of issues per election for far-right parties",
       x = "Issue",
       y = "Percentage of sentences",
       fill = "Year")


eu |> 
  filter(country == "GER") |> 
  group_by(year, party) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label %in% c("Agriculture", "Environment", "Immigration")) |> 
  ggplot(aes(x = year, y = percent, color = party, group = party)) +
  geom_point() + 
  geom_line() +

  scale_color_manual(values = c("AFD" = "blue", "CDUCSU" = "black", 
                    "SPD" = "red", "FDP" = "yellow", "GRÜNE" = "green",
                    "LINKE" = "pink", "BSW" = "darkred")) + 
  theme_minimal() +
  facet_wrap(~ label)

eu |> 
  filter(country == "FRA" & label == "Immigration" & year == "2024") |> 
  view()

eu |> 
  filter(country == "FRA") |> 
  group_by(year, party) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label %in% c("Agriculture", "Environment", "Immigration")) |> 
  ggplot(aes(x = year, y = percent, color = party, group = party)) +
  geom_point() + 
  geom_line() +
  
  scale_color_manual(values = c("LR" = "lightblue", "RN" = "blue", 
                                "PS" = "red", "REN" = "yellow", "EELV" = "green",
                                "LFI" = "pink", "BSW" = "darkred",
                                "REC" = "black")) + 
  theme_minimal() +
  facet_wrap(~ label)


deu <- read_csv("data/Germany_Corpus.csv") |> 
  filter(date >= 201709) |>
  mutate(country = "GER",
         party = case_when(
           party == 41521 ~ "CDUCSU",
           party == 41320 ~ "SPD",
           party == 41953 ~ "AFD",
           party == 41420 ~ "FDP",
           party == 41113 ~ "GRÜNE",
           party == 41223 ~ "LINKE",
           party == "Sonstige" ~ "Sonstige"
         ),
         year = str_sub(as.character(date), 1, 4)) |> 
  select(-c(countryname, text_id, date)) |> 
  group_by(party, year) |> 
  mutate(manifesto = str_c(party, year, sep = "_"))

write_csv(deu, "data/ger_national_manifestos.csv")




deu <- read_csv("data/ger_nat_manifesto_sentences_cap_classified.csv") |> 
  select(-1) |> 
  mutate(label = case_when(
    predicted == 1 ~ "Macroecnomics",
    predicted == 2 ~ "Civil Rights",
    predicted == 3 ~ "Health",
    predicted == 4 ~ "Agriculture",
    predicted == 5 ~ "Labor",
    predicted == 6 ~ "Education",
    predicted == 7 ~ "Environment",
    predicted == 8 ~ "Energy",
    predicted == 9 ~ "Immigration",
    predicted == 10 ~ "Transportation",
    predicted == 12 ~ "Law and Crime",
    predicted == 13 ~ "Social Welfare",
    predicted == 14 ~ "Housing",
    predicted == 15 ~ "Domestic Commerce",
    predicted == 16 ~ "Defense",
    predicted == 17 ~ "Technology",
    predicted == 18 ~ "Foreign Trade",
    predicted == 19 ~ "International Affairs",
    predicted == 20 ~ "Government Operations",
    predicted == 21 ~ "Public Lands",
    predicted == 23 ~ "Culture"
  ),
  year = as.character(year)) |> 
  select(-c(partyname))

deu_all <- rbind(deu, eu |> filter(country == "GER")) |> 
  mutate(election_type = case_when(
    year %in% c(2017, 2021) ~ "National",
    year %in% c(2019, 2024) ~ "EU"
  ))

deu_all |> 
  group_by(party, year, election_type) |> 
  count(label) |> 
  mutate(percent = n / sum(n)*100) |> 
  filter(label %in% c("Immigration", "Environment")) |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = election_type)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ label) +
  scale_fill_brewer(palette = "Set1")

deu_all |> 
  filter(country == "GER") |> 
  group_by(party, year) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  ggplot(aes(x = year, y = percent, color = party, group = party)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("AFD" = "blue", "CDUCSU" = "black", 
                                "SPD" = "red", "FDP" = "yellow", "GRÜNE" = "green",
                                "LINKE" = "pink", "BSW" = "darkred")) +
  theme_minimal() +
  facet_wrap(~ label)

