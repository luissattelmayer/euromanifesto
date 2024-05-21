needs(tidyverse, fs, here)

manifestos_2019 <- dir_ls("docs/2019/txt") |> 
  map_dfr(~ read_file(.x)) |>
  # pivot
  pivot_longer(cols = everything(), names_to = "party", values_to = "text") |> 
  # extract party name and year 
  mutate(party = str_replace_all(party, "docs/2019/txt/", ""),
    year = 2019,
    party = str_remove(party, "\\_.*"))

manifestos_2024 <- dir_ls("docs/2024/txt") |> 
  map_dfr(~ read_file(.x)) |>
  # pivot
  pivot_longer(cols = everything(), names_to = "party", values_to = "text") |> 
  # extract party name and year 
  mutate(party = str_replace_all(party, "docs/2024/txt/", ""),
         year = 2024,
         party = str_remove(party, "\\_.*"))

manifestos <- bind_rows(manifestos_2019, manifestos_2024)


manifestos_clean <- manifestos |> 
  mutate(
    # Remove dots following one or two-digit numbers
    text = str_replace_all(text, "\\b\\d{1,2}\\.", ""),
    text = str_replace_all(text, "\n", " "),
    text = str_trim(text),
    # Split text into sentences on dots, ignoring dots followed by whitespace or end of text
    text = str_split(text, "(?<=\\.)\\s+(?=[A-Z])|(?<=\\.)$")
  ) |> 
  unnest(text) |> 
  # Optionally remove empty entries if any remain
  filter(text != "") |> 
  unique() |> 
  group_by(party, year) |>
  mutate(manifesto = str_c(party, year, sep = "_"))

write_csv(manifestos_clean, here("data" ,"manifestos_clean.csv"))

#####

data <- read_csv("data/euromanifesto_sentences_cap_classified.csv") |> 
  select(-1)

data <- data |> 
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

# issues per election overall

data |> 
  group_by(year) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  ggplot(aes(x = fct_reorder(label, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of issues per election",
       x = "Issue",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/overall_salience.jpeg")




# immigration per party

data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Immigration") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Immigration per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/immigration_salience.jpeg")  


# environment per party

data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Environment") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Environment per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/environment_salience.jpeg")

# energy per party

data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Energy") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Energy per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/energy_salience.jpeg")

# defense 

data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Defense") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Defense per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/defense_salience.jpeg")

# show highest label percentage for each party

data |> 
  group_by(party, year) |> 
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  slice_max(percent, n = 1)

# health


data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Health") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Health per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/health_salience.jpeg")


# technology


data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Technology") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Technology per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/technology_salience.jpeg")



data |>
  group_by(party, year) |>
  count(label) |>
  mutate(percent = n / sum(n)*100) |>
  filter(label == "Labor") |> 
  ggplot(aes(x = fct_reorder(party, percent), y = percent, fill = year)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Salience of Labor per Party", 
       x = "Party",
       y = "Percentage of sentences",
       fill = "Year")

ggsave("figures/labor_salience.jpeg")
