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


