dataset <- read_csv("./data/data_copy.csv") %>%
  as_tibble() %>%
  select(-Code) %>%
  rename(
    Province = "Prov",
    Location = "Loc"
  )

description <- read_csv("./data/datadescriptor.csv") %>%
  as_tibble() %>%
  select(-`Column no`) %>%
  `[`(-c(1:4, 37:65), )
