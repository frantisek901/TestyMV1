#### Doplnit



# Head
rm(list=ls())

## Packages
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)
library(tibble)



### Naètení obodovaných výsledkù HYP
vys = read_xlsx("obodovanoHYP.xlsx") %>% select(email, contains("bod"), contains("hyp")) 
names(vys) = c("email", paste0("bod.", 1:10), paste0("hyp.", 1:10))
vys = vys %>%  mutate_at(vars(contains("bod")), ~as.character(.)) %>% 
  pivot_longer(cols = 2:21) %>%   # Abychom nedìlali vše 10x, reshapnu otázku, øešení a odpovìdi. Nejdøív vše najednou... 
  separate(col = name, into = c("name", "poradi")) %>%  # Teï oddìlíme promìnnou a poøadové èíslo
  pivot_wider(id_cols = c(email, poradi)) %>%  # Teï koneènì vydìlíme tøi promìnné: otázku, odpovìï a øešení
  mutate(bod = as.numeric(bod)) %>%   # Teï urèím pomocí funkce 'bod()' body za každou otázku
  group_by(email) %>% mutate(
    celkem = ifelse(poradi == 1, sum(bod), NA)) %>% 
  filter(!is.na(celkem)) %>% select(email, celkem)



### Naètení emailù a jmen
id = read_xlsx("PV1_studenti_body.xlsx") %>% select(osCislo, jmeno, prijmeni, email)



### Spojeni souboru
spoj = left_join(id, vys, by = "email") %>% mutate(celkem = ifelse(is.na(celkem), "X", as.character(celkem)))


### Ulozeni pro lepsi kopirovani
write_xlsx(spoj, "vysledkyCelkemHYP.xlsx")






# ## OPERACIONALIYACE -----------------------------------------------------

### Naètení obodovaných výsledkù OPER
vys = read_xlsx("obodovanoOPER.xlsx") %>% select(email, contains("bod"), contains("co"), contains("jak")) 
names(vys) = c("email", paste0("bod.", 1:10), paste0("co.", 1:10), paste0("jak.", 1:10))
vys = vys %>%  mutate_at(vars(contains("bod")), ~as.character(.)) %>% 
  pivot_longer(cols = 2:31) %>%   # Abychom nedìlali vše 10x, reshapnu otázku, øešení a odpovìdi. Nejdøív vše najednou... 
  separate(col = name, into = c("name", "poradi")) %>%  # Teï oddìlíme promìnnou a poøadové èíslo
  pivot_wider(id_cols = c(email, poradi)) %>%  # Teï koneènì vydìlíme tøi promìnné: otázku, odpovìï a øešení
  mutate(bod = as.numeric(bod)) %>%   # Teï urèím pomocí funkce 'bod()' body za každou otázku
  group_by(email) %>% mutate(celkem = ifelse(poradi == 1, sum(bod), NA)) %>%  
  filter(!is.na(celkem)) %>% select(email, celkem)

### Naètení emailù a jmen
id = read_xlsx("PV1_studenti_body.xlsx") %>% select(osCislo, jmeno, prijmeni, email)

### Spojeni souboru
spoj = left_join(id, vys, by = "email") %>% mutate(celkem = ifelse(is.na(celkem), "X", as.character(celkem)))


### Ulozeni pro lepsi kopirovani
write_xlsx(spoj, "vysledkyCelkemOPER.xlsx")
