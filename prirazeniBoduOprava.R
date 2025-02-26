#### Skript na vyhodnocen� opravn�ch test�


## FrK vytvo�il 2021-03-13
## FrK upravil  2021-05-17
## encoding: windows-1250

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



### Na�ten� obodovan�ch v�sledk� HYP
vys = read_xlsx("obodovanoHYPoprava.xlsx") %>% select(email, contains("bod"), contains("hyp")) 
names(vys) = c("email", paste0("bod.", 1:10), paste0("hyp.", 1:10))
vys = vys %>%  mutate_at(vars(contains("bod")), ~as.character(.)) %>% 
  pivot_longer(cols = 2:21) %>%   # Abychom ned�lali v�e 10x, reshapnu ot�zku, �e�en� a odpov�di. Nejd��v v�e najednou... 
  separate(col = name, into = c("name", "poradi")) %>%  # Te� odd�l�me prom�nnou a po�adov� ��slo
  pivot_wider(id_cols = c(email, poradi)) %>%  # Te� kone�n� vyd�l�me t�i prom�nn�: ot�zku, odpov�� a �e�en�
  mutate(bod = as.numeric(bod)) %>%   # Te� ur��m pomoc� funkce 'bod()' body za ka�dou ot�zku
  group_by(email) %>% mutate(
    celkem = ifelse(poradi == 1, sum(bod), NA)) %>% 
  filter(!is.na(celkem)) %>% select(email, celkem)



### Na�ten� email� a jmen
id = read_xlsx("PV1_studenti_body.xlsx") %>% select(osCislo, jmeno, prijmeni, email)



### Spojeni souboru
spoj = left_join(id, vys, by = "email") %>% mutate(celkem = ifelse(is.na(celkem), "X", as.character(celkem)))


### Ulozeni pro lepsi kopirovani
write_xlsx(spoj, "vysledkyCelkemHYPoprava.xlsx")






# ## OPERACIONALIYACE -----------------------------------------------------

### Na�ten� obodovan�ch v�sledk� OPER
vys = read_xlsx("obodovanoOPERoprava.xlsx") %>% select(email, contains("bod"), contains("co"), contains("jak"),
                                                       -contains("Pracovn�")) 
names(vys) = c("email", paste0("bod.", 1:10), paste0("co.", 1:10), paste0("jak.", 1:10))
vys = vys %>%  mutate_at(vars(contains("bod")), ~as.character(.)) %>% 
  pivot_longer(cols = 2:31) %>%   # Abychom ned�lali v�e 10x, reshapnu ot�zku, �e�en� a odpov�di. Nejd��v v�e najednou... 
  separate(col = name, into = c("name", "poradi")) %>%  # Te� odd�l�me prom�nnou a po�adov� ��slo
  pivot_wider(id_cols = c(email, poradi)) %>%  # Te� kone�n� vyd�l�me t�i prom�nn�: ot�zku, odpov�� a �e�en�
  mutate(bod = as.numeric(bod)) %>%   # Te� ur��m pomoc� funkce 'bod()' body za ka�dou ot�zku
  group_by(email) %>% mutate(celkem = ifelse(poradi == 1, sum(bod), NA)) %>%  
  filter(!is.na(celkem)) %>% select(email, celkem)

### Na�ten� email� a jmen
id = read_xlsx("PV1_studenti_body.xlsx") %>% select(osCislo, jmeno, prijmeni, email)

### Spojeni souboru
spoj = left_join(id, vys, by = "email") %>% mutate(celkem = ifelse(is.na(celkem), "X", as.character(celkem)))


### Ulozeni pro lepsi kopirovani
write_xlsx(spoj, "vysledkyCelkemOPERoprava.xlsx")
