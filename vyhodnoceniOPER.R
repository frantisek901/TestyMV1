#### Tento skript slouží ke zpracování výsledkù z testù na KSS/PV1


## FrK vytvoøil 2021-04-05
## FrK upravil  2021-05-03
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


### Naètení odpovìdí z jednoho souboru
raw = read_xlsx("20210503073901-Export---Operacionaliace-PV1---jedna-operacionaliace-jedna-strana.xlsx")
odp = raw[2:12,c(12, 31:60)]
names(odp)[seq(3, 30, 3)] = paste0("co", 1:10) 
names(odp)[seq(2, 30, 3)] = paste0("get", 1:10) 
names(odp)[seq(1, 31, 3)] = c("email", paste0("jak", 1:10)) 
odp

### Naètení odpovìdí z druhého souboru
raw2 = read_xlsx("20210413101127-Export---Operacionalizace-PV1---vše-na-jedné-stranì.xlsx")
odp2 = raw2[3:32,c(12, 21, 31:40, 42:51)]
names(odp2)[seq(3, 21, 2)] = paste0("co", 1:10) 
names(odp2)[seq(4, 22, 2)] = paste0("jak", 1:10) 
names(odp2)[1:2] = c("email", "get1") 
odp2


### Spojení dat
odpAll = bind_rows(odp, odp2) %>% select(email, contains("get"), contains("co"), contains("jak")) %>% 
  add_column(bod1 = 0, .after = "jak1") %>% 
  add_column(bod2 = 0, .after = "jak2") %>% 
  add_column(bod3 = 0, .after = "jak3") %>% 
  add_column(bod4 = 0, .after = "jak4") %>% 
  add_column(bod5 = 0, .after = "jak5") %>% 
  add_column(bod6 = 0, .after = "jak6") %>% 
  add_column(bod7 = 0, .after = "jak7") %>% 
  add_column(bod8 = 0, .after = "jak8") %>% 
  add_column(bod9 = 0, .after = "jak9") %>% 
  add_column(bod10 = 0, .after = "jak10") 


### Uložení do souboru k obodování
write_xlsx(odpAll, "kBodovaniOPER.xlsx")



### Naètení obodovaných výsledkù
vys = read_xlsx("obodovanoOPER.xlsx") %>% select(email, contains("bod"), contains("co"), contains("jak")) 
names(vys) = c("email", paste0("bod.", 1:10), paste0("co.", 1:10), paste0("jak.", 1:10))
vys = vys %>%  mutate_at(vars(contains("bod")), ~as.character(.)) %>% 
  pivot_longer(cols = 2:31) %>%   # Abychom nedìlali vše 10x, reshapnu otázku, øešení a odpovìdi. Nejdøív vše najednou... 
  separate(col = name, into = c("name", "poradi")) %>%  # Teï oddìlíme promìnnou a poøadové èíslo
  pivot_wider(id_cols = c(email, poradi)) %>%  # Teï koneènì vydìlíme tøi promìnné: otázku, odpovìï a øešení
  mutate(bod = as.numeric(bod)) %>%   # Teï urèím pomocí funkce 'bod()' body za každou otázku
  group_by(email) %>% mutate(
    celkem = ifelse(poradi == 1, sum(bod), NA),
    koment = paste0(
      "Operacionalizaci è. ", poradi," jste formuloval/a takto: \nCO:''", co, 
      "'' \nJAK:''", jak, "'' \n\ntato operacionalizace je ", ifelse(bod == 1, "správná", "chybná"),
      ", tudíž za ní máte ", bod, " bod/u/ù."),
    zaklad = paste0(koment, collapse = "\n----------------------------------------------------\n")) 

dopisy = vys %>% filter(!is.na(celkem)) %>% select(email, celkem, zaklad) %>% 
  # filter(email == "pprotivo@students.zcu.cz") %>% mutate(email = "Pavla.Protivova@seznam.cz") %>% 
  mutate(zaklad = paste0(
    "Milá/ý studující, \n\n\n\nzasílám Vám automatické vyhodnocení testu na operacionalizace z KSS/PV1. Pokud vùèi nìmu máte výhrady, neváhejte mne kontaktovat. ", 
    "Mìjte prosím na pamìti má dvì základní pravidla pro revizi hodnocení testù:\n",
    "1) budeme se bavit pouze o Vašem testu, pokud chcete jako argument použít srovnání s testem spolužáka/èky, tak jen pouze pokud chcete oba dva spoleènì revidovat hodnocení, \n",
    "2) pokud pøi revizi narazím na to, že jsem Vám dal omylem více bodù, tak o nì pøijdete, tj. revize pùsobí na obì strany, oba druhy omylù opravíme. ",
    "\n\n******************* S H R N U T Í   T E S T U *******************\n\nTest pro držitele/ku emailu: ",
    email, ". \nCelkem z testu máte: ",
    celkem, " bodù/body. \n\n\n\nS úctou,\nFrantišek",
    "\n\n********* P O D R O B N Á   R E K A P I T U L A C E   T E S T U *********\n\n",
    zaklad
  ))
# SUPER! Máme to vyhodnocené a máme i dopisy hotové!



## Odesílání personalizovaných výsledkù:
library(emayili)
library(dplyr)
library(magrittr)

for (d in 1:nrow(dopisy)) {
  email <- envelope() %>% 
    from("kalvas@kss.zcu.cz") %>% 
    to(dopisy$email[d]) %>% 
    subject(qp_encode("Operacionalizace: Automaticky vyhodnocené výsledky testu ze semináøù KSS/PV1")) %>% 
    text(qp_encode(dopisy$zaklad[d]))
  
  # print(email, details = T)
  
  smtp <- server(host = "smtp.zcu.cz",
                 port = 465,
                 username = "kalvas",
                 password = "Dingo933")
  
  smtp(email, verbose = TRUE)  
}



