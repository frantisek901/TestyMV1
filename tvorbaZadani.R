## Skript pro vygenerov�n� zad�n� studuj�c�m na PV1

## FrK vytvo�il 2021-03-13
## FrK upravil  2021-05-11
## encoding: windows-1250


# C�lem je vybrat unik�tn�ch 40/50 pojm� a rozd�lit je do �tve�ic/p�tic,
# a pak z ka�d� �tve�ice/p�tice ud�lat jeden string, kter� se ulo�� do jednoho
# pol��ka matice, kter� bude m�t tolik ��dek, kolik je student� a 10 sloupc� 
# (pro ka�d� zad�n� jeden). A nakonec p�jde o to, p�ipojit k nim sloupec s e-maily a
# celou tuto matici exportovat jako excelov� soubor, kter� pak p�jde na��st do C4S.
rm(list = ls())

### Package
library(dplyr)
library(writexl)
library(readxl)


### Vstupn� objekty
pojmy = c("pohlav�", "v�k", "vzd�l�n�", "p��jem", "po�et d�t�", "po�et osob v dom�cnosti", 
          "po�et knih v dom�c� knihovn�", 
          "d�lka sp�nku", "zdravotn� stav", "spokojenost se �ivotem", 
          
          "m�ra samostatnosti", "znalost soc. slu�eb", "intenzita u��v�n� soc. slu�eb", 
          "frekvence u��v�n� soc. slu�eb", "spokojenost se soc. slu�bami", "z�jem o politiku",
          "vlastnictv� mobiln�ho telefonu", "vlastnictv� TV p�ij�ma�e", "intenzita sledov�n� TV",
          "intenzita u��v�n� soci�ln�ch s�t�", 
          
          "uplatn�n� vlastn� v�le p�i �e�en� situace",
          "p�edsudky", "lidsk� pr�va", "osobn� svobody", 
          "znalost pr�vn�ch norem",
          "znalost vlastn�ch pr�v", 
          "p�ij�m�n� dar�", "d�v�n� dar�", 
          "podm�nky poskytov�n� soci�ln� slu�by", 
          "mo�nosti poskytov�n� soci�ln� slu�by", 
          
          "informovanost o soci�ln� slu�b�", 
          "po�adavky na soc. slu�bu", 
          "odm�tnut� poskytnut� soc. slu�by", 
          "uzav�en� smlouvy o poskytov�n� soc. slu�by",
          "porozum�n� obsahu a ��elu smlouvy", 
          "sjedn�n� rozsahu poskytov�n� soc. slu�by",
          "sjedn�n� pr�b�hu poskytov�n� soc. slu�by ", 
          "osobn� c�le", "posl�n� soc. slu�by",
          "pl�nov�n� soci�ln� slu�by",
          
          "hodnocen� napln�n� c�l� soc. slu�by", 
          "dokumentace o poskytov�n� soci�ln� slu�by",
          "st�nosti na kvalitu soc. slu�by", 
          "st�nosti na zp�sob poskytov�n� soc. slu�by",
          "informovanost o mo�nosti st�nosti", 
          "forma st�nosti", "zp�sob vy��zen� st�nosti",
          "informovanost zam�stnanc� o mo�nostech st�nost�",
          "evidence st�nost�",
          "pro�et�en� vy�izov�n� st�nost�",
          
          "p��le�itost vyu��t b�n� dostupn� ve�ejn� slu�by",
          "zprost�edkov�n� slu�eb", 
          "kontakty s p�irozen�m soci�ln�m prost�ed�m",
          "vztahy s p�irozen�m soci�ln�m prost�ed�m",
          "po�et zam�stnanc�", "po�et �vazk� zam�stnanc�",
          "kvalifika�n� po�adavky na zam�stnance",
          "osobnostn� p�edpoklady zam�stnanc�",
          "za�kolov�n� nov�ch zam�stnanc�", "kapacita soci�ln� slu�by",
          
          "pravideln� hodnocen� zam�stnanc�", 
          "program dal��ho vzd�l�v�n� zam�stnanc�", 
          "syst�m finan�n�ho oce�ov�n� zam�stnanc�",
          "syst�m mor�ln�ho oce�ov�n� zam�stnanc�",
          "podpora zam�stnanc� od nez�visl�ho odborn�ka",
          "m�sto poskytov�n� soc. slu�by",
          "�as poskytov�n� soc. slu�by",
          "materi�ln� podm�nky soc. slu�by", "technick� podm�nky soc. slu�by", 
          "hygienick� podm�nky soc. slu�by",
          
          "d�stojn� prost�ed�", "nouzov� situace", "havarijn� situace",
          "�e�en� situace", 
          "sezn�men� zam�stnanc� s �e�en�m nouzov� situace",
          "sezn�men� klient� s �e�en�m nouzov� situace",
          "sezn�men� klient� s �e�en�m havarijn� situace",
          "kvalita soci�ln� slu�by", "posl�n� soc. slu�by", "c�le soc. slu�by")  # Seznam pojm�
length(pojmy)  # kontrola po�tu pojm�
nchar(pojmy)  # kontrola d�lky pojm�
nchar(pojmy) %>% max()
#maily = read_xls("getStudentiByPredmet-2021-03-16-6-35.xls") %>%  # Emaily student� plus m�j
maily = read_xls("OpravyPV1.xls") %>%  
    select(email) %>% as.matrix() %>% as.vector() %>% c("kalvas@kss.zcu.cz", .)  
studs = length(maily)  # Po�et studuj�c�ch

# Matice, kter� bude obsahovat zad�n� pro ka�d�ho studenta, 
# 10 �tve�ic/p�tic, v ka�d� bu�ce jedna, tedy,
# matice m� tolik ��dek, kolik student�, a 10 sloupc� jako 10 hypot�z.
zadaniTen = matrix(rep("0", studs * 10), ncol = 10)  
delky = matrix(rep(0, studs * 10), ncol = 10)  # Matice pro kontrolu d�lek string� v 'zadaniTen'

# Matice, kter� obsahuje jedno zad�n� pro ka�d�ho studenta, tj. 20 pojm� najednou,
# matice m� tolik ��dek, kolik student� a 1 sloupec.
zadaniOne = matrix(rep("0", studs), ncol = 1)


### Cykly, kter� vygeneruj� matici se zad�n�m
for (s in 1:studs) {  # cyklus, kter� iteruje p�es v�echny studuj�c�
  m = pojmy %>% sample(., 50, replace = F) %>% 
    matrix(., ncol = 5, byrow = F)  # Vylosov�n� pojm� a jejich rozd�len� do 10 �tve�ic/p�tic
  for(z in 1:10){  # cyklus, kter� iteruje p�es 10 zad�n� hypot�z u konkr�tn�ho studuj�c�ho 's'
    radek = m[z,]  # Vybereme p��slu�n� ��dek z matice s v�b�rem pojm�
    zadaniTen[s, z] =  # Vytvo��me string v�etn� html zna�ek a ulo��me do matice 'zadani'
      paste0("Hypot�za ", z, ": ", paste0(paste0("<u>", radek[1:5], "</u>"), collapse = "; "))
  }
  v = pojmy %>% sample(., 20, replace = F)
  zadaniOne[s, ] = paste0(paste0("<u>", v, "</u>"), collapse = "; ")
}
delky = nchar(zadaniTen)
max(delky)
zadaniOne %>% nchar() %>% max()

### P�ipojen� e-mail�
zadaniTen = cbind(maily, zadaniTen)
zadaniOne = cbind(maily, zadaniOne)


### Export do excelu
dat = zadaniTen %>% as_tibble() 
names(dat) = c("email", paste0("hyp", 1:10))
write_xlsx(dat, "TenOprava.xlsx")

dat = zadaniOne %>% as_tibble() 
names(dat) = c("email", "zad�n�")
write_xlsx(dat, "OneOprava.xlsx")



# 
# dat = zadaniTen %>% as_tibble() 
# names(dat) = c("email", paste0("hyp", 1:10))
# write_xlsx(dat[1,], "TenTest.xlsx")
# 
# dat = zadaniOne %>% as_tibble() 
# names(dat) = c("email", "zad�n�")
# write_xlsx(dat[1,], "OneTest.xlsx")


