#### Skript pro vygenerování uživatelských dat pro obsah testu ze semináøù ZS1(Y) na C4S

## Encoding: windows-1250
## Vytvoøil: 2021-03-30 FrK
## Upravil:  2021-04-01 FrK


# Head
rm(list=ls())

## Packages
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(tidyr)


## Naètení e-mailù:
maily = read_excel("UvodDoSociologie-Vsichni.xlsx") %>% select(email) %>%  # Naèteme jen maily
  filter(!is.na(email)) %>% distinct() %>%  as.matrix() %>% as.vector()  # Vyhodíme duplicity a pøevedeme na vektor


## Ruèní vytvoøení listù s texty otázek a odpovìdí, strukturovat to budeme podle:
## Textu, otázky, správné varianty, nesprávné varianty
t1 = list(  # List s otázkami a odpovìïmi na první text
  "q01" = list(
    "Q01. Jaké dvì kultury rozeznává C. P. Snow?",
    "a" = c("1" = "Vìdecká (angl. science)", "1" = "Humanistická (angl. humanities)",
            "0" = "Magická (angl. magic)", 
            "0" = "Mírová (angl. peaceful)", "0" = "Váleèná (angl. warfare)")),
  "q02" = list(
    "Q02. Které tøi obecné tendence k deformaci 'sociologické imaginace' v závìru textu zmiòuje C. W. Mills?",
    "a" = c("1" = "Tendence k teorii dìjin", "1" = "Tendence k 'systematické teorii èlovìka a spoleènosti'", 
            "1" = "Tendence k empirickým výzkumùm soudobých spoleèenských faktù a problémù",
            "0" = "Tendence k sociálnímu inženýrství", "0" = "Tendence k rovnostáøství",
            "0" = "Tendence k feminismu")),
  "q03" = list(
    "Q03. Co to podle C. W. Millse je 'sociologická imaginace' a co nám umožòuje?",
    "a" = c("1" = "Umožòuje pochopit širší historickou scénu a její význam pro vnitøní život a vnìjší životní dráhu rùzných jednotlivcù",
            "1" = "Umožòuje jedinci poznat jeho životní možnosti, pokud si uvìdomí možnosti všech individuí ve stejných okolnostech.",
            "1" = "Umožòuje pochopit historii a biografii a jejich vzájemný vztah ve spoleènosti.",
            "1" = "'Sociologická imaginace' znamená schopnost pøecházet od jednoho pohledu k druhému ... schopnost pøecházet od nejneosobnìjších a nejvzdálenìjších zmìn k nejintimnìjším rysùm èlovìka -- a chápat souvislosti mezi nimi.",
            "0" = "Umožòuje pochopit vztah základny a nadstavby.", "0" = "Umožòuje pochopit vztah 'me' a 'self'.", 
            "0" = "Umožòuje pochopit  Milgramùv teorém.",
            "1" = "Umožòuje rozlišit mezi 'osobními obtížemi danými prostøedím' a 'veøejnými problémy sociální struktury'.",
            "1" = "Je to schopnost odhalovat vztahy mezi velkým množstvím individuálních životních prostøedí.")),
  "q04" = list(
    "Q04. V èem se podle C. W. Millse liší 'osobní obtíže dané prostøedím' a 'veøejné problémy sociální struktury'?",
    "a" = c("1" = "'Obtíže, nesnáze, èi starosti jednotlivce' se objevují v souvislosti s jeho povahou a jeho bezprostøedními vztahy k ostatním lidem.",
            "1" = "'Veøejné problémy' vyplývají z okolností, jež pøesahují omezené prostøedí jednotlivce a hranice jeho vnitøního života.",
            "0" = "'Obtíže, nesnáze, èi starosti jednotlivce' jsou <u>vždy</u> dané sociální strukturou.",
            "0" = "'Veøejné problémy' <u>vždy</u> vyplývají z vlastností jednotlivcù a jejich bezprostøedních vztahù."))
)



t2 = list(  # List s otázkami a odpovìïmi na první text
  "q05" = list(
    "Q05. Jaké základní dimenze McDonaldizace uvádí G. Ritzer?",
    "a" = c("0" = "Vìdoucnost", "0" = "Humanistiènost", "1" = "Kvantifikovatelnost a zkalkulovatelnost",
            "1" = "Efektivnost", "0" = "Magiènost (prostøednictvím magie vyvážených chutí)", 
            "1" = "Pøedvídatelnost", "1" = "Kontrola (prostøednictvím náhrady humánní technologie za nehumánní)")),
  "q06" = list(
    "Q06. Jaké postoje vùèi železné kleci McDonaldizace nabízí G.Ritzer?",
    "a" = c("1" = "Samet - užít si McDonaldizovaný život zcela spokojenì, vítat ho.",
            "1" = "Guma - užívat výhod McDonaldizovaného svìta a pøed nevýhodami unikat.", 
            "1" = "Železo - snaha rozbít klec McDonaldizovaného svìta",
            "0" = "Toaletní papír - nechat McDonaldizaci svému osudu, neøešit jí",
            "0" = "Voda - nechat po sobì volnì stéct dùsledky McDonaldizace, nechat jí odplynout",
            "0" = "Oheò - cílenými fyzickými útoky nièit McDonaldizované podniky a instituce"))
)

  

t3 = list(  # List s otázkami a odpovìïmi na první text
  "q07" = list(
    "Q07. Proè je podle R. B. Moora dùležité vìnovat pozornost jazyku?",
    "a" = c("1" = "Protože náš jazyk nejen vyjadøuje ideje a koncepty, ale také tvaruje naše myšlení.", 
            "0" = "Není to dùležité.", "0" = "Protože jazyk lze kvantifikovat a kalkulovat.",
            "1" = "Pokud si budeme vìdomi vlivu jazyka na naše myšlení, tento vliv se výraznì oslabí.")),
  "q08" = list(
    "Q08. Jaké aspekty rasismu v anglickém jazyce R. B. Moore diskutuje?",
    "a" = c("0" = "Ekonomiku",
            "1" = "Symbolismus", 
            "1" = "Politiku",
            "1" = "Etnocentrismus",
            "0" = "Gramatiku"))
)

# OK 1. Proè je dùležité zkoumat, zda je náš konceptuální systém založený na metaforách? Jaké to má dùsledky, pokud takový je? 
# OK 2. Èím se liší konvenèní a nové/imaginativní metafory? 
# OK Jakých pìt aspektù má termín “reverberation”? 
# no... 3. Jak se autoøi staví k myšlence, že existuje absolutní a nepodmínìná objektivní pravda? Znamená to, že subjektivita a objektivita jsou pouze naše jediné volby? 
# no... 4. Co znamená, že je pravda relativní vzhledem k pochopení?
  

t4 = list(  # List s otázkami a odpovìïmi na první text
  "q09" = list(
    "Q09. Proè je podle Lakoffa a Johnsona dùležité zkoumat, zda je náš konceptuální systém založený na metaforách?",
    "a" = c("1" = "Protože koncepty, které øídí naše myšlení nejsou pouze intelektuální záležitostí. Øídí také naše každodenní fungování.", 
            "0" = "Protože metafora umožòuje rozlišit mezi 'osobními obtížemi danými prostøedím' a 'veøejnými problémy sociální struktury'.",
            "0" = "Protože metafora Umožòuje pochopit historii a biografii a jejich vzájemný vztah ve spoleènosti.",
            "1" = "Protože naše koncepty strukturují naše vnímání, jak pøistupujeme ke svìtu a druhým lidem.",
            "1" = "Protože naše hodnoty nejsou nezávislé, ale musí vytvoøit koherentní systém s používanými metaforami naší kultury.")),
  "q10" = list(
    "Q10. Jaké aspekty podle Lakoffa a Johnsona má termín 'reverberation'?",
    "a" = c("1" = "Metafora sankcionuje jednání, zdùvodòuje souvislosti a pomáhá nám nastavit cíle.",
            "1" = "Metafora zdùrazní urèité rysy a potlaèí jiné.",
            "0" = "Metafora je jen ozvìna, nic nepùsobí, jen vytváøí iluzi pùsobení a dìje.",
            "1" = "Metafora nezahrnuje pouze jiné koncepty, ale specifické aspekty tìchto konceptù.",
            "1" = "Jelikož metafora zdùrazòuje dùležité aspekty zkušenosti a èiní je koherentními, zatímco potlaèuje jiné, tak vytváøí nový význam.",
            "1" = "Význam metafory, který pro mne má, je tak z èásti dán kulturou a z èásti mou osobní zkušeností.")),
  "q11" = list(
    "Q11. Èím se liší podle Lakoffa a Johnsona 'konvenèní' a 'nové/imaginativní' metafory?",
    "a" = c("0" = "'Konvenèní' dávají možnost nového pochopení naší zkušenosti.",
            "1" = "'Konvenèní' strukturují bìžný konceptuální systém naší kultury.", 
            "1" = "'Nové/imaginativní' dávají možnost nového pochopení naší zkušenosti.",
            "0" = "'Nové/imaginativní' strukturují bìžný konceptuální systém naší kultury."))
)


# 1) Jaký je vliv izolace na socializaci dítìte? Je nìjak podmínìný? Jaké dùležité èasové hranice text ilustruje?
# 2) Co byste oznaèili jako hlavní rozdíly ve výsledcích Isabellu a Annu ústavnì socializovat a jaké hlavní rozdíly vidíte v jejich osudech pøed nalezením a ústavní socializací? Souvisí? 
# OK 3) Je Anna vhodný pøípad pro studium dopadu deprivace na socializaci? Proè ano, proè ne?
  

t5 = list(  # List s otázkami a odpovìïmi na první text
  "q12" = list(
    "Q12. Proè jsou a proè nejsou Anna a Isabela z textu K. Davise vhodné pøípady pro studium dopadu deprivace na socializaci?",
    "a" = c("1" = "Nejsou vhodné, protože se pøíliš liší jejich podmínky pøed definitivním pøijetím do ústavu.", 
            "0" = "Jsou vhodné, protože se významnìji liší jen v tom, že Isabelina matka s Isabelou znakovala a tudíž mùžeme vidìt vliv raného osvojení i neverbálního symbolického systému.",
            "1" = "Nejsou vhodné, protože Anna má urèitou pravdìpodobnost dìdièného zatížení a také byla hodnì nemocná.",
            "1" = "Nejsou vhodné, protože Anna mìla v kojenecké fázi velmi nestabilní prostøedí, které èasto mìnila.",
            "1" = "Jsou vhodné, protože pøíklad Isabely ukazuje, že za urèitých podmínek mùže být i pozdìjší socializace úspìšná.")),
  "q13" = list(
    "Q13. Co byste oznaèili jako hlavní rozdíly ve výsledcích ústavnì socializovat Isabellu a Annu z textu K. Davise?",
    "a" = c("1" = "Isabela velmi rychle, bìhem cca 2 let, dosáhla stupnì zralosti, který odpovídal jejímu vìku.",
            "1" = "Anna nikdy nedosáhla stupnì zralosti, který by odpovídal jejímu vìku.",
            "0" = "Obì dìvèata dosáhla bìhem cca 2 let stejného stupnì zralosti, který odpovídal jejich vìku.",
            "1" = "Isabela se nauèila plynnì mluvit.",
            "1" = "Anna se nikdy nenauèila plynulé øeèi s širší slovní zásobou.",
            "0" = "Obì dìvèata dosáhla bìhem cca 2 let stejného stupnì zralosti, který odpovídal zhruba vìku pìtiletého dítìte."))
)

# Na 10/3: Romero, Mary: “Life as a Maids Daughter.” Pp.: 102-113 In Newman, David M. A Jodi O’Brien. 2004. Sociology. Exploring the architecture of everyday life. Readings. Thousand Oaks: Pine Forge Press.
  # 1) O jakých rùzných typech hranic text pojednává a jak spolu tyto typy souvisí?
  # 2) Jaké statusy Teresa vystøídá? Jak se Teresa uèila pravidlùm v jednotlivých místech? Jakou roli v tom procesu hrála její matka a jakou zamìstnavatelé matky?
  # 3) Jak fungují sociální hranice, které Teresa zakouší?
  # 4) Jak se Teresa nauèila anglicky a jak si tento jazyk vybrala? V èem byly pro Teresu traumatické oslavy Díkùvzdání?
  

t6 = list(  # List s otázkami a odpovìïmi na první text
  "q14" = list(
    "Q14. O jakých rùzných typech hranic pojednává text M. Romero?",
    "a" = c("1" = "O hranicích ve fyzickém prostoru domova.", 
            "0" = "O státní hranici mezi USA a Mexikem.",
            "1" = "O sociálních hranicích mezi rùznými skupinami (napø. majitelé, zamìstnanci).",
            "0" = "O hranicích mezi rùznými pojmy.")),
  "q15" = list(
    "Q15. Jaké statusy vystøídá Teresa z textu M. Romero? ",
    "a" = c("1" = "Status dcery ženy, která živí svou mexickou rodinu.",
            "1" = "Status dcery domácí posluhovaèky, ubytované u svých domácích.",
            "0" = "Status matky environmentání aktivistky.",
            "1" = "Status dcery majitelù domu.",
            "0" = "Status dcery environmentální aktivistky."))
)

#Na 17/3: Bloom, Lisa. 2011. “How to Talk to Little Girls.” Huffington Post, June 22. http://www.huffingtonpost.com/lisa-bloom/how-to-talk-to-little-gir_b_882510.html  
# OK 1) Vùèi jakému stylu komunikace s malými dívkami se autorka ohrazuje? Proè?
#   2) Na jaké další ‘nástrahy’ nevhodné socializace dívek autorka se svou malou komunikaèní partnerkou narazí? Jak jim pak èelí?
#   3) Zkuste rekonstruovat mechanismus, jak nevhodná komunikace s malými dívkami vede nakonec k poruchám pøíjmu stravy a závislosti na estetické medicínì.


t7 = list(  # List s otázkami a odpovìïmi na první text
  "q16" = list(
    "Q16. Vùèi jakému stylu komunikace s malými dívkami se L. Bloom ve svém textu ohrazuje?",
    "a" = c("1" = "Vùèi automatickému zdùrazòování krásy tìchto dívek.", 
            "0" = "Vùèi rozhovorùm o knihách s tìmito dívkami.",
            "0" = "Vùèi konverzacím o technice a matematice.",
            "1" = "Vùèi automatickému opomíjení jiných vlastností a aspektù, než je krása, u tìchto dívek.",
            "0" = "Vùèi pøedkládání unisexových hraèek a chlapeckých hraèek tìmto dívkám.")),
  "q17" = list(
    "Q17. Jaké dùsledky má podle L. Bloom nevhodná komunikace s malými dívkami? ",
    "a" = c("1" = "Poruchy pøíjmu stravy",
            "1" = "Pøíliš brzské používání líèidel a makeupu",
            "0" = "Pøíliš velký zájem o chlapecké aktivity, zejména techniku",
            "1" = "Závislost na estetické medicínì",
            "0" = "Pøíliš velký až nezdravý zájem o literaturu"))
)


# Na 17/3: Mead, George H. 1972 (1934). Mind, Self and Society. Chicago: University of Chicago Press. Pp. 173-178. 
# 1) Jak vnímáte Meadovo pojetí ‘me’, co vše je jeho souèástí? 
#   2) Jak se Vám ve svìtle takové koncepce ‘me’ jeví vztah našeho já a druhých lidí?
# OK  3) Jak vnímáte Meadovo pojetí ‘I’? Co ‘I’ vlastnì je?
# OK  4) Jak vnímáte vztah ‘I’ a ‘me’? 
  

t8 = list(  # List s otázkami a odpovìïmi na první text
  "q18" = list(
    "Q18. Jak vnímáte pojetí ‘me’ G. H. Meada, co vše je jeho souèástí? ?",
    "a" = c("1" = "Zkušenosti s chováním druhých lidí v urèitých situacích", 
            "1" = "Zkušenosti s vlastním chováním v urèitých situacích",
            "1" = "Pøedstava o oèekáváních, která druzí mají od nás a našeho chování",
            "1" = "Pøedstava èi plán jak s v urèité situaci zacováme",
            "0" = "Mechanismus, kdy se rozhodneme, co opravdu udìláme")),
  "q19" = list(
    "Q19. Jak vnímáte pojetí ‘I’ dle G. H. Meada? Co ‘I’ vlastnì je? ",
    "a" = c("1" = "Neuvìdomovaný a v principu nepozorovatelný proces, kde se rodí skuteèné rozhodnutí, jak se zachováme",
            "1" = "Jednající složka našeho 'self'",
            "0" = "Pøedstava o oèekávání druhých od nás  a našeho jednání a chování",
            "0" = "Kulturní vrstva našeho 'self'",
            "0" = "Naše pøedstavy o druhých lidech a postoje k nim")),
  "q20" = list(
    "Q20. Jak vnímáte pojetí ‘I’ a 'me' dle G. H. Meada? ",
    "a" = c("1" = "'I' je neuvìdomovaný a v principu nepozorovatelný proces, kde se rodí skuteèné rozhodnutí, jak se zachováme",
            "1" = "'I' a 'me' jsou dvì oddìlené  složky našeho 'self'",
            "1" = "'me' je (krom jiného) pøedstava o oèekávání druhých od nás  a našeho jednání a chování",
            "1" = "'me' je socializovaná složka našeho 'self'",
            "0" = "'I' a 'me' nemají nic spoleèného",
            "0" = "'I' jsou naše pøedstavy o druhých lidech a postoje k nim"))
)


# Na 24/3:Goffman, Erving.  1999. Všichni hrajem divadlo. Sebeprezentace v každodenním životì. Pp.: 39-52. Praha: Nakladatelství Studia Ypsilon.
# OK  2) Proè nìkteøí jedinci ve svých pøedstaveních dìlají zámìrné chyby (napø. pravopis)? (s.42-45)
# OK 3) Co oznaèuje Goffman jako “tajnou spotøebu”? A jaké další druhy jednání se snažíme skrývat (Goffman uvádí ètyøi) (s. 45-47)
# no... 5) Co oznaèuje Goffman jako “segregaci obecenstva”? (s. 51) 
# 



t9 = list(  # List s otázkami a odpovìïmi na první text
  # "q23" = list(
  #   "Q23. Co oznaèuje E. Goffman jako “segregaci obecenstva”?",
  #   "a" = c("1" = "Snahu pøedvádìt svou roli pøed vhodným publikem.", 
  #           "0" = "Vùèi rozhovorùm o knihách s tìmito dívkami.",
  #           "0" = "Vùèi konverzacím o technice a matematice.",
  #           "1" = "Vùèi automatickému opomíjení jiných vlastností a aspektù, než je krása, u tìchto dívek.",
  #           "0" = "Vùèi pøedkládání unisexových hraèek a chlapeckých hraèek tìmto dívkám.")),
  "q21" = list(
    "Q21. Proè nìkteøí jedinci podle E. Goffmana ve svých pøedstaveních dìlají <u>zámìrné</u> chyby (napø. pravopis)?",
    "a" = c("1" = "Potvrzují tak stereotypy navázané na jejich role, které zrovna hrají.", 
            "0" = "Dìlají je, protože jsou hloupí, kdyby byli chytøí, vybrali by si jiné role.",
            "0" = "Dìlají to pro zábavu, baví se tím, že si druzí myslí, že chyby neudìlali schválnì.",
            "1" = "Potvrzují tak oèekávání vùèi své osobì a roli, sociální interakce je pak snazší.",
            "0" = "Oèekávání druhých je natolik rozruší, že zaènou dìlat chyby.")),
  "q22" = list(
    "Q22. Jaké druhy jednání se podle E. Goffmana snažíme skrývat? ",
    "a" = c("1" = "Obecnì jednání, které se nesluèuje s naší rolí a pøedstavou spoleènosti o výkonu této role.",
            "1" = "Tzv. tajnou spotøebu",
            "0" = "Hladký prùbìh práce -- pøedstíráme, že jsme se velmi nadøeli",
            "1" = "Zakrýváme své chyby, které jsme udìlali pøi výkonu rolí",
            "1" = "Nemorální zpùsob obživy (napø. drobné krádeže, hazard"))
)





# Na 24/3: Becker, Howard S. „Becoming a Marihuana User.” Pp: 3-11 In Newman, David M. a Jodi O’Brien. 2004. Sociology. Exploring the architecture of everyday life. Readings. Thousand Oaks: Pine Forge Press. 
# 1) Co je tøeba se nauèit, aby se jedinec mohl stát kuøákem marihuany?
#   3) Jakou teorii Becker nabízí pro to, že se nìkdo stane uživatelem drog? Vùèi jaké teorii se vymezuje? Jak její odmítnutí zdùvodòuje?
# 



t10 = list(  # List s otázkami a odpovìïmi na první text
  "q24" = list(
    "Q24. Co je tøeba se nauèit, aby se jedinec mohl stát kuøákem marihuany podle H. S. Beckera? ",
    "a" = c("1" = "Marihuanu správnì konzumovat",
            "1" = "Rozeznat úèinky marihuany",
            "0" = "Najít si levný zdroj marihuany",
            "0" = "Odhadnout, kdy bude dìlat policie zátah na kuøáky marihuany",
            "0" = "Odhadnout, jaké množství marihuany není vìtší než malé a je tudíž beztrestné jej pøechovávat",
            "1" = "Spojit si úèinky marihuany s faktem kouøení marihuany",
            "1" = "Užít si úèinky marihuany"))
)






## Pøipravíme si matici pro vkládání
mx = matrix("blank", nrow = 128, ncol = 11)
colnames(mx) = c("email", paste0("UP", 1:10))

## Upravíme 'mat' tak, že tam budou maily a uživatelská pole UP1--UP10, tj. jedenáct sloupeèkù
for (m in 1:length(maily)) {
  # Uložíme mail pøíslušného studenta
  mx[m, 1] = maily[m]
  
  # Vylosujeme k textùm otázky a k nim právì 4 odpovìdi
  for (i in 1:10) {
    eval(str2lang(paste0('x', i, ' = t', i, '[[names(t', i, ') %>% sample(1)]]')))  
    eval(str2lang(paste0('x',i,'$a = sample(x',i,'$a, 4)')))  # POZOR! v tuto chvíli tu není kontrola, jestli je alespoò jedna odpovìï pravdivá, zatím jsem to vyøešil tak, že u každé otázky jsou maximálnì 3 nepravdivé odpovìdi a neomezený nenulový poèet pravdivých, tím je zajištìno, že je pravdivá alespoò jedna ze 4 odpovìdí.
  }
  
  # Vylosované otázky s odpovìïmi zamícháme a máme data testu
  test = sample(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), 10)
  
  # Z dat vytvoøíme obsah uživatelských polí a ta uložíme jako stringy do matice
  for (t in 1:length(test)){
    mx[m, (t+1)] = paste0(
      "<!---", paste0(names(test[[t]][[2]]), collapse = ""), 
      "---><hr>",
      test[[t]][[1]], "<p>",
      "(1) ", test[[t]][[2]][[1]], "<p>",
      "(2) ", test[[t]][[2]][[2]], "<p>",
      "(3) ", test[[t]][[2]][[3]], "<p>",
      "(4) ", test[[t]][[2]][[4]])
  }
}

write_xlsx(as_tibble(mx), "TestSeminare01.xlsx")
