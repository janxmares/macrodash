# Creating an up to date dashboard for the czech economy
# Jan Mares

library(data.table)
library(here)
library(czso)
library(dplyr)
library(stringr)
library(ggplot2)
library(eurostat)
library(readr)
library(readxl)
library(firatheme)
library(extrafont)

##########################################################
# span of data, get the current year
date <- Sys.Date()
year_sel <- format(date, "%Y")

##########################################################
# unemployment
query <- search_eurostat('unemployment', type='dataset')
query
View(query)
unemp <- get_eurostat('ei_lmhr_m')

unemp_cz <- as.data.table(unemp)
unemp_cz <- unemp_cz[geo == "CZ" & year(time) == year_sel &  s_adj == "SA" & indic == "LM-UN-T-TOT", ]


fig_u <- ggplot(unemp_cz, aes(x = time, y= values))
fig_u + geom_line() + geom_point() + xlab('') + ylab('') + scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(serv_current$time), as.Date('2020-12-01'))) + theme_fira()

##########################################################
# value added across sectors
query <- search_eurostat('sales', type='dataset')
query <- search_eurostat('industry', type='dataset')
query <- search_eurostat('services', type='dataset')
query

catalogue <- czso_get_catalogue()

#
#

#czso_get_codelist(100)
# get documentation to a specific dataset
czso_get_dataset_doc("030030", action = "download", format = "pdf")
czso_get_dataset_doc('130141r19', action = 'download', format = 'pdf')
head(catalogue)
# Search query
cat <- catalogue %>% 
  filter(str_detect(dataset_id, "ZAM11")) %>% 
  select(dataset_id, title, description)
cat$title
cat
data <- czso_get_table('250169')
head(data)
unique(data$vuk_text)


# Podíl nezaměstnaných osob (MPSV - úřady práce)
# read the basic data
urate_mpsv <- data.table(read_csv('urate_mpsv.csv'))
# reformat the date
urate_mpsv[, time := as.Date(time, '%d-%m-%y')]
urate_mpsv[, id := 'mpsv'] # identification for graphs


# https://www.mpsv.cz/o/rest/statistiky/nezamestnanost/2020/05
link <- 'https://www.mpsv.cz/o/rest/statistiky/nezamestnanost/2020/05'

download.file(link, here('stat-2020-05.zip'), mode='wb')
unzip('stat-2020-05.zip', files = '4. Nez0520h.xlsx', exdir=here())

data <- read_excel('4. Nez0520h.xlsx', sheet = 'nuts3', range="AD101:AD101", col_names = F)
data

#
#

##########################################################
# sales in services
services <- data.table(czso_get_table('030030'))
# services_sch <- czso_get_table_schema("030030")

# list of nace categories
cznace <- services[, .(cznace_kod, cznace_txt)]
cznace <- cznace[!duplicated(cznace_kod),]
# cznace # check

# filter the values
# seasonally adjusted, current prices, same period prevous year comparison, all services
# selection of categories: services overall and then subcategories
#
categ <- c("49820001",'45470001','55','56','J','H','68','N','69740001')
categ_txt <- c('Services (NACE I,J,H,L,M,N)','Retail','Accommodation','Food and beverage','Information and communication',
               'Transportation','Real estate','Administrative','Professional, technical, and research')
categ_concat <- data.table(categ, categ_txt)

serv_current <- services[rok >= as.numeric(year_sel) & ocisteni_kod == 'O' & is.na(oceneni_kod) &
                      cznace_kod %in% categ_concat$categ & casz_kod == 'C',]

#                
#

# only keep required columns
serv_current <- serv_current[, .(hodnota, mesic, rok, categ = cznace_kod)]

# merge with text value of nace category
serv_current <- serv_current[categ_concat, on = c('categ')]

# reformat month number
serv_current[, mesic := ifelse(nchar(mesic) == 1, paste0('0', mesic), mesic)]

# create a date value
serv_current[, time := as.Date(paste('01',mesic,rok, sep = '-'), '%d-%m-%Y')]

# create split column for overall services and sub-categories
serv_current[categ_txt == 'Services (NACE I,J,H,L,M,N)', plot := 'Services']
serv_current[is.na(plot), plot := 'Subcategories']

#
#

# plot
fig_sales_serv <- ggplot(serv_current, aes(x = time, y= hodnota, group = categ_txt, colour = categ_txt))
fig_sales_serv + geom_line() + geom_point() + xlab('') + ylab('') + 
               scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(serv_current$time), as.Date('2020-12-01'))) +
               scale_colour_fira(name = element_text("")) + theme_fira() + theme(legend.title = element_text(face='bold'), plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust=0)) +
               facet_wrap(~plot) + labs(title='Sales in services', subtitle='Monthly index - relative to the same period previous year')

#################
# industry production
industry <- data.table(czso_get_table('150196'))

#################
# construction
construction <- data.table(czso_get_table('200075'))

#
#

# electricity consumption

#
#

#

el_data <- data.table(read_csv2('https://www.ceps.cz/download-data/?format=txt', skip = 2))

library(RCurl)

headerFields =
  c(Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    SOAPAction = "https://www.ceps.cz/CepsData/Load")

body = '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <Load xmlns="https://www.ceps.cz/CepsData/Load">
      <dateFrom>DateTime(2019, 10, 16)</dateFrom>
      <dateTo>DateTime(2020, 10, 16)</dateTo>
      <agregation>"DY"</agregation>
      <function>"AVG"</function>
      <version>"RT"</version>
    </Load>
  </soap:Body>
</soap:Envelope>'

reader <- basicTextGatherer()
curlPerform(url = "https://www.ceps.cz/CepsData/Load",
                          httpheader = headerFields,
                          postfields = body,
                          writefunction = reader$update
                          )

xml <- reader$value()
xml