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
library(rsdmx)

# Load functions on start
  
# Function for lags
  lagp <- function(x, k=1) {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  
# Function for differences
  diffp <- function(x, k=1) {
    lagx <- lagp(x,k)
    x-lagx
  }

# Function for demeaning
  demean <- function(x, na.rm=FALSE) {
    x-mean(x, na.rm=na.rm)
  }
  
# Function for growth rate
  grate <- function(x,k=1) {
    (x-lagp(x,k))/lagp(x,k)
  }

#
#

# span of data, get the current year
date <- Sys.Date()
year_sel <- format(date, "%Y")

##########################################################
# unemployment
query <- search_eurostat('unemployment', type='dataset')

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

#
#
catalogue <- czso_get_catalogue()
#czso_get_codelist(100)
# get documentation to a specific dataset
czso_get_dataset_doc("030030", action = "download", format = "pdf")
czso_get_dataset_doc('130141r19', action = 'download', format = 'pdf')
head(catalogue)

# Search query
cat <- catalogue %>% 
  filter(str_detect(title, "Index")) %>% 
  select(dataset_id, title, description)

cat$title

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
categ_txt <- c('Overall (NACE I,J,H,L,M,N)','Retail','Accommodation','Food and beverage','Information and communication',
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
serv_current[categ_txt == 'Overall (NACE I,J,H,L,M,N)', plot := 'Services']
serv_current[is.na(plot), plot := 'Subcategories']
serv_current[, categ_factor := factor(categ_txt, levels = c('Overall (NACE I,J,H,L,M,N)',
                                                             'Accommodation',
                                                             'Administrative',
                                                             'Food and beverage',
                                                             'Information and communication',
                                                             'Professional, technical, and research',
                                                             'Real estate',
                                                             'Retail',
                                                             'Transportation'))]

#
#

# plot
fig_sales_serv <- ggplot(serv_current, aes(x = time, y= hodnota, group = categ_factor, colour = categ_factor))
fig_sales_serv + geom_line() + geom_point() + xlab('') + ylab('') + 
               scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(serv_current$time), as.Date('2020-12-01'))) +
               scale_colour_fira(name = element_text("")) + theme_fira() + theme(legend.title = element_text(face='bold'), plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust=0)) +
               facet_wrap(~plot) + labs(title='Sales in services', subtitle='Monthly index - relative to the same period previous year')

#################
# industry production
industry <- data.table(czso_get_table('150196'))

# filter the specific entries from dasta, same period previous year index, industry overall, and automotive
industry_sel <- industry[cznace_kod %in% c('05350001','29') & rok == as.numeric(year_sel) & casz_kod == 'C',]

# only keep required columns
ind_current <- industry_sel[, .(hodnota, mesic, rok, categ = cznace_kod)]

# reformat month number
ind_current[, mesic := ifelse(nchar(mesic) == 1, paste0('0', mesic), mesic)]

# create a date value
ind_current[, time := as.Date(paste('01',mesic,rok, sep = '-'), '%d-%m-%Y')]

ind_current[categ == '29', categ_txt := 'Motor vehicles']
ind_current[categ == '05350001', categ_txt := 'Overall (NACE B,C,D)']

ind_current[, categ_factor := factor(categ_txt, levels = c('Overall (NACE B,C,D)','Motor vehicles'))]

#
#

# plot
fig_prod_ind <- ggplot(ind_current, aes(x = time, y= hodnota, group = categ_factor))
fig_prod_ind + geom_line() + geom_point() + xlab('') + ylab('') + 
               scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(ind_current$time), as.Date('2020-12-01'))) +
               scale_colour_fira(name = element_text("")) + theme_fira() + theme(legend.title = element_text(face='bold'), plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust=0)) +
               facet_wrap(~categ_factor) + labs(title='Industry production', subtitle='Monthly index - relative to the same period previous year')


#################
# construction
construction <- data.table(czso_get_table('200075'))
construction
# filter the specific entries from data, same period previous year index, industry overall, and automotive
construction_sel <- construction[rok == as.numeric(year_sel) & casz_kod == 'C' & ocisteni_kod == 'P',]

# only keep required columns
cons_current <- construction_sel[, .(hodnota, mesic, rok, stavprace_kod)]

# reformat month number
cons_current[, mesic := ifelse(nchar(mesic) == 1, paste0('0', mesic), mesic)]

# create a date value
cons_current[, time := as.Date(paste('01',mesic,rok, sep = '-'), '%d-%m-%Y')]

cons_current[stavprace_kod == '024', categ_txt := 'Civil engineering']
cons_current[stavprace_kod == '023', categ_txt := 'Construction of buildings']
cons_current[is.na(stavprace_kod), categ_txt := 'Overall (NACE F)']

cons_current[, categ_factor := factor(categ_txt, levels = c('Overall',"Construction of buildings",'Civil engineering'))]
cons_current[!(is.na(stavprace_kod)), plot := 'Subcategories']
cons_current[is.na(stavprace_kod), plot := 'Overall (NACE F)']

#
#

# plot
fig_prod_cons <- ggplot(cons_current, aes(x = time, y= hodnota, group = categ_factor, colour = categ_factor))
fig_prod_cons + geom_line() + geom_point() + xlab('') + ylab('') + 
               scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(con_current$time), as.Date('2020-12-01'))) +
               scale_colour_fira(name = element_text("")) + theme_fira() + theme(legend.title = element_text(face='bold'), plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust=0)) +
               facet_wrap(~plot) + labs(title='Construction production', subtitle='Monthly index - relative to the same period previous year')

#
#

# Prices

# download sdmx data from CZSO

# download CPI index
download.file('https://vdb.czso.cz/pll/eweb/sdmx.getXML?d=CPI', destfile='cpi.xml')

# read into R
cpi_data <- as.data.table(readSDMX('cpi.xml', isURL = F))
cpi_data[, time := as.Date(paste0(TIME_PERIOD,'-01'), '%Y-%m-%d')] # adjust date

# select required columns
cpi_data <- cpi_data[, .(time, value = as.numeric(OBS_VALUE))]

# compute inflation
cpi_data[, value := round(grate(value, k = 12)*100,1)]

cpi_data <- cpi_data[year(time) == year_sel,]
cpi_data <- cpi_data[,.(time, value, type="CPI inflation")]

#
#

# Core inflation
# adjust month
mon <- ifelse(nchar(month(date)) == 1, paste0('0', month(date)), month(date))

# create link for cnb data
core_link_cnb <- paste0('https://www.cnb.cz/cnb/STAT.ARADY_PKG.VYSTUP?p_period=1&p_sort=2&p_des=50&p_sestuid=21727&p_uka=9&p_strid=ACBAA&p_od=',year(date),'01','&p_do=',year(date),mon,'&p_lang=CS&p_format=2&p_decsep=.')

core_infl_data <- data.table(read.table(core_link_cnb, header = T, sep='|')) # read the data

setnames(core_infl_data, c('time','value')) # adjust column names
# adjust date
core_infl_data[, time := as.Date(time, format = '%d.%m.%Y')] # adjust date
core_infl_data[, time := as.Date(paste(year(time),month(time), '01' , sep='-'))] # adjust to beginning of the month for consistency
core_infl_data[, type := 'Core inflation']

# bind the CPI and core data together
infl <- rbind(cpi_data, core_infl_data)

fig_infl <- ggplot(infl, aes(x = time, y = value, group = type, colour = type))
fig_infl + geom_line() + geom_point() + xlab('') + ylab('') + 
               scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(cpi_data$time), as.Date('2020-12-01'))) +
               scale_y_continuous(labels = scales::label_number(accuracy=0.1)) +
               scale_colour_fira(name = element_text("")) + theme_fira() + theme(legend.position ='bottom', legend.title = element_text(face='bold'), plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust=0)) +
               labs(title='Inflation', subtitle='Base = same period previous year')
#
#

# download PPI index
download.file('https://vdb.czso.cz/pll/eweb/sdmx.getXML?d=PPI', destfile='ppi.xml')

# read into R
ppi_data <- as.data.table(readSDMX('ppi.xml', isURL = F))
ppi_data[, time := as.Date(paste0(TIME_PERIOD,'-01'), '%Y-%m-%d')] # adjust date

# select required columns
ppi_data <- ppi_data[, .(time, value = as.numeric(OBS_VALUE))]

# compute inflation
ppi_data[, value := round(grate(value, k = 12)*100,1)]

ppi_data <- ppi_data[year(time) == year_sel,]

fig_infl_ppi <- ggplot(ppi_data, aes(x = time, y = value))
fig_infl_ppi + geom_line() + geom_point() + xlab('') + ylab('') + 
               scale_x_date(labels = scales::label_date(format = "%m"), date_breaks = "months", 
               expand = expansion(c(0.05, 0.05)), limits = c(min(cpi_data$time), as.Date('2020-12-01'))) +
               scale_y_continuous(labels = scales::label_number(accuracy=0.1)) +
               scale_colour_fira(name = element_text("")) + theme_fira() + theme(legend.position ='bottom', legend.title = element_text(face='bold'), plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust=0)) +
               labs(title='PPI inflation', subtitle='Base = same period previous year')

#
#
#

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