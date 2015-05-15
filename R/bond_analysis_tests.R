library(devtools)
install_github("Tsunamical28/mdpr")
library(mdpr)
require(RODBC)
require(tidyr)
require(dplyr)
require(lubridate)
require(Rbbg)
require(zoo)
require(ggplot2)
require(scales)
require(NMOF)

######################MUNI POSITIONS####################

close_date <- as.Date("2015-05-14")

as_of_datetime <- Sys.time()
hour(as_of_datetime) <- 23; minute(as_of_datetime) <- 59; second(as_of_datetime) <- 59
trader_inits <- c("RJ", "LF", "MB", "EM", "OOC", "GHC")
exempt_accounts <- paste0(trader_inits, "-EX")
taxable_accounts <- paste0(trader_inits, "-TAX")
tsy_accounts <- paste0(trader_inits, "-TSY")


channel <- odbcDriverConnect("driver=SQL Server;
                             server=nj1pvsql01;uid=sql_Munidesk_App;pwd=Munidesk!")
bond_pos <- tbl_df(sqlQuery(channel,
                            paste("R_LoadMuniDeskPositions '",
                                  as.character(close_date),"','", as.character(as_of_datetime),"','",
                                  paste(exempt_accounts, taxable_accounts,
                                        tsy_accounts,sep = ",",collapse=","),"'"), 
                            stringsAsFactors = F))

swap_pos <- tbl_df(sqlQuery(channel,
                            paste("R_LoadMRLPositions '",
                                  as.character(close_date),"','", as.character(as_of_datetime),"','",
                                  paste(exempt_accounts,collapse=","),"'"),
                            stringsAsFactors = F))

tax_lookup <- tbl_df(sqlQuery(channel,"SELECT description, mapping 
                              FROM mdp_lookup WHERE lookup_type = 'Tax Provision'",
                              stringsAsFactors = F))
odbcClose(channel)

bond_pos <- transform(bond_pos, initials = factor(initials, 
                                                  levels = c("RJ", "LF","MB","EM","OOC","GHC")))

tax_lookup <- rename(tax_lookup,
                     muni_tax_prov = description,
                     tax_status = mapping)


fields <- c("ID_CUSIP","MARKET_SECTOR_DES", "ISSUER_BULK", "ISSUER_DESCRIPTION_2ND_LINE_BULK", "CPN",
            "MATURITY","WORKOUT_DT_BID", "NXT_CALL_DT", "NXT_CALL_PX", "SETTLE_DT",
            "STATE_CODE", "CPN_TYP","MUNI_TAX_PROV")

bbg_securities <- unique(paste(bond_pos$cusip,bond_pos$yellow_key))

bma_tenors <- c(1:5, 7, 10, 12, 15, 20, 25, 30)
bma_securities <- paste0("USSMSB", bma_tenors, " Curncy")
bma_fields <- c("PX_LAST")



conn <- blpConnect()
bbg_data <- tbl_df(bdp(conn, bbg_securities ,fields))
bbg_bma_data <- tbl_df(bdh(conn, bma_securities, bma_fields, close_date, close_date))
blpDisconnect(conn)

bbg_bma_data <- bbg_bma_data %>%
  mutate(tenor = bma_tenors, maturity = close_date + years(tenor)) %>%
  rename(rate = PX_LAST) %>%
  select(tenor, maturity, rate)

bma_spline <- smooth.spline(bbg_bma_data[["tenor"]], bbg_bma_data[["rate"]])




bbg_data <- rename(bbg_data, 
                   cusip = ID_CUSIP,
                   yellow_key = MARKET_SECTOR_DES,
                   issuer1 = ISSUER_BULK,
                   issuer2 = ISSUER_DESCRIPTION_2ND_LINE_BULK,
                   coupon = CPN,
                   maturity = MATURITY,
                   workout_date = WORKOUT_DT_BID,
                   call_date = NXT_CALL_DT,
                   call_price = NXT_CALL_PX,
                   settle_date = SETTLE_DT,
                   state = STATE_CODE,
                   coupon_type = CPN_TYP,
                   muni_tax_prov = MUNI_TAX_PROV)

bbg_data <- bbg_data %>% 
  mutate(maturity = as.Date(maturity),
         workout_date = as.Date(workout_date),
         call_date = as.Date(call_date),
         settle_date = as.Date(settle_date)) %>%
  left_join(tax_lookup,by = "muni_tax_prov")
bbg_data[bbg_data$yellow_key == "Corp", "tax_status"] <- "TAXABLE" #for the corporate cusip munis

pos_data <- bond_pos %>% 
  left_join(bbg_data, by = c("cusip", "yellow_key")) %>%
  select(initials, corp_hedge, cusip, yellow_key, maturity, coupon.y,
         tax_status, curr_trader_position, call_date, call_price, settle_date, mtm_price ) %>%
  rename(coupon = coupon.y, net_size = curr_trader_position) %>%
  mutate(net_size = net_size * 1000.0)
muni_des <- filter(pos_data, yellow_key %in% c("Muni", "Corp")) %>%
  select(cusip, yellow_key, maturity, coupon, tax_status, call_date, call_price,
         settle_date, mtm_price) %>%
  distinct()

tsy_des <- filter(pos_data, yellow_key == "Govt") %>%
  select(cusip, yellow_key, maturity, coupon, tax_status, call_date, call_price,
         settle_date, mtm_price) %>%
  distinct()




muni_bonds <- with(muni_des,
                   mapply(bond,
                          maturity = maturity,
                          coupon = coupon, 
                          dated_date = settle_date,
                          id = cusip,
                          call_date = call_date,
                          call_px = call_price,
                          SIMPLIFY = FALSE))

muni_des <-   muni_des %>%
  mutate(mtm_yield = mapply(calc_yield, b = muni_bonds, 
                            settle = muni_des$settle_date,
                            clean_px = muni_des$mtm_price, 
                            SIMPLIFY = TRUE))

tsy_bonds <- with(tsy_des,
                  mapply(bond,
                         maturity = maturity,
                         coupon = coupon, 
                         dated_date = settle_date,
                         id = cusip,
                         conv = "Act/Act",
                         SIMPLIFY = FALSE))

tsy_des <-   tsy_des %>%
  mutate(mtm_yield = mapply(calc_yield, b = tsy_bonds, 
                            settle = tsy_des$settle_date,
                            clean_px = tsy_des$mtm_price, 
                            SIMPLIFY = TRUE))

mrl_des <-  swap_pos %>% 
  filter(tenor != "BMA") %>%
  select(hedge, tenor, term, strike_rate, effective_date,
         maturity_date, DV01, effective_mtm_yield) %>%
  mutate(hedge = as.character(hedge), 
         effective_date = as.Date(effective_date),
         maturity_date = as.Date(maturity_date)) %>%
  unique()

bma_des <-  swap_pos %>% 
  filter(tenor == "BMA") %>%
  select(hedge, tenor, term, strike_rate, effective_date,
         maturity_date, DV01, effective_mtm_yield) %>% 
  unique() %>%
  mutate(effective_date = as.Date(effective_date),
         maturity_date = as.Date(maturity_date),
         close_curve_yield = sapply(as.numeric((maturity_date - close_date)/365.25),
                                    function(x){predict(bma_spline, x)$y}))


swap_data <- swap_pos %>%
  mutate(corp_hedge = NA, cusip = as.character(hedge), yellow_key = "Index",
         tax_status = NA, net_size = curr_group_qty * 1000,
         initials = factor(initials, trader_inits)) %>%
  select(initials, corp_hedge, cusip, yellow_key, tax_status, net_size)


bma_bonds <- with(bma_des,
                  mapply(bond,
                         maturity = maturity_date,
                         coupon = strike_rate, 
                         dated_date = effective_date,
                         id = hedge,
                         conv = "30/360",
                         freq = 4,
                         SIMPLIFY = FALSE))



muni_cfs <- mapply(calc_risk, b = muni_bonds, settle = muni_des$settle_date, 
                   price_yield = muni_des$mtm_price, input_type = "P", 
                   returnCFs = TRUE, SIMPLIFY = FALSE)
muni_cfs <- bind_rows(muni_cfs) %>% select(id, cf_date, cf_amount, dv01)

tsy_cfs <-mapply(calc_risk, b = tsy_bonds, settle = tsy_des$settle_date, 
                 price_yield = tsy_des$mtm_price, input_type = "P", 
                 returnCFs = TRUE, SIMPLIFY = FALSE)
tsy_cfs <- bind_rows(tsy_cfs) %>% select(id, cf_date, cf_amount, dv01)

bma_cfs <-mapply(calc_risk, b = bma_bonds, settle = rep(close_date, nrow(bma_des)), 
                 price_yield = bma_des$close_curve_yield, input_type = "Y", 
                 returnCFs = TRUE, SIMPLIFY = FALSE)
bma_cfs <- bind_rows(bma_cfs) %>% select(id, cf_date, cf_amount, dv01)

mrl_cfs <-  mrl_des %>%
  mutate(id = hedge, cf_date = maturity_date, 
         cf_amount = 100, dv01 = DV01) %>%
  select(id, cf_date, cf_amount, dv01)






################ TEST SECTION ###############
curve_shocks <- seq(-1.00, 1.00, by = 0.10)
# factor(paste(curve_shocks * 100, "bps"), levels = paste(curve_shocks * 100, "bps"))
muni_shock_dv01 <- lapply(seq_along(curve_shocks), function(i){
  mapply(calc_risk, b = muni_bonds, settle = muni_des$settle_date, 
         price_yield = (muni_des$mtm_yield + curve_shocks[i]), input_type = "Y", 
         returnCFs = TRUE, SIMPLIFY = FALSE) %>%
    bind_rows(muni_cfs) %>% 
    select(id, cf_date, cf_amount, dv01) %>%
    mutate(shock = curve_shocks[i] * 100)                      
})
muni_shock_dv01 <- bind_rows(muni_shock_dv01)

tsy_shock_dv01 <- lapply(seq_along(curve_shocks), function(i){
  mapply(calc_risk, b = tsy_bonds, settle = tsy_des$settle_date, 
         price_yield = (tsy_des$mtm_yield + curve_shocks[i]), input_type = "Y", 
         returnCFs = TRUE, SIMPLIFY = FALSE) %>%
    bind_rows(tsy_cfs) %>% 
    select(id, cf_date, cf_amount, dv01) %>%
    mutate(shock = curve_shocks[i] * 100)
})
tsy_shock_dv01 <- bind_rows(tsy_shock_dv01) 

bma_shock_dv01 <- lapply(seq_along(curve_shocks), function(i){
  mapply(calc_risk, b = bma_bonds, settle = rep(close_date, nrow(bma_des)), 
         price_yield = (bma_des$close_curve_yield + curve_shocks[i]), 
         input_type = "Y", returnCFs = TRUE, SIMPLIFY = FALSE) %>%
    bind_rows(bma_cfs) %>% 
    select(id, cf_date, cf_amount, dv01) %>%
    mutate(shock = curve_shocks[i] * 100)
})
bma_shock_dv01 <- bind_rows(bma_shock_dv01) 

mrl_shock_dv01 <-  cbind(mrl_cfs[rep(seq_len(nrow(mrl_cfs)), each = length(curve_shocks)),],
                         data.frame(shock = curve_shocks * 100))


shock_dv01 <- bind_rows(muni_shock_dv01, tsy_shock_dv01, bma_shock_dv01, mrl_shock_dv01)


desk_dv01_shocked <-  bind_rows(pos_data, swap_data) %>% 
  select(initials, corp_hedge, cusip, yellow_key, tax_status, net_size) %>%
  inner_join(shock_dv01, by= c("cusip" = "id")) %>%
  mutate(cf_amount = cf_amount * net_size /100,
         dv01 = dv01 * net_size/100) %>%
  #                       filter(tax_status == "EXEMPT") %>%
  mutate(bin = as.Date(cut_interval(cf_date, length = "1 year")))

# ggplot(arrange(desk_dv01_shocked, shock), aes(bin, dv01, group = shock)) + 
#   geom_histogram(aes(weight = dv01), stat = "identity", fill = "maroon") +  
#   facet_wrap( ~ shock) +
#   scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"))

desk_summary <- desk_dv01_shocked %>%
  rename(year = bin) %>%
  mutate(year = year(year)) %>%
  select(shock, year, dv01) %>% 
  group_by(shock, year) %>%
  summarise(dv01 = sum(dv01)) %>%
  spread(shock, dv01)

trader_summary <- lapply(seq_along(trader_inits), function(i){
  desk_dv01_shocked %>%
    filter(initials == trader_inits[i]) %>%
    rename(year = bin) %>%
    mutate(year = year(year)) %>%
    select(initials, shock, year, dv01) %>% 
    group_by(initials, shock, year) %>%
    summarise(dv01 = sum(dv01)) %>%
    spread(shock, dv01)
})
trader_summary <- bind_rows(trader_summary)



desk_summary[is.na(desk_summary)] <- 0

trader_summary[is.na(trader_summary)] <- 0

trader_summary <- select(trader_summary, -year) %>% group_by(initials) %>% summarise_each(funs(sum))
trader_summary <- trader_summary %>% ungroup() %>% 
  gather("shock","dv01",-1 ) %>% spread(initials, dv01)

write.csv (desk_summary, "D:/Download/dv01_summary.csv", row.names = FALSE)
write.csv (trader_summary, "D:/Download/dv01_summary_by_trader.csv", row.names = FALSE)

# desk_summary %>% gather("shock", "dv01", -1)
# 
# py <- plotly(username="dcashear", key="6w6h9vten5")
# 
# p <- ggplot(desk_summary %>% gather("shock", "dv01", -1), 
#        aes(x = year, y = shock, z = dv01)) + geom_tile(aes(fill = dv01))
# py$ggplotly(p)

# sum(filter(desk_dv01_shocked, initials == "MB", shock == 0, yellow_key == "Muni")$dv01)

# hedge_breaks <- as.Date(Sys.Date() + c(0, 5, 10, 15, 20, 25, 30, 100)*years(1))
# hedge_labels <- paste0(c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-100"),"Y")


hedge_breaks <- as.Date(Sys.Date() + c(0, 3, 5, 7, 16, 100)*years(1))
hedge_labels <- paste0(c("0-3", "4-5", "6-7", "8-16", "17-100"),"Y")
hedge_labels <- factor(hedge_labels, levels = hedge_labels)


fields <- c("ID_CUSIP", "CPN", "MATURITY","ISSUE_DT", "SETTLE_DT", "PX_LAST" )
tsy_hedge_maturities <- c(5, 7, 10, 30)
tsy_hedge_letters <- c("","B")
tsy_hedges <- paste(mapply(paste0, paste0("CT", tsy_hedge_maturities), 
                           rep(tsy_hedge_letters, each = length(tsy_hedge_maturities))), "Govt")
conn <- blpConnect()
hedge_bbg_data <- tbl_df(bdp(conn, tsy_hedges ,fields))
blpDisconnect(conn)

hedge_bbg_data <- hedge_bbg_data %>% 
  rename(cusip = ID_CUSIP,
         coupon = CPN,
         maturity = MATURITY,
         dated_date = ISSUE_DT,
         settle = SETTLE_DT,
         price = PX_LAST) %>%          
  mutate(maturity = as.Date(maturity), 
         dated_date = as.Date(dated_date),
         settle = as.Date(settle))

hedge_bonds <- with(hedge_bbg_data,
                    mapply(bond,
                           maturity = maturity,
                           coupon = coupon, 
                           dated_date = dated_date,
                           id = cusip,
                           conv = "Act/Act",
                           SIMPLIFY = FALSE))

hedge_cfs <-mapply(calc_risk, b = hedge_bonds, settle = hedge_bbg_data$settle, 
                   price_yield = hedge_bbg_data$price, input_type = "P", 
                   returnCFs = TRUE, SIMPLIFY = FALSE)
hedge_dv01_binned <- bind_rows(hedge_cfs) %>%
  select(id, cf_date, cf_amount, dv01) %>%
  mutate(bin = cut(cf_date, breaks = hedge_breaks, labels = hedge_labels)) %>%
  group_by(id, bin) %>% summarise(dv01 = sum(dv01)) %>%
  spread(bin, dv01, drop = FALSE, fill = 0)

########## GET TAXABLE CUSIPS #########
taxable_pos <- filter(pos_data, tax_status == "EXEMPT")
taxable_cusips <- taxable_pos %>% select(cusip) %>% unique() %>%`[[`("cusip")
taxable_bonds <- simplify2array(lapply(muni_bonds,function(m){m[["cusip"]]})) %in% taxable_cusips
taxable_bonds <- muni_bonds[taxable_bonds]
taxable_dv01_binned <-  filter(muni_cfs, id %in% taxable_cusips) %>%
  select(id, cf_date, cf_amount, dv01) %>%
  mutate(bin = cut(cf_date, breaks = hedge_breaks, labels = hedge_labels)) %>%
  group_by(id, bin) %>% summarise(dv01 = sum(dv01)) %>%
  ungroup() %>%
  inner_join((taxable_pos %>%
                select(initials, corp_hedge, cusip, yellow_key, tax_status, net_size)), 
             by= c("id" = "cusip")) %>%
  mutate(dv01 = dv01 * net_size/100) %>%
  select(initials, bin, dv01) %>%
  group_by(initials, bin) %>%
  summarise(dv01 = sum(dv01)) %>%
  spread(bin, dv01,  drop = FALSE, fill = 0) %>% ungroup()




# mru <- function (m, n){
#   array(runif(m * n), dim = c(m, n))
# }
# b <- 1000 * runif(length(tsy_hedges))
# b <- Data$min + diag(Data$max - Data$min) %*% mru(length(tsy_hedges), 100)


obj_func <- function(b, Data){
  h <- Data$h
  p <- Data$p
  if(dim(b)[2] > 1){    
    res <- colSums(abs(apply(t(as.matrix(h)) %*% b * 10, 2, function(x){x - t(as.matrix(p))})))
  }
  else{
    res <- sum(abs(colSums(b *  h * 10) - p))  
  }
  
  res[is.na(res)] <- 1e10
  res
}
Data <- list(h = hedge_dv01_binned[-1],
             p = filter(taxable_dv01_binned, initials == "MB")[-1],
             max = rep.int(50000, length(tsy_hedges)),
             min = rep.int(0, length(tsy_hedges)))




repair <- function(x, ...) {
  up <- algo$max
  lo <- algo$min
  xadjU <- x - up
  xadjU <- xadjU + abs(xadjU)
  xadjL <- lo - x
  xadjL <- xadjL + abs(xadjL)
  x - (xadjU - xadjL)/2
}

algo <- list(nP = 200,
             nG = 500,
             F = 0.5,
             CR = 0.90,
             min = Data$min,
             max = Data$max,
             pen = NULL,
             repair = repair,
             loopOF = FALSE,
             loopPen = TRUE,
             loopRepair = TRUE,
             storeF = FALSE)
sol <- DEopt(OF = obj_func, algo = algo, Data = Data)

colSums(sol$xbest * hedge_dv01_binned[-1] * 10) - Data$p


# hedge_bbg_data <- hedge_bbg_data %>% mutate(cusip = as.character(cusip))
new_b <- sol$xbest



(hedge_bbg_data[match(hedge_dv01_binned[[1]], hedge_bbg_data$cusip),] %>%
   mutate(amount = plyr::round_any(sol$xbest * 0.41,5) ))



#############################################

muni_dv01 <- pos_data %>% 
  select(initials, corp_hedge, cusip, yellow_key, tax_status, net_size) %>%
  inner_join(muni_cfs, by= c("cusip" = "id")) %>%
  mutate(cf_amount = cf_amount * net_size /100,
         dv01 = dv01 * net_size/100) %>%
  filter(tax_status == "EXEMPT") %>%
  mutate(bin = as.Date(cut_interval(cf_date, length = "1 year")))

# sum(muni_dv01$dv01)

tsy_dv01 <- pos_data %>% 
  select(initials, corp_hedge, cusip, yellow_key, tax_status, net_size) %>%
  inner_join(tsy_cfs, by= c("cusip" = "id")) %>%
  mutate(cf_amount = cf_amount * net_size /100,
         dv01 = dv01 * net_size/100) %>%
  mutate(bin = as.Date(cut_interval(cf_date, length = "1 year")))

# sum(tsy_dv01$dv01)

# breaks <- levels(cut_interval(muni_dv01$cf_date, length = "1 year"))
# breaks <- as.Date(breaks[seq(1, length(breaks), by = 5)])


ggplot(muni_dv01, aes(bin, dv01)) + 
  geom_histogram(aes(weight = dv01), stat = "identity") +  
  facet_wrap( ~ initials) +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"))

ggplot(arrange(muni_dv01, initials), aes(bin, dv01, group = initials, fill = initials)) + 
  geom_histogram(aes(weight = dv01), stat = "identity") +  
  facet_wrap( ~ initials) +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"))

############################END########################            
