# Database Defaults -------------------------------------------------------

c_server  <- "NJ1PVSQL01"
c_database  <- "MuniRepository"
c_uid <- "sql_Munidesk_App"
c_pwd <- "Munidesk!"

# Traders and Accounts ----------------------------------------------------

c_trader_inits <- c("RJ", "LF", "MB", "EM", "MXA", "OOC", "GHC")
c_trader_inits <- factor(c_trader_inits, levels = c_trader_inits)

c_exempt_accounts <- paste0(c_trader_inits, "-EX")
c_exempt_accounts <- factor(c_exempt_accounts, levels = c_exempt_accounts)

c_taxable_accounts <- paste0(c_trader_inits, "-TAX")
c_taxable_accounts <- factor(c_taxable_accounts, levels = c_taxable_accounts)

c_tsy_accounts <- paste0(c_trader_inits, "-TSY")
c_tsy_accounts <- factor(c_tsy_accounts, levels = c_tsy_accounts)


# Bloomberg Fields --------------------------------------------------------

c_muni_bbg_fields <- c("ID_CUSIP","MARKET_SECTOR_DES", "ISSUER_BULK", 
                     "ISSUER_DESCRIPTION_2ND_LINE_BULK", "CPN",
                     "MATURITY","WORKOUT_DT_BID", "NXT_CALL_DT", 
                     "NXT_CALL_PX", "SETTLE_DT", "STATE_CODE",
                     "CPN_TYP","MUNI_TAX_PROV")

c_bma_tenors <- c(1:5, 7, 10, 12, 15, 20, 25, 30)
c_bma_securities <- paste0("USSMSB", c_bma_tenors, " Curncy")
c_bma_securities <- factor(c_bma_securities, levels = c_bma_securities)
c_bma_bbg_fields <- c("PX_LAST")


c_tsy_bbg_fields <- c("ID_CUSIP", "CPN", "MATURITY","ISSUE_DT", 
                    "SETTLE_DT", "PX_LAST" )
c_tsy_hedge_maturities <- c(5, 7, 10, 30)
c_tsy_hedge_letters <- c("","B")
c_tsy_hedges <- paste(mapply(paste0, paste0("CT", c_tsy_hedge_maturities), 
                           rep(c_tsy_hedge_letters, 
                               each = length(c_tsy_hedge_maturities))), "Govt")
c_tsy_hedges <- factor(c_tsy_hedges, levels = c_tsy_hedges)


# Analysis Defaults -------------------------------------------------------

c_curve_shocks_default <- seq(-1.00, 1.00, by = 0.10)