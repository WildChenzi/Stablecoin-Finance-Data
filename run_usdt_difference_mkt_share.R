library(readxl)
library(ivreg)
library(dplyr)

data <- read_excel("USDT_reg.xlsx")
names(data)


data$USDT_dev = abs(data$USDT - 1)*100
data$USDT_Crypto_share <- data$USDT_MktCap/data$crypto_MktCap
data$DAI_Crypto_share <- data$DAI_MktCap/data$crypto_MktCap
data$log_crypto_MktCap = log(data$crypto_MktCap)

data <- data %>%
  arrange(date) %>%
  mutate(USDT_Crypto_share_diff = USDT_Crypto_share - lag(USDT_Crypto_share, 1))

iv_model_USDT <- ivreg(USDT_dev ~ HHI + GL_month_diff + SOFR_ONRRP + VIX +
                         `Fear Greedy Index` + dxy + jpy + gold_return + `4w_bid_cover` +
                         `3m_bid_cover` + `3m_rate` + repo_share + bill_share + CorporateBond_PreciousMetal  | 
                         USDT_Crypto_share_diff |  
                         log_crypto_MktCap + DAI_Crypto_share,  
                       data = data)

summary(iv_model_USDT)


iv_model_USDT2 <- ivreg(USDT_dev ~ HHI + `BloomBerg Financial Condition` + SOFR_ONRRP + VIX +
                          `Fear Greedy Index` + dxy + jpy + gold_return + `4w_bid_cover` +
                          `3m_bid_cover` + `3m_rate` + repo_share + bill_share + CorporateBond_PreciousMetal  | 
                          USDT_Crypto_share_diff |  
                          log_crypto_MktCap + DAI_Crypto_share,  
                        data = data)

summary(iv_model_USDT2)

