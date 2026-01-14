; =========================================
; Japanese Market Savings → Stablecoin → BTC
; =========================================
; Agents:
; - people (risk-averse vs risk-loving)
; - banks (3)
; - 1 stablecoin issuer investing reserves at risk-free (2%/yr)
; Assumptions:
; - JPY only.
; - 1 tick = 1 day.
; - Bank interest = 0.1%/yr; applied daily.
; - Risk-free rate (issuer reserve yield) = 1%/yr; applied daily to reserves.
; - Stablecoin holders earn 0% by default (issuer keeps the RF carry).
; - Initial tick: all savings in banks.
extensions [fetch table]
; ---------- Breeds ----------
breed [ people person ]
breed [ banks bank ]
breed [ issuers issuer ]   ; exactly one

; ---------- Globals ----------
globals [
  year-length
  bank-apy          ; e.g., 0.001 (0.1%/yr)
  bank-daily-rate   ; bank-apy / year-length
  rf-apy            ; risk-free 0.02 (2%/yr)
  rf-daily-rate     ; rf-apy / year-length

  ;btc-price         ; JPY per BTC (use only if we're doing GBM)
  ;mu-btc            ; annual drift for BTC (use only if we're doing GBM)
  ;sigma-btc         ; annual vol for BTC (use only if we're doing GBM)
  ;I decided to not do GBM

  stablecoin-yield-apy    ; default 0.0 for holders
  stablecoin-yield-daily

  total-stablecoin-supply ; liability of issuer (JPY units, 1:1)
  issuer-reserves-jpy     ; assets in JPY
  issuer-equity-jpy       ; reserves - liabilities

  trust-in-bank
  trust-in-JPYC
  ; tracking
  day-counter

   ;---- GARCH(1,1) parameters & state (daily) ----
  garch-mu-annual        ; annual drift of log-returns (e.g., 0.10 = 10%/yr)
  garch-mu-daily         ; = garch-mu-annual / year-length
  garch-omega            ; long-run variance intercept (>0)
  garch-alpha            ; ARCH term weight (>=0)
  garch-beta             ; GARCH term weight (>=0), usually alpha+beta < 1
  garch-h                ; current conditional variance (daily)
  garch-eps              ; last shock ε_{t-1} (daily return minus mu)
  btc-log-price          ; log(BTC price)

  ;Using API to fetch bitcoin price data (experimenting)
  btc-jpy-5y
  dates-ms        ;; list of timestamps
  prices-jpy      ;; list of prices (JPY)
  n-days          ;; total # of daily points
  price-today     ;; price for the current tick/day
  date-ms-today   ;; timestamp for the current tick/day

  ;suggested by gpt to do the token analysis
  monetary-regime            ;; 0: none, 1: stablecoin-only, 2: CBDC-only, 3: both
  cbdc-privacy-penalty       ;; utility penalty (>=0)
  cbdc-yield-apy             ;; CBDC deposit rate (could be >= bank-apy)
  cbdc-yield-daily
  cbdc-merchant-accept       ;; % acceptance bump -> utility boost
  ;onramp-friction            ;; extra basis points when moving between bank/SC/BTC
  sc-redemption-friction     ;; % haircut during stress (0 in normal times)
  chance-of-blunder
  cbdc-blunder-counter
  JPYC-blunder-counter
  JPYC-merchant-accept
  JPYC-privacy-penalty

]

; ---------- People variables ----------
people-own [
  risk-type               ; "averse" or "loving"
  bank-jpy                ; JPY bank deposits
  sc-jpy                  ; stablecoin amount (JPY liability units)
  cbdc-jpy
  btc-units               ; BTC units
  target-w-bank           ; target weight for bank
  target-w-sc             ; target weight for stablecoin
  target-w_btc            ; target weight for bitcoin
  target-w_cbdc
  my-bank
  my-issuer
  yearly-salary
  employed
]

; ---------- Banks variables ----------
banks-own [
  bank-transaction-cost
  total-money-in-this-bank
]

; ---------- Issuer variables ----------
issuers-own [
  sc-transaction-cost; nothing extra; globals hold balance sheet
]

; =========================================
; Setup & Go
; =========================================

to setup
  clear-all
  set year-length 365
  ; interest rates
  set bank-apy 0.001
  set bank-daily-rate bank-apy / year-length
  set rf-apy 0.02
  set rf-daily-rate rf-apy / year-length
  set stablecoin-yield-apy 0.01 ;Since it has to be "stable", I'm assuming they really can't take that much of risks
  set stablecoin-yield-daily stablecoin-yield-apy / year-length

  ; BTC dynamics (defaults; add sliders if desired) USED FOR GBM
;  set mu-btc 0.30            ; 10%/yr drift (adjust via slider)
;  set sigma-btc 0.80         ; 80%/yr vol (adjust via slider)
;  set btc-price 10000000     ; 10,000,000 JPY/BTC initial price

  ; issuer balance sheet starts empty
  set total-stablecoin-supply 0
  set issuer-reserves-jpy 0
  set issuer-equity-jpy 0
  set trust-in-bank 100
  set trust-in-JPYC 20

  ; create banks and the issuer
  crt-banks
  crt-issuer

  ; create people
  crt-people
  set Unemployment-rate 3.5

  ;people create links with banks and stablecoin issuer
  setup-links

  ask banks [set total-money-in-this-bank ((sum [initial-wealth-jpy] of people with [my-bank = self]) + (sum [cbdc-jpy] of people with [my-bank = self]))]
  ifelse Historical/GARCH[
  fetch-btc-jpy-5y
  set n-days length prices-jpy
  set price-today item 1 item 0 btc-jpy-5y
  ]
  [set-up-garch]
  ; visuals
  setup-the-plots

  ;suggested by gpt for token analysis
  set cbdc-yield-apy 0.00
  set cbdc-yield-daily (cbdc-yield-apy / year-length)
  set cbdc-merchant-accept 0
 ; set onramp-friction 0.0
  set sc-redemption-friction 0.0

  apply-regime-params

  set day-counter 0
  reset-ticks

end

to go
  if not any? people [ stop ]
  if Historical/GARCH [if ticks >= n-days [ stop ]]

  set day-counter day-counter + 1

;  ; 0) people work and make money. Poor people are risk-averse, and Rich people are risk-loving.
;  work-and-make-money

  ; 1) interest accruals
  accrue-bank-interest
  accrue-stablecoin-holder-yield
  accrue-issuer-reserve-yield

  ; 2) update BTC price via historical data for the past 5 years, or use GARCH

  ifelse Historical/GARCH [
    let n ticks
    set price-today item n prices-jpy] [update-btc-price] ;; <- n-th day’s price (JPY) if Historical

  ; 3) CBDC/JPYC acceptance rate among merchants & CBDC/JPYC privacy rate adjusted here
  cbdc-environment-adjust
  JPYC-environment-adjust

  ; 4.0) Employment status subject to change every 2 years
  if day-counter mod 730 = 0 [ask people [set employed (random-float 1 >= unemployment-rate / 100)]]

  ; 4) People make daily expenses
  ask people [ make-money-and-make-expenses-and-allocate-the-remaining ]

  ; 5) people rebalance toward targets (via stablecoin as the bridge)
  ask people [ rebalance-portfolio ]

  ; 6) compute issuer equity
  set issuer-equity-jpy issuer-reserves-jpy - total-stablecoin-supply

  ; 7) update plots & tick
  update-the-plots
  tick
end

; =========================================
; Creators
; =========================================

to crt-banks
  create-banks num-of-banks [
    set color (30 + random-normal 5 1)
    set size 1.2
    set shape "square 2"
    set bank-transaction-cost random-normal 100 50
    setxy random-xcor 10
  ]
end

to crt-issuer
  create-issuers 1 [
    set color orange
    set size 1.4
    set shape "circle 2"
    set sc-transaction-cost random-normal 20 10
    setxy 0 0
  ]
end

to crt-people

  create-people Num-risk-averse-people [
    set color green
    set shape "person"
    set risk-type "averse"
    set my-bank one-of banks
    set my-issuer one-of issuers
    init-person
  ]

  create-people Num-risk-loving-people [
    set color red
    set shape "person"
    set risk-type "loving"
    set my-bank one-of banks
    set my-issuer one-of issuers
    init-person
  ]
end

to setup-links
  ask people[
    create-link-with my-bank
    create-link-with my-issuer]
end

to init-person  ;; turtle context (person)
  set size 0.8
  setxy random-xcor random-ycor
  ; initial allocation: all bank deposits
  set bank-jpy random-normal initial-wealth-jpy initial-wealth-jpy / 2
  set sc-jpy 0
  set btc-units 0

  ; default target weights (tune via sliders if you add them)
  if risk-type = "averse" [
    set target-w_btc 0.05
    set target-w_cbdc 0.15
    set target-w-sc  0.1
    set target-w-bank (1 - target-w_btc - target-w-sc)
    set yearly-salary random-normal (2.6 * 1000000) (0.9 * 1000000)
    set employed (random-float 1 >= unemployment-rate / 100)
  ]
  if risk-type = "loving" [
    set target-w_btc 0.1
    set target-w_cbdc  0.2
    set target-w-sc  0.3
    set target-w-bank (1 - target-w_btc - target-w-sc)
    set yearly-salary random-normal (6.5 * 1000000) (1.9 * 1000000)
    set employed (random-float 1 >= unemployment-rate / 100)
  ]
end

;work and make money
;to work-and-make-money 
;  let working-population round ((Num-risk-averse-people + Num-risk-loving-people) * (1 - (unemployment-rate / 100)))
;  ask n-of working-population people [
;      set bank-jpy bank-jpy + (yearly-salary / 365) * 0.2 ;All salary is first deposited in their bank account
;  ]
;end

; =========================================
; Interest & Price Dynamics
; =========================================

to accrue-bank-interest
  ask people [
    set bank-jpy bank-jpy * (1 + bank-daily-rate)
    if cbdc-jpy > 0 [ set cbdc-jpy cbdc-jpy * (1 + cbdc-yield-daily) ]
  ]
end

to accrue-stablecoin-holder-yield
  if stablecoin-yield-daily = 0 [ stop ]
  ask people [ if issuer-equity-jpy >= 0[
    let add sc-jpy * stablecoin-yield-daily
    set sc-jpy sc-jpy + add
    ; When JPYC holders earn yield, issuer must fund it from reserves
    set total-stablecoin-supply total-stablecoin-supply + add
    set issuer-reserves-jpy max (list 0 (issuer-reserves-jpy - add))]]
end

to accrue-issuer-reserve-yield
  ; issuer invests reserves at risk-free
  ; They will use JGB
  ; I could use API for this, but since I already did API for bitcoin it won't be that interesting I guess.
  set issuer-reserves-jpy issuer-reserves-jpy * (1 + rf-daily-rate)
end

to set-up-garch
  set garch-mu-annual 0.2
  set garch-mu-daily  (garch-mu-annual / year-length)

  ; Typical starting values; expose as sliders if you like
  set garch-omega 1e-6       ; intercept for variance
  set garch-alpha 0.1      ; ARCH weight on last shock^2
  set garch-beta  0.85    ; GARCH weight on last variance (alpha+beta < 1 recommended)

  set garch-h    0.001       ; initial daily variance (≈ 3.16% daily vol)
  set garch-eps  0           ; no prior shock at t=0

  set price-today 10000000     ; e.g., ¥10,000,000/BTC initial level
  set btc-log-price ln price-today
end

to update-btc-price
  ; GARCH(1,1): r_t = μ + sqrt(h_t)*z_t
  ; h_t = ω + α * ε_{t-1}^2 + β * h_{t-1}
  let z random-normal 0 1

  ; Update conditional variance h_t (keep it positive)
  set garch-h max list 1e-12 (garch-omega + garch-alpha * (garch-eps ^ 2) + garch-beta * garch-h)

  ; Daily log-return
  let r (garch-mu-daily + (sqrt garch-h) * z)

  ; Update last shock ε_t for next step
  set garch-eps (r - garch-mu-daily)

  ; Evolve log price and level
  set btc-log-price btc-log-price + r
  set price-today exp btc-log-price

  ; Safety floor
  if price-today < 1 [ set price-today 1  set btc-log-price ln 1 ]
end

;CBDC environment changes defined below.

to cbdc-environment-adjust; activated only when CBDC is available.
  if CBDC-available = TRUE[
  if random-float 1 <= 1 / 365 [
    set cbdc-merchant-accept  cbdc-merchant-accept + 1
  ]; assuming on avg. one merchant start accepting CBDC.

  if (random-float 1) > ((1 - chance-of-blunder) ^ (cbdc-merchant-accept)) [
    set cbdc-privacy-penalty cbdc-privacy-penalty + 10 ;Each time (even one) merchant(s) makes a blunder (eg. spillage of personal information) over the course of CBDC payments, overall concern for CBDC increases by 10%
    set cbdc-blunder-counter cbdc-blunder-counter + 1
  ]
  ]
end

to JPYC-environment-adjust ; always activated.
  if random-float 1 <= 1 / 365 [
    set JPYC-merchant-accept  JPYC-merchant-accept + 1
  ]; assuming on avg. one merchant start accepting JPYC.

  if (random-float 1) > ((1 - (chance-of-blunder / 2)) ^ (JPYC-merchant-accept)) [ ;decentralization = lower chance of blunder than CBDC (my assumption)
    set JPYC-privacy-penalty JPYC-privacy-penalty + 10;Each time (even one) merchant(s) makes a blunder (eg. spillage of personal information) over the course of JPYC payments, overall concern for JPYC increases by 10%
    set JPYC-blunder-counter JPYC-blunder-counter + 1
  ]
end

; People make expenses here
to make-money-and-make-expenses-and-allocate-the-remaining
  ;I've ignored remittances among people, because the amount of transaction of which would normally be much smaller than that of people's payments to merchants.

  ask people with [employed]  [
  let left-after-spending-today (0.2 * (yearly-salary / 365)); (Important assumption!) they leave 20% of the allocated money in the new place.

  let U_bank_expenses trust-in-bank
  let U_sc_expenses trust-in-JPYC + JPYC-merchant-accept - JPYC-privacy-penalty
  let U_cbdc_expenses trust-in-Bank + cbdc-merchant-accept - cbdc-privacy-penalty

  ifelse cbdc-available[
    let total_expenses_utility (e ^ U_bank_expenses + e ^ U_sc_expenses + e ^ U_cbdc_expenses)
    ;Above three utility functions represent the utilty they get from using each payment method. Note that this is separate from the ones in rebalance-portfolio

    let spend-from-bank (e ^ U_bank_expenses) / total_expenses_utility
    let spend-from-sc (e ^ U_sc_expenses) / total_expenses_utility
    let spend-from-cbdc (e ^ U_cbdc_expenses) / total_expenses_utility
    ;softmaxing. Trying to simulate more flexibility rather than winner-takes-all situations.

    set bank-jpy (bank-jpy + (spend-from-bank * left-after-spending-today))
    set sc-jpy (sc-jpy + (spend-from-sc * left-after-spending-today))
    set cbdc-jpy (cbdc-jpy + (spend-from-cbdc * left-after-spending-today))
  ][
    let total_expenses_utility (e ^ U_bank_expenses + e ^ U_sc_expenses)
    ;Above three utility functions represent the utilty they get from using each payment method. Note that this is separate from the ones in rebalance-portfolio

    let spend-from-bank (e ^ U_bank_expenses) / total_expenses_utility
    let spend-from-sc (e ^ U_sc_expenses) / total_expenses_utility
    ;softmaxing. Trying to simulate more flexibility rather than winner-takes-all situations.

    set bank-jpy (bank-jpy + (spend-from-bank * left-after-spending-today))
    set sc-jpy (sc-jpy + (spend-from-sc * left-after-spending-today))
  ]
  ]

  ask people with [employed = FALSE]  [
  let left-after-spending-today ( - my-net-worth / 2000); (Important assumption!) they spend 1/2000 of their total wealth in a day, if unemployed.

  let U_bank_expenses trust-in-bank
  let U_sc_expenses trust-in-JPYC + JPYC-merchant-accept - JPYC-privacy-penalty
  let U_cbdc_expenses trust-in-Bank + cbdc-merchant-accept - cbdc-privacy-penalty

  ifelse cbdc-available[
    let total_expenses_utility (e ^ U_bank_expenses + e ^ U_sc_expenses + e ^ U_cbdc_expenses)
    ;Above three utility functions represent the utilty they get from using each payment method. Note that this is separate from the ones in rebalance-portfolio

    let spend-from-bank (e ^ U_bank_expenses) / total_expenses_utility
    let spend-from-sc (e ^ U_sc_expenses) / total_expenses_utility
    let spend-from-cbdc (e ^ U_cbdc_expenses) / total_expenses_utility
    ;softmaxing. Trying to simulate more flexibility rather than winner-takes-all situations.

    set bank-jpy (bank-jpy + (spend-from-bank * left-after-spending-today))
    set sc-jpy (sc-jpy + (spend-from-sc * left-after-spending-today))
    set cbdc-jpy (cbdc-jpy + (spend-from-cbdc * left-after-spending-today))
  ][
    let total_expenses_utility (e ^ U_bank_expenses + e ^ U_sc_expenses)
    ;Above three utility functions represent the utilty they get from using each payment method. Note that this is separate from the ones in rebalance-portfolio

    let spend-from-bank (e ^ U_bank_expenses) / total_expenses_utility
    let spend-from-sc (e ^ U_sc_expenses) / total_expenses_utility
    ;softmaxing. Trying to simulate more flexibility rather than winner-takes-all situations.

    set bank-jpy (bank-jpy + (spend-from-bank * left-after-spending-today))
    set sc-jpy (sc-jpy + (spend-from-sc * left-after-spending-today))
  ]
  ]
end

; =========================================
; Rebalancing Logic
; =========================================

;Important assumption:To my knowledge, no countries, including Japan, has a plan/ is already allowing direct transaction between cryptos and CBDC. So, in this rebalancd-portfolio section, CBDC is not implicated for the most part.
;But the only exception is when you don't have enough deposit to be converted to stablecoin/BTC. In such cases, you first convert that to bank deposit, then convert again to whichever desired crypto.
;This is not really efficient way to buy JPYC/BTC because transaction between CBDC and bank deposit incurs additional fees, compared to just using bank deposit for buying the cryptos.
;So people always first try using cash deposit.

to rebalance-portfolio  ;; turtle context: person
  let nw my-net-worth

  if nw <= 0 [ stop ]

  let U_bank trust-in-Bank - [bank-transaction-cost] of my-bank - cbdc-blunder-counter; blunders in CBDC will also undermine the trust in banking system overall.
  let U_sc trust-in-JPYC - [sc-transaction-cost] of my-issuer - JPYC-blunder-counter

  ;suggested by gpt for token analysis
;  let U_bank_adj U_bank
;  let U_sc_adj   U_sc
;  let U_cbdc     0
;
;  if CBDC-available = TRUE [
;    ;; CBDC perceived as bank-like with higher acceptance but privacy penalty
;    set U_cbdc (trust-in-Bank + cbdc-merchant-accept - cbdc-privacy-penalty)
;    ;; Allow CBDC yield to sweeten bank utility (simple way): add to U_bank_adj
;    set U_bank_adj U_bank_adj + (100 * cbdc-yield-apy) ; this 100 is rather arbitrary
;  ]

;  ;; Apply common frictions
;  set U_bank_adj U_bank_adj - (onramp-friction * 10000) ;; convert to "bps-like" utility
;  set U_sc_adj   U_sc_adj   - (onramp-friction * 10000)
;(mahoro) doesn't really make sense to me. Commented out.

;  ;adjustment according to risk preferences.
;  if risk-type = "averse" [ set U_cbdc U_cbdc - (0.5 * cbdc-privacy-penalty) ]
;  if risk-type = "loving" [ set U_sc_adj U_sc_adj + 5 ]  ;; “crypto-curious” bonus

  ;; Decision remains the same structure; just compare the adjusted utilities.

  ;up until here.

  ifelse U_sc + random-normal 0 10 > U_bank [
   set target-w-sc target-w-sc + 0.01
    set target-w-bank target-w-bank - 0.01]
  [set target-w-sc target-w-sc - 0.01
    set target-w-bank target-w-bank + 0.01]
  set target-w-sc min(list 1 (max list 0 target-w-sc))
  set target-w-bank min(list 1 (max list 0 target-w-bank))

  let target-bank  target-w-bank * nw
  let target-sc    target-w-sc  * nw
  let target-btcyen  target-w_btc * nw
  let target-btc-units target-btcyen / price-today

  ; Move a fraction adj-speed toward each target
  let delta-bank (target-bank - bank-jpy) * adj-speed
  let delta-sc   (target-sc   - sc-jpy)   * adj-speed
  let delta-btc  (target-btc-units - btc-units) * adj-speed


  ; Step 1: adjust bank <-> stablecoin (mint/burn to move JPY)
  if delta-sc > 0 [
    ; need more stablecoin: mint from bank, OR CBDC if bank deposit is not sufficient
    let mint-amt min (list delta-sc (bank-jpy + cbdc-jpy))
    ifelse bank-jpy > (mint-amt + [bank-transaction-cost] of my-bank) [
      set bank-jpy bank-jpy - mint-amt - [bank-transaction-cost] of my-bank
      set sc-jpy sc-jpy + mint-amt
      set total-stablecoin-supply total-stablecoin-supply + mint-amt; this is liability for stablecoin issuer
      set issuer-reserves-jpy issuer-reserves-jpy + mint-amt
      ask my-bank [set total-money-in-this-bank (total-money-in-this-bank - mint-amt + 0.5 * bank-transaction-cost)]; banks pocket part of the transaction costs.
    ][
      if issuer-equity-jpy >= 0 and total-stablecoin-supply >= 0 [
        set bank-jpy 0
        set cbdc-jpy cbdc-jpy - (mint-amt - bank-jpy) - [bank-transaction-cost] of my-bank
        set sc-jpy sc-jpy + mint-amt
        set total-stablecoin-supply total-stablecoin-supply + mint-amt; this is liability for stablecoin issuer
        set issuer-reserves-jpy issuer-reserves-jpy + mint-amt
        ask my-bank [set total-money-in-this-bank (total-money-in-this-bank - mint-amt + 0.5 * bank-transaction-cost)]; banks pocket part of the transaction costs.
    ]]
  ]
  if delta-sc < 0 [
    ; have too much stablecoin: redeem (burn) to bank
    let burn-amt min (list (- delta-sc) sc-jpy)
    if burn-amt > 0 [
      if issuer-equity-jpy >= 0 and total-stablecoin-supply >= 0[
        set sc-jpy sc-jpy - burn-amt - [sc-transaction-cost] of my-issuer
        set bank-jpy bank-jpy + burn-amt
        set total-stablecoin-supply total-stablecoin-supply - burn-amt
        set issuer-reserves-jpy issuer-reserves-jpy - burn-amt + 0.8 * [sc-transaction-cost] of my-issuer ;JPYC issuer pocket part of transaction costs (and assuming JPYC's system is more efficient, they take higher ratio than banks can.)
        ask my-bank [set total-money-in-this-bank (total-money-in-this-bank + burn-amt)]
    ]
  ]
  ]

  ; After bank<->stablecoin move, try to adjust BTC using EITHER stablecoin or cash deposit, DEPENDS ON the calculated utility (I heard CBDC is not really used to make crypto-related transactions. So for simplicity, I'm not allowing people to directly use CBDC here)
  ; Also, I assumed no transaction costs between BTC and JPYC. I heard the main advantage of stablecoin is tractability to exchange with other cryptos
  ifelse U_sc + random-normal 0 10 > U_bank[
  if delta-btc > 0 [
    ; Buy BTC: spend stablecoin
    let needed-jpy (delta-btc * price-today)
    let spend min (list needed-jpy sc-jpy)
    if spend > 0 [
      set sc-jpy sc-jpy - spend
      set btc-units btc-units + (spend / price-today)
      ; issuer liability stays the same (holders transfer SC among themselves/exchanges),
      ; but to keep it simple we leave supply unchanged here. -> what does this mean?(important)
    ]
  ]
  if delta-btc < 0 [
    ; Sell BTC: receive stablecoin
    let btc-to-sell min (list (- delta-btc) btc-units)
    if btc-to-sell > 0 [
      let proceeds btc-to-sell * price-today
      set btc-units btc-units - btc-to-sell
      set sc-jpy sc-jpy + proceeds
      ; no immediate mint/burn; keeps supply unchanged (trades internal to ecosystem)
    ]
  ]
  ][ if delta-btc > 0 [
    ; Buy BTC: spend CASH DEPOSIT, not stablecoin
    let needed-jpy (delta-btc * price-today)
    let spend min (list needed-jpy (bank-jpy + cbdc-jpy))
    ifelse (spend + [bank-transaction-cost] of my-bank + 100) < bank-jpy [
      set bank-jpy bank-jpy - spend - ([bank-transaction-cost] of my-bank + 100);highest transaction costs between financial instruments
      set btc-units btc-units + (spend / price-today)
      ask my-bank [set total-money-in-this-bank total-money-in-this-bank - spend + 0.5 * (bank-transaction-cost + 100)]
      ; issuer liability stays the same (holders transfer SC among themselves/exchanges),
      ; but to keep it simple we leave supply unchanged here.
    ][
      if issuer-equity-jpy >= 0 and total-stablecoin-supply >= 0[
      set bank-jpy 0
      set cbdc-jpy cbdc-jpy - (spend - bank-jpy) - [bank-transaction-cost] of my-bank - 100
      set btc-units btc-units + (spend / price-today)
      ask my-bank [set total-money-in-this-bank total-money-in-this-bank - spend + 0.5 * (bank-transaction-cost + 100)]
      ]]
  ]
  if delta-btc < 0 [
    ; Sell BTC: receive CASH DEPOSIT, not stablecoin
    let btc-to-sell min (list (- delta-btc) btc-units)
    if btc-to-sell > 0 [
      let proceeds btc-to-sell * price-today
        set btc-units btc-units - btc-to-sell - (([bank-transaction-cost] of my-bank + 100) / price-today)
        ask my-bank [set total-money-in-this-bank total-money-in-this-bank + proceeds + 0.5 * (bank-transaction-cost + 100)]
    ]
  ]
]

  ; Optional micro-adjust bank using remaining delta-bank (small drift)
  ; Here we gently nudge toward target by shifting between bank and stablecoin.
;  if delta-bank > 0 [
;    ; move from sc to bank via redemption
;    let amt min (list delta-bank sc-jpy)
;    if amt > 0 [
;      set sc-jpy sc-jpy - amt
;      set bank-jpy bank-jpy + amt
;      set total-stablecoin-supply total-stablecoin-supply - amt
;      set issuer-reserves-jpy max (list 0 (issuer-reserves-jpy - amt))
;    ]
;  ]
;  if delta-bank < 0 [
;    ; move from bank to sc via mint
;    let amt min (list (- delta-bank) bank-jpy)
;    if amt > 0 [
;      set bank-jpy bank-jpy - amt
;      set sc-jpy sc-jpy + amt
;      set total-stablecoin-supply total-stablecoin-supply + amt
;      set issuer-reserves-jpy issuer-reserves-jpy + amt
;    ]
;  ]
end

;suggested by gpt for token analysis
to apply-regime-params
  ifelse CBDC-available = FALSE
  [
    ;; stablecoin-only
    set trust-in-JPYC 20
    set JPYC-merchant-accept 10
    set JPYC-privacy-penalty 10;decentralized=lower risk of privacy issues
    set cbdc-yield-apy 0
    set cbdc-yield-daily 0
    set cbdc-merchant-accept 0
    set cbdc-privacy-penalty 0
    set chance-of-blunder 0.0001 ;(mahoro) a given one merchant have 99.99% chance of successfully protecting privacy, per day.
    set cbdc-blunder-counter 0
    set JPYC-blunder-counter 0
    ;set onramp-friction 0.001  ;; SC makes crypto on/off-ramps cheaper
  ]
  [
    ;; both stablecoin and CBDC available
    set trust-in-JPYC 20
    set JPYC-merchant-accept 10
    set JPYC-privacy-penalty 10
    set cbdc-yield-apy 0.001; (mahoro) I wasn't sure if CBDC offers APY. It's not conclusive because BOJ is yet to release CBDC. For now, I assumed it to be 1%.
    set cbdc-yield-daily cbdc-yield-apy / year-length
    set cbdc-merchant-accept 10
    set cbdc-privacy-penalty 10
    set chance-of-blunder 0.0001 ;(mahoro) a given one merchant have 99.99% chance of successfully protecting privacy, per day.
    set cbdc-blunder-counter 0
    set JPYC-blunder-counter 0
    ;set onramp-friction 0.0008
  ]
end
; up until here

to-report my-net-worth  ;; turtle context: person
  report bank-jpy + cbdc-jpy + sc-jpy + (btc-units * price-today)
end

; =========================================
; bitcoin price data with API
; =========================================

;; Fetch around 5 years of daily BTC/JPY data (about 1827 days, incl. leap)
to fetch-btc-jpy-5y
  let url "https://min-api.cryptocompare.com/data/v2/histoday?fsym=BTC&tsym=JPY&limit=2000"
  let txt fetch:url url
  let root table:from-json txt
  let data table:get root "Data"         ; check structure: might be nested under "Data"
  let arr table:get data "Data"          ; actual array of daily records
  set btc-jpy-5y map
  [ d -> (list ((table:get d "time") * 1000) (table:get d "close")) ] arr
  set prices-jpy map [p -> item 1 p] btc-jpy-5y
end
; =========================================
; Plots & Monitors
; =========================================

to setup-the-plots
  set-current-plot "BTC Price (JPY, Million)"
  clear-plot

  set-current-plot "Issuer Balance Sheet (JPY, Billion)"
  clear-plot

  set-current-plot "Average Net Worth by Risk Preference Type (JPY, Million)"
  clear-plot

  set-current-plot "Average Asset Allocation (Risk Averse)"
  clear-plot

  set-current-plot "Average Asset Allocation (Risk Loving)"
  clear-plot
end

to update-the-plots
  set-current-plot "BTC Price (JPY, Million)"
  plotxy day-counter price-today / 1000000

  set-current-plot "Issuer Balance Sheet (JPY, Billion)"
  set-current-plot-pen "Reserves (Bn)"
  plotxy day-counter issuer-reserves-jpy / 1000000000
  set-current-plot-pen "Liabilities (Bn)"
  plotxy day-counter total-stablecoin-supply / 1000000000
  set-current-plot-pen "Equity (Bn)"
  plotxy day-counter issuer-equity-jpy / 1000000000

  set-current-plot "Average Net Worth by Risk Preference Type (JPY, Million)"
;  let min-range 0.8 * (min (avg-nw "averse") (avg-nw "loving"))
;  let max-range 1.2 * (max ((avg-nw "averse") (avg-nw "loving")))
;  set-plot-y-range min-range max-range
  set-current-plot-pen "Averse"
  plotxy day-counter avg-nw "averse"
  set-current-plot-pen "Loving"
  plotxy day-counter avg-nw "loving"

  set-current-plot "Average Asset Allocation (Risk Averse)"
  set-current-plot-pen "Bank Deposit"
  plotxy day-counter ((mean [bank-jpy] of people with [risk-type = "averse"]) /  (mean [ my-net-worth ] of people with [risk-type = "averse"]))
  set-current-plot-pen "CBDC"
  plotxy day-counter ((mean [cbdc-jpy] of people with [risk-type = "averse"]) /  (mean [ my-net-worth ] of people with [risk-type = "averse"]))
  set-current-plot-pen "JPYC"
  plotxy day-counter ((mean [sc-jpy] of people with [risk-type = "averse"]) /  (mean [ my-net-worth ] of people with [risk-type = "averse"]))
  set-current-plot-pen "Bitcoin"
  plotxy day-counter (1 - (((mean [bank-jpy] of people with [risk-type = "averse"]) /  (mean [ my-net-worth ] of people with [risk-type = "averse"])) + ((mean [cbdc-jpy] of people with [risk-type = "averse"]) /  (mean [ my-net-worth ] of people with [risk-type = "averse"])) + ((mean [sc-jpy] of people with [risk-type = "averse"]) /  (mean [ my-net-worth ] of people with [risk-type = "averse"]))))

  set-current-plot "Average Asset Allocation (Risk Loving)"
  set-current-plot-pen "Bank Deposit"
  plotxy day-counter ((mean [bank-jpy] of people with [risk-type = "loving"]) /  (mean [ my-net-worth ] of people with [risk-type = "loving"]))
  set-current-plot-pen "CBDC"
  plotxy day-counter ((mean [cbdc-jpy] of people with [risk-type = "loving"]) /  (mean [ my-net-worth ] of people with [risk-type = "loving"]))
  set-current-plot-pen "JPYC"
  plotxy day-counter ((mean [sc-jpy] of people with [risk-type = "loving"]) /  (mean [ my-net-worth ] of people with [risk-type = "loving"]))
  set-current-plot-pen "Bitcoin"
  plotxy day-counter (1 - (((mean [bank-jpy] of people with [risk-type = "loving"]) /  (mean [ my-net-worth ] of people with [risk-type = "loving"])) + ((mean [cbdc-jpy] of people with [risk-type = "loving"]) /  (mean [ my-net-worth ] of people with [risk-type = "loving"])) + ((mean [sc-jpy] of people with [risk-type = "loving"]) /  (mean [ my-net-worth ] of people with [risk-type = "loving"]))))



end

to-report avg-nw [ kind ]
  let ps people with [ risk-type = kind ]
  if any? ps [ report (mean [ my-net-worth ] of ps) / 1000000 ]
  report 0
end

;to-report averse-avg-ratio [asset]
;  let ps people with [ risk-type = "averse" ]
;  if any? ps [ report ((mean [bank-jpy] of ps) /  (mean [ my-net-worth ] of ps)) ]
;  report 0
;end
@#$#@#$#@
GRAPHICS-WINDOW
1162
417
1447
703
-1
-1
8.4
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
25
32
91
65
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
101
32
164
65
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
11
77
445
238
BTC Price (JPY, Million)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
11
244
444
419
Issuer Balance Sheet (JPY, Billion)
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Reserves (Bn)" 1.0 0 -16777216 true "" ""
"Liabilities (Bn)" 1.0 0 -13345367 true "" ""
"Equity (Bn)" 1.0 0 -2674135 true "" ""

PLOT
10
427
434
613
Average Net Worth by Risk Preference Type (JPY, Million)
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Averse" 1.0 0 -16777216 true "" ""
"Loving" 1.0 0 -7500403 true "" ""

MONITOR
7
627
211
672
BTC price (JPY, Million)
price-today / 1000000
3
1
11

MONITOR
223
627
435
672
JPYC issuer equity/reserves ratio (%)
issuer-equity-jpy * 100 / issuer-reserves-jpy
2
1
11

SWITCH
491
37
644
70
Historical/GARCH
Historical/GARCH
1
1
-1000

TEXTBOX
648
38
866
66
Dictates which price data to use; Historical if On, GARCH if Off.
11
0.0
1

SLIDER
936
317
1134
350
initial-wealth-jpy
initial-wealth-jpy
1000000
10000000
1.0E7
1000000
1
NIL
HORIZONTAL

SLIDER
939
363
1111
396
adj-speed
adj-speed
0
0.5
0.001
0.001
1
NIL
HORIZONTAL

TEXTBOX
8
678
201
720
If you choose GARCH, initial price of BTC is set at 10 million JPY
11
0.0
1

TEXTBOX
1144
324
1421
352
Initial wealth ~ N(10 million, 5 million) yen in their bank/person
11
0.0
1

TEXTBOX
978
628
1165
698
green people = risk-averse\nred people = risk-loving\nsquares = banks (not identical)\ncircle = JPYC issuer
11
0.0
1

TEXTBOX
1116
367
1448
404
Size of nudge toward desired portfolio (max=1). Meant to simulate liquidity; high number = more liquid
11
0.0
1

SLIDER
937
413
1115
446
Unemployment-rate
Unemployment-rate
0
10
3.5
0.1
1
NIL
HORIZONTAL

SLIDER
939
459
1147
492
Num-risk-averse-people
Num-risk-averse-people
0
100
19.0
1
1
NIL
HORIZONTAL

SLIDER
940
500
1145
533
Num-risk-loving-people
Num-risk-loving-people
0
100
31.0
1
1
NIL
HORIZONTAL

SWITCH
492
79
643
112
CBDC-available
CBDC-available
1
1
-1000

TEXTBOX
1122
423
1272
441
(in %)
11
0.0
1

TEXTBOX
651
81
941
123
2 Scenarios: with/without CBDC adoption. Note that JPYC is always available (consistent with reality).
11
0.0
1

MONITOR
943
10
1097
55
NIL
cbdc-blunder-counter
17
1
11

TEXTBOX
945
58
1265
103
Number of ticks where at least one merchant makes a blunder (ex. information spillage), leading to loss of reliability.
11
0.0
1

MONITOR
944
119
1095
164
NIL
cbdc-merchant-accept
17
1
11

TEXTBOX
946
166
1286
208
Number of merchants currently accepting each payment method. For simplicity, initial number is set at 10, probably smaller than real-life cases.
11
0.0
1

MONITOR
1105
10
1256
55
NIL
JPYC-blunder-counter
17
1
11

MONITOR
1106
118
1260
163
NIL
JPYC-merchant-accept
17
1
11

BUTTON
172
32
272
65
go one tick
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
945
225
1092
270
NIL
cbdc-privacy-penalty
3
1
11

MONITOR
1108
221
1252
266
NIL
JPYC-privacy-penalty
3
1
11

PLOT
451
131
894
296
Average Asset Allocation (Risk Averse)
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Bank Deposit" 1.0 0 -16777216 true "" ""
"CBDC" 1.0 0 -7500403 true "" ""
"JPYC" 1.0 0 -2674135 true "" ""
"Bitcoin" 1.0 0 -955883 true "" ""

PLOT
452
301
894
475
Average Asset Allocation (Risk Loving)
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Bank Deposit" 1.0 0 -16777216 true "" ""
"CBDC" 1.0 0 -7500403 true "" ""
"JPYC" 1.0 0 -2674135 true "" ""
"Bitcoin" 1.0 0 -955883 true "" ""

SLIDER
935
277
1107
310
num-of-banks
num-of-banks
1
50
50.0
1
1
NIL
HORIZONTAL

TEXTBOX
939
536
1138
592
19:31 split approximates the income distribution in Japan. Also, smaller num. of agents = faster simulation
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
