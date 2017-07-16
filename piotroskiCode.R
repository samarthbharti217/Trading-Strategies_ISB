library(data.table)

CompleteData<-read.csv("CompleteData.CSV",header=TRUE)
CD<-data.frame(CompleteData)

#roa
CD$ROA <- with(CD, (CD$PAT -CD$Extra.Ordinary.items.cash.inflow+CD$Extra.Ordinary.items.cash.outflow)/shift(CD$Total.Assets, 1L, type="lag"))

#cfo
CD$ZCFO <- with(CD, (CD$CFO/shift(CD$Total.Assets, 1L, type="lag")))

#delroa
CD$DELROA <- with(CD, (CD$ROA -shift(CD$ROA, 1L, type="lag")))
#Accural
CD$Accural <- with(CD, (CD$ROA-CD$ZCFO))

#LEVERRATIO
CD$levr<-((CD$Long.Term.Borrowings*2)/(CD$Total.Assets+shift(CD$Total.Assets, 1L, type="lag")))

#Leverage
CD$Leverage <- with(CD, (CD$levr -shift(CD$levr, 1L, type="lag")))

#CurrentRatio
CD$CR<-CD$Current.Assets/CD$Current.Liabilities

#Liquid
CD$Liquid<-with(CD, (CD$CR -shift(CD$CR, 1L, type="lag")))

#EQ_OFFER
CD$EQ<-with(CD, (CD$Total.Capital -shift(CD$Total.Capital, 1L, type="lag")))

#TurnoverRAtio
CD$TOR<-with(CD, (CD$Sales /shift(CD$Total.Assets, 1L, type="lag")))

#TURN
CD$TURN<-with(CD, (CD$TOR -shift(CD$TOR, 1L, type="lag")))

#GMR
CD$GMR<-with(CD, (CD$Sales-CD$COGS)/CD$Sales)

#MARGIN
CD$MARGIN<-with(CD, (CD$GMR-shift(CD$GMR, 1L, type="lag")))

#F_ROA
CD$F_ROA <- with(CD, ifelse(CD$ROA>0, 1, 0))

#F_CFO
CD$F_CFO <- with(CD, ifelse(CD$ZCFO>0, 1, 0))

#F_DELROA
CD$F_DELROA <- with(CD, ifelse(CD$DELROA>0, 1, 0))

#F_DELLEVER
CD$F_DELLEVER <- with(CD, ifelse(CD$Leverage>0, 0, 1))

#F_ACCURAL
CD$F_ACCURAL <- with(CD, ifelse(CD$ZCFO>CD$ROA, 1, 0))

#F_DELLIQUID
CD$F_DELIQUID <- with(CD, ifelse(CD$Liquid>0, 1, 0))

#F_EQOFFER
CD$F_EQOFFER <- with(CD, ifelse(CD$EQ>0, 0, 1))

#F_MARGIN
CD$F_MARGIN<-with(CD,ifelse(CD$MARGIN>0,1,0))

#F_TURN
CD$F_TURN<-with(CD,ifelse(CD$TURN>0,1,0))

#F_SCORE
CD$F_SCORE<-with(CD,CD$F_ROA+CD$F_ACCURAL+CD$F_CFO+CD$F_DELIQUID+CD$F_DELLEVER+CD$F_DELROA+CD$F_EQOFFER+CD$F_MARGIN+CD$F_TURN)

CD_2015<-CD[substring(CD$Financial.Year,7,10)=="2015",]

CD_HIGH<-CD_2015[is.element(CD_2015$F_SCORE, c(8,9)),]
CD_LOW<-CD_2015[is.element(CD_2015$F_SCORE, c(0,1)),]



