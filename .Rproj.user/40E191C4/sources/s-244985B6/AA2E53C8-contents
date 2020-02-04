#' Calculate weighted RFM scores based on recency, frequency and monetary values of customer purchases and sort customers to groups
#'
#'Arguments
#' @param data - database of customer purchases with details
#' @param weight_recency - weight of recency factor
#' @param weight_frequency - weight of frequency factor
#' @param weight_monetary - weight of monetary values of purchases
#'
#' @return Returns a data file with weighted scores of customers and their assigned group
#' @export


RFMfunction <- function(data,
                        weight_recency=1,
                        weight_frequency=1,
                        weight_monetary=1){

  # Load libraries with require()
  require(data.table)
  require(lubridate)
  require(Hmisc)

  # Ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency,
                                        weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency,
                                            weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency,
                                          weight_frequency, weight_monetary)

  # RFM measures
  max.Date <- max(data$TransDate)
  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = sum(PurchAmount)/.N),
    by="Customer"
    ]

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+
         weight_frequency2*frequency+
         weight_monetary2*monetary,
       by=Customer]

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}
