library(rsconnect)

rsconnect::setAccountInfo(name='nathany', token='505C6D6E45CB82B211B6418446055DA5', secret='Kbgiq4XHaqQeFCChUGVBgQLooaIK8HAFveDqcj5T')

rsconnect::deployApp("shinyApp", appName = "education-statistics")
