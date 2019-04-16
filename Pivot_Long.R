# get data
    wide.data <- readr::read_csv('responses//_Master.csv')
# tidy
    long.data <- wide.data %>% tidyr::gather(key = 'field', value = 'value', -c('agency', 'date_submitted', 'timestamp')) %>% arrange(agency)
# write file
    readr::write_csv(long.data, path = 'responses//_Master_Long.csv')
