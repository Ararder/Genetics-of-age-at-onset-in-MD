check_for_code <- function(codes, col_name, data){
  # A function for dealing with datasets where you want to find all individuals with a specific code,
  # where the code can be in any of the columns.
  # The function assumes that the first column is the identifier for each row
  derive <- function(data, code){
    if(code %>% is.character()){
      column <- data %>% 
        filter(if_any(-1, any_vars(str_detect(.,code)))) %>% 
        mutate(x = 1) %>% select(1, x) 
      names(column)[2] <- code
      
    } else {
      column <- data %>% 
        filter(if_any(2:ncol(data),  ~.x %in% all_of(code))) %>% 
        mutate(x = 1) %>% select(1, x) 
      names(column)[2] <- paste0("code", code)
    }
    
    print( paste0("For code: ", code, " found a total of ", nrow(column), " cases"))
    column %>% 
      mutate(temp = 1) %>% 
      select(1, {{ col_name }} := temp) %>% 
      tibble()
  }
  # Check if the input is one or multiple codes
  if(length(codes) == 1){
    derive(data, codes) %>% 
      mutate(temp = 1) %>% 
      select(1, {{ col_name }} := temp) %>% 
      tibble()
      
  } else {
    # Sometimes you want to merge multiple codes into "one" definition.
    output <- codes %>% 
      map(derive, data = data) %>% 
      reduce(full_join) %>% 
      mutate(temp = 1) %>% 
      select(1, {{ col_name }} := temp) %>% 
      tibble()
    print(
      paste0("All joined together, found a total of ", nrow(output), " cases")
    )
    return(output)
  }
}