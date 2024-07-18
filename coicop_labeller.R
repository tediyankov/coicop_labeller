pacman::p_load(dplyr, 
               fuzzyjoin, 
               e1071,
               openai,
               stringr,
               worlddataverse, 
               tm, 
               fastNaiveBayes,
               tidytext, 
               SnowballC, 
               forcats, 
               ggplot2, 
               WeightSVM, 
               caret, 
               superml, 
               text2vec, 
               tidyr)

source("0_gpt_prep.R")
source("config.R")

args <- commandArgs(trailingOnly=TRUE)

err_no_params <- paste0("No parameters were given. This function requires three arguments. \n",
              "\tfirst argument should be a string specifying the path to the products .csv\n",
              "\tsecond argument should be a string with the name of the product id column\n",
              "\tthird argument should be a string with the name of the product name column\n",
              "try:\n",
              "\tRscript prep_model_data.R products.csv product_id product_name")

# Parse arguments
if (length(args) < 3){
  stop(err_no_params)
} else if (length(args) == 3){
  product_path <- args[1]
  product_col_name <- args[2]
  product_id_col_name <- as.numeric(args[3])
} else {
  stop(err)
}

if (is.na(COICOP) || is.na(SAMPLE_FRAC) || is.na(MODEL_SET)) stop(err_no_params)

products <- read.csv(product_path)

Sys.setenv(
  OPENAI_API_KEY = OPEN_API_KEY
)

# This function takes in a csv of products that contains the following columns: 
#   product_id_col_name: unique per row, this can simply be the index
#   product_col_name: the name of the product attempting to be classified

coicop_labeller <- function(products,
                product_col_name,
                product_id_col_name
                ){
  
  execution_times_path <- paste(local_path, "execution_times_per_100k_", OPEN_AI_MODEL, ".csv", sep="")
  
  get_file_name <- function(chunk_index) {
    # Make the file name based on the date of when the labels were created
    return(paste("gen_labels_", MODEL_SET, "_", chunk_index, sep=""))
  }
  
  generate_labels <- function(chunk_index, products_list) {
    prompt <- paste("Return the coicop codes for the items below in a csv format with the product_id and the coicop label such as: \n product_id, coicop label. ",
                    "Classify all ", CHUNK_SIZE, " of the labels provided below.",
                    chunk_to_str(products_list[[chunk_index]]),sep="")
    out_file_name <- paste(local_path, get_file_name(chunk_index), ".txt", sep="")
    
    response <- create_chat_completion(
      model = OPEN_AI_MODEL,
      temperature = .5,
      n = 1,
      messages=list(
        list("role" = "system", 
             "content" = systeminput),
        list("role" = "user", 
             "content" = prompt)
      )
    )
    writeLines(response$choices$message.content, out_file_name)
    cost = as.numeric(response$usage$prompt_tokens)*0.005 / 1000 + 
      as.numeric(response$usage$completion_tokens)*0.0025 / 1000
    if (verbose){print(paste("Full Tokens used: ", response$usage$total_tokens, " Approximate Cost: $", round(cost, digits=2), sep=""))}
  }
  
  predictions_to_numeric <- function(db) {
    db$coicop_modeled_1 <- as.numeric(db$coicop_modeled_1)
    db$coicop_modeled_2 <- as.numeric(db$coicop_modeled_2)
    db$coicop_modeled_3 <- as.numeric(db$coicop_modeled_3)
    db$coicop_modeled_4 <- as.numeric(db$coicop_modeled_4)
    return(db)
  }
  
  # Convert to a CSV
  gen_labels_to_csv <- function(chunk_index) {
    out_file_name <- paste(local_path, get_file_name(chunk_index), ".txt", sep="")
    
    # Removes any lines that don't follow the product code format
    lines <- readLines(out_file_name)
    csv_line <- grep("csv", lines)
    lines <- lines[(csv_line + 1):length(lines)]
    temp_file <- tempfile()
    writeLines(lines, temp_file)
    
    txt <- read.csv(temp_file, header = TRUE, stringsAsFactors = FALSE)
    # Removes Whitespace
    txt[] <- lapply(txt, function(x) if(is.character(x)) trimws(x) else x)
    colnames(txt) <- c("product_id", "coicop.label")
    colnames(txt)
    product_id_pattern <- "(Codebook-\\d+)|(^[A-Z]{3}-\\d{4}-[A-Za-z0-9]{6,})$"
    code_pattern <- "^\\d{1,2}\\.\\d{1}\\.\\d{1}\\.\\d{1}$"
    
    labelled_by_index <- txt %>%
      filter(str_detect(product_id, product_id_pattern) & 
               (str_detect(coicop.label, code_pattern) | 
                  str_detect(coicop.label, "^\\d{1,2}\\.\\d{1}\\.\\d{1}$") | 
                  str_detect(coicop.label, "^\\d{1,2}\\.\\d{1}$") | 
                  str_detect(coicop.label, "^\\d{1,2}$")
                )
             ) %>%
      rename(code = coicop.label)
    
    labelled_by_index <- labelled_by_index %>%
      mutate(code = case_when(
        str_detect(code, "^\\d{1,2}\\.\\d{1}\\.\\d{1}$") ~ paste0(code, ".0"),
        str_detect(code, "^\\d{1,2}\\.\\d{1}$") ~ paste0(code, ".0.0"),
        str_detect(code, "^\\d{1,2}$") ~ paste0(code, ".0.0.0"),
        TRUE ~ code
      ))
    
    labelled_by_index <- separate(labelled_by_index, code, into = c("coicop_modeled_1", "coicop_modeled_2", 
                                                                    "coicop_modeled_3", "coicop_modeled_4"),
                                  sep = "\\.", fill = "right", convert = TRUE)
    labelled_by_index <- predictions_to_numeric(labelled_by_index)
    
    write.csv(labelled_by_index, paste(local_path, get_file_name(chunk_index), ".csv", sep=""))
  }
  
  update_labelled_df <- function(chunk_index, execution_time=NA) {
    newly_labelled_data <- read.csv(paste(local_path, get_file_name(chunk_index), ".csv", sep=""))
    execution_time_per_100k <- as.numeric(execution_time) / nrow(newly_labelled_data) * 100000
    if (verbose){print(paste(nrow(newly_labelled_data), "labels were generated out of", CHUNK_SIZE))}
    if(file.exists(gpt_output_file)) {
      labelled_data <- read.csv(gpt_output_file)
      
      shared_cols = intersect(colnames(labelled_data), colnames(newly_labelled_data))
      
      shared_cols = c("product_id", "coicop_modeled_1", "coicop_modeled_2", "coicop_modeled_3", "coicop_modeled_4")
      
      labelled_data <- predictions_to_numeric(labelled_data)
      newly_labelled_data <- predictions_to_numeric(newly_labelled_data)
      
      # Add newly labelled rows to the existing set of labelled entries. 
      newly_labelled_data <- rbind(labelled_data %>% select(shared_cols), 
                                   newly_labelled_data %>% select(shared_cols))
    }
    write.csv(newly_labelled_data, gpt_output_file, row.names=FALSE)
  }
  
  products_to_label <- products
  
  # Checks if existing GPT output file exists
  if(file.exists(gpt_output_file)) {
    # Only need this to remove rows that have already been labelled by GPT
    if (verbose){print(paste("Existing full GPT output file exists at", gpt_output_file))}
    labelled_data <- read.csv(gpt_output_file) %>% 
      select(product_id_col_name)
    
    # Only process the rows that contain product codes not in the existing file
    products_to_label <- products[!(products[product_id_col_name] %in% labelled_data[product_id_col_name]), ] %>% 
      select(product_id_col_name, product_col_name)
    
  } else {
    if (verbose){
      print(paste("No full GPT output file exists with the name: ", gpt_output_file, 
                ". Creating new file with this name and location.",
                sep=""))
    }
  }
  
  if (verbose){print(paste("Out of", nrow(products), "products,", nrow(products_to_label), "labels will be generated. If the numbers differ, then there may have been some products that have already been labelled."))}
  
  chunk_to_str <- function(chunk){
    output_string <- apply(chunk, 1, function(row) {
      paste0(row[product_id_col_name], " ", row[product_col_name])
    })
    return(paste(output_string, collapse = "\n"))
  }
  
  # Batch into labels to maximize GPT output
  products_to_label_chunked <- products_to_label %>%
    mutate(group_index = ceiling(row_number() / CHUNK_SIZE))
  products_to_label_chunked <- split(products_to_label_chunked, products_to_label_chunked$group_index)
  
  for (chunk_index in names(products_to_label_chunked)) {
    chunk <- products_to_label_chunked[[chunk_index]]
    
    if (verbose){print(paste("Generating Labels for Chunk", chunk_index, "of", length(products_to_label_chunked), "chunks of size", CHUNK_SIZE))}
    execution_time <- system.time(generate_labels(chunk_index, products_to_label_chunked))
    gen_labels_to_csv(chunk_index)
    if (verbose){print(paste("Execution Time: ", execution_time['elapsed']))}
    if (verbose){print(paste("Updating Files in existing DB for Chunk", chunk_index))}
    update_labelled_df(chunk_index, execution_time=execution_time['elapsed'])
  }
}

# Additional parameters can be set in the config.R file
coicop_labeller(products, product_id_col_name, product_col_name) 
