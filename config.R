verbose=TRUE
OPEN_API_KEY = ''

# -----------
# GPT Function Defaults
# -----------
systeminput <- paste("You are a classifier that needs to classify an inputted revenue stream description as one of many CoiCop labels. These CoiCop labels have 4 levels. For example, Food & Beverage is level 1, Food is level 2, Bread & Cereals is level 3, Cereals is level 4. The format of the label is x.x.x.x, with each x being a level. If it is a general enough category, input 0s for each of the following level labels. The following is a string containing the entire labelling system.", 
                     coicop_labels_str_stripped)
CHUNK_SIZE = 500

# the Open AI Model parameter needs to be in the Open AI Model format found here: https://platform.openai.com/docs/models/
OPEN_AI_MODEL = "gpt-4o" 

# path and name of output file
gpt_output_file = "" 

# path to folder where all GPT files will be uploaded
local_path = "" 

# model can be anything that is a unique model
MODEL_SET = Sys.Date() 