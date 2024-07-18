![wdl](images/wdl_square_logo.png)

# COICOP Labeller
This algorithm will categorize a set of product names into the UN 2018 standard for [Classification of Individual Consumption According to Purpose (COICOP)](https://unstats.un.org/unsd/classifications/Econ/Download/COICOP_2018_draft_publication.pdf). This algorithm can be used for a variety of purposes incluing ingesting country household expenditure surveys, company production line documents, and more. 

[World Data Pro](worlddata.pro) uses COICOP Labeller to ingest survey data from over 40 countries to forecast key economic indicators. See [here](worlddata.pro) if World Data Pro fits your enterprise needs. This code is published by [World Data Lab](worlddata.io) (WDL). 

## Description
The algorithm uses the [Open AI API](https://platform.openai.com/docs/overview) to categorize a set of product names into the COICOP labelling standard. For WDL, large generalized language models have proven to have better accuracies than existing categorization methods such as Support Vector Machines or Random Forests. 

The algorithm takes a set of product names in .csv format. The algorithm labels these products by sending the products in chunks to the Open AI API and ingests the output into a .csv format. The default chunk size is 500. 

### About COICOP
COICOP is a UN hierarchical standard for labelling individual consumption products (i.e. flour, shoes, mortgages). Agencies and companies use COICOP to standardize their household product consumption data into categories. 
COICOP uses a hierarchical system of categorization. There are 3 levels of categorization (see [documentation](https://unstats.un.org/unsd/classifications/Econ/Download/COICOP_2018_draft_publication.pdf)). Each COICOP label is in the form X.X.X, where each X represents a label. 

![wdl](images/coicop_tree.png)

> For example, a can of white northern beans will be classified as 1.1.7 Vegetables.  

### Features
- **Language Universality**: The algorithm can digest products in any language. Based on WDL's internal classification mechanism, the language does not affect the accuracy. 
- **Large Capacity**: The algorithm can digest any number of labels. More labels will mean more calls to OpenAI's API. 
    - *Speed*: The algorithm labels at approximately 333 products per minutes. 
    - *Cost*: Since the algorithm sends requests to Open AI's API, the requests cost approximately $1 for 3,000 labels. 
- **Accuracy**: The accuracy of the model is at >99% for level 1, based on a subsample of WDL's existing labelled products. 

## Getting Started

### Dependencies

- OPEN AI API Key: You can obtain an Open AI API Key by following the instructions [here](https://platform.openai.com/docs/overview). 
- R + RStudio: The code is run using R and RStudio. You can download both of these [here](https://posit.co/download/rstudio-desktop/). 
- Download the code using git

    ```
    git clone https://github.com/dshuman12/coicop_labeller.git
    ```
    or by donwloading directly. Hit Code > download .zip. Once the .zip is downloaded, move the .zip into your disired directory and extract zip by rightclicking and hitting extract. 

### Executing program

Once R and RStudio have been installed, download this code. By default, the code searches for a dataset in the existing directory as the downloaded code. 

Function Inputs: 
- Open AI API Key: *str*
- Path to Products to label from code block: *str*
- Path to output intermediary GPT files and full outputted dataset: *str*

## Authors
This algorithm is published by World Data Lab Inc. ([worlddata.io](https://worlddata.io/)) as a feature in World Data Lab Pro ([worlddata.pro](https://worlddata.pro/)). World Data Pro is a platform to access the most accurate and forward-looking data on consumer headcounts, demogrpahics, and spending across the world. 

Any specific questions about the algoritm can be directed towards Daniela Shuman, daniela.shuman@worlddata.io. 

## License

This project is licensed under the [NAME HERE] License - see the LICENSE.md file for details