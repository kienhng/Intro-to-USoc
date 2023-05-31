# Notes on Understanding Society dataset

> These information are collected mainly from the course **[Introduction to Understanding Society using R](https://open.essex.ac.uk/course/view.php?id=221)** provided by Essex University.
> 

## 1. Key materials

Introduction to the study: https://www.understandingsociety.ac.uk/about/about-the-study

FAQs on the UK Data Service website: https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000053#!/faqs

- **Introduction slides:**
    - [Overview](https://open.essex.ac.uk/pluginfile.php/221375/mod_resource/content/27/StudyOverview.pdf) (specific data files description from Slides 80 to 82; naming convention in Slides 92 - 93)
    - [Data Access](https://open.essex.ac.uk/pluginfile.php/221379/mod_resource/content/9/DataAccess.pdf) (names of datasets, descriptions and serial numbers in Slides 3, 4 and 5)
    - [Walkthrough the USoc website](https://open.essex.ac.uk/pluginfile.php/221383/mod_resource/content/13/WebsiteWalkThrough.pdf)
    - [Longterm Content Plan](https://www.understandingsociety.ac.uk/sites/default/files/downloads/general/long-term-content-plan.pdf) (showing the frequency of datasets from Wave 1 in 2009 to Wave 16 in 2020)
    - [Geographical Identifiers](https://www.understandingsociety.ac.uk/documentation/linked-data/geographical-identifiers)
- Identiying individuals and households:
    - Using **`pidp`**: Unique person identifier to link across waves and across files within the same wave. This identifier is constant, unique within and across waves.
    - Using **`w-pidp`**: Household identifier to identify members of the same households within one wave. Not constant, so *cannot be used to link households across waves* (no concept of longitudinal household)
- **Data Structure (Graphs only)**

![Untitled](https://fluorescent-radar-fd5.notion.site/image/https%3A%2F%2Fs3-us-west-2.amazonaws.com%2Fsecure.notion-static.com%2F440d5081-0121-4924-b138-226b296428ff%2FUntitled.png?id=85da995e-52da-4415-ae44-c33e51315ab5&table=block&spaceId=64069652-53fe-4133-b189-a19caef06fe3&width=2000&userId=&cache=v2)

![Untitled](https://fluorescent-radar-fd5.notion.site/image/https%3A%2F%2Fs3-us-west-2.amazonaws.com%2Fsecure.notion-static.com%2F03ebe243-f974-444f-8c5e-877a70aafa00%2FUntitled.png?id=4f5c2bee-993a-48a8-901e-97fb011b48dc&table=block&spaceId=64069652-53fe-4133-b189-a19caef06fe3&width=2000&userId=&cache=v2)

## 2. Some important notes

- USoc has subsample for Ethinic minorities (**Ethnic Minority Boost sample and an Immigrant and Ethnic Minority Boost sample).** The Boost sample is to improve the analysis of under-represented groups of people by oversampling such groups.
- The USoc made up of 2 big datasets: _**BHPS**_ (from 1991 to 2008) and _**UKHLS**_ (from 2009 until now). Key changes in the datasets timeline are:
    
    - 🗓️ 1991 Interviews (Started as **BHPS**) ⇒ 2001 Interviews ⇒ 2009 Interviews (Changed to **UKHLS**) ⇒ 2015 interviews
    - Each annual interview is referred to as a _**wave**_. There are are 31 waves now.
    - Using the dataset for analysis during a long period of time, therefore, should find a way to incorporate data from all _**waves**_ 

- After the first surveys (original sample), other surveyees in next waves are all connected to the original sample in some ways

## 3. Other Longtitunal Datasets

- Library of UK longtitunal datasets: https://closer.ac.uk/explore-the-studies/
- Open datasets in other fields which can be linked to USoc: https://www.understandingsociety.ac.uk/documentation/linked-data/geographical-identifiers 
