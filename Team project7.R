############################################
### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Team project                #####
### TEAM: 07                           #####
### Member: 21800562 Lee Jaepyeong,    #####
###         22000186 Kim Jiho,         #####
###         22200693 Jeong Chanju      #####
############################################

##### DATA COLLECTION #####
# Some job postings may no longer be available, which could result in the inability to retrieve certain data. 
# Please use the data collection code for reference only, and utilize the attached final_df.RData instead.

### Chanju ###
library(dplyr)
library(rvest)

#1
# Set the URL of the job posting
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45968108?rPageCode=SL&logpath=21&sn=6" 
# Read the HTML content of the webpage
page1 <- read_html(url)

# Extract all 'div' elements, clean text data, and remove extra spaces
text_data <- page1 %>%
  html_nodes('div') %>% # Select all 'div' nodes
  html_text() %>% # Extract text from nodes
  gsub("\\s+|\\r\\n", " ", .) %>%  # Replace multiple whitespaces and line breaks with a single space
  gsub("\\s{2,}", " ", .) %>%  # Replace sequences of two or more spaces with a single space    
  trimws()  # Trim leading and trailing spaces                       

# Split a specific element (76th) of the cleaned text into individual words
text_data <- lapply(text_data[76], function(x) strsplit(x, " ")[[1]])

# Extract specific text elements for processing
text_data[[1]][1] # Extract the first element (company name)
text_data[[1]][c(13:21)] # Extract elements 13 through 21 (job posting title)
combined_text1 <- paste(text_data[[1]][c(13:21)], collapse = " ") # Combine extracted words into a single string

# Define job requirements and preferred qualifications as character vectors
requirements1 <- c(
  "이미지, 텍스트 머신러닝/딥러닝 엔진 개선 및 Fine-tuning",
  "Pytorch, Keras, Tensorflow 등 라이브러리 활용 해 보신 분",
  "논문을 읽고 이해하며 구현할 수 있는 능력을 가지신 분",
  "오픈 소스 생성형 인공지능 (Generative AI) 개발 경험",
  "데이터 분석을 위한 프로그래밍 역량 (Python, R, SQL 등)",
  "데이터 크롤링, EDA(Exploratory Data Analysis), 전처리 업무 숙련자",
  "Pandas 및 Numpy 등 라이브러리 활용하여 데이터 전처리 및 가공 가능하신 분",
  "텍스트마이닝을 위한 라이브러리 사용 가능자",
  "PPT, 한글 작성에 능하신 분",
  "자기주도적 업무처리와 원활한 커뮤니케이션 능력을 지니신 분",
  "주어진 일에 대해 책임감이 강한 분",
  "꼼꼼한 성격의 소유자"
)

preferred_qualifications1 <- c(
  "정부 또는 기관에서 진행하는 빅데이터 분석과정을 수료하거나 관련 공모전에서 수상한 경험이 있는 분",
  "데이터분석 관련 논문 또는 관련 교육 기관에서 진행한 포트폴리오가 있으신 분",
  "분석결과에 대한 명확한 의사소통 및 문서화 기술이 있으신 분",
  "상경계열, 사회과학계열, 수학/통계학",
  "Pytorch, Keras, Tensorflow 등 라이브러리 활용 해 보신 분",
  "분석 툴 사용 및 보고서 기획/작성 경험이 있는 분",
  "이미지 처리, NLP (머신러닝/딥러닝)에 경험이 있는 분 (경력/프로젝트 모두 포함)",
  "소셜빅데이터 분석을 해본 경험이 있는 분(경력/프로젝트 모두 포함)",
  "시각화 툴 사용 및 개발 경험이 있는 분",
  "데이터 분석 관련 자격증 보유하신 분",
  "R, Python 등 언어를 활용한 데이터 정제, 분석, 모델링, 시각화 경험이 있으신 분",
  "데이터 기반 실험 및 모델을 실 서비스에 적용하고 개선하여 비즈니스 Insight를 창출해 본 경험이 있으신 분"
)

# Combine requirements and preferred qualifications into single strings
combined_text2 <- paste(requirements1, collapse = " ") # Combine requirements into one string
combined_text3 <- paste(preferred_qualifications1, collapse = " ") # Combine qualifications into one string

company_name <- text_data[[1]][1] # Extract the company name
title1 <- combined_text1     # Use the combined job title
Eligibility_Requirements1 <- combined_text2 # Set combined job requirements
Preferred_Qualifications1 <- combined_text3 # Set combined preferred qualifications

# Create a data frame to organize job information
job_info1 <- data.frame(
  회사명 = company_name, # Company name
  채용제목 = title1, # Job title
  지원자격 = Eligibility_Requirements1, # Job requirements
  우대사항 = Preferred_Qualifications1, # Preferred qualifications
  stringsAsFactors = FALSE # Prevent automatic conversion of strings to factors
)

# Print the data frame containing job information
print(job_info1)


# The method applied in '#1' was also applied to other job postings.

#2
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45952026?rPageCode=SL&logpath=21&sn=6" 
page2 <- read_html(url)

text_data1 <- page2 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data1 <- lapply(text_data1[77], function(x) strsplit(x, " ")[[1]])


text_data1[[1]][1]
text_data1[[1]][c(10:14)]
combined_text4 <- paste(text_data1[[1]][c(10:14)], collapse = " ")

requirements2 <- c(
  "이공계 석사 학위 이상",
  "Python 기반 프로그래밍을 경험하신 분",
  "통계, 머신러닝, 딥러닝 중 1가지 이상 기술을 경험하신 분",
  "Tensorflow, PyTorch, Scikit-learn 등 
   오픈소스 기반 개발을 경험하신 분"
)

preferred_qualifications2 <- c(
  "통계학이나 컴퓨터공학을 전공하신 분",
  "파이선, 데이터 분석 등 인공지능 모델 개발 및 현업 적용 경험이 있으신 분",
  "제조 공정/시스템 관련 프로젝트 개발 및 현업 적용 경험이 있으신 분"
)

combined_text5 <- paste(requirements2, collapse = " ")
combined_text6 <- paste(preferred_qualifications2, collapse = " ")

company_name1 <- text_data1[[1]][1]
title2 <- combined_text4     
Eligibility_Requirements2 <- combined_text5
Preferred_Qualifications2 <- combined_text6

job_info2 <- data.frame(
  회사명 = company_name1,
  채용제목 = title2,
  지원자격 = Eligibility_Requirements2,
  우대사항 = Preferred_Qualifications2,
  stringsAsFactors = FALSE 
)

print(job_info2)

#3
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45937517?rPageCode=SL&logpath=21&sn=6" 
page3 <- read_html(url)

text_data2 <- page3 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data2 <- lapply(text_data2[77], function(x) strsplit(x, " ")[[1]])


text_data2[[1]][1]
text_data2[[1]][c(10:13)]
combined_text7 <- paste(text_data2[[1]][c(10:13)], collapse = " ")

requirements3 <- c(
  "데이터 과학, 통계학, 수학 또는 관련 분야의 학사 이상 학위 소지자",
  "데이터 사이언스, 머신러닝, 인공지능(AI) 업무 경험자"
)

preferred_qualifications3 <- c(
  "자연어 처리(NLP) 및 거대언어모델(LLM) 개발 경험자",
  "생물정보학 관련 도메인 분석 경험자",
  "영어 능통자"
)

combined_text8 <- paste(requirements3, collapse = " ")
combined_text9 <- paste(preferred_qualifications3, collapse = " ")

company_name2 <- text_data2[[1]][1]
title3 <- combined_text7     
Eligibility_Requirements3 <- combined_text8
Preferred_Qualifications3 <- combined_text9

job_info3 <- data.frame(
  회사명 = company_name2,
  채용제목 = title3,
  지원자격 = Eligibility_Requirements3,
  우대사항 = Preferred_Qualifications3,
  stringsAsFactors = FALSE 
)

print(job_info3)

#4
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45928419?rPageCode=SL&logpath=21&sn=6" 
page4 <- read_html(url)

text_data3 <- page4 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data3 <- lapply(text_data3[76], function(x) strsplit(x, " ")[[1]])


text_data3[[1]][1]
text_data3[[1]][c(14:17)]
combined_text10 <- paste(text_data3[[1]][c(14:17)], collapse = " ")

requirements4 <- c(
  "AI / ML 기반의 모델 개발 및 테스트에 대한 기본 지식",
  "AI / ML 기반의 모델 개발에 대한 이해와 흥미",
  "자연어 처리 (Natural Language Processing)에 대한 기초 지식",
  "데이터 분석을 위한 데이터 가공 및 정제 등 데이터 엔지니어링 경험",
  "Python에 대한 우수한 이해와 활용 능력",
  "Python 등 개발 언어를 활용한 API 개발 경험",
  "다양한 자연어 처리 모델에서 최적의 결과를 얻기 위해 프롬프트 엔지니어링 경험"
)

preferred_qualifications4 <- c(
  "관련 전공 석사 졸업자",
  "OCR을 통한 데이터 식별 경험",
  "BERT, GPT, BART 등 거대 언어모델 구축, 학습, 배포 및 운영에 대한 경험",
  "다양한 환경의 퍼블릭 클라우드에서 API 및 모델 개발 및 배포, 운영 경험",
  "AWS, Docker, DevOps, CI/CD에 대한 경험",
  "애자일 방식의 일하는 방식에 대한 경험"
)

combined_text11 <- paste(requirements4, collapse = " ")
combined_text12 <- paste(preferred_qualifications4, collapse = " ")

company_name3 <- text_data3[[1]][1]
title4 <- combined_text10     
Eligibility_Requirements4 <- combined_text11
Preferred_Qualifications4 <- combined_text12

job_info4 <- data.frame(
  회사명 = company_name3,
  채용제목 = title4,
  지원자격 = Eligibility_Requirements4,
  우대사항 = Preferred_Qualifications4,
  stringsAsFactors = FALSE 
)

print(job_info4)

#5
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45911857?rPageCode=SL&logpath=21&sn=6" 
page5 <- read_html(url)

text_data4 <- page5 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data4 <- lapply(text_data4[77], function(x) strsplit(x, " ")[[1]])


text_data4[[1]][1]
text_data4[[1]][c(10:17)]
combined_text13 <- paste(text_data4[[1]][c(10:17)], collapse = " ")

requirements5 <- c(
  "학력: 학사 또는 석사 이상",
  "경력: 신입/경력(3년이상)",
  "근무지: 서울 / 제주"
)

preferred_qualifications5 <- c(
  "관련 학과 전공 우대",
  "해당직무 근무경험",
  "자격증: 정보처리기사 등"
)

combined_text14 <- paste(requirements5, collapse = " ")
combined_text15 <- paste(preferred_qualifications5, collapse = " ")

company_name4 <- text_data4[[1]][1]
title5 <- combined_text13     
Eligibility_Requirements5 <- combined_text14
Preferred_Qualifications5 <- combined_text15

job_info5 <- data.frame(
  회사명 = company_name4,
  채용제목 = title5,
  지원자격 = Eligibility_Requirements5,
  우대사항 = Preferred_Qualifications5,
  stringsAsFactors = FALSE 
)

print(job_info5)

#6
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45904583?rPageCode=SL&logpath=21&sn=6" 
page6 <- read_html(url)

text_data5 <- page6 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data5 <- lapply(text_data5[77], function(x) strsplit(x, " ")[[1]])


text_data5[[1]][1]
text_data5[[1]][c(10:13)]
combined_text16 <- paste(text_data5[[1]][c(10:13)], collapse = " ")

requirements6 <- c(
  "대학원(석사) 이상",
  "Python, C++"
)

preferred_qualifications6 <- c(
  "프로젝트 완수 경험자",
  "C/C++, Python 개발 경험자",
  "AWS Cloud Service 프로젝트 경험자",
  "딥러닝/머신러닝 경험자",
  "운전면허 소지자 및 운전 가능자",
  "영어 능통자"
)

combined_text17 <- paste(requirements6, collapse = " ")
combined_text18 <- paste(preferred_qualifications6, collapse = " ")

company_name5 <- text_data5[[1]][1]
title6 <- combined_text16     
Eligibility_Requirements6 <- combined_text17
Preferred_Qualifications6 <- combined_text18

job_info6 <- data.frame(
  회사명 = company_name5,
  채용제목 = paste(title6, "(소프트웨어개발)"),
  지원자격 = Eligibility_Requirements6,
  우대사항 = Preferred_Qualifications6,
  stringsAsFactors = FALSE 
)

print(job_info6)

#7
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45904583?rPageCode=SL&logpath=21&sn=6" 
page7 <- read_html(url)

text_data6 <- page7 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data6 <- lapply(text_data6[77], function(x) strsplit(x, " ")[[1]])


text_data6[[1]][1]
text_data6[[1]][c(10:13)]
combined_text19 <- paste(text_data6[[1]][c(10:13)], collapse = " ")

requirements7 <- c(
  "대학원(석사) 이상",
  "Python, C++"
)

preferred_qualifications7 <- c(
  "영상신호처리 경험자",
  "C/C++, Python 개발 경험자",
  "AWS 개발 경험자",
  "딥러닝/머신러닝 경험자",
  "운전면허 소지자 및 운전 가능자",
  "영어 능통자"
)

combined_text20 <- paste(requirements7, collapse = " ")
combined_text21 <- paste(preferred_qualifications7, collapse = " ")

company_name6 <- text_data6[[1]][1]
title7 <- combined_text19     
Eligibility_Requirements7 <- combined_text20
Preferred_Qualifications7 <- combined_text21

job_info7 <- data.frame(
  회사명 = company_name6,
  채용제목 = paste(title7, "(딥러닝/머신러닝기반 알고리즘 개발)"),
  지원자격 = Eligibility_Requirements7,
  우대사항 = Preferred_Qualifications7,
  stringsAsFactors = FALSE 
)

print(job_info7)

#8
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45905864?rPageCode=SL&logpath=21&sn=6" 
page8 <- read_html(url)

text_data7 <- page8 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data7 <- lapply(text_data7[77], function(x) strsplit(x, " ")[[1]])


text_data7[[1]][1]
text_data7[[1]][c(10:11)]
combined_text22 <- paste(text_data7[[1]][c(10:11)], collapse = " ")

requirements8 <- c(
  "데이터 과학, 컴퓨터 과학, 통계학, 또는 관련 분야에서 학사 학위 보유자 또는 유관 필드에서 2년 이상 경력자",
  "Python 또는 R과 같은 코딩 언어에 능숙",
  "LLM 기법 활용 및 RAG 기술에 대한 높은 이해력",
  "데이터 시각화 도구 및 기법에 대한 비즈니스 경험 보유",
  "탁월한 문제 해결 및 비판적 사고 능력",
  "강력한 커뮤니케이션 및 프레젠테이션 능력"
)

preferred_qualifications8 <- c(
  "스타트업 근무 경험",
  "새로운 기술에 관심이 있는 사람",
  "급변하는 요구사항과 빠른 소통에 능한 사람"
)

combined_text23 <- paste(requirements8, collapse = " ")
combined_text24 <- paste(preferred_qualifications8, collapse = " ")

company_name7 <- text_data7[[1]][1]
title8 <- combined_text22     
Eligibility_Requirements8 <- combined_text23
Preferred_Qualifications8 <- combined_text24

job_info8 <- data.frame(
  회사명 = company_name7,
  채용제목 = title8,
  지원자격 = Eligibility_Requirements8,
  우대사항 = Preferred_Qualifications8,
  stringsAsFactors = FALSE 
)

print(job_info8)

#9
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45908486?rPageCode=SL&logpath=21&sn=6" 
page9 <- read_html(url)

text_data8 <- page9 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data8 <- lapply(text_data8[77], function(x) strsplit(x, " ")[[1]])


text_data8[[1]][1]
text_data8[[1]][c(10:16)]
combined_text25 <- paste(text_data8[[1]][c(10:16)], collapse = " ")

requirements9 <- c(
  "머신러닝 관련 개발, 연구 또는 실무 경력이 2년 이상이거나 그에 준하는 역량을 보유하신 분 (단, 전문연구요원의 경우 경력에 관계없이 지원 가능합니다)",
  "아래 내용에 대한 지식, 논문 작성 경험 및 논문 구현 능력이 있는 분 - 컴퓨터비전, 자연어처리, 딥러닝 / 머신러닝",
  "본인이 구현한 코드, 업무의 진행 상황 및 결과 등을 정확하고 체계적으로 문서화할 수 있는 능력이 있는 분",
  "이공계(전공무관) 학사 이상이신 분"
)

preferred_qualifications9 <- c(
  "Object Detection / Recognition / Segmentation",
  "Multi modality (Image, Text 등)",
  "Sequence labeling",
  "Text segmentation",
  "Multi modality (Image, Text 등) 기반 머신러닝 알고리즘",
  "SW 개발 리딩 경험",
  "SaaS 서비스 또는 자체 프로덕트 개발 사이클 전반의 경험",
  "AI / 머신러닝 관련 대회 참가 경험이 있는 분",
  "AI / 머신러닝 관련 국제학술대회에 논문을 게재한 경험이 있는 분"
)

combined_text26 <- paste(requirements9, collapse = " ")
combined_text27 <- paste(preferred_qualifications9, collapse = " ")

company_name8 <- text_data8[[1]][1]
title9 <- combined_text25     
Eligibility_Requirements9 <- combined_text26
Preferred_Qualifications9 <- combined_text27

job_info9 <- data.frame(
  회사명 = company_name8,
  채용제목 = title9,
  지원자격 = Eligibility_Requirements9,
  우대사항 = Preferred_Qualifications9,
  stringsAsFactors = FALSE 
)

print(job_info9)

#10
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45829057?rPageCode=SL&logpath=21&sn=6" 
page10 <- read_html(url)

text_data9 <- page10 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data9 <- lapply(text_data9[77], function(x) strsplit(x, " ")[[1]])


text_data9[[1]][1]
text_data9[[1]][c(10:16)]
combined_text28 <- paste(text_data9[[1]][c(10:16)], collapse = " ")

requirements10 <- c(
  "학력: 대졸(4년) 이상, 석/박사 우대",
  "분석업무를 리딩하면서 진행한 경험자",
  "기계학습, 딥러닝 모델 개발자",
  "마이데이터를 기반으로 한 서비스 개발",
  "주도적으로 일관리가 가능하신분"
)

preferred_qualifications10 <- c(
  "LLM 언어모델에 관심 있는 분",
  "생성형 AI의 프롬프트 엔지니어링, 파인튜닝, RAG 경험자 우대",
  "의료분야 데이터를 분석한 경험이 있으신 분",
  "인공지능분야에서의 연구 및 실무경험",
  "Hadoop기반 빅데이터 플랫폼을 활용하여 개발한 경험이 있으신 분",
  "엔지니어링 업무 경험이 있으신 분"
)

combined_text29 <- paste(requirements10, collapse = " ")
combined_text30 <- paste(preferred_qualifications10, collapse = " ")

company_name9 <- text_data9[[1]][1]
title10 <- combined_text28     
Eligibility_Requirements10 <- combined_text29
Preferred_Qualifications10 <- combined_text30

job_info10 <- data.frame(
  회사명 = company_name9,
  채용제목 = title10,
  지원자격 = Eligibility_Requirements10,
  우대사항 = Preferred_Qualifications10,
  stringsAsFactors = FALSE 
)

print(job_info10)

#11
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45882674?rPageCode=SL&logpath=21&sn=6" 
page11 <- read_html(url)

text_data10 <- page11 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data10 <- lapply(text_data10[77], function(x) strsplit(x, " ")[[1]])


text_data10[[1]][1]
text_data10[[1]][c(10:14)]
combined_text31 <- paste(text_data10[[1]][c(10:14)], collapse = " ")

requirements11 <- c(
  "데이터 추출, 분석을 위한 언어(SQL, Python)를 능숙하게 다루는 분",
  "데이터 시각화 툴(Tableau)에 대한 기본 지식을 보유한 분",
  "Funnel, AARRR, Cohort Analysis 등 데이터 분석 방법에 대한 지식과 경험이 있으신 분"
)

preferred_qualifications11 <- c(
  "AWS(DynamoDB, EC2, RDS), GCP(BigQuery), Google Analytics 사용 경험이 있으신 분",
  "데이터에 기반한 가설을 세워 A/B Test를 설계하고 진행해보신 분"
)

combined_text32 <- paste(requirements11, collapse = " ")
combined_text33 <- paste(preferred_qualifications11, collapse = " ")

company_name10 <- text_data10[[1]][1]
title11 <- combined_text31     
Eligibility_Requirements11 <- combined_text32
Preferred_Qualifications11 <- combined_text33

job_info11 <- data.frame(
  회사명 = company_name10,
  채용제목 = title11,
  지원자격 = Eligibility_Requirements11,
  우대사항 = Preferred_Qualifications11,
  stringsAsFactors = FALSE 
)

print(job_info11)

#12
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45860108?rPageCode=SL&logpath=21&sn=6" 
page12 <- read_html(url)

text_data11 <- page12 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data11 <- lapply(text_data11[76], function(x) strsplit(x, " ")[[1]])


text_data11[[1]][1]
text_data11[[1]][c(13:17)]
combined_text34 <- paste(text_data11[[1]][c(13:17)], collapse = " ")

requirements12 <- c(
  "하기 업무 경력 10년 이상"
)

preferred_qualifications12 <- c(
  "데이터 관련 전략/컨설팅/기획 업무 수행 경험",
  "데이터분석, 엔지니어링, 아키텍처등 기술적인 백그라운드 보유",
  "데이터관련 솔루션 지식 및 활용 경험 보유"
)

combined_text35 <- paste(requirements12, collapse = " ")
combined_text36 <- paste(preferred_qualifications12, collapse = " ")

company_name11 <- text_data11[[1]][1]
title12 <- combined_text34     
Eligibility_Requirements12 <- combined_text35
Preferred_Qualifications12 <- combined_text36

job_info12 <- data.frame(
  회사명 = company_name11,
  채용제목 = paste(title12, "(데이터경영)"),
  지원자격 = Eligibility_Requirements12,
  우대사항 = Preferred_Qualifications12,
  stringsAsFactors = FALSE 
)

print(job_info12)

#13
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45853677?rPageCode=SL&logpath=21&sn=6" 
page13 <- read_html(url)

text_data12 <- page13 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data12 <- lapply(text_data12[77], function(x) strsplit(x, " ")[[1]])


text_data12[[1]][1]
text_data12[[1]][c(10:13)]
combined_text37 <- paste(text_data12[[1]][c(10:13)], collapse = " ")

requirements13 <- c(
  "구조적 글쓰기와 비즈니스 커뮤니케이션 역량 갖추신 분",
  "AI 기술에 대한 지식을 갖고 이해하고 계신 분(도메인 지식 필수)",
  "최신 AI 기술 및 비즈니스 트렌트를 파악하고 계신 분",
  "Python을 사용한 간단한 데이터 전처리 및 관리 경험"
)

preferred_qualifications13 <- c(
  "개발자와 원활한 협업 및 커뮤니케이션 능력",
  "다양한 고객과의 소통에 두려움이 없으신 분",
  "Python을 통한 데이터 관리 및 자동화 경험"
)

combined_text38 <- paste(requirements13, collapse = " ")
combined_text39 <- paste(preferred_qualifications13, collapse = " ")

company_name12 <- text_data12[[1]][1]
title13 <- combined_text37     
Eligibility_Requirements13 <- combined_text38
Preferred_Qualifications13 <- combined_text39

job_info13 <- data.frame(
  회사명 = company_name12,
  채용제목 = title13,
  지원자격 = Eligibility_Requirements13,
  우대사항 = Preferred_Qualifications13,
  stringsAsFactors = FALSE 
)

print(job_info13)

#14
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45850320?rPageCode=SL&logpath=21&sn=6" 
page14 <- read_html(url)

text_data13 <- page14 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data13 <- lapply(text_data13[77], function(x) strsplit(x, " ")[[1]])


text_data13[[1]][1]
text_data13[[1]][c(10:17)]
combined_text40 <- paste(text_data13[[1]][c(10:17)], collapse = " ")

requirements14 <- c(
  "학력: 학사이상(졸업예정자 가능)",
  "경력: 신입, 경력",
  "Python, C++, Tensorflow, Keras, OpenCV 등 개발 가능자",
  "머신러닝/딥러닝에 대한 폭넓은 기본지식",
  "최신 연구논문에 대한 이해 및 활용 가능자",
  "AI 빅데이터 관련분야와 흥미 및 열정적인 연구 가능자"
)

preferred_qualifications14 <- c(
  "관련분야 석사 이상 우대",
  "유관업무 경력자 (2년)",
  "장기근무 가능자",
  "자격증: 정보처리기사",
  "전공: 전산학, 컴퓨터고학, 산업공학, 통계학",
  "AI 빅데이터 관련 정부과제 연구개발 경험자",
  "AI 빅데이터 관련 솔루션 개발 실무경험자",
  "JAVA/JSP, Node.JS, SQL/NoSQLDB, Linux, NT 등 개발 경험자"
)

combined_text41 <- paste(requirements14, collapse = " ")
combined_text42 <- paste(preferred_qualifications14, collapse = " ")

company_name13 <- text_data13[[1]][1]
title14 <- combined_text40     
Eligibility_Requirements14 <- combined_text41
Preferred_Qualifications14 <- combined_text42

job_info14 <- data.frame(
  회사명 = company_name13,
  채용제목 = title14,
  지원자격 = Eligibility_Requirements14,
  우대사항 = Preferred_Qualifications14,
  stringsAsFactors = FALSE 
)

print(job_info14)

#15
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45849430?rPageCode=SL&logpath=21&sn=6" 
page15 <- read_html(url)

text_data14 <- page15 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data14 <- lapply(text_data14[77], function(x) strsplit(x, " ")[[1]])


text_data14[[1]][1]
text_data14[[1]][c(10:14)]
combined_text43 <- paste(text_data14[[1]][c(10:14)], collapse = " ")

requirements15 <- c(
  "SQL, Python 등 데이터 분석에 필요한 기본 언어를 잘 다루시는 분",
  "비즈니스에 대한 호기심이 많고 가설을 수립하고 데이터로 증명하는 것을 좋아하시는 분",
  "머신러닝 및 딥러닝을 비즈니스에 적용하여 의사결정을 돕는 경험을 해보고 싶은 분",
  "유연한 사고를 바탕으로 토론을 선호 하시는 분"
)

preferred_qualifications15 <- c(
  "컨설팅 방법론 및 AI/ML 기술을 바탕으로 엔터프라이즈 기업의 문제를 해결하고 싶은 분",
  "문장 생성 및 요약, 추천, 예측 분야의 AI/ML 연구를 통해 비지니스 가치를 만들어 보고 싶은 분",
  "데이터 분석 뿐 아니라 데이터 Pipe-line 설게 및 개발까지 역량과 경험을 확장하고 싶은 분",
  "재미있게 일하고 싶은 스타트업 DNA가 있으신 분"
)

combined_text44 <- paste(requirements15, collapse = " ")
combined_text45 <- paste(preferred_qualifications15, collapse = " ")

company_name14 <- text_data14[[1]][1]
title15 <- combined_text43     
Eligibility_Requirements15 <- combined_text44
Preferred_Qualifications15 <- combined_text45

job_info15 <- data.frame(
  회사명 = company_name14,
  채용제목 = title15,
  지원자격 = Eligibility_Requirements15,
  우대사항 = Preferred_Qualifications15,
  stringsAsFactors = FALSE 
)

print(job_info15)

#16
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45874040?rPageCode=SL&logpath=21&sn=6" 
page16 <- read_html(url)

text_data15 <- page16 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data15 <- lapply(text_data15[77], function(x) strsplit(x, " ")[[1]])


text_data15[[1]][c(1:5)]
combined_text46 <- paste(text_data15[[1]][c(1:5)], collapse = " ")
text_data15[[1]][c(14:22)]
combined_text47 <- paste(text_data15[[1]][c(14:22)], collapse = " ")

requirements16 <- c(
  "학력: 학사 이상 졸업 (경력자)",
  "관련 전공: 컴퓨터 사이언스, 프로그래밍, IT관련"
)

preferred_qualifications16 <- c(
  "모집 분야 관련 전공 졸업"
)

combined_text48 <- paste(requirements16, collapse = " ")
combined_text49 <- paste(preferred_qualifications16, collapse = " ")

company_name15 <- combined_text46  
title16 <- combined_text47     
Eligibility_Requirements16 <- combined_text48
Preferred_Qualifications16 <- combined_text49

job_info16 <- data.frame(
  회사명 = company_name15,
  채용제목 = title16,
  지원자격 = Eligibility_Requirements16,
  우대사항 = Preferred_Qualifications16,
  stringsAsFactors = FALSE 
)

print(job_info16)

#17
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45874537?rPageCode=SL&logpath=21&sn=6" 
page17 <- read_html(url)

text_data16 <- page17 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data16 <- lapply(text_data16[77], function(x) strsplit(x, " ")[[1]])


text_data16[[1]][1]
text_data16[[1]][c(10:14)]
combined_text50 <- paste(text_data16[[1]][c(10:14)], collapse = " ")

requirements17 <- c(
  "JAVA, Python, Scala 등 관련 개발 스킬 보유자",
  "경력 5년 이상"
)

preferred_qualifications17 <- c(
  "관련 학과 전공자 (수학, 통계, 컴퓨터공학)",
  "Hadoop, ElasticSearch 유경험자"
)

combined_text51 <- paste(requirements17, collapse = " ")
combined_text52 <- paste(preferred_qualifications17, collapse = " ")

company_name16 <- text_data16[[1]][1]  
title17 <- combined_text50     
Eligibility_Requirements17 <- combined_text51
Preferred_Qualifications17 <- combined_text52

job_info17 <- data.frame(
  회사명 = company_name16,
  채용제목 = paste(title17, "(R&D 부문 - AI)"),
  지원자격 = Eligibility_Requirements17,
  우대사항 = Preferred_Qualifications17,
  stringsAsFactors = FALSE 
)

print(job_info17)

#18
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45850491?rPageCode=SL&logpath=21&sn=6" 
page18 <- read_html(url)

text_data17 <- page18 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data17 <- lapply(text_data17[77], function(x) strsplit(x, " ")[[1]])


text_data17[[1]][1]
text_data17[[1]][c(10:16)]
combined_text53 <- paste(text_data17[[1]][c(10:16)], collapse = " ")

requirements18 <- c(
  "소프트웨어 제품의 기술지원 경험",
  "복합적인 고객의 문제 분석 및 해결 경험",
  "논리적 사고 및 해결책 제시 능력",
  "고객 요구사항에 대한 깊은 이해 및 대응 능력",
  "RDBMS 구조 및 동작 원리에 대한 높은 이해",
  "Cloud / Linux / Storage 에 대한 경험 및 이해",
  "Python / Bash 작성 능력 보유",
  "SQL 작성 및 튜닝에 대한 경험 보유",
  "대면 및 원격 커뮤니케이션 능력 (경력)",
  "기술적 정보를 비기술적 이해 관계자에게 전달하는 능력",
  "기술 문서 작성을 통해 지원 이력 및 경험 전파 능력",
  "영어 구사에 어려움이 없을 것",
  "Open Source 및 신규 소프트웨어의 학습에 적극적이고 익숙하신 분",
  "신입 지원자는 다음 학과 또는 관련 학과의 학사 이상 취득자 - 컴퓨터공학 / 소프트웨어 / 인공지능 / 수학, - 직접적으로 Software / Data에 대한 학문이 중심인 관련학과",
  "남자의 경우 병역필 또는 면제자로 해외 여행 결격 사유 없을 것"
)

preferred_qualifications18 <- c(
  "Java / Rust / C 언어에 대한 작성 능력 보유",
  "다국적 기업의 Global 조직 근무 및 협력 경력자 우대",
  "영어 구사에 어려움이 없는 자 우대"
)

combined_text54 <- paste(requirements18, collapse = " ")
combined_text55 <- paste(preferred_qualifications18, collapse = " ")

company_name17 <- text_data17[[1]][1]  
title18 <- combined_text53     
Eligibility_Requirements18 <- combined_text54
Preferred_Qualifications18 <- combined_text55

job_info18 <- data.frame(
  회사명 = company_name17,
  채용제목 = title18, 
  지원자격 = Eligibility_Requirements18,
  우대사항 = Preferred_Qualifications18,
  stringsAsFactors = FALSE 
)

print(job_info18)

#19
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45850522?rPageCode=SL&logpath=21&sn=6" 
page19 <- read_html(url)

text_data18 <- page19 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data18 <- lapply(text_data18[77], function(x) strsplit(x, " ")[[1]])


text_data18[[1]][1]
text_data18[[1]][c(10:16)]
combined_text56 <- paste(text_data18[[1]][c(10:16)], collapse = " ")

requirements19 <- c(
  "DBMS 또는 관련 소프트웨어의 고객 기술지원 경력 또는 Oracle / MySQL / MongoDB 등 주요 DBMS의 운영 경력 및 깊은 지식 보유",
  "Software product의 PoC / BMT 등 presales support 경력",
  "이기종 DBMS의 migration 또는 U2L project 경력자 우대",
  "RDBMS / data pipeline 구축에 대한 이해",
  "Data processing program 개발 경험",
  "Python / Bash 작성 능력 보유",
  "경력: SQL 작성 능력 보유 및 튜닝에 대한 경험 보유",
  "신입: SQL 작성 능력",
  "대면 및 원격 커뮤니케이션 능력 (경력)",
  "기술적 정보를 비기술적 이해 관계자에게 전달하는 능력",
  "복합적이며, 복잡한 현상에 대한 신속한 이해 및 분석 능력",
  "부서간 협력 및 팀원간 협력 능력",
  "Open Source 및 신규 소프트웨어의 학습에 적극적이고 익숙하신 분",
  "신입 지원자는 다음 학과 또는 관련 학과의 학사 이상 취득자 - 컴퓨터공학 / 소프트웨어 / 인공지능 / 수학, - 직접적으로 Software / Data에 대한 학문이 중심인 관련학과",
  "남자의 경우 병역필 또는 면제자로 해외 여행 결격 사유 없을 것"
)

preferred_qualifications19 <- c(
  "Java / Rust / C 언어에 대한 작성 능력 보유자 우대",
  "온라인 또는 다수의 청중을 대상으로한 프레젠테이션 스킬(우대)",
  "영어 구사에 어려움이 없는 자 우대"
)

combined_text57 <- paste(requirements19, collapse = " ")
combined_text58 <- paste(preferred_qualifications19, collapse = " ")

company_name18 <- text_data18[[1]][1]  
title19 <- combined_text56     
Eligibility_Requirements19 <- combined_text57
Preferred_Qualifications19 <- combined_text58

job_info19 <- data.frame(
  회사명 = company_name18,
  채용제목 = title19, 
  지원자격 = Eligibility_Requirements19,
  우대사항 = Preferred_Qualifications19,
  stringsAsFactors = FALSE 
)

print(job_info19)

#20
url <- "https://www.jobkorea.co.kr/Recruit/GI_Read/45850507?rPageCode=SL&logpath=21&sn=6" 
page20 <- read_html(url)

text_data19 <- page20 %>%
  html_nodes('div') %>%
  html_text() %>%
  gsub("\\s+|\\r\\n", " ", .) %>%  # 불필요한 공백 및 \r\n 제거
  gsub("\\s{2,}", " ", .) %>%      # 연속된 공백을 하나의 공백으로 축소
  trimws()                         # 앞뒤 공백 제거

text_data19 <- lapply(text_data19[77], function(x) strsplit(x, " ")[[1]])


text_data19[[1]][1]
text_data19[[1]][c(10:15)]
combined_text59 <- paste(text_data19[[1]][c(10:15)], collapse = " ")

requirements20 <- c(
  "DBMS 또는 관련 소프트웨어의 개발 경력",
  "DBMS 또는 관련 소프트웨어의 support / engineering 경력",
  "신입: RDBS / data pipeline 구축에 대한 이해, Data processing program 개발 경험",
  "Python / Bash 작성 능력 보유",
  "경력: SQL 작성 능력 보유 및 튜닝에 대한 경험 보유",
  "신입: SQL 작성 능력",
  "대면 및 원격 커뮤니케이션 능력 (경력)",
  "기술적 정보를 비기술적 이해 관계자에게 전달하는 능력",
  "기술 문서 작성을 통해 지원 이력 및 경험 전파 능력",
  "Open Source 및 신규 소프트웨어의 학습에 적극적이고 익숙하신 분",
  "신입 지원자는 다음 학과 또는 관련 학과의 학사 이상 취득자 - 컴퓨터공학 / 소프트웨어 / 인공지능 / 수학, - 직접적으로 Software / Data에 대한 학문이 중심인 관련학과",
  "남자의 경우 병역필 또는 면제자로 해외 여행 결격 사유 없을 것"
)

preferred_qualifications20 <- c(
  "Java / Rust / C 언어에 대한 작성 능력 보유자 우대",
  "영어 구사에 어려움이 없는 자 우대"
)

combined_text60 <- paste(requirements20, collapse = " ")
combined_text61 <- paste(preferred_qualifications20, collapse = " ")

company_name19 <- text_data19[[1]][1]  
title20 <- combined_text59     
Eligibility_Requirements20 <- combined_text60
Preferred_Qualifications20 <- combined_text61

job_info20 <- data.frame(
  회사명 = company_name19,
  채용제목 = title20, 
  지원자격 = Eligibility_Requirements20,
  우대사항 = Preferred_Qualifications20,
  stringsAsFactors = FALSE 
)

print(job_info20)


# Combine multiple job information data frames into a single data frame
combined_df <- bind_rows(job_info1, job_info2, job_info3, job_info4, job_info5,
                         job_info6, job_info7, job_info8, job_info9, job_info10,
                         job_info11, job_info12, job_info13, job_info14, job_info15,
                         job_info16, job_info17, job_info18, job_info19, job_info20)

# Save the combined data frame
save(combined_df, file = "data.RData")


### Jiho ###
library(rvest)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)

#1
url1 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45919450?rPageCode=SL&logpath=21&sn=6")
a <- url1%>%
  html_nodes('span')%>%
  html_text()

com1<-a[35]
title1<-"R&D - Tire 설계 시스템 기획 및 개발"

El1<-("[석사 이상] 산업/컴퓨터/기계공학 전공 또는 유관업무 경력 2년 이상,
      Java,Python등 개발언어 한가지 이상 능숙한 활용가능한 자,
      SQL 기반의 데이터베이스 모델을 설계하거나 쿼리 작성 경험 보유")

Pr1<-("DB모델링,데이터 아키텍처,데이터 분석 역량 또는 관련 자격증 보유,산업/컴퓨터/기계공학 박사학위 보유 또는 유관업부 경력 5년 이상")


job_info1 <- data.frame(
  회사명 = com1,
  채용제목 = title1,
  지원자격 = El1,
  우대사항 = Pr1,
  stringsAsFactors = FALSE
)

#2 
url2 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45919428?rPageCode=SL&logpath=21&sn=6")
a2 <- url2%>%
  html_nodes('span')%>%
  html_text()

com2<-a2[35]
title2<-"R&D - 클라우드 기반 AI 시스템 구축"

El2<-("[석사 이상] 인공지능/컴퓨터공학/산업공학/통계학 등의 분야 학위 소지자|기계/전자/전기공학 배경의 AI/ML 유관전공자 포함|Python,R등 프로그래밍 언어 사용 가능자")

Pr2<-("AI 관련 논문 또는 대회 수상 실적을 보유하신 분|SQL을 활용해 데이터베이스 관리 경험이 있으신 분|Vision,Text Mining,Signal Processing등 프로젝트 경험이 있으신 분")


job_info2 <- data.frame(
  회사명 = com2,
  채용제목 = title2,
  지원자격 = El2,
  우대사항 = Pr2,
  stringsAsFactors = FALSE
)

#3 
url3 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45919428?rPageCode=SL&logpath=21&sn=6")
a3 <- url3%>%
  html_nodes('span')%>%
  html_text()

com3<-a3[35]
title3<-"R&D - 데이터 수집 및 분석"
El3<-("[석사 이상] 인공지능/컴퓨터공학/산업공학/통계학 등의 분야 학위 소지자|기계/전자/전기공학 배경의 AI/ML 유관전공자 포함|Python,R등 프로그래밍 언어 사용 가능자")

Pr3<-("AI 관련 논문 또는 대회 수상 실적을 보유하신 분|SQL을 활용해 데이터베이스 관리 경험이 있으신 분|Vision,Text Mining,Signal Processing등 프로젝트 경험이 있으신 분")


job_info3 <- data.frame(
  회사명 = com3,
  채용제목 = title3,
  지원자격 = El3,
  우대사항 = Pr3,
  stringsAsFactors = FALSE
)

#4
url4 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45919428?rPageCode=SL&logpath=21&sn=6")
a4 <- url4%>%
  html_nodes('span')%>%
  html_text()

com4<-a4[35]
title4<-"R&D - 신패턴 개발 및 기술 연구"
El4<-("[학사 이상] 기계계열 관련 전공 학위 소유자, 3D 설계 Tool 활용 능력, 분석 방법에 대한 이해과 통계관련 지식 보유자")

Pr4<-("CATIA활용 능력 보유자,Abaqus 또는 Hypermesh활용 능력 보유자, 통계방법에 기반한 DATA 분석 능력 보유자,차량동역학 관련 경험자")


job_info4 <- data.frame(
  회사명 = com4,
  채용제목 = title4,
  지원자격 = El4,
  우대사항 = Pr4,
  stringsAsFactors = FALSE
)

#5
url5 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45919428?rPageCode=SL&logpath=21&sn=6")
a5 <- url5%>%
  html_nodes('span')%>%
  html_text()

com5<-a5[35]
title5<-"R&D - 신구조 연구"
El5<-("[학사 이상] 이공계열 분야 학위 소지자,유한요소해석 및 해석 기법 개발 경험자,Python 및 Abaqus 사용 경력자")

Pr5<-("유한요소해석 분야 석사 이상 학위 소시자,구조공학 전공의 석사 이상 학위 소지자")

job_info5 <- data.frame(
  회사명 = com5,
  채용제목 = title5,
  지원자격 = El5,
  우대사항 = Pr5,
  stringsAsFactors = FALSE
)

#6
url6 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45918731?rPageCode=SL&logpath=21&sn=6")
a6 <- url6%>%
  html_nodes('span')%>%
  html_text()

com6<-a6[35]
title6<-"[삼정KPMG]데이터 가공업무 인턴 채용용"
El6<-("국내외 대학 4년제 졸업(예정)자,Excel기본활용(함수,피벗,데이터 분석도구 등)가능한 자, Data에 대한 이해 및 분석 능력을 보유한 자")

Pr6<-("STEM또는 상경계열 관련 전공, 데이터분석,대용량 데이터 처리 등 유사 경험 보유자,협업을 통한 프로젝트 완성 경험 보유자, 빠른학습력,원활한 의사소통 능력을 보유한 자")

job_info6 <- data.frame(
  회사명 = com6,
  채용제목 = title6,
  지원자격 = El6,
  우대사항 = Pr6,
  stringsAsFactors = FALSE
)

#7
url7 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45888494?rPageCode=SL&logpath=21&sn=6")
a7 <- url7%>%
  html_nodes('span')%>%
  html_text()

com7<-a7[35]
title7<-"라이브 서비스 분석 담당자 채용 (신입/경력)"
El7<-("로그 기반의 이슈와 문제점 분석 능력,실시간 데이터를 확인하고 해석하는 능력,현황에 대한 시각화 및 지표작성,정기적인 보고서 작성,원활한 커뮤니케이션 역량")
Pr7<-("QA 업무를 경험해보신분,라이브 서비스의 데이터베이스에 대한 이해와 분석 경험이 있으신분, 다양한 게임을 경험해보신분,Python을 활용한 데이터 처리 및 시각화 경험이 있는분,실행을 통해 실질적 결과를 가져오는 집요함을 소유하신 분")
job_info7 <- data.frame(
  회사명 = com7,
  채용제목 = title7,
  지원자격 = El7,
  우대사항 = Pr7,
  stringsAsFactors = FALSE
)

#8
url8 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45861448?rPageCode=SL&logpath=21&sn=6")
a8 <- url8%>%
  html_nodes('span')%>%
  html_text()

com8<-a8[35]
title8<-"[글로벌] 디지털 퍼포먼스 광고 경력 AE모집"
El8<-("대졸이상(4년),동종업계 경력,비즈니스회화 가능")

Pr8<-("영어권 지역 거주 이력,DA 매체 다수 집행 경력,엑셀 능력 우수자")

job_info8 <- data.frame(
  회사명 = com8,
  채용제목 = title8,
  지원자격 = El8,
  우대사항 = Pr8,
  stringsAsFactors = FALSE
)

#9
url9 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/46001387?rPageCode=SL&logpath=21&sn=6")
a9 <- url9%>%
  html_nodes('span')%>%
  html_text()

com9<-a9[35]
title9<-"[신입]소셜 빅데이터 분석 컨설턴트(데이터 사이언티스트)채용"
El9<-("대졸(2,3년)이상-전공무관")

Pr9<-("인문학 및 상기 업무 관련 전공자 우대,문서 작성 능력 우수자,R,Python 등 관련 교육 이수자 및 가능자, 외국어 가능자(영어,중국어,기타 언어권)")
job_info9 <- data.frame(
  회사명 = com9,
  채용제목 = title9,
  지원자격 = El9,
  우대사항 = Pr9,
  stringsAsFactors = FALSE
)

#10
url10 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45976025?rPageCode=SL&logpath=21&sn=6")
a10 <- url10%>%
  html_nodes('span')%>%
  html_text()

com10<-a10[35]
title10<-"[해외 디지털마케팅]검색엔진최적화 SEO 컨설턴트 채용(신입)"
El10<-("영어가능자(비즈니스 커뮤니케이션 가능),새로운 것에 대한 학습 의지가 높으신분,\"왜\"에 대하여 지속적으로 고민하고 답을 찾으려고 하시는분, 초 대졸 이상, SEO 컨설턴트로서 글로벌 검색 마케팅 시장을 정복하고자 하는 열정이 있으신분분")

Pr10<-("디지털 마케팅 코스 수료자, 엑셀,PPT활용 우수자, 웹이해도가 높거나 HTML/x-javascript이해 가능한 자,GA,AA,GSC 및 기타 SEO 관련 툴")
job_info10 <- data.frame(
  회사명 = com10,
  채용제목 = title10,
  지원자격 = El10,
  우대사항 = Pr10,
  stringsAsFactors = FALSE
)

#11
url11 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45976029?rPageCode=SL&logpath=21&sn=6")
a11 <- url11%>%
  html_nodes('span')%>%
  html_text()

com11<-a11[35]
title11<-"[신입]LLM 및 자연어 처리 연구원원"
El11<-("컴퓨터 공학 전공 또는 그에 준하는 교육 이수 또는 관련 경력, 트렌스포머 모델 구현 및 학습 경험,자연어처리(NLP)경험,Python 언어 사용 능력,팀원 간의 원활한 커뮤니케이션 능력 보유")
Pr11<-("외국어 능력 가지신분,논문 퍼블리싱 경험 있으신 분,강화학습 모델에 대한 깊은 이해가 있으신분,LLM학습 및 개발 경험이 있으신 분,RAG 경력자 ")

job_info11 <- data.frame(
  회사명 = com11,
  채용제목 = title11,
  지원자격 = El11,
  우대사항 = Pr11,
  stringsAsFactors = FALSE
)

#12
url12 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45976037?rPageCode=SL&logpath=21&sn=6")
a12 <- url12%>%
  html_nodes('span')%>%
  html_text()

com12<-a12[35]
title12<-"[신입]인공지능 딥러닝 개발자 모집집"
El12<-("대졸이상(컴퓨퓨터공학과 또는 이에 준하는 교육 수료)")

Pr12<-("자연어처리 프로젝트 경험자,유관업무 경력자,논문 퍼블리싱 경험 있으신분, 강화학습 모델에 대한 깊은 이해가 있으신분,트랜스포머 모델 개발 경험이 있으신분")

job_info12 <- data.frame(
  회사명 = com12,
  채용제목 = title12,
  지원자격 = El12,
  우대사항 = Pr12,
  stringsAsFactors = FALSE
)

#13
url13 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45973720?rPageCode=SL&logpath=21&sn=6")
a13 <- url13%>%
  html_nodes('span')%>%
  html_text()

com13<-a13[35]
title13<-"(주)버블콘 학습 분석 전문가로 성장할 인재 채용"
El13<-("인문학,철학,인류학 등 관련 분야 학사 이상(석사 우대),데이터 분석 및 통계에 대한 기본적인 이해,뛰어난 분석력과 통찰력을 바탕으로 복잡한 문제를 해결하는 능력,교육 분야에 대한 관심과 열정")
Pr13<-("교육공학,데이터 사이언스 등 관련 분야 복수 전공 또는 부전공,프로그래밍 언어(Python,R등)활용 능력")

job_info13 <- data.frame(
  회사명 = com13,
  채용제목 = title13,
  지원자격 = El13,
  우대사항 = Pr13,
  stringsAsFactors = FALSE
)

#14
url14 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45971933?rPageCode=SL&logpath=21&sn=6")
a14 <- url14%>%
  html_nodes('span')%>%
  html_text()

com14<-a14[35]
title14<-"AI Engineer 모집집"
El14<-("대학졸업예정자, 자연어처리(NLP),생성형 인공지능(Generative AI)전공자,RAG 경험이 있거나 LLM 모델 훈련 및 Fine-Tuning 가능자,논리적 사고력,커뮤니케이션 능력을 갖춘자")

Pr14<-("고객의 RAG 프로젝트 경험이 있는분, LLM Fine-tuning 관련 논문의 내용을 coding하여 구현 경험이 있는 분,그래프 분석을 통해 타인에게 그래프의 가치를 설득한 경험이 있는 분
       Neo4j(GraphDB)을 다뤄본 경험이 있거나 GraphRAG를 구축해본 경험이 있는분,그래프 기술과 비즈니스 접목하여 B2C 혹은 B2B 서비스한 경험이 있는분,해외 오픈커뮤니티에 자신의 글을 기고한 경험이 있는분
       자연어처리 및 리더보드 참여 경험이 있는분")

job_info14 <- data.frame(
  회사명 = com14,
  채용제목 = title14,
  지원자격 = El14,
  우대사항 = Pr14,
  stringsAsFactors = FALSE
)

#15
url15 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45963855?rPageCode=SL&logpath=21&sn=6")
a15 <- url15%>%
  html_nodes('span')%>%
  html_text()

com15<-a15[35]
title15<-"[와이즈스톤]테스팅 자동화 도구 개발자 채용용"
El15<-("개발경력 10년 이상,테스팅 자동화 도구 설계 및 구현 경험,협업 도구 및 프로세스 이해도 보유")

Pr15<-("CI연계 솔루션 개발 경험 보유자,워크플로우 관련 솔루션 개발 경험 보유자,데이터 관리 솔루션 개발 경험 보유자,관련 분야 자격증 보유자")

job_info15 <- data.frame(
  회사명 = com15,
  채용제목 = title15,
  지원자격 = El15,
  우대사항 = Pr15,
  stringsAsFactors = FALSE
)

#16
url16 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45968108?rPageCode=SL&logpath=21&sn=6")
a16 <- url16%>%
  html_nodes('span')%>%
  html_text()

com16<-a16[35]
title16<-"AI 개발(Python),데이터 분석 및 사업 기획 신규/경력 모집-정규직직"
El16<-("AI모델 연구 및 개발(Python),Big Data,통계,분석,모델링,시각화,데이터분석 및 사업기획에 관심이 있으신 분")

Pr16<-("정부 또는 기관에서 진행하는 빅뎅티ㅓ 분석과정을 수료하거나 관련 공모전에서 수상한 경험이 있는분,데이터분석 관련 논문 또는 관련 교육 기관에서 진행한 포토폴리오가 있으신 분")

job_info16 <- data.frame(
  회사명 = com16,
  채용제목 = title16,
  지원자격 = El16,
  우대사항 = Pr16,
  stringsAsFactors = FALSE
)

#17
url17 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45965306?rPageCode=SL&logpath=21&sn=6")
a17 <- url17%>%
  html_nodes('span')%>%
  html_text()

com17<-a17[35]
title17<-"[인사이트뷰테크]데이터베이스 구축 및 기술 상품화(번개장터 자회사)"
El17<-("4년제 졸")

Pr17<-("컴퓨터 활용 능력 우수자,유관 업무 경험자, 장기 근무 가능자")

job_info17 <- data.frame(
  회사명 = com17,
  채용제목 = title17,
  지원자격 = El17,
  우대사항 = Pr17,
  stringsAsFactors = FALSE
)

#18
url18 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45937517?rPageCode=SL&logpath=21&sn=6")
a18 <- url18%>%
  html_nodes('span')%>%
  html_text()

com18<-a18[35]
title18<-"데이터 사이언티스트 신입/경력 채용"
El18<-("데이터 과학, 통계학,수학 또는 관련 분야의 학사 이상 학위 소지자,
       데이터 사이언스,머신러닝,인공지능(AI)업무 경험자")

Pr18<-("자연어 처리(NLP)및 거대언어모델(LLM) 개발 경험자,
       생물정보학 관련 도메인 분석 경험자")

job_info18 <- data.frame(
  회사명 = com18,
  채용제목 = title18,
  지원자격 = El18,
  우대사항 = Pr18,
  stringsAsFactors = FALSE
)

#19
url19 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45911857?rPageCode=SL&logpath=21&sn=6")
a19 <- url19%>%
  html_nodes('span')%>%
  html_text()

com19<-a19[35]
title19<-"[서울]데이터 분석 및 AI모델 개발 정규직/계약직 모집"
El19<-("학사 또는 석사 이상")
Pr19<-("관련 학과 전공 우대, 해당직무 근무 경험, 자격증: 정보처리기사 등등")

job_info19 <- data.frame(
  회사명 = com19,
  채용제목 = title19,
  지원자격 = El19,
  우대사항 = Pr19,
  stringsAsFactors = FALSE
)

#20
url20 <- read_html("https://www.jobkorea.co.kr/Recruit/GI_Read/45849405?rPageCode=SL&logpath=21&sn=6")
a20 <- url20%>%
  html_nodes('span')%>%
  html_text()

com20<-a20[35]
title20<-"(주)한국여론리서치 정규직 전환형 인턴 연구원 채용용"
El20<-("국내외 4년제 대학 졸업자, 사회과학계열 전공자, Excel,PPT 활용능력 중급 이상자,적극적인 자세,꼼꼼한 태도를 가진다,해외 출장에 결격 사유가 없는 자자")

Pr20<-("SPSS 활용 경험자, 조사방법론에 대한 이해가 있는 자 , 컴퓨터활용능력 우수자, 엑셀 고급능력 보유자, 프리젠테이션 능력우수자,유관업무 경험자(인턴,알바)")

job_info20 <- data.frame(
  회사명 = com20,
  채용제목 = title20,
  지원자격 = El20,
  우대사항 = Pr20,
  stringsAsFactors = FALSE
)


plus_df <- bind_rows(job_info1, job_info2, job_info3, job_info4, job_info5, job_info6,
                     job_info7, job_info8, job_info9, job_info10, job_info11, job_info12,
                     job_info13, job_info14,job_info15,job_info16,job_info17,job_info18,job_info19,job_info20)

# 2. Save the dataframe
save(plus_df, file = "plus_df.RData")


### Jaepyeong ###
library(dplyr)
library(tidyverse)
library(rvest)

# Data Import
# 제목: div.sumTit

jobkor.url <- read.csv("jobkorea_url.csv") |> pull()

html.list <- lapply(jobkor.url, read_html)
# tag 가져오기
tag.list <- lapply(html.list, html_nodes, 'div.sumTit')
# tag로 text 가져오기
notice.list <- lapply(tag.list, html_text)
# 가져온 text에서 회사명과 채용 공고 제목 가져오기
jobkor.sum <- lapply(notice.list, function(lst){
  text <- lst |>
    # 공백이 2칸 이상인 경우를 기준으로 텍스트 분할
    str_split("[:space:]{2,}") |>
    unlist()
})

title <- c()
company <- c()

for(i in 1:length(jobkor.sum)){
  # 각 리스트의 7번째가 공고 제목
  title[i] <- jobkor.sum[[i]][7]
  # 각 리스트의 2번째가 회사명
  company[i] <- jobkor.sum[[i]][2]
}

# 채용공고별 지원자격
qualification <- c("학력 : 대졸(4년제) 이상, 관련 경력 3년 이상 (신입은 경력 무관)",
                   "AI 기술에 관심을 가지고 지속적으로 관련 학습이나 프로젝트를 수행하시는 분, 데이터 라벨링, 전처리 등의 관련 경험과 그 과정에서 필요한 기본적인 지식이 있으신 분, 메이플스토리의 다양한 이벤트나 콘텐츠 플레이 경험이 있으신 분 ※ 지원서류 내 본인의 메이플스토리 닉네임 필수 기재",
                   "Splunk 솔루션에 대한 이해나 경험이 필요합니다, Linux Command 및 Shell, Python에 대한 기초 지식이 필요합니다, Data를 가공하여 필요한 정보를 뽑을 수 있는 분이 필요합니다, 이해력이 빠르고 학습능력이 좋아 이해한 것을 정리하고 실행으로 옮길 수 있는 분이 필요합니다, 적극적으로 커뮤니케이션하고 대인 관계가 원만한 분이 필요합니다",
                   "데이터 분석을 통한 인사이트 도출 또는 비지니스 문제 해결 경험이 있으신 분, 분석 결과를 논리적으로 설명하고 이해관계자에게 명확하게 전달할 수 있는 커뮤니케이션 스킬을 보유하신 분, 기초 통계 및 마케팅에 대한 이해가 있으신 분, 다양한 명령문과 조건문을 활용한 SQL 쿼리 작성이 능숙하신 분, MS 오피스(Excel, PPT)를 활용한 데이터 시각화에 능숙하신 분",
                   "지표의 흐름을 읽고 간결한 언어로 전달해 주실 수 있는 분, 데이터를 활용해 효과적으로 이해관계자를 설득할 수 있는 분, 논리적 사고와 수학적/통계학적 지식을 갖추고 그것을 잘 활용하시는 분, 문제가 생기면 집요하게 원인을 추적하는 데이터 탐정",
                   "학력 : 대졸이상, 경력 : 신입, 경력",
                   "학력 : 대졸이상 (졸업예정자 가능), 경력 : 신입·경력",
                   "신입 또는 관련 업무 경험이 7년 이내 이신 분, Excel 및 Google Sheets 활용 능력이 뛰어나신 분, 데이터 분석 툴 또는 기법에 대한 이해와 적용 능력을 보유하신 분",
                   "학력: 학사이상(졸업예정자 가능), 경력: 신입, 경력, Python, C++, Tensorflow, Keras, OpenCV 등 개발 가능자 머신러닝/딥러닝에 대한 폭넓은 기본지식 최신 연구논문에 대한 이해 및 활용 가능자 AI 빅데이터 관련분야의 흥미 및 열정적인 연구 가능자",
                   "SQL, Python 등 데이터 분석에 필요한 기본 언어를 잘 다루시는 분, 비즈니스에 대한 호기심이 많고 가설을 수립하고 데이터로 증명하는 것을 좋아하시는 분, 머신러닝 및 딥러닝을 비즈니스에 적용하여 의사결정을 돕는 경험을 해보고 싶은 분, 머신러닝 및 딥러닝을 비즈니스에 적용하여 의사결정을 돕는 경험을 해보고 싶은 분, 유연한 사고를 바탕으로 토론을 선호 하시는 분",
                   "신입 (병역특례 우대), 경력 : 경력3년~10년",
                   "학력 : 대졸 이상(대졸 예정)",
                   "학력 : 대학원 석사이상 (졸업자), 경력 : 신입·경력",
                   "부동산, 도시계획, 통계, 데이터 관련 전공 석사 이상, Python, SQL 등을 활용한 데이터 추출/가공/분석 가능자, QGIS 사용 가능자, MS오피스(엑셀, 워드, 파워포인트) 중상 이상",
                   "학력 : 대졸 이상 (2,3년), 석사/박사 우대, 경력 : 신입/경력 2년 이상(연구원, 책임연구원, 팀장급)",
                   "대졸 (컴퓨터 공학, 소프트웨어 공학, AI 관련 전공 우대), Python 언어에 대한 깊은 이해 및 개발 경험, API 개발 경험 (FastAPI, Flask 등), 클라우드 환경 또는 컨테이너화 기술(Kubernetes, Docker) 사용 경험, LLM 및 AI Agent 관련 기술에 대한 기본적인 이해, 문제 해결 능력과 팀워크, 소통 능력",
                   "학력 : 학력무관, 경력 : 신입 / 경력, (신입의 경우) 1년 이내, 활용 스킬 함양",
                   "대졸이상 (졸업예정자 가능), Python 언어로 논리 구현 가능하신 분, Pytorch, Tensorflow 등의 Deep learning 프레임워크를 사용해보신 분, 인공지능 관련 교육과정 이수 or 관련 연구소나 대학원 Lab에서 6개월 이상의 인턴 경험이 있으신 분",
                   "학력 : 학력무관, 경력 : 신입·경력")



# 채용 공고별 우대사할
preference <- c("자연어 처리(NLP이해, 라이브러리 활용 능력, 대화 설계 및 처리 능력), 머신 러닝 및 딥러닝(대형 언어 모델 이해, 딥러닝 프레임워크 활용, 모델 최적화 및 성능 개선), API 통합 및 웹 개발(AIP 설계 및 통합, 웹 개발 지식, 클라우드 서비스 활용, 프로그래밍 언어), LLM 플랫폼을 이용한 서비스 기획(LLM(대형 언어 모델) 플랫폼의 활용 및 서비스 기획 경험, 사용자 인이스와 사용자 경험을 최적화한 챗봇 설계 능력), 프로젝트 관리 및 협업 도구(프로젝트 관리, 버전 관리, CI/CD 구축 경험)",
                "이미지, 텍스트 등 다양한 유형의 데이터를 다뤄본 경험이 있으신 분, Python 을 활용한 데이터 처리 및 분석 경험이 있으신 분, 수학/통계/컴퓨터 공학 등 데이터 분석 관련 학위를 보유하신 분",
                "Linux 및 Windows OS, Public Cloud(AWS, Azure, GCP)에 대한 이해가 있으면 우대합니다, Splunk, ELK, Kafka 등 다양한 로그수집 및 분석과 시스템 구축 경험이 있으면 우대합니다, 정보처리/정보보안기사, CISA/CISSP, Cloud, Bigdata 관련 자격증이 있으면 우대합니다",
                "SQL 자격증을 취득하신 분, 미디어 및 소비자 이용 형태, 광고/마케팅 효과 분석 및 컨설팅 경험이 있으신 문, Cohort 분석 또는 마케팅 분석 기법을 사용한 데이터 분석 경험이 있으신 분, PPT를 사용한 Pitch Deck/비지니스 제안서 작성 경험이 있으신 분",
                "다양한 게임을 깊이 있게 즐기시는 분, 게임을 비즈니스적인 시각에서 바라보실 수 있는 분, SQL을 활용하여 생각하는 데이터를 추출할 수 있는 분, Tableau와 같은 BI 툴을 이용한 데이터 시각화 경험이 있는 분, 비즈니스 레벨 이상의 영어 능력을 보유하신 분, 전산학, 컴퓨터공학, 산업공학, 통계학, 수학 등 관련 학위를 소지하셨거나 해당 분야에 대한 경험이 있는 분",
                "석/박사학위 수여자, 수학적 알고리즘 지식 소유자, 머신러닝, 데이터 마이닝 등 관련 지식 소유자, 딥러닝 프레임워크(Torch, TensorFlow 등)를 이용한 개발 경험이 있는 자, 윈도우 앱/UI 개발 경험 소유자, 오픈소스 사용 경험 소유자",
                "석/박사학위 수여자, 전공 : 통계학, 산업공학, 수학, 컴퓨터공학등 유관 전공, 분석 툴 ECMiner 활용에 대한 사전 이해가 있는분 (무료버전 3개월 사용가능), Python/R 언어를 활용하여 데이터 정제, 분석, 모델링, 시각화 프로그래밍 경험, 분석 결과에 대한 명확한 의사소통 및 고객과의 원만한 커뮤니케이션 능력",
                "E-commerce 기반의 물류센터에서 업무 경험이 있으신 분, 데이터 분석 및 관리 도구 사용이 가능하신 분 (BigQuery, SQL, R, Python 등), 팀워크와 원활한 소통 능력을 갖추신 분",
                "관련분야 석사 이상 우대, 유관업무 경력자 (2년), 장기근무 가능자, 자격증: 정보처리기사, 전공: 전산학·컴퓨터공학, 산업공학, 통계학 AI 빅데이터 관련 정부과제 연구개발 경험자, AI 빅데이터 관련 솔루션 개발 실무경험자, JAVA/JSP, Node.JS, SQL/NoSQLDB, Linux, NT 등 개발 경험자",
                "컨설팅 방법론 및 AI/ML 기술을 바탕으로 엔터프라이즈 기업의 문제를 해결하고 싶은 분, 문장 생성 및 요약, 추천, 예측 분야의 AI/ML 연구를 통해 비지니스 가치를 만들어 보고 싶은 분, 데이터 분석 뿐 아니라 데이터 Pipe-line 설계 및 개발까지 역량과 경험을 확장하고 싶은 분, 재미있게 일하고 싶은 스타트업 DNA가 있으신 분",
                "전공 : 컴퓨터 공학 또는 인공지능 관련 학과, 관련 분야의 석사, 박사 학위 취득자, 의료영상 진단 AI 프로그램을 개발하여, 국내외 식약처 3등급 소프트웨어 의료기기 인증을 받은 경험자, 의료영상 진단 AI 분야의 제품 개발 경험자, 이미지 프로세싱 전문가, 병역특례 (석사 이상 현역/보충역/전입자) 우대",
                "Python 사용 경험자 우대, Splunk(스플렁크) 경험자 우대, 통합로그분석솔루션 경험자 우대, IT관련 전공자 우대",
                "영어능통자(원어민수준), 통계분석·리서치 능숙자, 석사학위 수여자, 박사학위 수여자, 관련 학과 전공자, 관련 자격증 보유자, 리더쉽 소유자, 프리젠테이션 능력우수자, 문서작성 우수자, 국가(공공)기관 출신자, 대기업 근무 경험자, 외국어 : OPI, OPIc, TOEIC(Speaking), 자격증 : 정보통신기사, 정보처리기사, 전자계산기조직응용기사, 전공 : 소프트웨어공학과, 데이터사이언스ㆍ인공지능전공",
                "데이터구축, 가공, 로직 설계에 대한 경험이 있으신 분, 상업용 부동산 시장에 관한 데이터 분석 경험이 있으신 분, 문제를 정의하고, 다양한 문제를 해결한 경험이 있으신 분, 통계적 접근법과 논리적인 사고 능력이 있으신 분",
                "전기/전자공학, 컴퓨터/시스템공학, 수학/통계학, 정보처리기사, 빅데이터분석기사, 데이터분석전문가, 영어가능자, 인근거주자, 해당직무 근무경험",
                "LLM 모델 서빙 개발 경험, Large Scale Web Application 개발 경험, K8s 및 Cloud에서 Application을 개발 및 운영 경험, AI 모델 및 LangGraph 사용 경험, 대규모 언어 모델(LLM) 관련 프로젝트 경험,React와 Chakra를 이용한 웹 UI 개발 경험",
                "ML,RAG 관련 서비스 관련 경험 우대, 대규모 ML 관련 서비스 및 SI 관련 경험 우대, 공모전 입상자, 데이터 엔지니어링 관련 자격증 보유, 컴퓨터 공학 학사/석사 전공자, 머신러닝/딥러닝 모델 경량화, 최적화 프로젝트 경험, 즉시출근 가능",
                "학점우수(대졸 4.0이상), 유관업무 경험자(인턴·알바),  관련 학과(전산학·컴퓨터공학 계열, 수학·물리학 등 기초 과학) 전공, 관련 학과 석ㆍ박사 학위 수여자",
                "관련 학과 전공자, 관련 자격증 보유자, 통계분석·리서치 능숙자, 유관업무 경력자 (인턴), 데이터 분석 대회 참여 경험")


# 하나의 데이터 프레임으로 변경
notice <- data.frame(회사명 = company,
                     채용제목 = title,
                     지원자격 = qualification,
                     우대사항 = preference)

# 데이터 프레임을 RData로 저장
save(notice, file = "TM_data.RData")


##### DATA COMBINE #####
# Load each collected data
data_CJ <- load('data.RData')
data_CJ

data_JH <- load('plus_df.RData')
data_JH

data_JP <- load('TM_data.RData')
data_JP

# Combine into one data frame
final_df <- rbind(combined_df, plus_df, notice)

# Save the dataframe
save(final_df, file = "final_df.RData")


##### DATA PREPROCESSING #####

library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud2)


load("final_df.RData")

# Function for text preprocessing
text_prep <- function(text) {
  text |>
    tolower() |>
    str_replace_all("[:punct:]{1,}", " ") |>
    str_replace_all("빅데이터|big data", replacement = "BigData ") |>
    str_replace_all("데이터사이언스|데이터 사이언스|데이터 과학", replacement = "DataScience ") |>
    str_replace_all("데이터", replacement = "data ") |>
    str_replace_all("파이선", replacement = "python ") |>
    str_replace_all("코딩", replacement = "coading") |>
    str_replace_all("text mining|텍스트 마이닝|텍스트마이닝", replacement = "TextMining ") |>
    str_replace_all("자연어처리|자연어 처리|nlp[:alpha:]*", replacement = "NLP ") |>
    str_replace_all("NLP[:space:]{1,}NLP", replacement = "NLP ") |>
    str_replace_all("거대언어모델|거대 언어 모델|large language model|대형 언어 모델|llm[:alpha:]*", replacement = "LLM ") |>
    str_replace_all("LLM[:space:]{1,}LLM", replacement = "LLM ") |>
    str_replace_all("소프트웨어|sw", replacement = "software ") |>
    str_replace_all("머신러닝|기계학습|머신 러닝|ml|machine learning", replacement = "MachineLearning ") |>
    str_replace_all("ci[:alpha:]*", replacement = "ci ") |>
    str_replace_all("cd[:alpha:]*", replacement = "cd ") |>
    str_replace_all("딥러닝|dl|deep learning|deeplearning", replacement = "DeepLearning ") |>
    str_replace_all("엔지니어링", replacement = "engineering ") |>
    str_replace_all("엔지니어", replacement = "engineer ") |>
    str_replace_all("인공지능|ai|ai[:alpha:]*", replacement = "AI ") |>
    str_replace_all("AI[:space:]{1,}AI", replacement = "AI") |>
    str_replace_all("인사이트", replacement = "insight ") |>
    str_replace_all("엑셀", replacement = "excel ") |>
    str_replace_all("워드", replacement = "word ") |>
    str_replace_all("파워포인트", replacement = "ppt ") |>
    str_replace_all("경험[:alpha:]*", replacement = "경험 ") |>
    str_replace_all("경력[:space:]*신입[:space:]*경력", replacement = "신입 경력 ") |>
    str_replace_all("경력[:space:]*경력", replacement = "경력 ") |>
    str_replace_all("자\\b|분\\b|\\b수\\b|\\b잘\\b|은\\b|는\\b|이\\b|가\\b|을\\b|를\\b|의\\b|\\b및\\b|중\\b|에\\b|에서\\b|에게\\b|\\b수\\b|것\\b|또는|대한|있[:alpha:]*|\\b등\\b|뿐\\b|\\b싶|이며\\b|우대|보유|관련|\\|", " ") |>
    str_replace_all("[:space:]{1,}", " ")
}


# qualification & preference
prep.data <- final_df |>
  mutate(지원자격 = text_prep(지원자격),
         우대사항 = text_prep(우대사항))
prep.data$지원자격
# 전처리된 데이터 보기
head(prep.data)

# '지원자격' token data
token.지원 <- prep.data$지원자격 |>
  str_split(" ") |>
  lapply(function(x){x[x != ""]}) |>
  unlist()
token.지원 |> head(20)

# '우대사항' token data
token.우대 <- prep.data$우대사항 |>
  str_split(" ") |>
  lapply(function(x){x[x != ""]}) |>
  unlist()
token.우대 |> head(20)

# Check the frequency
freq.지원 <- data.frame(word = token.지원) |>
  count(word, sort = T)
freq.지원 |> head(20)

freq.우대 <- data.frame(word = token.우대) |>
  count(word, sort = T)
freq.우대 |> head(20)


##### VISUALIZATION #####
# '지원자격' bar plot
top15.지원 <- freq.지원 %>% head(15)
top15.지원 |>
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  
  ggtitle("Word Frequency of Qualification") +
  xlab("Word") +
  ylab("Freq") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1))

# '우대사항' bar plot
top15.우대 <- freq.우대 %>% head(15)
top15.우대 |>
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  
  ggtitle("Word Frequency of Preference") +
  xlab("Word") +
  ylab("Freq") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1))

# Wordcloud
wordcloud2(data = freq.지원, size = 0.5, color = "random-light")
wordcloud2(data = freq.우대, size = 1.2, color = "random-light")


##### N-gram #####
# Generate n-grams (3-grams) for "지원자격"
n_gram_df_지원 <- prep.data %>%
  unnest_tokens(ngram, 지원자격, token = "ngrams", n = 3) %>%  # n = 2로 2-gram 적용
  count(ngram, sort = TRUE)


# Filter n-grams containing specific keywords for "지원자격"
filtered_n_gram_df_경험 <- n_gram_df_지원 %>%
  filter(str_detect(ngram, "경험"))

filtered_n_gram_df_이상 <- n_gram_df_지원 %>%
  filter(str_detect(ngram, "이상"))


filtered_n_gram_df_능력 <- n_gram_df_지원 %>%
  filter(str_detect(ngram, "능력"))

filtered_n_gram_df_경력 <- n_gram_df_지원 %>%
  filter(str_detect(ngram, "경력"))

filtered_n_gram_df_python <- n_gram_df_지원 %>%
  filter(str_detect(ngram, "python"))

# Combine filtered dataframes and display results
python2<-filtered_n_gram_df_python%>%head(10)
경력2<-filtered_n_gram_df_경력%>%head(10)
이상2<-filtered_n_gram_df_이상%>%head(10)
능력2<-filtered_n_gram_df_능력%>%head(10)
경험2<-filtered_n_gram_df_경험%>%head(10)



# Generate n-grams (3-grams) for "우대사항"
n_gram_df_우대 <- prep.data %>%
  unnest_tokens(ngram, 우대사항, token = "ngrams", n = 3) %>%  # n = 2로 2-gram 적용
  count(ngram, sort = TRUE)

# Filter n-grams containing specific keywords for "우대사항"
filtered_n_gram_df_경험 <- n_gram_df_우대 %>%
  filter(str_detect(ngram, "경험"))


filtered_n_gram_df_개발 <- n_gram_df_우대 %>%
  filter(str_detect(ngram, "개발"))

filtered_n_gram_df_분석 <- n_gram_df_우대 %>%
  filter(str_detect(ngram, "분석"))

filtered_n_gram_df_ai <- n_gram_df_우대 %>%
  filter(str_detect(ngram, "ai"))

filtered_n_gram_df_능력 <- n_gram_df_우대 %>%
  filter(str_detect(ngram, "능력"))

filtered_n_gram_df_전공 <- n_gram_df_우대 %>%
  filter(str_detect(ngram, "전공"))

#결과보기
경험<-filtered_n_gram_df_경험%>%head(10)
개발<-filtered_n_gram_df_개발%>%head(10)
분석<-filtered_n_gram_df_분석%>%head(10)
ai<-filtered_n_gram_df_ai%>%head(10)
전공<-filtered_n_gram_df_전공%>%head(10)


# Create word clouds using the top n-grams
library(wordcloud)
library(dplyr)
library(tm)

# Combining Data Frames
지원_df <- bind_rows(python2, 경력2, 이상2, 능력2, 경험2)
우대_df <- bind_rows(경험, 개발, 분석, ai, 전공)
# Extract words and frequency columns within data frames
# merged_df is Suppose that there are two columns that represent words and frequencies
word_freq <- 지원_df %>%
  group_by(ngram) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n))

# Create WordCloud
wordcloud(words = word_freq$ngram,
          freq = word_freq$n,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# For "지원자격"
word_freq <- 우대_df %>%
  group_by(ngram) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n))

# Create WordCloud
wordcloud(words = word_freq$ngram,
          freq = word_freq$n,
          min.freq = 1,
          max.words = 10,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))


