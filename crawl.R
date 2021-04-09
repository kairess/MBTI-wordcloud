#### 페키지 로드 #####
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
#### 페키지 로드 #####

# 글 최대 id
max_id = 1000

#딜레이 시간 설정
times = 2

for (c in list('ENFJ','ENFP','ENTJ','ENTP','ESFP','ESFJ','ESTJ','ESTP','INFJ','INFP','INTJ','intp_mbti','ISFJ','ISFP','ISTJ','ISTP')) {
  print(c)

  # 데이터 프레임 생성
  data_full <- data.frame()

  # 게시판 글 숫자 만큼의 루프문 생성
  for (i in 1:max_id) {
    # URL 지정
    url_intp <- paste0("https://gall.dcinside.com/mgallery/board/view/?id=",tolower(c),"&no=",i)

    # read html page #
    html_page <- tryCatch(html_page <- read_html(url_intp),
            error = function(e) return("error"),
            warning = function(w) return("warning"),
            finally = NULL)
    # read html page #

    # 404 에러가 발생하는 경우 다음 루프를 진행
    if(html_page == "error") {
      print("error 404! next loop!")
      Sys.sleep(times)
      next
    }

    #### title 추출 ####
    html_title <- html_text(html_nodes(html_page, '.title_subject'))

    #### contents 추출 #### 
    html_content1 <- html_nodes(html_page, '.writing_view_box')
    html_content2 <- html_nodes(html_content1, 'div')

    html_content3 <- str_replace_all(html_text(html_content2)[4],"\r"," ")

    if(is.na(html_content3)){
      html_content3 <- str_replace_all(html_text(html_content2)[3],"\r"," ")
    }

    if(is.na(html_content3)){
      html_content3 <- str_replace_all(html_text(html_content2)[2],"\r"," ")
    }


    html_content4 <- str_replace_all(html_content3,"\t"," ")
    html_content_fin <- str_replace_all(html_content4,"\n"," ")


    # 데이터 프레임 병합
    data <- data.frame(num = i, title = html_title, content = html_content_fin)

    # 데이터 프레임 병합
    data_full <- bind_rows(data_full,data)

    # 딜레이 지정
    Sys.sleep(times)

    print(i)
  }

  # 결과파일 저장
  write.csv(data_full, paste0("./", c, ".csv"))
}

