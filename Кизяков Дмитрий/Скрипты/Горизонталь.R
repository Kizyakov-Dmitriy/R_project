EXT_SUPPLY <-'in'
EXT_SALE <-'out'


check <- function(way){
  way_lst <- unlist(strsplit(x = way, split = "/"))
  if (length(way_lst) == 1){
    return ()
  }
  if (dir.exists(way)){
    check(do.call(paste0,as.list(paste0(way_lst[1:length(way_lst) - 1], "/"))))
    return()
  }
  else{
    check(do.call(paste0,as.list(paste0(way_lst[1:length(way_lst) - 1], "/"))))
    setwd(do.call(paste0,as.list(paste0(way_lst[1:length(way_lst) - 1], "/"))))
    dir.create(tail(way_lst, 1))
  }
}


generate.data <- function(name = "Горизонталь", way = getwd(), type = "поставка", mn = 0, mx = 100, days = 7){
  if (way != ""){
    check(way)
    setwd(way)
  }
  if (tolower(type) == "поставка"){ex <- EXT_SUPPLY}
  else{ex <- EXT_SALE}
  if (1 - file.exists(paste0(name, ".", EXT_SUPPLY))){
    if (1 - file.exists(paste0(name, ".", EXT_SALE))){
      col1 <- 1:days
      col2 <- sample(mn:mx, size = days, replace = TRUE)
    }
    else{
      col1 <- 1:nrow(post)
      col2 <- c()
      post <- read.table(paste0(name, ".", EXT_SALE), head = TRUE, encoding = "UTF-8")
      for (i in 1:nrow(post)){
        if (mx >= post[i, 2]){
          col2 <- c(col2, sample(post[i, 2]:mx, size = 1))
        }
        else{
          col2 <- c(col2, sample(mx:mx+10, size = 1))
        }
      }
    }
  }
  tab1 <- data.frame("День" = col1, "Поставка" = col2)
  write.table(tab1, file = paste0(way, "/", name, ".", EXT_SUPPLY), sep = "\t", row.names = FALSE)
  if (tolower(type) == "продажа"){
    post <- read.table(paste0(name, ".", EXT_SUPPLY), head = TRUE, encoding = "UTF-8")
    col4 <- 1:nrow(post)
    col3 <- c()
    for (i in 1:nrow(post)){
      if (mn <= post[i, 2]){
        col3 <- c(col3, sample(mn:post[i, 2], size = 1))
      }
      else{
        col3 <- c(col3, sample(0:mn, size = 1))
      }
    }
    tab2 <- data.frame(День = col4, Продажа = col3)
    write.table(tab2, file = paste0(way, "/", name, ".", EXT_SALE), sep = "\t", row.names = FALSE)
  }
}

generate.price <- function(goods, way){
  dir_check(way)
  tabl <- data.frame("Действие" = c("Покупка", "Продажа", "Утилизация"))
  for (i in 1:length(goods)){
    tabl[1, i+1] <- goods[[i]]$sup
    tabl[2, i+1] <- goods[[i]]$sal
    tabl[3, i+1] <- goods[[i]]$utl
    colnames(x = tabl)[i+1] = goods[[i]]$name
  }
  write.table(tabl, file = paste0(way, "/", "price", ".txt"), sep = "\t", row.names = FALSE)
  return(tabl)
}

{
  P_sale <- 12960
  P_supply <- 5400
  P_util <- 4644
  
  
  
  
  setwd("C:/Кизяков Дмитрий/Горизонталь/Анализ")
  
  in1 <- read.table("Магазин1_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out1 <- read.table("Магазин1_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in2 <- read.table("Магазин2_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out2 <- read.table("Магазин2_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in3 <- read.table("Магазин3_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out3 <- read.table("Магазин3_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in4 <- read.table("Магазин4_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out4 <- read.table("Магазин4_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in5 <- read.table("Магазин5_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out5 <- read.table("Магазин5_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in6 <- read.table("Магазин6_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out6 <- read.table("Магазин6_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in7 <- read.table("Магазин7_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out7 <- read.table("Магазин7_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in8 <- read.table("Магазин8_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out8 <- read.table("Магазин8_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in9 <- read.table("Магазин9_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out9 <- read.table("Магазин9_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  in10 <- read.table("Магазин10_Горизизонталь.in", head = TRUE, encoding = "UTF-8")
  out10 <- read.table("Магазин10_Горизизонталь.out", head = TRUE, encoding = "UTF-8")
  
  inn <- list(in1, in2, in3, in4, in5, in6, in7, in8, in9, in10)
  outt <- list(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10)
  
  
  names(in1)[names(in1) == "День"] <- "День недели"
  

  
  rev <- rep(0, 12)
  profit <- rep(0, length(rev))
  name = c("Магазин 1","Магазин 2","Магазин 3","Магазин 4","Магазин 5","Магазин 6","Магазин 7","Магазин 8","Магазин 9","Магазин 10","Итого","Среднее")
  res.tab <- data.frame("Выручка" = rev, "Прибыль" = profit, row.names = name)
  Implementing <-rep(0, nrow(res.tab))
  res.tab$Реализация <- Implementing
  write_off <-rep(0, nrow(res.tab))
  res.tab$Списание <- write_off
  uniformity <-rep(0, nrow(res.tab))
  res.tab$'sd' <- uniformity
  sale_max <-c(rep(0, nrow(res.tab) - 2), "", "")
  res.tab$'Продажи макс' <- sale_max
  day1 <-c(rep(0, nrow(res.tab) - 2), "", "")
  res.tab$День <- day1
  sale_min <-c(rep(0, nrow(res.tab) - 2), "", "")
  res.tab$'Продажи мин' <- sale_min
  day2 <-c(rep(0, nrow(res.tab) - 2), "", "")
  res.tab$' День' <- day2
  write_off_max <-c(rep(0, nrow(res.tab) - 2), "", "")
  res.tab$'Списание макс' <- write_off_max
  day3 <-c(rep(0, nrow(res.tab) - 2), "", "")
  res.tab$'  День' <- day3

  for (i in 1:length(outt)) {
    res.tab[i, 5] <- sd(outt[[i]][, 2])
    TR <- P_sale * sum(outt[[i]][, 2])
    TC <- P_supply * sum(inn[[i]][, 2]) + P_util * (sum(inn[[i]][, 2]) - sum(outt[[i]][, 2]))
    res.tab[i, 2] <- TR - TC
    res.tab[i, 3] <- sum(outt[[i]][, 2])
    res.tab[i, 4] <- sum(inn[[i]][, 2]) - sum(outt[[i]][, 2])
    res.tab[i, 1] <- P_sale * sum(outt[[i]][, 2])
    res.tab[i, 6] <- max(outt[[i]][, 2])
    res.tab[i, 7] <- which.max(outt[[i]][, 2])
    res.tab[i, 8] <- min(outt[[i]][, 2])
    res.tab[i, 9] <- which.min(outt[[i]][, 2])
    res.tab[i, 10] <- max(inn[[i]][, 2] - outt[[i]][, 2])
    res.tab[i, 11] <- which.max(inn[[i]][, 2] - outt[[i]][, 2])
    
  }
  res.tab[11, 5] <- sum(res.tab[, 5])
  res.tab[12, 5] <- res.tab[11, 5] / (length(dir()) / 2)
  res.tab[11, 2] <- sum(res.tab[, 2])
  res.tab[12, 2] <- res.tab[11, 2] / (length(dir()) / 2)
  res.tab[11, 3] <- sum(res.tab[, 3])
  res.tab[12, 3] <- res.tab[11, 3] / (length(dir()) / 2)
  res.tab[11, 4] <- sum(res.tab[, 4])
  res.tab[12, 4] <- res.tab[11, 4] / (length(dir()) / 2)
  res.tab[11, 1] <- sum(res.tab[, 2])
  res.tab[12, 1] <- res.tab[11, 1] / (length(dir()) / 2)
  
  
  #write.table(res.tab, file = "C:/Кизяков Дмитрий/Скрипты/аналитика.csv", sep = ";", row.names = FALSE)
  
  
  
  
  
  
  
  res.tab
}




generate.data(name = "Файл", way = "C:/Это/Моя/Папочка", type = "продажа", mn = 40, mx = 60, days = 7)
