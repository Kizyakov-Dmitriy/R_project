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

generate.data <- function(name = "Горизонталь", way = getwd(), type = "поставка", days = 7, goods = list(good = "Тушенка", mn = 30, mx = 50)){
  if (way != ""){
    check(way)
    setwd(way)
  }
  if (1 - file.exists(paste0(name, ".", EXT_SUPPLY))){
    if (1 - file.exists(paste0(name, ".", EXT_SALE))){
      tab1 <- data.frame("День" = 1:days)
      for (i in 1:length(goods)){
        tab1[i+1] <- sample(x = goods[[i]]$mn:goods[[i]]$mx, size = days, replace = TRUE)
        colnames(x = tab1)[i+1] = goods[[i]]$good
      }
    }
    else{
      data.out <- read.table(paste0(name, ".", EXT_SALE), head = TRUE, encoding = "UTF-8")
      tab1 <- data.frame("День" = 1:days)
      for (i in 1:length(goods)){
        tab1[i+1] <- sample(x = goods[[i]]$mn:goods[[i]]$mx, size = days, replace = TRUE)
        colnames(x = tab1)[i+1] = goods[[i]]$good
      
        for (j in 1:nrow(tab1)){
          if (tab1[j, i+1] < data.out[j, i+1]){
            tab1[j, i+1] <- data.out[j, i+1]
          }
        }
      }
    }
    write.table(tab1, file = paste0(way, "/", name, ".", EXT_SUPPLY), sep = "\t", row.names = FALSE)
    return(tab1)
  }
  if (tolower(type) == "продажа"){
    data.in <- read.table(paste0(name, ".", EXT_SUPPLY), head = TRUE, encoding = "UTF-8")
    tab2 <- data.frame("День" = 1:days)
    for (i in 1:length(goods)){
      tab2[i+1] <- sample(x = goods[[i]]$mn:goods[[i]]$mx, size = days, replace = TRUE)
      colnames(x = tab2)[i+1] = goods[[i]]$good
      
      for (j in 1:nrow(tab2)){
        if (tab2[j, i+1] > data.in[j, i+1]){
          tab2[j, i+1] <- data.in[j, i+1]
        }
      }
    }
    write.table(tab2, file = paste0(way, "/", name, ".", EXT_SALE), sep = "\t", row.names = FALSE)
    return(tab2)
  }
}

generate.price <- function(goods, way){
  check(way)
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
  
  
  goods_prices <- list(
    list(name = "Товар1", sup = 100, sal = 200, utl = 20), 
    list(name = "Товар2", sup = 105, sal = 210, utl = 35), 
    list(name = "Товар3", sup = 110, sal = 215, utl = 50), 
    list(name = "Товар4", sup = 115, sal = 220, utl = 65),
    list(name = "Тушёнка", sup = 120, sal = 225, utl = 70)
  )
  
  goods_list = list(
    list(good = "Товар1", mn = 100, mx = 200), 
    list(good = "Товар2", mn = 105, mx = 205), 
    list(good = "Товар3", mn = 110, mx = 210), 
    list(good = "Товар4", mn = 115, mx = 215),
    list(good = "Тушёнка", mn = 115, mx = 215)
  )
  
  d <- 20
  
  specific <- list(
    list(shop = 3, good = "Товар3"),
    list(shop = 5, good = "Товар1"),
    list(shop = 8, good = "Товар2")
  )
  
  spec_shop <- list(1, 2, 4, 8)
  
  spec_good <- list("Товар4", "Товар2", "Товар3", "Товар1", "Тушёнка")
  
#--------------генерация------------  
  in1 <- generate.data(name = "Магазин1_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out1 <- generate.data(name = "Магазин1_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in2 <- generate.data(name = "Магазин2_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out2 <- generate.data(name = "Магазин2_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in3 <- generate.data(name = "Магазин3_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out3 <- generate.data(name = "Магазин3_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in4 <- generate.data(name = "Магазин4_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out4 <- generate.data(name = "Магазин4_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in5 <- generate.data(name = "Магазин5_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out5 <- generate.data(name = "Магазин5_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in6 <- generate.data(name = "Магазин6_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out6 <- generate.data(name = "Магазин6_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in7 <- generate.data(name = "Магазин7_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out7 <- generate.data(name = "Магазин7_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in8 <- generate.data(name = "Магазин8_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out8 <- generate.data(name = "Магазин8_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in9 <- generate.data(name = "Магазин9_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out9 <- generate.data(name = "Магазин9_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  in10 <- generate.data(name = "Магазин10_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "поставка", days = d)
  out10 <- generate.data(name = "Магазин10_Горизизонталь", goods = goods_list, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ", type = "продажа", days = d)
  
  inn <- list(in1, in2, in3, in4, in5, in6, in7, in8, in9, in10)
  outt <- list(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10)
  
  Price <- generate.price(goods = goods_prices, way = "C:/Кизяков Дмитрий/Горизонталь2/Анализ")

#---------основная таблица----------  

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
    res.tab[i, 3] <- sum(outt[[i]][,2:ncol(out1)]) #done
    res.tab[i, 4] <- sum(outt[[i]][,2:ncol(out1)]) + sum(inn[[i]][,2:ncol(in1)]) #done
    for (j in 2:ncol(out1)) {
      nam <- colnames(x = in1)[j]
      TR <- sum(out1[,j]) * Price[2, ][[nam]]
      TC <- sum(in1[,j]) * Price[1, ][[nam]] + (sum(in1[,j]) - sum(out1[,j])) * Price[3, ][[nam]]
      res.tab[i, 1] <- res.tab[i, 1] + Price[2, ][[nam]] * sum(outt[[i]][, j])
      res.tab[i, 2] <- res.tab[i, 2] + TR - TC
      res.tab[i, 5] <- res.tab[i, 5] + sd(outt[[i]][, j])
      res.tab[i, 6] <- max(outt[[i]][, j])
      res.tab[i, 7] <- which.max(outt[[i]][, j])
      res.tab[i, 8] <- min(outt[[i]][, j])
      res.tab[i, 9] <- which.min(outt[[i]][, j])
      res.tab[i, 10] <- max(inn[[i]][, j] - outt[[i]][, j])
      res.tab[i, 11] <- which.max(inn[[i]][, j] - outt[[i]][, j]) 
    }
  }
  
  res.tab[11, 5] <- sum(res.tab[, 5])
  res.tab[12, 5] <- mean(res.tab[1:10,5])
  res.tab[11, 2] <- sum(res.tab[, 2])
  res.tab[12, 2] <- mean(res.tab[1:10,2])
  res.tab[11, 3] <- sum(res.tab[, 3])
  res.tab[12, 3] <- mean(res.tab[1:10,3])
  res.tab[11, 4] <- sum(res.tab[, 4])
  res.tab[12, 4] <- mean(res.tab[1:10,4])
  res.tab[11, 1] <- sum(res.tab[, 2])
  res.tab[12, 1] <- mean(res.tab[1:10,1])
  
  res.tab$'Магазины' <- c("Магазин 1","Магазин 2","Магазин 3","Магазин 4","Магазин 5","Магазин 6","Магазин 7","Магазин 8","Магазин 9","Магазин 10","Итого","Среднее")
  
  #write.table(res.tab, file = "C:/Кизяков Дмитрий/Скрипты/аналитика.csv", sep = ";", row.names = FALSE)
  
#-----------один магазин один товар-------------  
  
  tabs1 = list()
  for (vol in 1:length(specific)){
    rev1 <- sum(outt[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]]) * Price[2, ][[specific[[1]][["good"]]]]
    profit1 <- sum(outt[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]]) * Price[2, ][[specific[[1]][["good"]]]] - sum(inn[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]]) * Price[1, ][[specific[[1]][["good"]]]] - (sum(inn[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]]) - sum(outt[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]])) * Price[3, ][[specific[[1]][["good"]]]]
    spec.tab <- data.frame("Выручка руб"=rev1, "Прибыль руб"=profit1)             
    spec.tab$'Реализация, конт.' <- sum(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]])
    spec.tab$'Списание, конт.' <- sum(inn[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]]) - sum(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]])
    spec.tab$'sd' <- round(sd(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]]), 3)
    spec.tab$'Продажи макс' <- max(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]])
    spec.tab$День <- which.max(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]])
    spec.tab$'Продажи мин' <- min(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]])
    spec.tab$' День' <- which.min(outt[[specific[[vol]][["shop"]]]][[specific[[1]][["good"]]]])
    spec.tab$'Списание макс' <- max(inn[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]] - outt[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]])
    spec.tab$'  День' <- which.max(inn[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]] - outt[[specific[[vol]][["shop"]]]][,specific[[1]][["good"]]])
    
    tabs1[[vol]] <- spec.tab
    rm(spec.tab)
  }
  
#-----------------один магазин несколько товаров------------
  
  tabs2 = list()
  for (vol in 1:length(spec_shop)){
    coll <- rep(0, ncol(outt[[spec_shop[[vol]]]]) - 1)
    sp_sh.tab <- data.frame("Выручка руб"=coll, "Прибыль руб"=coll)             
    sp_sh.tab$'Реализация, конт.' <- coll
    sp_sh.tab$'Списание, конт.' <- coll
    sp_sh.tab$'sd' <- coll
    sp_sh.tab$'Продажи макс' <- coll
    sp_sh.tab$День <- coll
    sp_sh.tab$'Продажи мин' <- coll
    sp_sh.tab$' День' <- coll
    sp_sh.tab$'Списание макс' <- coll
    sp_sh.tab$'  День' <- coll
    
    
    for (i in 2:ncol(outt[[spec_shop[[vol]]]])) {
      sp_sh.tab[i-1, 3] <- sum(outt[[spec_shop[[vol]]]][,i]) #done
      sp_sh.tab[i-1, 4] <- sum(outt[[spec_shop[[vol]]]][,i]) + sum(inn[[spec_shop[[vol]]]][,i]) #done
      nam <- colnames(x = inn[[spec_shop[[vol]]]])[i]
      TR <- sum(outt[[spec_shop[[vol]]]][,i]) * Price[2, ][[nam]]
      TC <- sum(inn[[spec_shop[[vol]]]][,i]) * Price[1, ][[nam]] + (sum(inn[[spec_shop[[vol]]]][,i]) - sum(outt[[spec_shop[[vol]]]][,i])) * Price[3, ][[nam]]
      sp_sh.tab[i-1, 1] <- sp_sh.tab[i-1, 1] + Price[2, ][[nam]] * sum(outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 2] <- sp_sh.tab[i-1, 2] + TR - TC
      sp_sh.tab[i-1, 5] <- sp_sh.tab[i-1, 5] + sd(outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 6] <- max(outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 7] <- which.max(outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 8] <- min(outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 9] <- which.min(outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 10] <- max(inn[[spec_shop[[vol]]]][, i] - outt[[spec_shop[[vol]]]][, i])
      sp_sh.tab[i-1, 11] <- which.max(inn[[spec_shop[[vol]]]][, i] - outt[[spec_shop[[vol]]]][, i]) 
      
    }
    row.names(sp_sh.tab) = colnames(inn[[spec_shop[[vol]]]])[2:ncol(inn[[spec_shop[[vol]]]])]
    tabs2[[vol]] <- sp_sh.tab
    rm(sp_sh.tab)
  }

  
  
  
  
  
#----------------один товар несколько магазинов------------
  
  
  tabs3 = list()
  for (j in 1:length(spec_good)){
    rev <- rep(0, 12)
    profit <- rep(0, length(rev))
    name = c("Магазин 1","Магазин 2","Магазин 3","Магазин 4","Магазин 5","Магазин 6","Магазин 7","Магазин 8","Магазин 9","Магазин 10","Итого","Среднее")
    sp_gd.tab <- data.frame("Выручка" = rev, "Прибыль" = profit, row.names = name)
    Implementing <-rep(0, nrow(sp_gd.tab))
    sp_gd.tab$Реализация <- Implementing
    write_off <-rep(0, nrow(sp_gd.tab))
    sp_gd.tab$Списание <- write_off
    uniformity <-rep(0, nrow(sp_gd.tab))
    sp_gd.tab$'sd' <- uniformity
    sale_max <-c(rep(0, nrow(sp_gd.tab) - 2), "", "")
    sp_gd.tab$'Продажи макс' <- sale_max
    day1 <-c(rep(0, nrow(sp_gd.tab) - 2), "", "")
    sp_gd.tab$День <- day1
    sale_min <-c(rep(0, nrow(sp_gd.tab) - 2), "", "")
    sp_gd.tab$'Продажи мин' <- sale_min
    day2 <-c(rep(0, nrow(sp_gd.tab) - 2), "", "")
    sp_gd.tab$' День' <- day2
    write_off_max <-c(rep(0, nrow(sp_gd.tab) - 2), "", "")
    sp_gd.tab$'Списание макс' <- write_off_max
    day3 <-c(rep(0, nrow(sp_gd.tab) - 2), "", "")
    sp_gd.tab$'  День' <- day3
    
    for (i in 1:length(outt)) {
      
      sp_gd.tab[i, 3] <- sum(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 4] <- sum(inn[[i]][, spec_good[[j]]]) - sum(outt[[i]][, spec_good[[j]]])
      TR <- sum(outt[[i]][,spec_good[[j]]]) * Price[2, ][[spec_good[[j]]]]
      TC <- sum(inn[[i]][,spec_good[[j]]]) * Price[1, ][[spec_good[[j]]]] + (sum(inn[[i]][,spec_good[[j]]]) - sum(outt[[i]][,spec_good[[j]]])) * Price[3, ][[spec_good[[j]]]]
      sp_gd.tab[i, 1] <- sp_gd.tab[i, 1] + Price[2, ][[spec_good[[j]]]] * sum(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 2] <- sp_gd.tab[i, 2] + TR - TC
      sp_gd.tab[i, 5] <- sp_gd.tab[i, 5] + sd(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 6] <- max(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 7] <- which.max(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 8] <- min(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 9] <- which.min(outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 10] <- max(inn[[i]][, spec_good[[j]]] - outt[[i]][, spec_good[[j]]])
      sp_gd.tab[i, 11] <- which.max(inn[[i]][, spec_good[[j]]] - outt[[i]][, spec_good[[j]]])
      
    }
    sp_gd.tab[11, 5] <- sum(sp_gd.tab[, 5])
    sp_gd.tab[12, 5] <- mean(sp_gd.tab[1:10,5])
    sp_gd.tab[11, 2] <- sum(sp_gd.tab[, 2])
    sp_gd.tab[12, 2] <- mean(sp_gd.tab[1:10,2])
    sp_gd.tab[11, 3] <- sum(sp_gd.tab[, 3])
    sp_gd.tab[12, 3] <- mean(sp_gd.tab[1:10,3])
    sp_gd.tab[11, 4] <- sum(sp_gd.tab[, 4])
    sp_gd.tab[12, 4] <- mean(sp_gd.tab[1:10,4])
    sp_gd.tab[11, 1] <- sum(sp_gd.tab[, 2])
    sp_gd.tab[12, 1] <- mean(sp_gd.tab[1:10,1])
    
    tabs3[[j]] <- sp_gd.tab
    rm(sp_gd.tab)
    
  }
  
}
res.tab

#---------------динамика продаж 1 магазин 1 товар----------

{
  k = 3
  k_name = "Товар1"
  gr1.1 = plot(
    outt[[k]][, 1], 
    outt[[k]][, k_name], 
    type = "o",
    main = paste0("Продажи ", k_name, " в магазине ", k),
    xlab = "День",
    ylab = "Единиц проданно",
    pch = 20, 
    col = "red",
    lwd = 2
  )
  abline(v = seq(1, 20, 1), col = "#66CCFF", lty = 3)
  abline(h = seq(50, 170, 10), col = "#66CCFF", lty = 3)
}

{
  k = 6
  k_name = "Товар4"
  gr1.2 = plot(
    outt[[k]][, 1], 
    outt[[k]][, k_name], 
    type = "o",
    main = paste0("Продажи ", k_name, " в магазине ", k),
    xlab = "День",
    ylab = "Единиц проданно",
    pch = 20, 
    col = "red",
    lwd = 2
  )
  abline(v = seq(1, 20, 1), col = "#66CCFF", lty = 3)
  abline(h = seq(50, 170, 10), col = "#66CCFF", lty = 3)
  #dev.copy(tiff, "ляляля.tif")
  #dev.off()
}

#---------------динамика продаж 1 магазин 1 товар----------
colour <- c("green", "blue", "yellow", "red", "pink")

{
  k = 2
  xrange <- range(outt[[k]][, 1])
  yrange <- range(outt[[k]][, 2]*Price[2, colnames(outt[[k]])[2]], outt[[k]][, ncol(outt[[k]])]*Price[2, colnames(outt[[k]])[ncol(outt[[k]])]])
  gr2.1 = plot(
    xrange, 
    yrange, 
    
    ,
    main = paste0("Продажи в магазине ", k),
    xlab = "День",
    ylab = "Единиц проданно"
  )
  abline(v = seq(1, 20, 1), col = "#66CCFF", lty = 3)
  abline(h = seq(15000, 40000, 5000), col = "#66CCFF", lty = 3)

  
  for (i in 2:ncol(outt[[k]])){
    print(1)
    points(outt[[k]][, 1], outt[[k]][, i]*Price[2, colnames(outt[[k]])[i]], pch=20, col=colour[i-1], lwd = 2)
    lines(outt[[k]][, 1], outt[[k]][, i]*Price[2, colnames(outt[[k]])[i]], pch=20, col=colour[i-1], lwd = 2)
  }
}
legend("topright", legend=spec_good,col = colour, pch=15)


{
  k = 3
  xrange <- range(outt[[k]][, 1])
  yrange <- range(outt[[k]][, 2]*Price[2, colnames(outt[[k]])[2]], outt[[k]][, ncol(outt[[k]])]*Price[2, colnames(outt[[k]])[ncol(outt[[k]])]])
  gr2.2 = plot(
    xrange, 
    yrange, 
    type = "n",
    main = paste0("Продажи в магазине ", k),
    xlab = "День",
    ylim = c(20000,65000),
    ylab = "Единиц проданно"
  )
  abline(v = seq(1, 20, 1), col = "#66CCFF", lty = 3)
  abline(h = seq(15000, 40000, 5000), col = "#66CCFF", lty = 3)
  
  
  for (i in 2:ncol(outt[[k]])){
    points(outt[[k]][, 1], outt[[k]][, i]*Price[2, colnames(outt[[k]])[i]], pch=20, col=colour[i-1], lwd = 2)
    lines(outt[[k]][, 1], outt[[k]][, i]*Price[2, colnames(outt[[k]])[i]], pch=20, col=colour[i-1], lwd = 2)
  }
}
legend("topright", legend=spec_good,col = colour, pch=15)


{
  gr3.1 <- barplot(
    tabs2[[1]][, 2], 
    names.arg = row.names(tabs2[[1]]), 
    col = c("#0099FF", "pink"),
    ylim = c(200000, 280000), 
    las = 3,
    main = paste0("Продажи в магазине ", spec_shop[[1]]),
    ylab = "Прибыль по товарам",
    xpd = F
  )
  
}


{
  gr3.2 <- barplot(
    tabs2[[2]][, 2], 
    names.arg = row.names(tabs2[[2]]), 
    col = c("#0099FF", "pink"),
    ylim = c(0,300000),
    las = 3,
    main = paste0("Продажи в магазине ", spec_shop[[2]]),
    ylab = "Прибыль по товарам"
  )
  
}

{
  gr4.1 <- barplot(
    tabs3[[1]][1:10, 2], 
    names.arg = row.names(tabs3[[1]])[1:10], 
    col = c("#0099FF", "pink"),
    ylim = c(0,300000),
    las = 3,
    main = paste0("Продажи " , spec_good[[1]], " во всех магазинах "),
    ylab = "Прибыль по товарам"
  )
  
}

{
  gr4.2 <- barplot(
    tabs3[[2]][1:10, 2], 
    names.arg = row.names(tabs3[[2]])[1:10], 
    col = c("#0099FF", "pink"),
    ylim = c(0,300000),
    las = 3,
    main = paste0("Продажи " , spec_good[[2]], " во всех магазинах "),
    ylab = "Прибыль по товарам"
  )
  
}


{
  lst <- list()
  lst2 <- c()
  for (i in 1:10){
    lst[[i*5 - 4]] <- tabs3[[1]][i, 2]
    lst[[i*5 - 3]] <- tabs3[[2]][i, 2]
    lst[[i*5 - 2]] <- tabs3[[3]][i, 2]
    lst[[i*5 - 1]] <- tabs3[[4]][i, 2]
    lst[[i*5]] <- tabs3[[5]][i, 2]
      
    lst2 <- c(lst2, " ")
    lst2 <- c(lst2, " ")
    lst2 <- c(lst2, row.names(tabs3[[2]])[i])
    lst2 <- c(lst2, " ")
    lst2 <- c(lst2, " ")
  }
  
  
  gr5.1 <- barplot(
    unlist(lst), 
    names.arg = lst2, 
    col = colour[1:5],
    las = 3,
    main = paste0("Прибыль по всем товарам во всех магазинах "),
    ylab = "Прибыль по товарам"
  )
  legend("bottomright", legend=spec_good,col = colour, pch=15)
  
}


{
  gr6.1 <- boxplot(tabs3[[1]][1:10, 2], tabs3[[2]][1:10, 2], tabs3[[3]][1:10, 2], tabs3[[4]][1:10, 2], tabs3[[5]][1:10, 2],
          col = colour,
          names.arg = c(2, 3, 5, 6),
          main = paste0("Прибыль по всем товарам во всех магазинах "),
          ylab = "Прибыль по товарам"
          )
  legend("bottomleft", legend=spec_good,col = colour, pch=15)
}




