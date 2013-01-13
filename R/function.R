########################################################################
# ライブラリのロード
########################################################################
# LaTeX表出力
# install.packages("Hmisc") # LaTeXの表
library(Hmisc)
options(latexcmd='platex')
options(xdvicmd='pxdvi') # 別途pxdviのインストールが必要
# RMeCab
library("RMeCab")
########################################################################


########################################################################
# 品詞の種別をグラフにします
########################################################################
品詞種別グラフ <- function(file, info1 = "", height = 0) {
  library("RMeCab")
  頻度 = RMeCabFreq("all-講評.txt")
  if(info1 != "") {
    品詞 = 頻度[頻度$Info1 == info1,] # 指定された品詞のみを取り出す
    水準 = levels(factor(品詞$Info2)) # その品詞の種別を取り出す
  } else {
    品詞 = 頻度
    水準 = levels(factor(品詞$Info1)) # 品詞を取り出す
  }
  品詞種別 = c(); 品詞数 = c()
  for(i in 1:length(水準)) {
    if(info1 != "") {
      頻度数 = nrow(頻度[頻度$Info2 == 水準[i],])
    } else {
      頻度数 = nrow(頻度[頻度$Info1 == 水準[i],])
    }
    品詞種別 = c(品詞種別,
      sprintf("%s(%d)", 水準[i], 頻度数 ))
    品詞数 = c(品詞数, 頻度数)
  }
  品詞 = data.frame(品詞種別,品詞数)
  品詞 = 品詞[order(品詞$品詞数, decreasing = FALSE),]
  
  # グラフを作成します
  # http://oku.edu.mie-u.ac.jp/~okumura/stat/090503a.html
  if(height > 0) {
    X11(height=height) # A4横
  }
  oldpar <- par()        # グラフィックパラメータを保存
  on.exit(par(oldpar))   # 関数終了時に復帰
  on.exit(dev.off())
  par(las = 1)
  par(mar = c(4, 8, 0, 2) + 0.1) # c(5,4,4,2)+0.1 bottom,left,top,right
  barplot(品詞$品詞数, names.arg = 品詞$品詞種別, horiz = TRUE)
  # barplot(品詞数, col = col)
  dev.copy2pdf(file=file,family="Japan1")
}

########################################################################
# 頻度順に品詞をグラフにする
# info1 - 品詞
# info2 - 細分類
########################################################################
品詞別頻度 <- function(file, info1, freq.min, info2 = "", freq.max = 0) {
  on.exit(dev.off())
  library("RMeCab")

  # 指定された品詞の頻度を求めます
  頻度 = RMeCabFreq("all-講評.txt")
  # # 頻度 <- 補正頻度(頻度) # カタカナ語などのゆらぎを補正します．
  if(info2 == "") {
    # 品詞にのみ注目する場合
    品詞 = 頻度[頻度$Info1 == info1,]
  } else {
    # 品詞の細分類にも注目する場合
    品詞 = 頻度[頻度$Info1 == info1 & 頻度$Info2 == info2,]
  }
  品詞 = 品詞[order(品詞$Freq, decreasing = TRUE),] # ソートします
  if(freq.max > 0) { # 範囲を指定
    頻出語 = 品詞[freq.min <= 品詞$Freq & 品詞$Freq < freq.max,] # 上限あり
  } else {
    頻出語 = 品詞[freq.min <= 品詞$Freq,] # 上限なし
  }    
  # グラフを描画します
  X11(x=0,y=0,width=29.7/2.54,height=21.0/2.54) # A4横
  ## タイトルを作成します
  main <- info1
  if(info2 != "") { # 細分類がある場合
    main <- paste(main, "/", info2)
  }
  main <- paste(main, "(", freq.min, "回以上", sep = "")
  if(freq.max > 0) {
    main <- paste(main, "，", freq.max, "回未満", sep = "")
  }
  main <- paste(main, ")", sep = "")
  ## グラフを用意します
  matplot(1:nrow(頻出語), 頻出語[,4], type = "n", main=main, xlab="単語", ylab="頻度")
  ## グレイスケールを用意します
  # 色 <- rainbow(nrow(頻出語))
  色 <- gray.colors(nrow(頻出語), gamma = 2.2)
  for(i in 1:nrow(頻出語)){
    matlines(i, 頻出語[i,4], type = "h", col = 色[i],  lwd =5)
  }
  ## 凡例を表示します
  legend("topright", legend = 頻出語$Term, col = 色, lwd = 5,
         ncol = 4, cex = 1.30)
  dev.copy2pdf(file=file, width=14,family="Japan1")

  # 頻度表を返します
  return(頻出語)
}
########################################################################


########################################################################
# コロケーション
# T 一般に,t スコアは 2 以上の場合に意味のある組み合わせであるとされる
# MI 相互情報量では,頻度は低いが特殊な結びつきをしているコロケーションがうまく検出できる
########################################################################
共起T <- function(node, span) {
  library("RMeCab")
  kekka <- collocate(file = "all-講評.txt", node  = node, span = span)
  cscore <- collScores(kekka, node, span = span)
  # naを取り除く
  cscore <- na.omit(cscore)
  # 句読点を取り除く
  cscore <- cscore[!(cscore$Term %in% c("．", "，", "。","、")),]
  # Tスコアの降順にする
  cscore = cscore[order(cscore$T, decreasing = TRUE),]
  # 2以下のものを除く
  return(cscore[cscore$T > 1.6,])
}

共起MI <- function(node, span) {
  library("RMeCab")
  kekka <- collocate(file = "all-講評.txt", node  = node, span = span)
  cscore <- collScores(kekka, node, span = span)
  # naを取り除く
  cscore <- na.omit(cscore)
  # 句読点を取り除く
  cscore <- cscore[!(cscore$Term %in% c("．", "，", "。","、")),]
  # MIの降順にする
  cscore = cscore[order(cscore$MI, decreasing = TRUE),]
  # 上位20を取り出す
  return(head(cscore,20))
}
########################################################################


########################################################################
# カタカナ語のゆらぎを補正します（テスト用）
########################################################################
補正頻度 <- function(頻度) {
  変換表 <- read.csv("henkan.csv")
  変換前 <- 変換表$変換前
  変換後 <- 変換表$変換後
  for(i in 1:nrow(変換表)) {
    # 注意：同じ語が異なる品詞として解析されていた場合，誤動作します．
    変換前頻度 <- 頻度[頻度$Term == 変換前[i],]
    if(nrow(変換前頻度) > 1) {
      cat(str(変換前頻度))
      stop("変換前の頻度が複数あります")
    }
    変換後頻度 <- 頻度[頻度$Term == 変換後[i],]
    if(nrow(変換後頻度) > 1) {
      cat(str(変換後頻度))
      stop("変換後の頻度が複数あります")
    }
    if(変換後頻度$Info1 == 変換前頻度$Info1 &
       変換後頻度$Info2 == 変換前頻度$Info2) {
      # 同じ素性の場合，1つに足しあわせます
      変換後頻度$Freq <- 変換後頻度$Freq + 変換前頻度$Freq
      頻度 <- 頻度[頻度$Term != 変換前[i],] # 行の削除
    } else {
      cat("変換前 = ", 頻度[頻度$Term == 変換前[i],]$Term, "\n")
      cat("変換後 = ", as.character(変換後[i]), "\n")
      頻度[頻度$Term == 変換前[i],]$Term <- as.character(変換後[i])
      warning("変換後と変換前の品詞が異なっています．\n")
    }
  }
  return(頻度)
}
