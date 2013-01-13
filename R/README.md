PBL成績評価分析
===============

# RMeCabの読み込み

    > library("RMeCab")
    > RMeCabC("ここではきものをぬいでください")

# Excelとの接続

http://cran.r-project.org/web/packages/XLConnect/index.html
http://cran.r-project.org/web/packages/XLConnect/XLConnect.pdf

次でインストール

    > install.packages("XLConnect")

# 評価ファイルの読み込み

    > library("XLConnect")
	> wb = loadWorkbook("evaluation.xls")
	> getSheets(wb)
	> data <- readWorksheet(wb, sheet = "Q1 PBL評価（総合）")
	> head(data)
	> data["学生名"])
	> data$学生名
	> data[,3]

dataはdata.frameになる．

# RMeCabDF

    > RMeCabDF
    function (dataf, coln, mypref = 0, dic = "", mecabrc = "", etc = "") 
    {
        if (!is.data.frame(dataf)) {
            stop("the first argument must be a data frame!")
        }
        kekka <- list(length(dataf[, coln]))
        for (i in 1:length(dataf[, coln])) {
            if (!is.factor(dataf[i, coln]) || is.na(dataf[i, coln]) || 
                dataf[i, coln] == "") {
                kekka[[i]] <- NA
            }
            else {
                kekka[[i]] <- unlist(RMeCabC(dataf[i, coln], mypref, 
                    dic, mecabrc, etc))
            }
        }
        return(kekka)
    }
    <environment: namespace:RMeCab>
	

    > is.factor(data[1,"コメント"])
    [1] FALSE

  これが原因っぽい．
	
	> dat <- read.csv("photo.csv", head = T)
	> dat
	  ID Sex              Reply
	1  1   F   写真とってくれよ
	2  2   M 写真とってください
	3  3   F       写真とってね
	4  4   F 写真とってください
	5  5   M     写真とってっす
	> dat[1,"Reply"]
	[1] 写真とってくれよ
	Levels: 写真とってね 写真とってっす 写真とってくれよ 写真とってください
	> is.factor(dat[1,"Reply"])
	[1] TRUE
  
  read.csvだとfactorになるのね．

    > data2 = data.frame(lapply(data, factor))
    > kekka = RMeCabDF(data2, "コメント")
    > kekka

  めでたく，解析完了．
  
  kekkaの構造はstructureってのでできているっぽい．
  なんだそりゃ？
  http://stackoverflow.com/questions/12565733/how-to-extract-numeric-values-from-a-structure-object-in-r
  
    > unname(kekka[[1]])

  なるほど．
  
# 頻度情報を解析したい

  全てのコメントに出てくる単語の頻度を解析したい
  ファイルから頻度表を作成するにはRMeCabFreqを使う．本体はCで実装してるぽい．
  これを利用するためにファイルに書きだしてあげるのが得策か．
  
  http://cse.naro.affrc.go.jp/takezawa/r-tips/r/45.html
  
    > write.table(data$コメント, "comment.txt")
	> RMeCabFreq("comment.txt")
    > kekka = collocate("comment.txt", node="能力", span = 3)
    > collScores(kekka, node = "能力", span = 3)
	
  品詞毎の頻度表もつくりたいね．

# 正規化・ID化

  Sheetの名前を正規化したい．
  学生や教員の名前をIDにしたい．

  Calcでやるか，Rでやるか・・・．
  ここは勉強のためRでやる．

  emacs R モード・・・．なんだか機能がありすぎでめんどい．
  R Studioを試してみよう．やっぱり，キーバインドが orz
  なんか，常時バッファが作成されるようだが，ESSでやってみよう．
  
# 解析対象

  2009年から2011年(3年間分)までが同じスキームなので，これらを分析対象とする．
  素点と総合については・・・両方．
  分量は？
  
  総合について，成績を考慮したい．
  
  
	
	
