########################################################################
# データの基本的な属性を出す
########################################################################
df <- read.csv("all-base.csv")
属性表 <- data.frame()
#
属性   <- "教員数"
値     <- paste(length(levels(df$教員ID)), "名")
備考   <- "実務家教員"
属性表 <- rbind(属性表,data.frame(属性,値,備考))
#
属性   <- "学生数"
値     <- paste(length(levels(df$学生ID)), "名")
備考   <- "約8割が社会人"
属性表 <- rbind(属性表,data.frame(属性,値,備考))
#
属性   <- "評価表の数"
値     <- paste(length(levels(df$タイトル)), "枚")
備考   <- "3年×4Q"
属性表 <- rbind(属性表,data.frame(属性,値,備考))
#
属性   <- "評価対象数"
値     <- paste(nrow(df), "名")
備考   <- "評価した学生数"
属性表 <- rbind(属性表,data.frame(属性,値,備考))
#
属性   <- "合計文字数"
値     <- paste(sum(nchar(as.character(df$総合.講評.))),"字")
備考   <- "総合評価に記述された文字数"
属性表 <- rbind(属性表,data.frame(属性,値,備考))
#
属性   <- "平均文字数"
#値     <- paste(sprintf("%.1f", sum(nchar(as.character(df$総合.講評.))) / nrow(df)),"字")
ave <- sprintf("%.1f", sum(nchar(as.character(df$総合.講評.))) / nrow(df))
値     <- paste(ave,"字")
備考   <- "合計文字数 $\\div$ 評価対象数"
属性表 <- rbind(属性表,data.frame(属性,値,備考))
#
属性表
data.matrix(属性表)
latex.default(属性表, file = "table/attributes.tex",
              title="No",
              caption="分析対象の評価表の属性",
              label="tab:属性表",
              booktabs = T) # , dcolumn = T
########################################################################


########################################################################
# 未知語の頻度
########################################################################
未知語 <- read.table("michigo.txt")
library("RMeCab")
頻度 <- RMeCabFreq("all-講評.txt")
未知語頻度 <- 頻度[頻度$Term %in% 未知語$V1,]
未知語頻度 <- 未知語頻度[order(未知語頻度$Freq, decreasing=TRUE),]
未知語頻度結果 <- 未知語頻度[未知語頻度$Freq >= 4,]
未知語頻度結果[未知語頻度結果$Term == "R&D",]$Term <- "R\\&D" # LaTeXのためにエスケープ
latex.default(未知語頻度結果, file = "table/michigo_frequency.tex",
              title="ID",
              caption="未知語として判定された単語の出現頻度（4回以上）",
              label="tab:未知語頻度結果",
              booktabs = T) # , dcolumn = T
########################################################################


########################################################################
# RMeCabの頻度表機能を使い，品詞の種別を解析する
########################################################################
source("function.R")
品詞種別グラフ("figure/hinshi.pdf") # 全ての品詞の割合
品詞種別グラフ("figure/meishi.pdf", "名詞")
品詞種別グラフ("figure/doushi.pdf", "動詞", height = 2.5)
品詞種別グラフ("figure/fukushi.pdf", "副詞")
品詞種別グラフ("figure/keiyoushi.pdf", "形容詞", height = 2.5)
########################################################################


########################################################################
# カタカナ語など用語のゆらぎを補正する関数のテスト
########################################################################
# テスト
source("function.R")
頻度 = RMeCabFreq("all-講評.txt")
頻度[頻度$Term == "レビュー",]
頻度[頻度$Term == "レビュ",]
頻度 <- 補正頻度(頻度) # カタカナ語などのゆらぎを補正します．
頻度[頻度$Term == "レビュー",]
頻度[頻度$Term == "レビュ",]


########################################################################
# サ変名詞の頻度表（グラフとテーブル）を作成します
########################################################################
library(Hmisc) # install.packages("Hmisc") - LaTeXの表
options(latexcmd='platex')
options(xdvicmd='pxdvi') # 別途pxdviのインストールが必要

source("function.R")
頻度表 <- 品詞別頻度("figure/meishi_sahen_50.pdf", "名詞", 50, info2 = "サ変接続")
latex.default(頻度表, file = "table/meishi_sahen_50.tex",
              title="ID",
              caption="サ変接続名詞の出現頻度(50回以上)",
              label="tab:サ変接続名詞",
              booktabs = T) # , dcolumn = T


########################################################################
# 動詞の頻度表（グラフとテーブル）を作成します
########################################################################
library(Hmisc) # install.packages("Hmisc") - LaTeXの表
options(latexcmd='platex')
options(xdvicmd='pxdvi') # 別途pxdviのインストールが必要

source("function.R")
頻度表 <- 品詞別頻度("figure/doushi_15.pdf", "動詞", 15, info2 = "自立")
latex.default(頻度表, file = "table/doushi_15.tex",
              title="ID",
              caption="自立動詞の出現頻度(15回以上)",
              label="tab:自立動詞",
              booktabs = T) # , dcolumn = T


########################################################################
# 副詞の頻度表（グラフとテーブル）を作成します
########################################################################
library(Hmisc) # install.packages("Hmisc") - LaTeXの表
options(latexcmd='platex')
options(xdvicmd='pxdvi') # 別途pxdviのインストールが必要

source("function.R")
頻度表 <- 品詞別頻度("figure/fukushi_5.pdf", "副詞", 5)
latex.default(頻度表, file = "table/fukushi_5.tex",
              title="ID",
              caption="副詞の出現頻度(5回以上)",
              label="tab:副詞",
              booktabs = T) # , dcolumn = T


########################################################################
# 形容詞の頻度表（グラフとテーブル）を作成します
########################################################################
library(Hmisc) # install.packages("Hmisc") - LaTeXの表
options(latexcmd='platex')
options(xdvicmd='pxdvi') # 別途pxdviのインストールが必要

source("function.R")
頻度表 <- 品詞別頻度("figure/keiyoushi_5.pdf", "形容詞", 5)
latex.default(頻度表, file = "table/keiyoushi_5.tex",
              title="ID",
              caption="形容詞の出現頻度(5回以上)",
              label="tab:形容詞",
              booktabs = T) # , dcolumn = T
########################################################################


########################################################################
########################################################################
## 以下，本文では使用していません
########################################################################
########################################################################


########################################################################
# RMeCabの頻度表機能を使い，一般名詞の度数分布を解析する
# 出力：一般名詞の出現頻度の度数分布表
########################################################################
library("RMeCab")
頻度 = RMeCabFreq("all-講評.txt")
品詞 = 頻度[頻度$Info1 == "名詞" & 頻度$Info2 == "一般",] # 一般名詞のみ抽出する
品詞 = 品詞[order(品詞$Freq, decreasing = TRUE),]
# 度数分布を作る
source("http://aoki2.si.gunma-u.ac.jp/R/src/dosuu_bunpu.R", encoding="euc-jp")
度数分布 = dosuu.bunpu(品詞$Freq, 3, percent=TRUE)
度数分布[,2] <- round(度数分布[,2],2)
度数分布[,3] <- round(度数分布[,3],2)
latex.default(head(度数分布,5), file = "table/noum_frequency.tex",
              title="range",
              caption="一般名詞の出現頻度の度数分布(15回以上は省略)",
              label="tab:名詞の度数分布",
              booktabs = T) # , dcolumn = T
########################################################################


########################################################################
# 総合点のサマリを表示する
########################################################################
df <- read.csv("all-base.csv")
cat("総合点のサマリ\n");summary(df$総合.点数.)
cat("総合点のサマリ（0以外）\n");summary(df$総合.点数.[df$総合.点数. > 0])
cat("講評の文字数のサマリ\n");summary(nchar(as.character(df$総合.講評.)))
cat("講評の文字数のサマリ（0以外）\n");summary(nchar(as.character(df$総合.講評.[nchar(as.character(df$総合.講評.))>0])))
########################################################################


########################################################################
# 共起を色々と調べる
########################################################################

source("function.R")
共起T("質", 3)
共起T("量", 3)
共起T("活動", 3)
共起T("成果", 3)

共起T("高い", 5)
共起T("ない", 5)
共起T("多い", 5)
共起T("少ない", 5)
共起T("大きい", 5)
共起T("良い", 5)
共起T("新しい", 5)
共起T("無い", 5)
共起T("弱い", 5)
共起T("欲しい", 5)
共起T("よい", 5)
共起T("うまい", 5)
共起T("ほしい", 5)

共起MI("質", 3)
共起MI("量", 3)
共起MI("成果", 3)
共起MI("活動", 3)
