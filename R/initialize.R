# エクセル連携のための前処理
install.packages("XLConnect")

# ライブラリ
library("XLConnect")

# エクセルのシートをcsvに切り出す
wb = loadWorkbook("evaluation.xls")
# シートの名前リストを取得します．
sheets = getSheets(wb)
# 2009-2011年度の総合だけを取得し，CSVに書き出します．
sheets.target <- sheets[grep("情報20(09|10|11).*総合", sheets)]
for(i in 1:length(sheets.target)) {
  sheet <- sheets.target[i]
  cat(paste(sheet, "\n")) # シート名を表示
  # シートデータの整理
  ws <- readWorksheet(wb, sheet = sheet)
  ws[, "タイトル"] = sheet # タイトルをシートの名前に合わせます
  ws <- ws[-nrow(ws),] # 最終行がNAなので削除します
  fn <- paste("sheets", paste(sheet, "csv", sep = "."), sep = "/")
  cat(paste(fn, "\n"))
  write.csv(ws, fn)
}

##############################################################################
# 全てのファイル名のリストを得てCSVで読込し，結合する
# ここではただ単に結合します．
fns = paste("sheets/", dir("sheets"), sep = "")
df.all = data.frame() # 結合するための新しいDF
for(i in 1:length(fns)) {
  # cat(paste(fns[i], "\n"))
  ws <- read.csv(fns[i])
  df.all <- rbind(df.all, ws) # DFのrowに追加する
}
write.csv(df.all, "all.csv")

########################################################################
# DANGER
########################################################################
df.all = read.csv("all.csv")

# 学生名と作成者の一覧を作り，ランダムに並び替える
students = levels(df.all$学生名)
rnd.students = students[sample.int(length(students))]
df.students = data.frame(学生名 = rnd.students, RANDID = paste("S", 1:length(students), sep=""))
write.csv(df.students, "rand_students.csv")

# 教員名をランダムに並び替える
teachers = levels(df.all$作成者)
rnd.teachers = teachers[sample.int(length(teachers))]
df.teachers = data.frame(教員名 = rnd.teachers, RANDID = paste("T", 1:length(teachers), sep=""))
write.csv(df.teachers, "rand_teachers.csv")

########################################################################
#df.teachers = read.csv("rand_teachers.csv",header=TRUE)
df.teachers = read.csv("rand_teachers.csv")
df.teachers
df.students = read.csv("rand_students.csv")
df.students
df.all = read.csv("all.csv")
names(df.all)

# 学生名・教員名をランダムなIDに置換します．
df.all$学生ID <- factor(df.all$学生名, level=df.students$学生名, label=df.students$RANDID)
df.all$教員ID <- factor(df.all$作成者, level=df.teachers$教員名, label=df.teachers$RANDID)
names(df.all)
write.csv(df.all, "all-id.csv")

########################################################################
# 必要な列のみ取り出す
########################################################################
df.all.id = read.csv("all-id.csv")
names(df.all.id)
df.all.base = df.all.id[,c("タイトル","学生ID","教員ID","総合.点数.","総合.講評.")]
names(df.all.base)
write.csv(df.all.base, "all-base.csv")

########################################################################
# 講評の記述のみをテキストにする
########################################################################
library("Nippon")
df.all.base <- read.csv("all-base.csv")
class(df.all.base$総合.講評.)
write(zen2han(levels(df.all.base$総合.講評.)),"all-講評.txt")

