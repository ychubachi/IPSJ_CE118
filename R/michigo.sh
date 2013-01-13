mecab --unk-feature "未知語" < all-講評.txt | grep 未知語 | LC_ALL=C sort | uniq -c | sort -n -r
