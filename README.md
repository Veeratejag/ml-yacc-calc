# ml-yacc-calc
Calculator using ml-yacc

Walkthrough from Wellesley College:
http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf


use link to download sml
https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=ved=2ahUKEwj_iZGZurj-AhW82TgGHQGNB2sQFnoECBwQAQurl=https%3A%2F%2Fwww.smlnj.org%2Fdist%2Fworking%2F110.98.1%2Finstall.html&usg=AOvVaw1b0qYS3LH5zSqVOU4Qn1BT

expr = term | term ("+" | "-") term
term = factor | factor ("*" | "/" | "%") factor
factor = number