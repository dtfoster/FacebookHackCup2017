


source('./funcs.R')
library(data.table)

inFilePath = './data/progress_pie.txt'
outFilePath = './submission/progress_pie_submission.txt'

processFile(inFilePath, outFilePath)