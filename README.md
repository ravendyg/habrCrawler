# habrCrawler
Собирает новости с хабра, гиктаймс и мегамозга в .csv файл. По умолчанию в 7й винде валится в Документы.

Отслеживание авторов по умолчанию выключено, при необходимости исправить стр. 20 на 'traceAuthors = TRUE'

Написано на R, потому что он оказался под рукой, плюс реализация довольно тормозная — точно не подумают, что это DDoS )
Просматривает все страницы, на которых есть новости за последние dayShift дней. Если язык системы не русский, надо подлатать вектор monthConvert.

Обработка новостей в каждом цикле (для каждого из сайтов) прописана отдельно, хотя код тот же самый, потому что одно время к сайтам приходилось обращаться по разному. Так с тех пор и осталось.

Большая часть написана уже и не помню когда, на исправление индусского кода жалко времени.
