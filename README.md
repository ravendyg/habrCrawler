# habrCrawler
Собирает новости с хабра, гиктаймс и мегамозга в .csv файл. По умолчанию в 7й винде валится в Документы.

Для отслеживания авторов установить (стр. 99) 'traceAuthors = TRUE' - для "белого" списка, 'markAuthorsNegative = TRUE' - для "чёрного" списка

Написано на R, потому что он оказался под рукой, плюс реализация довольно тормозная — точно не подумают, что это DDoS )
Просматривает все страницы, на которых есть новости за последние dayShift дней. Если язык системы не русский, надо подлатать вектор monthConvert.

Большая часть написана уже и не помню когда, на исправление индусского кода жалко времени.

## Использование
Настроить пути:
```
where = "" # путь до папки с файлами
outFile = "" # файл для записи
authorsListSrc = "" # файл со списком авторов, статьи которых надо отслеживать
					# выделяются '@@@ ' перед заголовком
traceAuthors = TRUE # включить отслеживание авторов
markAuthorsNegative = TRUE
```
Запуск парсера:
```
readHabr("http://habrahabr.ru/all/")
readHabr("http://geektimes.ru/hub/google/", startFrom = 1, limitNumber = 20, dayShift = 107)
# startFrom - начать со страницы (по умолчанию 1)
# limitNumber - ограничить количество страниц (по умолчанию выключено)
# dayShift - смотреть за последние n дней (по умолчанию 7, 0 - отключить)
# stopAt - закончить на странице (по умолчанию отключено)
```
Работает со всеми ресурсами на хабре, гиктаймс, мегамозге, имеющими форму "ресурс/page{n}"

## Зависимости
Linux (Под Windows не тестировал)
* RCurl требует установки libcurl4-gnutls-dev (http://askubuntu.com/questions/359267/cannot-find-curl-config-in-ubuntu-13-04)
* XML требует libxml2-dev (https://stackoverflow.com/questions/19904083/how-to-fix-error-with-xml2-config-not-found-when-installing-php-from-sources)

## Где работало
* Mint
* Debian Jessie
* Debian Stretch
* BunsenLabs Hydrogen
