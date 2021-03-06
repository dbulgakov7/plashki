Программа "Плашки", версия 1.0.2
================================

Предназначение
--------------

Программа предназначена для проведения эксперимента по реакции испытуемого на значимое событие.

Порядок использования
---------------------

1. Помещаем все файлы из архива в одну директорию.
2. Запускаем программу plashki.exe, нажимаем кнопку "Начать", выбираем параметры эксперимента, нажимаем кнопку "Старт".
3. Перед началом эксперимента появится окно "Эксперимент начинается.". Целевая плашка будет выбрана
   произвольным образом и будет обведена в рамку. После того, как испытуемый будет готов нужно нажать "ОК".
4. Начнется эксперимент. Плашки будут загораться в произвольной последовательности, которая формируется заново
   после нажатия на кнопку "Старт". Целевая плашка может загораться 2 и более раз подряд.
5. По окончании эксперимента появится окно "Эксперимент закончен."
6. При выходе из программы конфигурация эксперимента будет сохранена в конфигурационный файл plashki.ini
   и будет использована при следующем старте программы.
7. Информация о параметрах эксперимента, времени и последовательности мигания плашек сохранится в файл
   plashki.db3, представляющий собой файл локальной СУБД SQLite 3. Время сохраняется с точностью до миллисекунды.
8. Для выгрузки информации из plashki.db3 в файлы с разделителями необходимо запустить exp2csv.bat -
   будут созданы файлы seans.csv и event.csv, которые можно импортировать, например, в Microsoft Excel.
9. Перед стартом эксперимента можно указывать сколько будет использовано плашек - 6 или 8,
   длительность паузы (никакая плашка не горит), длительность горения активной плашки,
   сколько раз должна загореться каждая из плашек. Значимым событием считается загорание целевой плашки.
   Целевая плашка - та, которая обведена жирной зеленой рамкой.
   Цвета основного окна-формы, пассивной плашки (когда она не горит), активной плашки (когда она горит),
   цвет рамки целевой плашки, толщину рамки целевой плашки можно задать в plashki.ini до запуска plashki.exe.
   Посла старта эксперимента, до того, как загорится первая плашка будет выдержана увеличенная пауза равная
   "Пауза между миганиями + время подсветки активной плашки", чтобы испытуемый лучше приготовился к эксперименту.

Файлы, входящие в дистрибутив
-----------------------------

- plashki.exe - исполняемый файл программы.
- plashki.ini - конфигурация программы и параметры последнего эксперимента.
- sqlite3.dll - библиотека, необходимая для работы с СУБД SQLite 3.
- README.txt  - данный файл.
- LICENSE     - информация о лицензии.
- plashki.db3 - информацией о проведенных экспериментах (SQLite 3). Будет создан при первом запуске plashki.exe
- exp2csv.bat - выгрузить информацию из plashki.db3 в файлы seans.csv, event.csv
- sqlite3.exe - консольная утилита работы с СУБД SQLite 3 - необходима для работы exp2csv.bat

Коды цветов в Delphi
--------------------

Эти коды цветов используются в plashki.ini

Для справки [https://kslift.ru/nastroyka-tsveta-koda-delphi]

- clSilver 12632256
- clGray 8421504
- clWhite 16777215
- clGreen 32768

Лицензия
--------

Программа разработана на языке программирования Free Pascal в среде разработки с открытым исходным кодом
Lazarus 2.2.0 [https://www.lazarus-ide.org/] и распространяется на условиях лицензии MIT,
которая позволяет использовать программу в любых целях и модифицировать её исходный код.

--  
Дмитрий Булгаков <dbulgakov7@yandex.ru>, Ярослава Булгакова <bulgakova_ya_v@staff.sechenov.ru>,
Ярослав Туровский <yaroslav_turovsk@mail.ru>

27.04.2022
