# Линии уровня

### Теория

**Нормальный дискриминантный анализ** - частный случай баесовской классификации,
когда предполагается, что плотности всех классов являются многомерными нормальными:

![](http://latex.codecogs.com/svg.latex?N%28x%3B%5Cmu%2C%5CSigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%282%5Cpi%29%5En%20%7C%5CSigma%7C%7D%7D%20%5Ccdot%20exp%5Cleft%28-%5Cfrac%7B1%7D%7B2%7D%28x%20-%20%5Cmu%29%5ET%20%5CSigma%5E%7B-1%7D%20%28x%20-%20%5Cmu%29%5Cright%29),

где ![](http://latex.codecogs.com/svg.latex?x%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– объект выборки, имеющий *n* признаков,
![](http://latex.codecogs.com/svg.latex?%5Cmu%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– математическое ожидание (центр выборки),
![](http://latex.codecogs.com/svg.latex?%5CSigma%20%5Cin%20%5Cmathbb%7BR%7D%5E%7Bn%20%5Ctimes%20n%7D)
– ковариационная матрица (симметричная, невырожденная, положительно
определенная).

### Программная реализация

Исходный код программы: [Levels/server.R](../Levels/server.R)

Программа на основе математического ожидания и ковариационной матрицы
изображает распределение линий уровня с вероятностями от 0,001 до 0,2.

Ковариационная матрица влияет на наклон, а также растяжение линий уровня по осям
координат. Вектор мат. ожидания влияет на расположение центра линий уровня.

### Результат

Программа реализована с помощью библиотеки *shiny*, которая реализует
интерфейс для задания параметров. Так что параметры пользователю
предлагается выбрать самому.

Программа доступна по
[ссылке](https://dmitriypenetrator.shinyapps.io/levels/)

Некоторые примеры:

1) Если признаки _некоррелированы_, линии уровня плотности распределения
имеют форму элипсоидов с центром в точке
![](http://latex.codecogs.com/svg.latex?%5Cmu)
и параллельны осям координат.

![](pict/levels2.png)

2) Если признаки имеют одинаковые дисперсии, то элипсоиды являются сферами

![](pict/levels1.png)

3) Чем больше значение матрицы ковариации, тем больше растяжение и класс становится
менее плотным.

![](pict/levels3.png)

----

[Вернуться в меню](../../README.md)

[Перейти к подстановочному алгоритму](plug-in.md)

