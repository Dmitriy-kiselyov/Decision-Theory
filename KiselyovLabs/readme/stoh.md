# Стохастический градиент

__Метод стахостического градиента__ используется для минимизации эмпирического
риска
![](http://latex.codecogs.com/svg.latex?Q%28w%2CX%5El%29)
в линейных классификаторах. Все линейные алгоритмы, рассматриваеные в этой
работе, основаны на нем, так что есть смысл разобрать принцип работы алгоритма 1 раз
и больше не повторяться.

### Теория

Алгоритм подбирает значения весов _w_ итеративным путем, на каждом шаге которого
веса сдвигаются в противоположную сторону вектора градиента
![](http://latex.codecogs.com/svg.latex?Q%27%28w%2CX%5El%29).
При чем вычисления градиента происходит каждый раз на случайном объекте из выборки.
Различные линейные алгоритмы между собой отличаются функцией потерь
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28%5Clangle%20w%2C%20x_i%20%5Crangle%20y_i%29)
, где
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2C%20x_i%20%5Crangle%20y_i)
– отступ.

### Алгоритм

На _вход_ алгоритма поступают обучающая выборка
![](http://latex.codecogs.com/svg.latex?X%5E%5Cell),
темп обучения
![](http://latex.codecogs.com/svg.latex?%5Ceta)
и параметр сглаживания
![](http://latex.codecogs.com/svg.latex?%5Clambda).

Существует много техник выбора
![](http://latex.codecogs.com/svg.latex?%5Ceta)
и
![](http://latex.codecogs.com/svg.latex?%5Clambda).
Мы же положим
![](http://latex.codecogs.com/svg.latex?%5Ceta%20%3D%20%5Cfrac%7B1%7D%7B5%7D)
и
![](http://latex.codecogs.com/svg.latex?%5Clambda%20%3D%20%5Cfrac%7B1%7D%7B%5Cell%7D).

Алгоиртм _возвращает_ вектор весов _w_.

Рассмотрим реализацию алгоритма пошагово:

1. Инициализируем веса
![](http://latex.codecogs.com/svg.latex?w_j%2C%20j%3D1%2C...%2Cn).
Не существует общего правила инициализации весов. Мы каждому
![](http://latex.codecogs.com/svg.latex?w_j)
присвоим значение 1/2.

2. Инициализировать начальную оценку
![](http://latex.codecogs.com/svg.latex?Q%20%3D%20%5Csum_%7Bi%20%3D%201%7D%5E%7B%5Cell%7D%20%5Cmathcal%7BL%7D%28%5Clangle%20w%2C%20x_i%20%5Crangle%20y_i%29)
3. Пока значени _Q_ не стабилизирутеся
![](http://latex.codecogs.com/svg.latex?%5Cleft%28%7CQ_%7Bprev%7D%20-%20Q%7C%20%3C%20%5Cfrac%7B1%7D%7B10%7D%5Cright%29)
 _повторять_:

    4. Выбрать случайный
    ![](http://latex.codecogs.com/svg.latex?x_i)
    из
    ![](http://latex.codecogs.com/svg.latex?X%5E%5Cell)
    случайным образом

    5. Вычислить ошибку:
    ![](http://latex.codecogs.com/svg.latex?%5Cvarepsilon%20_i%20%3D%20%5Cmathcal%7BL%7D%28%5Clangle%20w%2C%20x_i%20%5Crangle%20y_i%29)

    6. Сделать шаг градиентного спуска:
    ![](http://latex.codecogs.com/svg.latex?w%20%3D%20w%20-%20%5Ceta%20%5Cmathcal%7BL%7D%27%28%5Clangle%20w%2C%20x_i%20%5Crangle%20y_i%29x_iy_i)

    7. Оценить значение функционала:
    ![](http://latex.codecogs.com/svg.latex?Q%20%3D%20%281%20-%20%5Clambda%29Q%20&plus;%20%5Clambda%20%5Cvarepsilon_i)

### Примечание

Прежде, чем применять _метод стахостического градиента_, выборку
![](http://latex.codecogs.com/svg.latex?X%5E%5Cell)
необходимо _нормировать_ и _подготовить_.

__Нормирование__ (по каждому признаку)

![](http://latex.codecogs.com/svg.latex?f_j%20%3D%20%5Cfrac%7Bf_j%20-%20m%7D%7B%5Csigma%7D)
, где _m_ – среднее арифмитическое значение признака _j_,
![](http://latex.codecogs.com/svg.latex?%5Csigma)
– среднеквадратическое отклонение.

__Подготовка__

Уравнение разделяющей поверхности выглядит следующим образом:
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2C%20x%20%5Crangle%20%3D%200).

В нашем случае, когда объект состоит из двух параметров, уравнение принимает вид:
![](http://latex.codecogs.com/svg.latex?w_1x_1%20&plus;%20w_2x_2%20%3D%200).
Видно, что разделяющая прямая не имеет сободного коэффициента. Чтобы он появился,
вводят фиктивный параметр, и приравнивают его к -1, получая уравнение:
![](http://latex.codecogs.com/svg.latex?w_1x_1%20&plus;%20w_2x_2%20-%20w_3%20%3D%200).

----

[Вернуться в меню](../../README.md)

