# ���������� KNN

### ������

**���������� KNN** - ����������� �������� �������������, ��� � �������� ������
�������� �������
![u](http://latex.codecogs.com/svg.latex?%5Clarge%20u)
� ������
![y](http://latex.codecogs.com/svg.latex?%5Clarge%20y)
��������� �������
![w(i,u) = [i <= k]w(i)](http://latex.codecogs.com/svg.latex?%5Clarge%20W_y%28i%2Cu%29%3D%5Bi%20%5Cleq%20k%5Dw%28i%29),
���

- ![i](http://latex.codecogs.com/svg.latex?%5Clarge%20i)
� ������� ���������� ������ � ���������������� �����
![u](http://latex.codecogs.com/svg.latex?%5Clarge%20u);
- ![w(i)](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29)
� ��� ������������ �����
![u](http://latex.codecogs.com/svg.latex?%5Clarge%20u), ������ ���������
�������.

�������� **kwKNN** ������� �� ��������� _k_, ������� ����������� ��
_k = 1_ (_1NN_) �� _k = l_, � ������ ������� ������. � ����� �������
���� ������� ��������� �������:

![w(i) = (k + 1 - i) / k](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29%20%3D%20%5Cfrac%7Bk%20&plus;%201%20-%20i%7D%7Bk%7D)

��� ����������� ������� ��������� _k_ ����� ������������ **LOO (leave-one-out)**

### ����������� ����������

�������� ��� ���������: [KwKNN.R](../KwKNN.R)

��� ���������� �� [KNN.R](../KNN.R) ���� ��������
`mc.KwKNN(sortedDistances, k)` (�������� ����������)
� �������� �������������� �������
������� `mc.KwKNN.w`.

��������� �� ��� ��������� ��������� � [KNN.R](../KNN.R).

### ���������

![���������](pict/KwKNN.png)

��� ������������� ������������ ����������� ������� ������ ������ �� ���������.

����� �������� ������ **LOO**, ������� ��������� ����������� �������� _k_.
� ������ ������ ����������� ��������� _k = 4_, ������� ������ _0.4_, ���
������������ 6-�� ����������� ������������������ ������.

������� �������� ������������� ��������� **KwKNN** �� **KNN** ����� ���������
�� ��������� ��������:

![���������](pict/KNN-KwKNN.png)

�������� �� ��, ��� **KNN** ��� ����� ������� �����, ��� **KwKNN**, ��������
������������ ������� ��������� ��� ������. **KwKNN** �� ���������� ��� �����
_k_ � ���� ����������� ���������� ��������� ��� ����� _k_. ��� ������� � ���,
��� �� ��������� �� ������ ������� ����� ����� ��������� _k_-��������, ��
����� � �� �������. ��� ��� ������� �������� ����� �������, ��� �������
������ ����� ������ �������������� (������ � ����� ��������� ����������
������� ���������� �� �������).

----

[��������� � ����](../../README.md)

[��������� � KNN](KNN.md)

[�������� � PW](PW.md)

