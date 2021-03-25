; Задача 4
; Определите функции, порождающую по заданному натуральному числу N спи-сок, состоящий из натуральных чиселот 1 до N.

(defun _extGenList (n res_list)
    (cond
        ((= n 0) ;(if
            res_list ; then
        )
        (t
            (_extGenList (- n 1) (cons n res_list))
        );(_ else)
    )
)

(defun genList (n)
    (_extGenList n ())
)

(print (genList 10))

; Задача 7
; Определите функцию,удаляющую из исходного списка элементы с четными номерами.

(defun _evenCalc (lst)
    (cond
        ((null lst) lst)
        (t
            (cons (car lst) (_nonEvenCalc (cdr lst)))
        )
    )
)

(defun _nonEvenCalc (lst)
    (cond
        ((null lst) lst)
        (t
            (_evenCalc (cdr lst))
        )
    )
)

(defun delEvenIndexes (lst)
    (_evenCalc lst)
)

(print (delEvenIndexes `(0 1 2 3 4 5 6 7 8)))

; Задача 15
; Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел

(defun dot (v1 v2); dot = v1[0]*v2[0] + v1[1]*v2[1] + ... + v1[n]*v2[n]
    (cond
        ((null v1) 0)
        ((null v2) 0)
        (t
            (+ ;(v1[i]*v2[i]) + (v1[i+1]*v2[i+1] + v1[i+2]*v2[i+2] +...)
                (* (car v1) (car v2)); v1[i]*v2[i]
                (dot (cdr v1) (cdr v2)); (v1[i+1]*v2[i+1] + v1[i+2]*v2[i+2] +...)
            )
        )
    )
)


(print (dot `(1 2 3) `(4 5 6))); 4 + 10 + 18 = 32


; Задача 19
; Определите функцию (ЛУКОВИЦА n), строющую N-уровневый вложенный список, элементом которого на самом глубоком уровне является N

(defun _onion (i n)
    (cond
        ((= i 0) ;(if
            n ; then
        )
        (t
            (list (_onion (- i 1) n))
        );(_ else)
    )
)

(defun onion (n)
    (_onion n n)
)

(print (onion 4))


; Задача 21
; Определите функцию,удаляющую из списка первое вхождение данного элемента на верхнем уровне

(defun findEl (list el)
    (cond
        ((null list)
            0
        )
        ((= (car list) el)
            0
        )
        (t
            (+ (findEl (cdr list) el) 1)
         )
    )
)
(defun _subDelIndex (list index it)
    (cond
        ((null list)
            list
        )
        ((= index it)
            (cdr list)
        )
        (t
            (cons
             (car list)
             (_subDelIndex (cdr list) index (+ it 1))
            )
        )
    )
)
(defun delIndex (list index)
    (_subDelIndex list index 0)
)
(defun delEl (list el)
    (delIndex list (findEl list el))
)

(print (delEl `(2 1 2 3 4 5) 2))

; Задача 30
; Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную запись операций в префиксную и возвращает значение выражения

(defun calculate (express)
    (cond
        ((not (atom (car express)))
             (calculate (cons
                         (calculate (car express))
                         (cdr express)
                         ))
         )
        ((not (atom (caddr express)))
             (calculate (list
                         (car express)
                         (cadr express)
                         (calculate (caddr express))
                         ))
        )
        (t
             (eval (list (cadr express) (car express) (caddr express)))
        )
    )
)

(print (calculate `((-2 + 4) * (6 / 2)) ))

; Задача 33
; Определите функцию МНОЖЕСТВО, преобразующую список в множество.

(defun inset (uset el)
    (cond
        ((null uset)
             nil
        )
        ((equal (car uset) el)
             t
         )
        (t
             (inset (cdr uset) el)
        )
    )
)

(defun unique_set (lst &optional (uset `()))
    (cond
        ((null lst)
             uset
        )
        ((inset uset (car lst))
             (unique_set (cdr lst) uset)
         )
        (t
             (unique_set (cdr lst) (cons (car lst) uset))
        )
    )
)

(print (unique_set `(1 5 2 3 1 2 4 5) ))

; Задача 37
; Определите функцию ПЕРЕСЕЧЕНИЕ, формирующую пересечение двух множеств,т.е.множество из их общих элементов

(defun inset (uset el)
    (cond
        ((null uset)
             nil
        )
        ((equal (car uset) el)
             t
         )
        (t
             (inset (cdr uset) el)
        )
    )
)
(defun intersect (uset1 uset2)
    (cond
        ((null uset1)
             `()
        )
        ((inset uset2 (car uset1))
             (cons
                  (car uset1)
                  (intersect (cdr uset1) uset2)
              )
         )
        (t
             (intersect (cdr uset1) uset2)
        )
    )
)

(print (intersect `(1 2 3 4) `(3 5 2 4 6) ))

; Задача 41
; Реализовать генератор деревьев, чтобы выдаваемые им деревья имели количество вершин,точно соответствующее числу, указанному в его первом аргументе

(defun tree (n)
    (cond
        ((= n 1)
             `v
        )
        ((= n 2)
             `(v v)
         )
        (t
             (list
              (tree (floor n 2))
              (tree (ceiling n 2))
              )
        )
    )
)


(print (tree 10))

; Задача 46
; Предположим, что отец и мать некоторого лица, хранится как значение соответствующих свойству символа, обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x),
; которая возвращает в качестве значение родителей, и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 сестры или братья,
; родные или с одним общим родителем.

(defun parents (child)
    (list (get child `father) (get child `mother))
)

(defun sister-brother (child1 child2)
    (cond
        ((null (get child1 `father)) nil)
        ((null (get child2 `father)) nil)
        ((null (get child1 `mother)) nil)
        ((null (get child2 `mother)) nil)
        
        ((EQL (get child1 `father) (get child2 `father)) t)
        ((EQL (get child1 `mother) (get child2 `mother)) t)
        
        (t nil)
    )
)



(setf (get `mch1 `father) 'f1)
(setf (get `mch1 `mother) 'm1)
(setf (get `mch2 `father) 'f1)
(setf (get `mch2 `mother) 'm1)
(setf (get `mch3 `father) 'f3)
(setf (get `mch3 `mother) 'm3)

(print (parents `mch1))
(print (parents `mch2))
(print (parents `mch3))
(print (sister-brother `mch1 `mch2))
(print (sister-brother `mch1 `mch3))

