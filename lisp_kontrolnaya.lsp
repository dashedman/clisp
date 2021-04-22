
; Сумма делителей числа
(defun sum_of_dels (x &optional (n 1))
    (cond 
        ((<= x n) 0)
        ((= (rem x n) 0) (+ n (sum_of_dels x (+ n 1))))
        (t (sum_of_dels x (+ n 1)))
    )
)

; Функция поиска следующей пары близнецов на основе предыдущей
(defun find_twins (x1 x2)
    (cond
        ((= x1 (sum_of_dels (sum_of_dels x1)))
               (list x1 (sum_of_dels x1))
        )
        (t (find_twins (+ x1 1) x2))
    )
)

; генератор возвращает пару чисел при вызове, 
; полученую из функции func,
; исходя из предыдущей пары чисел
; func(a_i-1, b_i-1) -> (a_i, b_i)
(defun twinsGen (name func)
    (let ((t1 1) (t2 1))
      (set name (lambda () 
        (let ((new_twins (funcall func (+ t1 1) t2))) 
            (list
             (setq t1 (first new_twins))
             (setq t2 (second new_twins))
            )
        )))
    )
)


(twinsGen 'g1 #'find_twins)
(print (list (funcall g1) (funcall g1) (funcall g1)))

(twinsGen 'g2 #'find_twins)
(print (list (funcall g2)))
