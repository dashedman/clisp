; Определите функицонал (MAPLIST fn список) для одного списочного аргу-мента.
(defun maplst (fn lst)
    (cond
        ((null lst) ())
        (t (cons
                (funcall fn lst)
                (maplst fn (cdr lst))
            )
        )
    )
)

(print (maplst #'(lambda (x) (reverse x)) `(1 2 3)))

; Определите функциональный предикат (КАЖДЫЙ пред список),
; который истинен в том и только в том случае,
; когда, являющейся функциональным аргументом предикат пред
; истинен для всех элементов списка список.
(defun m_every (prd lst)
    (>=
     (length (mapcan
              #'(lambda (x) (if (funcall prd x) (list t) nil ))
              lst))
     (length lst)
     )
)

(print (m_every #'(lambda (x) (>= x 0)) `(1 -1 0 -2 3)))
(print (m_every #'(lambda (x) (>= x 0)) `(1 1 0 3)))

; Определите фильтр(УДАЛИТЬ-ЕСЛИ предсписок),
; удаляющий из списка список все элементы,
; которые обладают свойством,
; наличие которого проверяет предикат пред.
(defun del-if (prd lst)
    (mapcan
      #'(lambda (x) (if (funcall prd x) (list x) nil ))
      lst)
)

(print (del-if #'(lambda (x) (>= x 0)) `(1 -1 0 -2 3)))

; Напишите генератор натуральных чисел: 0,1,2,3,4,5,...
(defun getGen (name)
    (let ((x 0))
      (set name (lambda () (setq x (+ x 1))))
    )
)

(getGen 'g1)
(print (list (funcall g1) (funcall g1) (funcall g1)))
(getGen 'g2)
(print (list (funcall g2) (funcall g2)))


; Напишите генератор, порождающий последовательность (A), (BA), (ABA), (BABA), ...
(defun getGenAB (name)
    (let ((x ()))
      (set name (lambda () (
                            if (eq (car x) 'A)
                               (setq x (cons 'B x))
                               (setq x (cons 'A x))
                           )
                        ))
    )
)

(getGenAB 'g1)
(print (list (funcall g1) (funcall g1) (funcall g1)))
(getGenAB 'g2)
(print (list (funcall g2) (funcall g2)))


; Определите функцию, которая возвращает в качестве значения свой вызов.
(setq autocall (lambda (x y)
        (list 'funcall 'autocall x y)
     )
)

(print (funcall autocall 0 1))
(print (eval (funcall autocall 0 1)))

; Определите функцию, которая возвращает в качестве значения форму своего определения (DEFUN).
(defun autodef nil
    ((lambda (x) (list 'defun 'autodef 'nil (list x (list 'quote x))))
     `(lambda (x) (list 'defun 'autodef 'nil (list x (list 'quote x))))))

(print (autodef))
