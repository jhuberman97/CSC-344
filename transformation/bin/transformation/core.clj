(ns transformation.core)

(defn third [lst]
  (nth lst 2))

(defn bind-values [m l]
  (mapv (fn [i]
         (cond 
           (seq? i) (bind-values m i)
           (vector? i) (vec (bind-values m i))
           :default (m i i)))
       l))

(declare transform)

(defn simplify
  [exp]
  (if (= (first exp) 'transform)
    (if (= (first (third exp)) 'transform)
      (transform (second exp) (simplify (third exp)))
      (transform (second exp) (third exp)))
    (if (= (first exp) '*)
      (if (= (second exp) '1)
        (third exp)
        (if (= (third exp) '1)
          (second exp)
          (if (= (second exp) '0)
            '0
            (if (= (third exp) '0)
              '0
              (if (integer? (second exp))
                (if (integer? (third exp))
                  (* (second exp) (third exp))
                  (if (= (second exp) '-1)
                    (list '- (third exp))
                    (if (= (third exp) '-1)
                      (list '- (second exp))
                      exp)))
                (if (= (second exp) '-1)
                  (list '- (third exp))
                  (if (= (third exp) '-1)
                    (list '- (second exp))
                    exp)))))))
      (if (= (first exp) '+)
        (if (= (second exp) '0)
          (third exp)
          (if (= (third exp) '0)
            (second exp)
            (if (integer? (second exp))
              (if (integer? (third exp))
                (+ (second exp) (third exp))
                exp)
              exp)))
        (if (= (first exp) '-)
          (if (= (first (second exp)) '-)
            (second (second exp))
            exp)
          exp)))))

(defn transform
  [mat1 mat2]
  (vector
    (simplify
      (list '+
             (simplify(list '* 
                             (first(first mat1)) (first mat2)))
             (simplify(list '* 
                             (second(first mat1)) (second mat2)))))
    (simplify
      (list '+
             (simplify(list '* 
                            (first(second mat1)) (first mat2)))
             (simplify(list '* 
                            (second(second mat1)) (second mat2)))))))

(defn evalexp [exp bindings] 
  (simplify (bind-values bindings exp)))