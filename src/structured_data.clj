(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [g1 (get v 0)
        g3 (get v 2)]
    (+ g1 g3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [x1 (get (first rectangle) 0)
        x2 (get (second rectangle) 0)]
    (- x2 x1)))

(defn height [rectangle]
  (let [y1 (get (first rectangle) 1)
        y2 (get (second rectangle) 1)]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)(height rectangle)))

(defn contains-point? [[[x1 y1][x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 0)) (contains-point? outer (get inner 1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)]
    (if (contains? author :birth-year) (str name " (" (:birth-year author) " - " (:death-year author) ")") name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (> (count books) 0) (str (count books) " book" (if (> (count books) 1) "s" "") ". " (apply str (interpose ". " (map book->string books))) ".") "No books." ))

(defn books-by-author [author books]
  (let [x (fn [book] (has-author? book author))]
    (filter x books)
    ))

(defn author-by-name [name authors]
   (let [fname (fn [author] (= name (:name author)))
        fnames (filter fname authors)]
    (if (> (count fnames) 0) (first fnames) nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
