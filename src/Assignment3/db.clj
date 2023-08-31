(ns Assignment3.db
  (:require[clojure.string :as str]))
;C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Courses.txt
(def GradesMap (atom {}))
(def StudentsMap (atom {}))
(def coursesMap (atom {}))
(def courseWeight (atom {}))
(def coursesGrades (atom {}))
(def courseYear (atom {}))
(def courseID (atom {}))
(defn loadGrades []
  (let [fileContent (slurp "C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Grades.txt")
        lines (clojure.string/split-lines fileContent)]
    (doseq [line lines]
      (let [parts (clojure.string/split line #"\|")
            quotedParts (mapv #(str "\"" % "\"") parts)
            studentId (nth parts 0)
            courseId (nth parts 1)
            grades (nth parts 3)
            year (nth parts 2)
            rest (rest (rest quotedParts))] ; Skip the first two elements (studentId and courseId)
        (swap! GradesMap
               update
               studentId
               (fn [studentGrades]
                 (assoc studentGrades
                        (read-string courseId)
                        rest)))
        (swap! coursesGrades
               update
               courseId
               (fn [courseGrades]
                 (conj courseGrades grades)))
        (swap! courseYear assoc courseId (str "\"" year "\""))))))


(defn displayGrades []
  (let [fileContent (slurp "C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Grades.txt")
        lines (clojure.string/split-lines fileContent)]
    (doseq [line lines]
      (let [parts (clojure.string/split line #"\|")
            quotedParts (mapv #(str "\"" % "\"") parts)]
        (println quotedParts)))))

(defn loadStudents []
  (let [fileContent (slurp "C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Students.txt")
        lines (clojure.string/split-lines fileContent)]
    (doseq [line lines]
      (let [parts (clojure.string/split line #"\|")
            idStudent (nth parts 0)
            name (nth parts 1)
            quotedName (str "\"" name "\"")
            quotedStudentId (str "\"" idStudent "\"")]
        (swap! StudentsMap assoc quotedStudentId quotedName)))))


(defn displayStudents []
  (let [fileContent (slurp "C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Students.txt")
        lines (clojure.string/split-lines fileContent)]
    (doseq [line lines]
      (let [parts (clojure.string/split line #"\|")
            quotedParts (mapv #(str "\"" % "\"") parts)]
        (println quotedParts)))))
(defn loadCourses []
  (let [fileContent (slurp "C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Courses.txt")
        lines (clojure.string/split-lines fileContent)]
    (doseq [line lines]
      (let [parts (clojure.string/split line #"\|")
            courseId (nth parts 0)
            courseName (str "\"" (nth parts 1) " " (nth parts 2) "\"")
            courseDescription (nth parts 4)
            courseWeightFromCourseMap (nth parts 3)
            value [courseName (str "\"" courseDescription "\"")]]
        (swap! coursesMap assoc courseId value)
        (swap! courseWeight assoc courseId courseWeightFromCourseMap)
        (swap! courseID assoc courseName courseId)))))

(defn displayCourses []
  (let [fileContent (slurp "C:\\Users\\hp\\Desktop\\a3_chrit_baraa_40225403\\src\\Assignment3\\Courses.txt")
        lines (clojure.string/split-lines fileContent)]
    (doseq [line lines]
      (let [parts (clojure.string/split line #"\|")
            quotedParts (mapv #(str "\"" % "\"") parts)]
        (println quotedParts)))))


(defn getCourseDescription [courseId]
  (let [courseDescription (get @coursesMap courseId)]
    (if (nil? courseDescription)
      (do
        (println "Invalid course ID, no course with such ID!")
        nil)
      courseDescription)))

(defn displayStudentsRecord [studentId]
  (let [grades (@GradesMap studentId)]
    (if (nil? grades)
      (do
        (println "No student with such ID!")
        nil)

      (doseq [grade grades]
        (let [courseId (str (nth grade 0))
              yearAndGrade (get-in @GradesMap [studentId (Integer/parseInt courseId)])
              courseDescription (getCourseDescription courseId)]
          (println (into courseDescription yearAndGrade)))))))

(defn getCourseWeight [courseId]
  (let [courseWeight1 (get @courseWeight courseId)]
    (if (nil? courseWeight1)
      (do
        (println "No Course with such course ID.")
        nil)
      courseWeight1)))

(defn computeGPA [studentId]
  (let [grades (@GradesMap studentId)
        letterToNumericalGradeMap {"A+" 4.3, "A" 4, "A-" 3.7,
                                   "B+" 3.3, "B" 3, "B-" 2.7,
                                   "C+" 2.3, "C" 2, "C-" 1.7,
                                   "D+" 1.3, "D" 1, "D-" 0.7,
                                   "F" 0}]
    (if (nil? grades)
      (do
        (println "No student with such ID!")
        nil)
      (let [totalGPA (atom 0)
            totalCredits (atom 0)]
        (doseq [grade grades]
          (let [courseId (str (nth grade 0))
                letterGrade (nth (get-in @GradesMap [studentId (Integer/parseInt courseId)]) 1)
                letterGradeMod (str/replace letterGrade "\"" "")
                numericalGrade (get letterToNumericalGradeMap letterGradeMod)
                credits (getCourseWeight courseId)]
            (if (and (not (nil? numericalGrade))
                     (not (nil? credits)))
              (do
                (swap! totalGPA + (* numericalGrade (Double/parseDouble credits)))
                (swap! totalCredits + (Double/parseDouble credits)))
              nil)))
        (if (zero? @totalCredits)
          (do
            (println "No courses with valid grades found!")
            nil)
          (let [gpa (/ @totalGPA @totalCredits)]
            (println "GPA:" gpa)))))))



(defn computeCourseAverage [courseId]
  (let [grades (get @coursesGrades courseId)
        letterToNumericalGradeMap {"A+" 4.3, "A" 4, "A-" 3.7,
                                   "B+" 3.3, "B" 3, "B-" 2.7,
                                   "C+" 2.3, "C" 2, "C-" 1.7,
                                   "D+" 1.3, "D" 1, "D-" 0.7,
                                   "F" 0}]
    ;; (println "Grades for the course" courseId "id is" grades)
    (if (nil? grades)
      (println "No course with such Name!")
      (let [totalNumericalGrade (atom 0)
            totalStudents (count grades)]
        (doseq [grade grades]
          (let [numericalGrade (get letterToNumericalGradeMap grade)]
            ;; (println "Numerical Grade" numericalGrade)
            (when (not (nil? numericalGrade))
              (swap! totalNumericalGrade + numericalGrade))))
        (if (zero? totalStudents)
          (println "No grades found for the course!")
          (let [average (/ @totalNumericalGrade totalStudents)]
            ;; (println "The Course Average may be simply calculated by converting all letter grades to numerical values (using the above map) and computing the average per semester.")
            (println "[" (nth (get @coursesMap courseId) 0) (get @courseYear courseId) (str "\"" average "\"") "]")))))))



