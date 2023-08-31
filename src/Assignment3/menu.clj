(ns Assignment3.menu
  (:require [Assignment3.db :refer [displayCourses displayStudents displayGrades displayStudentsRecord
                                    computeGPA computeCourseAverage]]
            [ clojure.string :as str]))

(defn sisMenu []
  (loop [badAnswer true]
    (when badAnswer
      (println "-----------------")
      (println "*** SIS MENU ***")
      (println "-----------------")
      (println "1. Display Courses")
      (println "2. Display Students")
      (println "3. Display Grades")
      (println "4. Display Student Record")
      (println "5. Calculate GPA")
      (println "6. Course Average")
      (println "7. Exit")
      (print "Enter an option? ")
      (flush)
      (let [option (read-line)]
        (cond
          (= option "1")
          (do
            (displayCourses)
            (recur true))
          (= option "2")
          (do
            (displayStudents)
            (recur true))
          (= option "3")
          (do
            (displayGrades)
            (recur true))
          (= option "4")
        (do
          (println "What is the student Id?")
          (let [studentInput (read-line)
                studentInputQuoted (str "\"" studentInput "\"")
                studentName (get @Assignment3.db/StudentsMap studentInputQuoted)]
            (if studentName
              (println "[" studentName studentInputQuoted "]")
              nil)
            (displayStudentsRecord studentInput))
          (recur true))
          (= option "5")
          (do
            (println "What is the student Id?")
            (let [studentInput (read-line)
                  studentInputQuoted (str "\"" studentInput "\"")
                  studentName (get @Assignment3.db/StudentsMap studentInputQuoted)]
              (if studentName
                (println "[" studentName studentInputQuoted "]")
                nil)
              (computeGPA studentInput))
            (recur true))

          (= option "6")
          (do
            (println "What is the Course Name?")
            (let [userCourseName (-> (read-line) str/upper-case)
                  userCourseNameQuoted (str  "\"" userCourseName "\"")
                  courseId (get @Assignment3.db/courseID userCourseNameQuoted)]
              (computeCourseAverage courseId))
            (recur true))
          (= option "7")
          (do
            (println "Exiting the SIS-MENU. Thank you for using our program!")
            (recur false))
          :else
          (do
            (println (str "Please input a valid option, " option " is not a valid option. Valid options are [1-7]."))
            (recur true)))))))