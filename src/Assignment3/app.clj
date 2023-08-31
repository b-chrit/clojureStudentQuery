(ns Assignment3.app
  (:require [Assignment3.menu :refer [sisMenu]]
            [Assignment3.db :refer [loadGrades loadCourses loadStudents] ]))

(defn -main [] 
  (loadGrades)
  (loadCourses)
  (loadStudents)
  ;; (print @db/courseID)
  (sisMenu))
