(asdf:defsystem #:sandalphon.lambda-list
  :description "Lambda list parsing and usage"
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.1"
  :components ((:file "package")
	       (:file "classes" :depends-on ("package"))
	       (:file "parse" :depends-on ("classes" "package"))
	       (:file "standard-grammars" :depends-on ("package"))
	       (:file "binds" :depends-on ("classes" "package"))
	       (:file "unparse" :depends-on ("classes" "package"))))
