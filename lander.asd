(asdf:defsystem #:lander
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :version "0.0.0"
  :serial T
  :components ((:file "module")
               (:file "front"))
  :depends-on ((:interface :auth)
               :r-clip
               :cl-markless-plump))
