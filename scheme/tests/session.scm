; (load "test.scm")					-*- scwm -*-

(if (getenv "SESSION_MANAGER")
    (begin
      (test-case "get client id"
       (SM-client-id)
       #t)

      (test-case "no error"
       (SM-error-message)
       => ""))

    (test-case "SM not active"
     (SM-client-id)
     => #f)

    (test-case "check error message"
     (SM-error-message)
     => "SESSION_MANAGER environment variable not defined"))
