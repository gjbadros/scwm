;; $Id$
;; --08/05/98 gjb

(X-resource-put "Foo" "bar")
(X-resource-put "Foo.bar" "bong")

(X-resource-get "Foo.bar")
(X-resource-get "Foo")


