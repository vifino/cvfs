(use inclub)
(inclub "cvfs")
(use cvfs)
(inclub "cvfs-posix")

(define vfs (cvfs:new))

(print "init")
(cvfs:def-drive vfs "root" "posix" ".")
(print "list")
(cvfs:call vfs 'list "root:")
