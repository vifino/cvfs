(use inclub)
(inclub "cvfs")
(inclub "cvfs-posix")
(use cvfs cvfs-posix)

(define vfs (cvfs:new))

(print "init")
(cvfs:def-drive vfs "root" "posix" ".")
(print "list")
(cvfs:call vfs 'list "root:")
