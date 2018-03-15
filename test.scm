(use inclub)
(inclub "cvfs")
(inclub "cvfs-posix")
(inclub "cvfs-mem")
(use cvfs cvfs-posix cvfs-mem)

(define vfs (cvfs:new))
(print "init")
(cvfs:def-drive vfs "root" "posix" ".")
(cvfs:def-drive vfs "mem" "mem")

(let ([out (cvfs:open vfs "mem:test" 'write)])
     (display "Hello, World!" out)
     (close-output-port out))

(let ([in (cvfs:open vfs "mem:test" 'read)])
     (print (read-line in))
     (close-input-port in))
