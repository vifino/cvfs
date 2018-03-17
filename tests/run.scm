(require-extension test)
(use srfi-13)
(use (prefix cvfs cvfs:) cvfs-posix cvfs-mem)

(define vfs (cvfs:new))

(test-group "basic functionality"
            (test-assert "loads 'posix" (cvfs:def-drive vfs "posix" 'posix "."))
            (test-assert "loads 'mem" (cvfs:def-drive vfs "mem" 'mem))

            (test-error "complains if no default set and drive not in path" (cvfs:dir vfs ""))
            (test-assert "setting default drive" (cvfs:set-default vfs "mem"))
            (test "did set default drive" "mem" (cvfs:get-default vfs))
            (test-assert "doesn't complain if no drive in path but default set" (cvfs:dir vfs "")))

(define-constant teststr "Hello, World!")

(define (test-vfs drive)
  (let ([p (lambda (name) (string-append drive ":" name))])
    (test-group drive
                ;; create file
                (test-assert "create test file"
                  (let ([out (cvfs:open vfs (p "test") 'write)])
                    (display teststr out)
                    (close-output-port out)
                    #t))

                ;; check file
                (test-assert "test file actually exists" (cvfs:exists? vfs (p "test")))
                (let ([in (cvfs:open vfs (p "test") 'read)])
                  (test "basic file read-write" teststr (read-line in))
                  (close-input-port in))

                ;; copy file same-device
                (test-assert "copy test file"
                  (cvfs:copy vfs (p "test") (p "test2")))

                ;; check file
                (test-assert "test2 file actually exists" (cvfs:exists? vfs (p "test2")))
                (let ([in (cvfs:open vfs (p "test2") 'read)])
                  (test "copied file same-device" teststr (read-line in))
                  (close-input-port in))

                ;; test deletion
                (let ([deltest
                       (lambda (file)
                         (test-assert "delete test file"
                           (cvfs:delete vfs (p file)))
                         (test-assert "deleted test file doesn't exist"
                           (not (cvfs:exists? vfs (p file))))
                         (test-error "reading deleted file errors out"
                                     (let ([in (cvfs:open vfs (p file) 'read)])
                                       (read-line in)
                                       (close-input-port in))))])
                  (deltest "test")
                  (deltest "test2"))


               )))

(test-vfs "mem")
(test-vfs "posix")

(test-exit)
