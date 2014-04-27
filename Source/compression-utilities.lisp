
;; From chipz example
(defun gunzip (gzip-filename output-filename)
  (with-open-file (gzstream gzip-filename :direction :input
                            :element-type '(unsigned-byte 8))
    (with-open-file (stream output-filename :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (chipz:decompress stream 'chipz:gzip gzstream)
      output-filename)))

