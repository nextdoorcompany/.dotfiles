(require 'ert)

(defun format-test-helper (value)
  (with-temp-buffer
    (insert value)
    (js/format-calc-fraction))
  (current-kill 0 t))


(ert-deftest test-format-calc ()
  (should (string= "0" (format-test-helper "0")))
  (should (string= "1/2" (format-test-helper "0+1/2")))
  (should (string= "1 1/2" (format-test-helper "1+1/2")))
  (should (string= "100" (format-test-helper "100")))
  (should (string= "1.3" (format-test-helper "1.3")))
  (should (string= "0.2" (format-test-helper "0.2")))
  (should (string= "27 3/8" (format-test-helper "27+3/8")))
  (should (string= "10 1/16" (format-test-helper "10+1/16"))))

(ert "test-*")
