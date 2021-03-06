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

(ert-deftest test-barbell-load ()
  (should (equal nil (barbell--load 20)))
  (should (equal '(2.5) (barbell--load 25)))
  (should (equal '(25 10 2.5 1) (barbell--load 97)))
  (should (equal '(25 20) (barbell--load 110)))
  (should-error (barbell--load 21))
  (should-error (barbell--load 1000))
  (should-error (barbell--load 5)))


(ert-deftest test-jsround ()
  (should (= (/ 1.0 64) (string-to-number (calc-eval "jsround(0.016)"))))
  (should (= (/ 1.0 64) (string-to-number (calc-eval "jsround(0.008)"))))
  (should (= (/ 0.0 64) (string-to-number (calc-eval "jsround(0.007)"))))
  (should (= (/ 65.0 64) (string-to-number (calc-eval "jsround(1.016)"))))
  (should (= (+ 2 (/ 1.0 4)) (string-to-number (calc-eval "jsround(2.245)")))))

(ert "test-*")
