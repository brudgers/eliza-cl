(defun eliza ()
  "Respond to user input using pattern matching."
  (loop
     (print 'eliza>)
     (print (flatten (use-eliza-rules (read))))))
