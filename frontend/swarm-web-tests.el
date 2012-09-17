;;; swarm-web-tests.el --- tests for the frontend

(require 'elnode)
(require 'cl)

(defun swarmweb--test-make-user ()
  "Make a user.

Return the user and the params so they can be destructured."
  ;; FIXME - need a better test make user
  ;;
  ;; ideally take arguments but default them... so we can make
  ;; different users
  (let ((params '((username . "nic")
                  (password . "secret")
                  (email . "nic@email.com")
                  (key . "AFFSGSHhajsdkakdn"))))
    (list (swarmweb--make-user params) params)))

(defmacro swarmweb--mock-db (&rest body)
  "Mock the swarm databases and eval BODY."
  (declare (debug (&rest form))
           (indent 0))
  `(let* ((swarmweb--user-db
          (elnode-db-make
           `(elnode-db-hash
             :filename ,(format "/tmp/swarm-user-db-%s" (uuid-string)))))
          (swarmweb--group-db
           (elnode-db-make
            `(elnode-db-hash
              :filename ,(format "/tmp/swarm-group-db-%s" (uuid-string)))))
          (swarmweb--auth-token-db
           (elnode-db-make
            `(elnode-db-filter
              :source ,swarmweb--user-db
              :filter ,(lambda (key value)
                               (cdr (assoc 'token value)))))))
     ,@body))

(ert-deftest swarmweb--make-user ()
  "Test the user construction."
  (swarmweb--mock-db
    (destructuring-bind (user params) (swarmweb--test-make-user)
      ;; params get decorated with the token by the make-user
      (let ((params+ (cons 'token (kvalist->keys params)))
            (db-user (elnode-db-get "nic" swarmweb--user-db)))
        (should (equal (kvalist->keys user) params+))
        (should
         (equal (aget user 'username)
                (aget params 'username)))
        (should
         (equal (aget user 'email)
                (aget params 'email)))
        ;; Did the value get into the db?
        (should (equal (kvalist->keys db-user) params+))
        (should
         (equal (aget db-user 'username)
                (aget params 'username)))
        (should
         (equal (aget db-user 'email)
                (aget params 'email)))
        ;; Does the auth token work?
        (should (stringp (elnode-db-get "nic" swarmweb--auth-token-db)))))))

(ert-deftest swarmweb--make-group ()
  "Test the group construction."
  (swarmweb--mock-db
    (destructuring-bind (user params) (swarmweb--test-make-user)
      (let* ((group (swarmweb--make-group
                     user
                     '((name . "my group")
                       (description . "A test group"))))
             ;; Retrieve the group from the db using the key we made.
            (db-group (elnode-db-get (aget group 'uuid) swarmweb--group-db)))
        ;; Check we've got the details in the returned group details
        (should (equal (aget group 'name) "my group"))
        ;; Check that we looked them up
        (should (equal group db-group))))))

(defun swarmweb--test-grp-list (user)
  (mapcar
   (lambda (alist)
     ;; Each record also has a uuid and an owner
     (kvalist->filter-keys alist 'name 'description 'owner))
   (kvalist->values
    (swarmweb-group-list
     (assoc-default 'email user)))))

(ert-deftest swarmweb-group-list ()
  "Test the group listing stuff."
  (swarmweb--mock-db
    ;; FIXME - we ought to make a 2nd user and a group with that user
    ;; so that we test that a group created with it does not appear in
    ;; the list
    (destructuring-bind (user params) (swarmweb--test-make-user)
      (let ((group1 `((name . "group 1")
                      (description . "a test group")
                      (owner . ,(aget user 'email))))
            (group2 `((name . "group 2")
                      (description . "a test group")
                      (owner . ,(aget user 'email))))
            (group3 `((name . "group 3")
                      (description . "a test group")
                      (owner . ,(aget user 'email)))))
        (swarmweb--make-group
         user
         (kvalist->filter-keys group1 'name 'description))
        (swarmweb--make-group
         user
         (kvalist->filter-keys group2 'name 'description))
        (swarmweb--make-group
         user
         (kvalist->filter-keys group3 'name 'description))
        (should
         (equal
          (sort (mapcar (lambda (a) (sort a 'kvcmp))
                        (list group1 group2 group3)) 'kvcmp)
          (sort (mapcar (lambda (a) (sort a 'kvcmp))
                        (swarmweb--test-grp-list user)) 'kvcmp)))))))


(ert-deftest swarmweb-register ()
  "Test registration."
  (swarmweb--mock-db
    ;; Test the GET
    (with-elnode-mock-server 'swarmweb-router
      (should-elnode-response
       (elnode-test-call "/register/")
       :status-code 200
       :body-match
       ".*<label>password:[ ]+<input type=\"password\" name=\"password\""))
    ;; Test the POST
    (with-elnode-mock-server 'swarmweb-router
      (should-elnode-response
       (elnode-test-call
        "/register/"
        :method "POST"
        :parameters
        '(("username" . "testuser")
          ("password" . "secret")
          ("email" . "test@example.com")
          ("key" . "......")))
       :status-code 302
       :header-list-match
       `(("Location" . "/registered/")
         ("Set-Cookie"
          . ,(format ".*%s.*" swarmweb-cookie-name))))
      (should-elnode-response
       (elnode-test-call "/registered/" :method "GET")
       :status-code 200
       :body-match
       ".*<p>We've sent an email to test@example.com"))))


(ert-deftest swarmweb-group ()
  "Test group making."
  (swarmweb--mock-db
    ;; Make a user
    (swarmweb--test-make-user)
    (with-elnode-mock-server 'swarmweb-router
      ;; Not logged in - should fail
      (should-elnode-response
       (elnode-test-call "/group/")
       :status-code 302
       :header-name "Location"
       :header-value "/login/?redirect=/group/")
      ;; Now send login
      (should-elnode-response
       (elnode-test-login 'swarmweb-auth "/group/" "nic" "secret")
       :status-code 302
       :header-name "Location"
       :header-value "/group/")
      (should-elnode-response
       (elnode-test-call "/group/")
       :status-code 200
       :body-match "<p>Hey nic, make a group.*")
      (should-elnode-response
       (elnode-test-call
        "/group/"
        :method "POST"
        :parameters
        '(("name" . "test-group")
          ("description" . "just a quick test")))
       :status-code 200
       :body-match "<p>The group <em>test-group</em> was made.*"))))

(ert-deftest swarmweb-group-list ()
  "Test group listing."
  (swarmweb--mock-db
    ;; Make a user
    (destructuring-bind (user params) (swarmweb--test-make-user)
      (let ((grp (swarmweb--make-group
                  user
                  '((name . "my group")
                    (description . "A test group")))))
        (with-elnode-mock-server 'swarmweb-router
          (elnode-test-login 'swarmweb-auth "/group/" "nic" "secret")
          (should-elnode-response
           (elnode-test-call "/group/")
           :status-code 200
           :body-match
           (format "<a href='/group/%s'>" (assoc-default 'uuid grp))))))))

(ert-deftest swarmweb-html-a ()
  (equal
   "<a href='10'>ten</a>"
   (swarmweb-html-a "a" "b" '(("a" . "10") ("b" . "ten")))))

;;; swarm-web-tests.el ends here
