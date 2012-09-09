;;; swarm-web-tests.el --- tests for the frontend

(require 'elnode)

(ert-deftest swarmweb--make-user ()
  "Test the user construction."
  (let* ((swarmweb--auth-db
          (elnode-db-make
           `(elnode-db-hash :filename "/tmp/swarm-db")))
         (params '(("username" . "nic")
                   ("password" . "secret")
                   ("email" . "nic@email.com")
                   ("key" . "AFFSGSHhajsdkakdn")))
         ;; params get decorated with the token by the make-user
         (params+ (append
                   (list "token")
                   (kvalist->keys params)))
         (user (swarmweb--make-user params))
         (db-user (elnode-db-get "nic" swarmweb--auth-db)))
    (should
     (equal (kvalist->keys user)
            params+))
    (should
     (equal (aget user "username")
            (aget params "username")))
    (should
     (equal (aget user "email")
            (aget params "email")))
    ;; Did the value get into the db?
    (should
     (equal (kvalist->keys db-user) params+))
        (should
     (equal (aget db-user "username")
            (aget params "username")))
    (should
     (equal (aget db-user "email")
            (aget params "email")))
    ;; Does the auth token work?
    (should (stringp (elnode-db-get "nic" swarmweb--auth-token-db)))))

(ert-deftest swarmweb-register ()
  "Test registration."
  ;; Test the GET
  (with-elnode-mock-server 'swarmweb-main-handler t
    (should-elnode-response
     (elnode-test-call "/register/")
     :status-code 200
     :body-match
     ".*<label>password:[ ]+<input type=\"text\" name=\"password\""))
  ;; Test the POST
  (with-elnode-mock-server 'swarmweb-main-handler t
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
     :header-list-match `(("Location" . "/registered/")
                          ("Set-Cookie"
                           . ,(format ".*%s.*" swarm-web-cookie-name))))
    (should-elnode-response
     (elnode-test-call
      "/registered/"
      :method "GET")
     :status-code 200
     :body-match
     ".*<p>We've sent an email to test@example.com")))

;;; swarm-web-tests.el ends here
