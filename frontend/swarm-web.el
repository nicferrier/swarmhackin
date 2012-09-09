;;; swarm-web.el --- web frontend for swarm -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Softwarite Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The elnode frontend for the Swarm app.

;;; Code:

;;; swarm-web code

(elnode-app swarmweb-dir esxml kv)

(defconst swarmweb--user-db
  (elnode-db-make
   `(elnode-db-hash
     :filename
     ,(expand-file-name
       (concat swarmweb-dir "auth-db"))))
  "The database where we store authentication details.")

(defconst swarmweb--auth-token-db
  (elnode-db-make
   `(elnode-db-filter
     :source ,swarmweb--user-db
     :filter
     ,(lambda (key value)
              (cdr (assoc "token" value)))))
  "Token view of the user db.")

(defconst swarmweb-cookie-name "swarm-user"
  "The name of the cookie we use for auth.")

(defun swarmweb--make-user (user-params)
  "Check the USER-PARAMS and store in the `swarmweb-auth-db'.

Return the user alist."
  (let ((plist (kvalist->plist user-params)))
    (destructuring-bind (&key username password email key) plist
      (let ((token (elnode--auth-make-hash username password)))
        ;; FIXME not clear whether we should make a UUID for the user.
        (elnode-db-put
         username
         (append
          (list (cons "token" token))
          user-params)
         swarmweb--user-db)))))

(defun* swarmweb--login (httpcon
                        registered-page
                        &key
                        username
                        password)
  "Log the registered user in."
  (elnode-auth-http-login
   httpcon
   username password
   registered-page
   :cookie-name swarmweb-cookie-name
   :auth-db swarmweb--auth-token-db))

(defun swarmweb-get-user (httpcon)
  "Get the user via the cookie on the HTTPCON."
  (elnode-db-get
   (car
    (elnode-auth-cookie-decode
     (elnode-http-cookie
      httpcon
      swarmweb-cookie-name t)))
   swarmweb--user-db))

(defun swarmweb-session-vars ()
  "Get the session vars from the `swarmweb--user-db'."
  (swarmweb-get-user elnode-replacements-httpcon))

(defun swarmweb-register-handler (httpcon)
  "Take a registration and create a user."
  (elnode-method httpcon
    (GET
     (elnode-send-file
      httpcon
      (concat swarmweb-dir "register.html")))
    (POST
     (let* ((create-params
             (elnode-http-params
              httpcon
              "username" "password" "email" "key"))
            (user (swarmweb--make-user create-params)))
       (let ((plist (kvalist->plist create-params)))
         ;; Use the login to redirect us
         (swarmweb--login
          httpcon "/registered/"
          :username (plist-get plist :username )
          :password (plist-get plist :password)))))))

(defun swarmweb-group-handler (httpcon)
  "Handle the group page."
  (with-elnode-auth httpcon 'swarmweb-auth
    (let ((user (swarmweb-get-user httpcon))
          (group (concat swarmweb-dir "group.html")))
      (elnode-method httpcon
        (GET
         (elnode-send-file httpcon group :replacements user))
        (POST
         ;; What do we do?
         (elnode-send-file httpcon (concat swarmweb-dir "made.html")))))))

(defun swarmweb-main-handler (httpcon)
  "The handler for the main page."
  (if-elnode-auth httpcon 'swarmweb-auth
    (swarmweb-group-handler httpcon)
    ;; Else user is not authenticated so send the main file
    (elnode-send-file httpcon (concat swarmweb-dir "main.html"))))

(defun swarmweb-router (httpcon)
  (let ((registered (concat swarmweb-dir "registered.html"))
        (css (concat swarmweb-dir "style.css")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]*//registered/"
        . ,(elnode-make-send-file
            registered
            :replacements
            'swarmweb-session-vars))
       ("^[^/]*//group/" . swarmweb-group-handler)
       ("^[^/]*//register/" . swarmweb-register-handler)
       ("^[^/]*//style.css" . ,(elnode-make-send-file css))
       ("^[^/]*//$" . swarmweb-main-handler)))))

(elnode-auth-define-scheme
 'swarmweb-auth
 :auth-db swarmweb--auth-token-db
 :cookie-name swarmweb-cookie-name
 :redirect (elnode-auth-make-login-wrapper
            'swarmweb-router))

(provide 'swarm-web)

;;; swarm-web.el ends here
