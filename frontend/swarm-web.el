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

(elnode-app swarmweb-dir esxml kv uuid)

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
              (cdr (assoc 'token value)))))
  "Token view of the user db.")

(defconst swarmweb-cookie-name "swarm-user"
  "The name of the cookie we use for auth.")

(defconst swarmweb--group-db
  (elnode-db-make
   `(elnode-db-hash
     :filename
     ,(expand-file-name
       (concat swarmweb-dir "group-db"))))
  "The database where we store groups.")


;; These should go to esxml, failing that to kv.

(defun swarmweb-html-list (lst &optional fn)
  "Wrap the LST elements in HTML list elements.

Optionally pass each element through FN."
  (mapcar
   (lambda (e)
     (format
      "<li>%s</li>"
      (if (functionp fn)
          (apply fn e)
          e)))
   lst))

(defun swarmweb-html-a (ref value &optional alist)
  "Turn the REF and VALUE into an A element.

Optionally dererference the REF and VALUE via the ALIST."
  (apply
   'format
   (append
    (list "<a href='%s'>%s</a>")
    (if alist
        (list (assoc-default ref alist)
              (assoc-default value alist))
        (list ref value)))))


(defun swarmweb-list-keys ()
  "Make an alist of the ssh-keys from the user database."
  (elnode-db-map
   (lambda (key user-data)
     (when user-data
       (kvdotassoc "key" user-data)))
   swarmweb--user-db))

(defun* swarmweb--mkuser (&key username password email key)
  "Base make user."
  (let ((token (elnode--auth-make-hash username password)))
    ;; FIXME not clear whether we should make a UUID for the user.
    (elnode-db-put
     username
     (acons
      'token token
      (list
       (cons 'username username)
       (cons 'password password)
       (cons 'email email)
       (cons 'key key)))
     swarmweb--user-db)))

(defun swarmweb--make-user (user-params)
  "Check the USER-PARAMS and store in the `swarmweb-auth-db'.

Return the user alist."
  (let ((plist (kvalist->plist user-params)))
    (apply 'swarmweb--mkuser plist)))

(defun* swarmweb-user-get (&key username email)
  "Get a user either by USERNAME or by EMAIL."
  ;; Fixme - there's a better way of doing this - matching a plist to
  ;; an or on the key/values of the plist
  (car
   (kvalist->values
    (elnode-db-map
     (lambda (key rec)
       (cond
         (username
          (when (equal
                 (assoc-default 'username rec)
                 username)
            rec))
         (email
          (when (equal
                 (assoc-default 'email rec)
                 email)
            rec))))
     swarmweb--user-db))))

(defun swarmweb--make-group (owner group-params)
  "Make a group."
  (let ((uuid (uuid-to-stringy (uuid-create)))
        (plist (kvalist->plist group-params)))
    (destructuring-bind (&key name description) plist
      (elnode-db-put
       uuid
       (acons
        'uuid uuid
        (acons 'owner
               (assoc-default 'email owner)
               group-params))
       swarmweb--group-db))))


;; Retrieval bits

(defun swarmweb-get-user (&optional httpcon)
  "Get the user via the cookie on the HTTPCON."
  (elnode-db-get
   (car
    (elnode-auth-cookie-decode
     (elnode-http-cookie
      (if httpcon httpcon elnode-replacements-httpcon)
      swarmweb-cookie-name t)))
   swarmweb--user-db))

(defun swarmweb-get-user-http (&optional httpcon)
  "Get the user but force string keys."
  (kvalist-keys->* (swarmweb-get-user httpcon) 'symbol-name))

(defun swarmweb-group-get (id)
  "Get a single group by ID."
  (elnode-db-get id swarmweb--group-db))

(defun swarmweb-group-list (email &optional car-key cdr-key proper)
  "List out the groups for a specified EMAIL.

If CAR-KEY and CDR-KEY are present the values of the result are
reduced to an alist with just those 2 values."
  (let ((res (elnode-db-map
              (lambda (key value)
                (when (equal email (assoc-default 'owner value))
                  value)) swarmweb--group-db nil t)))
    (if car-key
        (kvalist2->alist
         (kvalist->values res) car-key cdr-key proper)
        ;; Else
        res)))

(defun swarmweb-group-html (groups)
  "Convert the GROUPS into an HTML list."
  (mapconcat
   'identity
   (swarmweb-html-list
    groups
    (lambda (ref value)
      (swarmweb-html-a
       (concat "/group/" ref) value)))
   ""))

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
          :username (plist-get plist :username)
          :password (plist-get plist :password)))))))

(defun swarmweb-group-create-handler (httpcon)
  "Handle the group page."
  (with-elnode-auth httpcon 'swarmweb-auth
    (let* ((user (swarmweb-get-user httpcon))
           (groups (swarmweb-group-list
                    (assoc-default 'email user)
                    'uuid 'name t)))
      (elnode-method httpcon
        (GET
         (elnode-send-file
          httpcon
          (concat swarmweb-dir "group.html")
          :replacements
          (acons "groups"
                 (or (swarmweb-group-html groups) "")
                 (kvalist-keys->* user 'symbol-name))))
        (POST
         (let* ((group-params
                 (elnode-http-params
                  httpcon "name" "description"))
                (group (swarmweb--make-group
                        user
                        (kvalist-keys->symbols group-params))))
           (elnode-send-file
            httpcon (concat swarmweb-dir "made.html")
            :replacements
            (append
             group-params
             (kvalist-keys->* user 'symbol-name)))))))))

(defun swarmweb-group-handler (httpcon)
  "Handle a specific group page."
  (with-elnode-auth httpcon 'swarmweb-auth
    (let ((user (swarmweb-get-user httpcon))
          (group (swarmweb-group-get (elnode-http-mapping httpcon 1))))
      (elnode-method httpcon
        (GET
         ;; need to add existing groups?
         (elnode-send-file
          httpcon
          (concat swarmweb-dir "group.html")
          :replacements
          (acons
           "groups" (swarmweb-html-list group 'swarmweb-html-a))))))))

(defun swarmweb-main-handler (httpcon)
  "The handler for the main page."
  (if-elnode-auth httpcon 'swarmweb-auth
    (swarmweb-group-create-handler httpcon)
    ;; Else user is not authenticated so send the main file
    (elnode-send-file httpcon (concat swarmweb-dir "main.html"))))

(define-elnode-handler swarmweb-router (httpcon)
  (let ((registered (concat swarmweb-dir "registered.html"))
        (css (concat swarmweb-dir "style.css")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]*//group/$" . swarmweb-group-create-handler)
       ("^[^/]*//group/\\(.+\\)$)" . swarmweb-group-handler)
       ("^[^/]*//profile/" . swarmweb-user-profile-handler)
       ("^[^/]*//register/" . swarmweb-register-handler)
       ("^[^/]*//registered/"
        . ,(elnode-make-send-file
            registered
            :replacements 'swarmweb-get-user-http))
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
