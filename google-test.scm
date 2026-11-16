;;; google-test.scm --- Google Workspace lists, Drive as Dired, request drafts.
;;;
;;; A stub stands in for the transport and writes each call to t--g-calls
;;; as (ACCOUNT METHOD URL PARAMS BODY), newest first. These were
;;; google_scene_test.exs; a key there is its command here, and a y or n
;;; is the answer typed into the prompt.

(require 'google)

(domain! 'testing)
(effects! '(write))

(define t--g-calls '())

(define (t--g-record! a m u p b) (set! t--g-calls (cons (list a m u p b) t--g-calls)))

(define (t--g-default-transport a m u p b k)
  (t--g-record! a m u p b)
  (k '(ok #t data (files ((id "file1" name "First document" mimeType "application/vnd.google-apps.document"))
                   nextPageToken "next"))))

(define (t--g-default-submit a r raw k)
  (google-request a (plist-get r 'service) (plist-get r 'method) (plist-get r 'path)
                  (plist-get r 'params) (plist-get r 'body) k))

(define (t--g-of method) (filter (lambda (c) (equal? (cadr c) method)) t--g-calls))
(define (t--g-last-params) (list-ref (car t--g-calls) 3))
(define (t--g-last-body) (list-ref (car t--g-calls) 4))

(define (t--g-kill-buffers!)
  (for-each (lambda (b) (when (and (string? b) (string-prefix? "*Google" b) (buffer-known? b))
                          (buffer-kill! b)))
            (buffer-list)))

;; THUNK runs in a fresh frame with the stub transports. Whatever a test
;; stubs with set! is put back here.
(define (t--g-with thunk)
  (let ((transport *google-transport*) (submit *google-submit-transport*)
        (http google-http!) (pick google--folder-pick) (client google--client)
        (oauth google-oauth-start!) (tab tab-open))
    (set! t--g-calls '())
    (set! *google-transport* t--g-default-transport)
    (set! *google-submit-transport* t--g-default-submit)
    (with-frame (lambda (frame) (thunk)))
    (minibuffer-cancel!)
    (set! *google-transport* transport)
    (set! *google-submit-transport* submit)
    (set! google-http! http)
    (set! google--folder-pick pick)
    (set! google--client client)
    (set! google-oauth-start! oauth)
    (set! tab-open tab)
    (set! google-default-account "")
    (t--g-kill-buffers!)))

(define (t--g-answer! . answers)
  (for-each (lambda (a) (minibuffer-input! a) (minibuffer-confirm!)) answers))

;; a y-or-n prompt takes the letter as it is typed
(define (t--g-y-or-n! letter) (minibuffer-change! letter))

;;; --- connecting -----------------------------------------------------------------

(deftest 'google-connect-opens-the-consent-url-through-the-browser-api
  "after a mode restore the command still reaches tab-open"
  (lambda ()
    (t--g-with
      (lambda ()
        (let ((opened #f))
          (set! google--client (lambda () "test-client"))
          (set! google-oauth-start! (lambda (client scopes)
                                      '(ok #t url "https://accounts.google.com/o/oauth2/v2/auth?test=1")))
          (set! tab-open (lambda (url) (set! opened url)))
          (run-command "google")
          (minibuffer-cancel!)
          (with-current-buffer "*Google*"
            (lambda ()
              (set-mode! "google-mode")
              (run-command "google-connect")))
          (check-equal! opened "https://accounts.google.com/o/oauth2/v2/auth?test=1" "the consent url"))))))

;;; --- headless file functions ----------------------------------------------------

(deftest 'headless-file-functions-keep-account-pagination-and-fresh-move-parents
  "no prompt, no buffer change; each call names its account"
  (lambda ()
    (t--g-with
      (lambda ()
        (set! google-http! (lambda (a m u p b)
                             (t--g-record! a m u p b)
                             '(ok #t status 200 data (id "file1" name "Document"
                                   mimeType "application/vnd.google-apps.document" parents ("old-parent")))))
        (let ((source (current-buffer)))
          (google-files "alpha" "root" "O'Reilly" "page2" "docs")
          (check-equal! (plist-get (t--g-last-params) 'pageToken) "page2" "the page")
          (check-contains! (plist-get (t--g-last-params) 'q) "application/vnd.google-apps.document" "the type")
          (google-file-move! "beta" "file1" "new-parent")
          (check-equal! (car (car t--g-calls)) "beta" "the account")
          (check-equal! (plist-get (t--g-last-params) 'removeParents) "old-parent" "the fresh parent")
          (google-file-copy! "alpha" "file1" "root" "Copy")
          (check-equal! (plist-get (t--g-last-body) 'name) "Copy" "copy")
          (google-file-rename! "alpha" "file1" "Renamed")
          (check-equal! (plist-get (t--g-last-body) 'name) "Renamed" "rename")
          (google-folder-create! "alpha" "root" "Folder")
          (check-equal! (plist-get (t--g-last-body) 'parents) '("root") "mkdir")
          (google-file-trash! "alpha" "file1")
          (check-equal! (plist-get (t--g-last-body) 'trashed) #t "trash")
          (check-equal! (current-buffer) source "the buffer did not change")
          (check-equal! (minibuffer-input) "" "no prompt")
          (check-contains! (value->string (plist-get (catalog-entry 'function "google-file-trash!") 'effects)) "destroy"
                       "trash is stamped destroy"))))))

(deftest 'headless-move-stops-on-lookup-failure-and-skips-a-same-parent-file
  "a failed lookup makes no write; a file already there is left alone"
  (lambda ()
    (t--g-with
      (lambda ()
        (set! google-http! (lambda (a m u p b) (t--g-record! a m u p b)
                             '(ok #f status 403 error "Permission denied")))
        (check-false! (plist-get (google-file-move! "alpha" "file1" "dest") 'ok) "the move fails")
        (check-equal! (length t--g-calls) 1 "after the lookup only")
        (set! google-http! (lambda (a m u p b) (t--g-record! a m u p b)
                             '(ok #t data (name "File" parents ("dest")))))
        (google-file-move! "alpha" "file1" "dest")
        (check-equal! (length (t--g-of "PATCH")) 0 "no write")))))

;;; --- lists ----------------------------------------------------------------------

(deftest 'google-open-makes-account-owned-lists-that-restore-their-mode
  "two accounts, two buffers; a restore keeps the owner"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-open "alpha" "docs")
        (let ((first (current-buffer)))
          (check-contains! (buffer-text first) "First document" "the rows")
          (check-equal! (buffer-local-value 'google-account first) "alpha" "alpha owns it")
          (google-open "beta" "docs")
          (let ((second (current-buffer)))
            (check-false! (equal? first second) "a second buffer")
            (check-equal! (buffer-local-value 'google-account first) "alpha" "the first keeps alpha")
            (check-equal! (buffer-local-value 'google-account second) "beta" "beta owns the second")
            (with-current-buffer second (lambda () (set-mode! "google-service-mode")))
            (check-equal! (buffer-local-value 'google-account second) "beta" "a restore keeps beta")
            (check-contains! (buffer-text second) "First document" "and the rows")))))))

(deftest 'drive-opens-as-a-directory-and-visit-and-up-keep-account-and-folder
  "Drive is Dired with a provider; the folder is the buffer's"
  (lambda ()
    (t--g-with
      (lambda ()
        (set! *google-transport*
              (lambda (a m u p b k)
                (t--g-record! a m u p b)
                (k '(ok #t data (files ((id "folder1" name "Projects" mimeType "application/vnd.google-apps.folder")))))))
        (google-open "alpha" "drive")
        (let ((root (current-buffer)))
          (check-equal! (buffer-local-value 'mode-name root) "Dired" "Dired")
          (check-contains! (buffer-text root) "Projects/" "the folder row")
          (check-contains! (plist-get (t--g-last-params) 'q) "'root' in parents" "the root query")
          (run-command "dired-visit")
          (let ((child (current-buffer)))
            (check-equal! (buffer-local-value 'google-parent child) "folder1" "the child folder")
            (check-equal! (buffer-local-value 'google-account child) "alpha" "the account")
            (check-contains! (buffer-text child) "My Drive/Projects" "the path")
            (set-mode! "google-drive-mode")
            (run-command "dired-up")
            (check-equal! (current-buffer) root "up is the root"))
          (run-command "list-mark")
          (run-command "google-drive-trash")
          (t--g-y-or-n! "y")
          (check-equal! (length (t--g-of "PATCH")) 1 "one write")
          (check-equal! (plist-get (list-ref (car (t--g-of "PATCH")) 4) 'trashed) #t "a trash")
          (check-equal! (buffer-local-value 'google-account root) "alpha" "the account stays"))))))

(deftest 'every-google-file-index-is-dired-and-copies-through-a-folder-picker
  "the copy waits for the folder and the yes"
  (lambda ()
    (t--g-with
      (lambda ()
        (for-each (lambda (service)
                    (google-open "alpha" service)
                    (check-equal! (buffer-local-value 'mode-name (current-buffer)) "Dired" service))
                  '("drive" "docs" "sheets" "slides" "forms" "script"))
        (google-open "alpha" "docs")
        (let ((source (current-buffer)))
          (run-command "list-mark")
          (run-command "dired-copy")
          (minibuffer-confirm!)
          (check-equal! (length (t--g-of "POST")) 0 "not before the yes")
          (t--g-y-or-n! "y")
          (check-equal! (length (t--g-of "POST")) 1 "one copy")
          (check-equal! (list-ref (car (t--g-of "POST")) 2)
                        "https://www.googleapis.com/drive/v3/files/file1/copy" "of file1")
          (check-equal! (buffer-local-value 'google-account source) "alpha" "the account stays"))))))

(deftest 'rename-asks-through-prompts-and-move-updates-the-drive-parents
  "rename patches the name; a move adds and removes parents"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-open "alpha" "sheets")
        (let ((source (current-buffer)))
          (run-command "dired-rename")
          (t--g-answer! "Rename" "Renamed sheet")
          (t--g-y-or-n! "y")
          (check-equal! (plist-get (list-ref (car (t--g-of "PATCH")) 4) 'name) "Renamed sheet" "renamed")
          (set! google--folder-pick (lambda (account k) (k "destination" "Projects")))
          (google--file-transfer (current-buffer) '((id "file1" name "Sheet" parents ("old-folder"))) #f)
          (t--g-y-or-n! "y")
          (check-equal! (plist-get (list-ref (car (t--g-of "PATCH")) 3) 'addParents) "destination" "added")
          (check-equal! (plist-get (list-ref (car (t--g-of "PATCH")) 3) 'removeParents) "old-folder" "removed")
          (check-equal! (buffer-local-value 'google-account source) "alpha" "the account stays"))))))

(deftest 'a-cancelled-trash-makes-no-request-and-a-failed-one-keeps-its-flag
  "no means nothing; a failure leaves the flag for a retry"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-open "alpha" "slides")
        (let ((source (current-buffer)))
          (run-command "google-drive-flag")
          (run-command "google-drive-trash")
          (t--g-y-or-n! "n")
          (check-equal! (length (t--g-of "PATCH")) 0 "no request")
          (set! *google-transport* (lambda (a m u p b k) (k '(ok #f error "Permission denied"))))
          (run-command "google-drive-trash")
          (t--g-y-or-n! "y")
          (check-equal! (list-marked (current-buffer) "D") '("file1") "the flag stays")
          (check-false! (buffer-local-value 'google-file-busy source) "and the buffer is free"))))))

(deftest 'google-dired-has-an-unmarkable-parent-row-and-reapplies-its-mode
  "the .. row goes up to Drive and takes no mark"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-open "alpha" "docs")
        (let ((buf (current-buffer)))
          (check-equal! (buffer-local-value 'mode-name buf) "Dired" "Dired")
          (check-contains! (buffer-text buf) ".." "the parent row")
          (set-mode! "Dired")
          (list-goto-index! (current-buffer) 0)
          (check-equal! (plist-get (list-current (current-buffer)) 'google-up) #t "row 0 is up")
          (run-command "list-mark")
          (check-equal! (list-marks (current-buffer)) '() "no mark")
          (list-goto-index! (current-buffer) 0)
          (run-command "dired-visit")
          (check-equal! (buffer-local-value 'google-service (current-buffer)) "drive" "Drive")
          (check-equal! (buffer-local-value 'mode-name (current-buffer)) "Dired" "as Dired"))))))

(deftest 'google-directories-stay-in-one-window-and-quit-walks-back
  "dired-quit pops the window's history and kills nothing"
  (lambda ()
    (t--g-with
      (lambda ()
        (buffer-create "*Google origin*")
        (switch-to-buffer-here! "*Google origin*")
        (set-window-prev-buffers! (active-window) '())
        (let ((window (active-window))
              (windows (length (window-list))))
          (google-open "alpha" "drive")
          (let ((root (current-buffer)))
            (google-open "alpha" "drive" "child")
            (let ((child (current-buffer)))
              (check-equal! (active-window) window "the same window")
              (check-equal! (length (window-list)) windows "no new window")
              (run-command "dired-quit")
              (check-equal! (current-buffer) root "back to the root")
              (run-command "dired-quit")
              (check-equal! (current-buffer) "*Google origin*" "back to the origin")
              (check-equal! (active-window) window "in the same window")
              (check-true! (buffer-exists? root) "the root lives")
              (check-true! (buffer-exists? child) "the child lives"))))))))

(deftest 'filesystem-dired-still-navigates-without-a-provider
  "the provider is Google's; a plain directory goes up as before"
  (lambda ()
    (let ((dir (string-append (compos-home) "/zz-google-dired")))
      (shell-command->string (string-append "rm -rf " dir))
      (make-directory! (string-append dir "/child"))
      (t--g-with
        (lambda ()
          (dired-open dir)
          (check-equal! (buffer-local-value 'mode-name (current-buffer)) "Dired" "Dired")
          (check-false! (buffer-local-value 'dired-provider (current-buffer)) "no provider")
          (list-goto-index! (current-buffer) 0)
          (run-command "dired-up")
          (check-false! (equal? (buffer-local-value 'dired-dir (current-buffer)) dir) "up")))
      (for-each (lambda (b) (when (and (string? b) (string-prefix? dir b)) (buffer-kill! b)))
                (buffer-list))
      (shell-command->string (string-append "rm -rf " dir)))))

(deftest 'search-escapes-drive-literals-and-pagination-reaches-the-api
  "a quote and a backslash are escaped; the page token travels"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-open "alpha" "docs")
        (buffer-set-local! (current-buffer) 'google-query "O'Reilly\\notes")
        (run-command "google-refresh")
        (check-contains! (plist-get (t--g-last-params) 'q) "O\\'Reilly" "escaped")
        (run-command "google-next-page")
        (check-equal! (plist-get (t--g-last-params) 'pageToken) "next" "the next page")
        (run-command "google-first-page")
        (check-false! (plist-get (t--g-last-params) 'pageToken) "the first page")))))

(deftest 'a-failed-refresh-keeps-the-rows-and-shows-the-error
  "the old rows stay under the error"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-open "alpha" "docs")
        (let ((buf (current-buffer)))
          (set! *google-transport* (lambda (a m u p b k) (k '(ok #f error "Permission denied"))))
          (run-command "google-refresh")
          (check-contains! (buffer-text buf) "First document" "the rows")
          (check-contains! (buffer-text buf) "Permission denied" "the error"))))))

;;; --- requests -------------------------------------------------------------------

(deftest 'request-drafts-keep-their-account-and-never-run-on-restore
  "a restore does not submit; a submit asks, then goes as the draft's account"
  (lambda ()
    (t--g-with
      (lambda ()
        (google-draft "alpha" (car google--operations))
        (let* ((buf (current-buffer))
               (request (json-parse (buffer-text buf))))
          (check-equal! (plist-get request 'service) "docs" "the service")
          (check-equal! (plist-get (plist-get request 'body) 'title) "Untitled" "the body")
          (set! google-default-account "beta")
          (set-mode! "google-request-mode")
          (check-equal! t--g-calls '() "a restore runs nothing")
          (run-command "google-submit")
          (check-equal! t--g-calls '() "not before the yes")
          (t--g-y-or-n! "y")
          (check-equal! (car (car t--g-calls)) "alpha" "as alpha"))))))

(deftest 'mail-drafts-encode-unicode-and-keep-header-injection-out
  "a line break in To does not start a header"
  (lambda ()
    (let* ((op (google-mail-draft "alpha" "you@example.com\r\nBcc:bad@example.com" "Hello 🌏" "Unicode body λ"))
           (mime (google--base64url-decode (plist-get (plist-get (plist-get op 'body) 'message) 'raw))))
      (check-false! (string-contains? mime "\r\nBcc:") "no Bcc header")
      (check-contains! mime "Content-Type: text/plain; charset=UTF-8" "the type")
      (check-contains! mime (base64-encode "Unicode body λ") "the body")
      (check-equal! (plist-get op 'path) "/users/me/drafts" "the path"))))

(deftest 'gmail-metadata-keeps-message-order-when-replies-come-out-of-order
  "the rows wait for every reply and keep the list order"
  (lambda ()
    (t--g-with
      (lambda ()
        (let ((pending '()) (rows #f))
          (set! *google-transport* (lambda (a m u p b k) (set! pending (cons k pending))))
          (google--mail-rows "alpha" '((id "first") (id "second")) (lambda (r) (set! rows r)))
          ((car pending) '(ok #t data (payload (headers ((name "Subject" value "Second message")
                                                           (name "From" value "second@example.com"))))))
          (check-false! rows "not yet")
          ((cadr pending) '(ok #t data (payload (headers ((name "Subject" value "First message")
                                                            (name "From" value "first@example.com"))))))
          (check-equal! (map google--row-title rows) '("First message" "Second message") "in order")
          (check-equal! (map google--row-detail rows) '("first@example.com" "second@example.com") "with their senders"))))))

(deftest 'every-template-names-a-real-service-and-the-api-is-catalogued
  "an operation draft always has somewhere to go"
  (lambda ()
    (check-true! (pair? google--operations) "there are templates")
    (check-equal! (filter (lambda (op) (not (google--service (plist-get op 'service)))) google--operations) '()
                  "each names a service")
    (check-equal! (plist-get (catalog-entry 'function "google-request") 'domain) "google" "the domain")))

(deftest 'a-later-search-wins-when-older-requests-finish-last
  "a stale reply is dropped"
  (lambda ()
    (t--g-with
      (lambda ()
        (let ((pending '()))
          (set! *google-transport* (lambda (a m u p b k) (set! pending (cons k pending))))
          (google-open "alpha" "docs")
          (let ((buf (current-buffer)))
            (buffer-set-local! buf 'google-query "new search")
            (run-command "google-refresh")
            ((car pending) '(ok #t data (files ((id "new" name "New result")))))
            ((cadr pending) '(ok #t data (files ((id "old" name "Stale result")))))
            (check-contains! (buffer-text buf) "New result" "the new rows")
            (check-false! (string-contains? (buffer-text buf) "Stale result") "not the stale ones")))))))

(deftest 'the-selected-document-fills-the-edit-request
  "the id and the revision come from the document on screen"
  (lambda ()
    (t--g-with
      (lambda ()
        (google--show-result "alpha" "docs" "Document" '(documentId "doc-123" revisionId "revision-7"))
        (let ((op (google--context-operation (cadr google--operations) (current-buffer))))
          (check-equal! (plist-get op 'path) "/documents/doc-123:batchUpdate" "the path")
          (check-equal! (plist-get (plist-get (plist-get op 'body) 'writeControl) 'requiredRevisionId)
                        "revision-7" "the revision"))))))

(deftest 'mail-contacts-tasks-and-calendars-have-their-own-list-contracts
  "each service lists from its own path and key"
  (lambda ()
    (for-each
      (lambda (c)
        (buffer-create "*Google contracts*")
        (buffer-set-local! "*Google contracts*" 'google-service (car c))
        (buffer-set-local! "*Google contracts*" 'google-parent (cadr c))
        (let ((spec (google--list-spec "*Google contracts*")))
          (check-equal! (plist-get spec 'path) (caddr c) (car c))
          (check-equal! (plist-get spec 'key) (cadddr c) (car c))))
      '(("gmail" #f "/users/me/messages" messages)
        ("contacts" #f "/people/me/connections" connections)
        ("tasks" #f "/users/@me/lists" items)
        ("tasks" "list-a" "/lists/list-a/tasks" items)
        ("calendar" #f "/users/me/calendarList" items)
        ("calendar" "a@example.com" "/calendars/a%40example.com/events" items)))
    (buffer-kill! "*Google contracts*")))
