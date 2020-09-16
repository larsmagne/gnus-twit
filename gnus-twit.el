;;; gnus-twit.el --- Read a Twitter thread in Gnus -*- lexical-binding: t -*-
;; Copyright (C) 2020 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: idiocy

;; gnus-twit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; gnus-twit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;; Usage: M-x gnus-group-twitter RET https://twitter.com/FiveThirtyEight/status/1299178217983619073 RET

;; Then weep.

(require 'dom)
(require 'gnus-group)
(require 'seq)

(defun gnus-group-twitter (url)
  "Visit an ephemeral Twitter group based on URL."
  (interactive "sTwitter URL: ")
  (let ((tmpfile (make-temp-file "gnus-temp-group-")))
    (unwind-protect
        ;; Add the debbugs address so that we can respond to reports easily.
        (let ((status (gnus-twit-status url))
              (threads (gnus-twit-build url)))
          (gnus-twit-make-mbox status threads tmpfile)
          (gnus-group-read-ephemeral-group
           (concat "nndoc+ephemeral:twit#" status)
           `(nndoc ,tmpfile
                   (nndoc-article-type mbox))))
      (delete-file tmpfile))))

(defun gnus-twit-fix-url (url)
  (replace-regexp-in-string "https?://.*?/" "https://mobile.twitter.com/" url))

(defun gnus-twit-pp (url)
  (setq url (gnus-twit-fix-url url))
  (dom-pp
   (with-current-buffer (url-retrieve-synchronously url)
     (goto-char (point-min))
     (prog1
         (when (re-search-forward "\r?\n\r?\n" nil t)
           (libxml-parse-html-region (point) (point-max)))
       (kill-buffer (current-buffer))))
   t))


(defun gnus-twit-build (url)
  (setq url (gnus-twit-fix-url url))
  (let ((threads (make-hash-table :test #'equal)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (prog1
          (when (re-search-forward "\r?\n\r?\n" nil t)
            (gnus-twit-check url
                             (libxml-parse-html-region (point) (point-max))
                             threads))
        (kill-buffer (current-buffer))))))

(defun gnus-twit-check (url dom threads)
  (let ((tweets (gnus-twit-tweets dom)))
    (when tweets
      (if (equal (gnus-twit-status url) (gnus-twit-status (car tweets)))
          ;; We have the data for the first tweet in the thread; start
          ;; building.
          (gnus-twit-build-main dom threads)
        (gnus-twit-build (concat "https://mobile.twitter.com" (car tweets)))))))

(defun gnus-twit-build-main (dom threads)
  (let* ((main (dom-by-class dom "main-tweet"))
         (data (list :text `(div nil
                                 ,(dom-by-class main "tweet-text")
                                 ,(dom-by-class main "card-photo")
                                 ,(dom-by-class main "card-summary"))
                     :date (string-trim
                            (dom-texts
                             (dom-by-class (dom-by-class main "tweet-content")
                                           "metadata")))
                     :from (string-trim
                            (dom-texts (dom-by-class main "fullname")))
                     :user-name (string-trim
                                 (dom-texts (dom-by-class main "username")))
                     :url (replace-regexp-in-string
                           "/actions.*" ""
                           (dom-attr
                            (dom-by-tag (dom-by-class main "action-last") 'a)
                            'href))
                     :avatar (dom-attr
                              (dom-by-tag (dom-by-class main "avatar") 'img)
                              'src)
                     :replies nil))
         (replies
          (cl-loop with replies
                   for tweet in (dom-by-tag (dom-by-class dom "replies") 'table)
                   when (or (null replies)
                            (not (equal (dom-text
                                         (dom-by-class main "username"))
                                        (plist-get data :user-name))))
                   do (push (dom-attr tweet 'href) replies)
                   finally (return (nreverse replies)))))
    (setf (gethash (gnus-twit-status (plist-get data :url)) threads)
          data)
    (plist-put data :replies (mapcar #'gnus-twit-status replies))
    (dolist (reply replies)
      (message "Fetching %s" (concat "https://mobile.twitter.com" reply))
      (unless (gethash (gnus-twit-status reply) threads)
        (with-current-buffer (url-retrieve-synchronously
                              (concat "https://mobile.twitter.com" reply)
                              t)
          (let ((buffer (current-buffer)))
            (goto-char (point-min))
            (when (re-search-forward "\r?\n\r?\n" nil t)
              (gnus-twit-build-main
               (libxml-parse-html-region (point) (point-max))
               threads))
            (kill-buffer buffer)))))
    threads))

(defun gnus-twit-tweets (dom)
  (cl-loop for tweet in (dom-by-tag dom 'table)
           when (string-match "\\`tweet\\|\\`main-tweet"
                              (or (dom-attr tweet 'class) ""))
           collect (replace-regexp-in-string
                    "\\?.*" ""
                    (or (dom-attr tweet 'href)
                        (dom-attr (dom-by-tag
                                   (dom-by-class tweet "action-last") 'a)
                                  'href)))))

(defun gnus-twit-status (url)
  (and (string-match "/status/\\([0-9]+\\)" url)
       (match-string 1 url)))


(defun gnus-twit-make-mbox (status threads file)
  (setq threads (copy-hash-table threads))
  (with-temp-buffer
    (gnus-twit-make-mbox-1 status threads nil)
    (write-region (point-min) (point-max) file)))

(defun gnus-twit-shorten (string)
  (if (> (length string) 63)
      (concat (substring string 0 60) "...")
    string))

(defun gnus-twit-make-mbox-1 (status threads parent)
  (when-let ((data (gethash status threads)))
    (remhash status threads)
    (insert (format "From twit@localhost Wed Aug 12 21:34:56 2020\n"))
    (insert (format "From: %s <%s@twitter.com>\n"
                    (mail-encode-encoded-word-string (plist-get data :from))
                    (replace-regexp-in-string "[@ ]+" ""
                                              (plist-get data :user-name))))
    (insert (format "Subject: %s\n"
                    (let ((rfc2047-encoding-type 'mime))
                      (rfc2047-encode-string
                       (gnus-twit-shorten
                        (string-trim
                         (replace-regexp-in-string
                          "[ \t\n]+" " "
                          (dom-texts (plist-get data :text)))))))))
    (insert (format "Message-ID: <%s@twitter.com>\n" status))
    (when parent
      (insert (format "References: <%s@twitter.com>\n" parent)))
    (insert (format "Date: %s\n" (gnus-twit-format-date
                                  (plist-get data :date))))
    (insert "Content-Transfer-Encoding: quoted-printable\n")
    (insert "Content-Type: text/html; charset=utf-8\n\n")
    (let ((start (point)))
      (dom-print (plist-get data :text))
      (encode-coding-region start (point) 'utf-8)
      (quoted-printable-encode-region start (point)))
    (insert "\n\n")
    (insert (format "<p><img src=\"%s\">\n"
                    (plist-get data :avatar)))
    (insert (format "<p><a href=\"https://twitter.com%s\">[Link]</a>\n\n"
                    (plist-get data :url)))
    (insert "\n\n\n")
    (dolist (reply (plist-get data :replies))
      (gnus-twit-make-mbox-1 reply threads status))))

(defvar gnus-twit-months
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun gnus-twit-format-date (string)
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\) +\\([AP]M\\)[- ]+\\([0-9]+\\) +\\([A-Za-z]+\\) +\\([0-9]+\\)"
                    string)
      (message-make-date
       (encode-time
        (make-decoded-time :second 0
                           :minute (string-to-number (match-string 2 string))
                           :hour (+ (string-to-number (match-string 1 string))
                                    (if (equal (match-string 3 string) "PM")
                                        (if (equal (match-string 1 string) "12")
                                            0
                                          12)
                                      (if (equal (match-string 1 string) "12")
                                          -12
                                        12)))
                           :day (string-to-number (match-string 4 string))
                           :year (string-to-number (match-string 6 string))
                           :month (1+ (seq-position gnus-twit-months
                                                    (match-string 5 string)
                                                    #'equal)))))
    string))

(provide 'gnus-twit)

;;; gnus-twit.el ends here
