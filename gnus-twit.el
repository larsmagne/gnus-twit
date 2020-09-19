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

;; TODO: Ignore registry?

(require 'dom)
(require 'gnus-group)
(require 'seq)

(defgroup gnus-twit nil
  "Read a Twitter thread in Gnus."
  :group 'gnus)

(defcustom gnus-twit-nitter-instance "https://nitter.net/"
  "URL of the Nitter instance to use."
  :group 'gnus-twit
  :type '(radio (const :tag "Nitter.net" "https://nitter.net/")
                (string :tag "URL to a Nitter instance")))

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

(defun gnus-twit-expand-relative (maybe-relative)
  (when maybe-relative
    (let ((url (url-generic-parse-url maybe-relative))
          (base (url-generic-parse-url gnus-twit-nitter-instance)))
      (if (url-host url)
          maybe-relative
        (setf (url-filename base)
              (concat (string-trim-right (car (url-path-and-query base)) "/+")
                      "/"
                      (string-trim-left (url-filename url) "/+")))
        (url-recreate-url base)))))

(defun gnus-twit-fix-url (url)
  (replace-regexp-in-string "https?://.*?/"
                            (gnus-twit-expand-relative "/") url))

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

(defun gnus-twit-retrieve (url)
  (if (url-is-cached url)
      (url-fetch-from-cache url)
    (with-current-buffer (url-retrieve-synchronously url t)
      (url-store-in-cache (current-buffer))
      (current-buffer))))

(defun gnus-twit-build (url)
  (setq url (gnus-twit-fix-url url))
  (let ((threads (make-hash-table :test #'equal)))
    (with-current-buffer (gnus-twit-retrieve url)
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
        (gnus-twit-build (gnus-twit-expand-relative (car tweets)))))))

(defun gnus-twit-build-main (dom threads)
  (let* ((main (dom-by-class dom "main-tweet"))
         (data (list :text `(div nil
                                 ,(dom-by-class main "tweet-content")
                                 ,(dom-by-class main "card-container"))
                     :date (dom-attr
                            (dom-by-tag (dom-by-class main "tweet-date") 'a)
                            'title)
                     :from (dom-attr (dom-by-class main "^fullname$") 'title)
                     :user-name (dom-attr
                                 (dom-by-class main "^username$") 'title)
                     :url (dom-attr
                            (dom-by-tag (dom-by-class main "tweet-date") 'a)
                            'href)
                     :avatar
                     (gnus-twit-expand-relative
                      (dom-attr
                       (dom-by-tag (dom-by-class main "avatar") 'img)
                       'src))
                     :replies nil))
         (show-mores (dom-by-tag (dom-by-class dom "show-more") 'a))
         (next-page
          (when (string-match "Load more" (dom-text (last show-mores)))
            (dom-attr (last show-mores) 'href)))
         (is-continuation (string-match "Load newest"
                                        (dom-text (car show-mores))))
         ;; "After tweets" are replies by the author to
         ;; split long text into consecutive tweets.
         ;; TODO: Do these also get truncated/paginated
         ;; at some point?
         (after-tweets
          ;; Ignore after tweets on continuation pages, as they have been
          ;; processed already.
          (unless is-continuation
            (dom-by-class (dom-by-class dom "after-tweet") "tweet-link")))
         (replies
          (cl-loop with replies
                   for tweet in (append
                                 after-tweets
                                 ;; "reply" divs also contain timelines, but
                                 ;; only the first link (reply itself) is
                                 ;; extracted below.
                                 (dom-by-class dom (rx "reply" eow)))
		   for reply = (dom-attr
				(car (dom-by-class tweet "tweet-link"))
				'href)
		   when reply
                   do (push reply replies)
                   finally (return (nreverse replies))))
         (status (gnus-twit-status (plist-get data :url)))
         (data (gethash status threads data)))
    ;; Expand relative URLs.
    (cl-loop
     for (tag . attr) in '((a . href) (img . src))
     do (dolist (elem (dom-by-tag (plist-get data :text) tag))
          (dom-set-attribute
           elem attr (gnus-twit-expand-relative (dom-attr elem attr)))))
    (setf (gethash status threads) data)
    (plist-put data :replies (append
                              (when is-continuation
                                (plist-get data :replies))
                              (mapcar #'gnus-twit-status replies)))
    (when next-page
      (push
       (setq next-page
             (concat
              (car (url-path-and-query
                    (url-generic-parse-url (plist-get data :url))))
              "?"
              (cdr
               (url-path-and-query
                (url-generic-parse-url next-page)))))
       replies))
    (dolist (reply replies)
      (message "Fetching %s" (gnus-twit-expand-relative reply))
      (unless (and (gethash (gnus-twit-status reply) threads)
                   (not (string-equal reply next-page)))
        (with-current-buffer (gnus-twit-retrieve
			      (gnus-twit-expand-relative reply))
          (let ((buffer (current-buffer)))
            (goto-char (point-min))
            (when (re-search-forward "\r?\n\r?\n" nil t)
              (gnus-twit-build-main
               (libxml-parse-html-region (point) (point-max))
               threads))
            (kill-buffer buffer)))))
    threads))

(defun gnus-twit-tweets (dom)
  (cl-loop for tweet in (dom-by-class dom "timeline-item")
           collect (or (dom-attr
                        (dom-by-tag (dom-by-class tweet "tweet-date") 'a)
                        'href)
                       (dom-attr (dom-by-class tweet "tweet-link")
                                 'href))))

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
      ;; There should not be any multibyte characters in the buffer at this
      ;; point.  As `encode-coding-region' converts 8-bit bytes to their
      ;; multibyte representation in multibyte buffers, switch to unibyte mode
      ;; temporarily, to make `quoted-printable-encode-region' work (?).
      ;; For example `’' should be encoded as `=E2=80=99', not
      ;; `=3FFFE2=3FFF80=3FFF99'.
      (set-buffer-multibyte nil)
      (quoted-printable-encode-region start (point))
      (set-buffer-multibyte 'to))
    (insert "\n\n")
    (insert (format "<p><img src=\"%s\">\n"
                    (plist-get data :avatar)))
    (insert (format "<p><a href=\"https://twitter.com%s\">[Link]</a>\n\n"
                    (plist-get data :url)))
    (insert "\n\n\n")
    (dolist (reply (plist-get data :replies))
      (gnus-twit-make-mbox-1 reply threads status))))

(defun gnus-twit-format-date (string)
  (if (string-match
       (rx (group (+ num)) "/" (group (+ num)) "/" (group (+ num)) ", "
           (group (+ num)) ":" (group (+ num)) ":" (group (+ num)))
                    string)
      (message-make-date
       (encode-time
        (make-decoded-time :day (string-to-number (match-string 1 string))
                           :month (string-to-number (match-string 2 string))
                           :year (string-to-number (match-string 3 string))
                           :hour (string-to-number (match-string 4 string))
                           :minute (string-to-number (match-string 5 string))
                           :second (string-to-number (match-string 6 string)))))
    string))

(provide 'gnus-twit)

;;; gnus-twit.el ends here
