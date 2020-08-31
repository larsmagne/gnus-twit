;;; nntwit.el --- Read Twitter -*- lexical-binding: t -*-

(defun nntwit-fix-url (url)
  (replace-regexp-in-string "https?://.*?/" "https://mobile.twitter.com/" url))

(defun nntwit-pp (url)
  (setq url (nntwit-fix-url url))
  (dom-pp
   (with-current-buffer (url-retrieve-synchronously url)
     (goto-char (point-min))
     (prog1
	 (when (re-search-forward "\r?\n\r?\n" nil t)
	   (libxml-parse-html-region (point) (point-max)))
       (kill-buffer (current-buffer))))
   t))


(defun nntwit-build (url)
  (setq url (nntwit-fix-url url))
  (let ((threads (make-hash-table :test #'equal)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (prog1
	  (when (re-search-forward "\r?\n\r?\n" nil t)
	    (nntwit-check url
			  (libxml-parse-html-region (point) (point-max))
			  threads))
	(kill-buffer (current-buffer))))))

(defun nntwit-check (url dom threads)
  (let ((tweets (nntwit-tweets dom)))
    (when tweets
      (if (equal (nntwit-status url) (nntwit-status (car tweets)))
	  ;; We have the data for the first tweet in the thread; start
	  ;; building.
	  (nntwit-build-main dom threads)
	(nntwit-build (concat "https://mobile.twitter.com" (car tweets)))))))

(defun nntwit-build-main (dom threads)
  (let* ((main (dom-by-class dom "main-tweet"))
	 (data (list :text (dom-by-class main "tweet-text")
		     :date (dom-text
			    (dom-by-class (dom-by-class main "tweet-content")
					  "metadata"))
		     :from (string-trim
			    (dom-texts (dom-by-class main "fullname")))
		     :user-name (string-trim
				 (dom-texts (dom-by-class main "username")))
		     :url (dom-attr
			   (dom-by-tag (dom-by-class main "action-last") 'a)
			   'href)
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
		   finally (return replies))))
    (setf (gethash (nntwit-status (plist-get data :url)) threads)
	  data)
    (plist-put data :replies (mapcar #'nntwit-status replies))
    (dolist (reply replies)
      (message "Fetching %s" (concat "https://mobile.twitter.com" reply))
      (unless (gethash (nntwit-status reply) threads)
	(with-current-buffer (url-retrieve-synchronously
			      (concat "https://mobile.twitter.com" reply)
			      t)
	  (let ((buffer (current-buffer)))
	    (goto-char (point-min))
	    (when (re-search-forward "\r?\n\r?\n" nil t)
	      (nntwit-build-main
	       (libxml-parse-html-region (point) (point-max))
	       threads))
	    (kill-buffer buffer)))))
    threads))

(defun nntwit-tweets (dom)
  (cl-loop for tweet in (dom-by-tag dom 'table)
	   when (string-match "\\`tweet\\|\\`main-tweet"
			      (or (dom-attr tweet 'class) ""))
	   collect (replace-regexp-in-string
		    "\\?.*" ""
		    (or (dom-attr tweet 'href)
			(dom-attr (dom-by-tag
				   (dom-by-class tweet "action-last") 'a)
				  'href)))))
    
(defun nntwit-status (url)
  (and (string-match "/status/\\([0-9]+\\)" url)
       (match-string 1 url)))

