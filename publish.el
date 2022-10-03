(require 'package)
(package-initialize)
(setq package-archives '(
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ))
(package-refresh-contents)
(package-install 'org-contrib)
(package-install 'htmlize)
(package-install 'dash)

(require 'org)
(require 'ox-publish)
;(require 'ox-rss) ;there's no ox-rss.el in org-contrib
(require 'dash)

;; Set default settings
(setq org-export-with-inlinetasks nil
      org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-statistics-cookies nil
      org-export-with-toc nil
      org-export-with-tasks nil
      org-export-html-style-include-default nil
      org-export-with-sub-superscripts nil
      org-export-html-head-include-default-style nil
      org-export-html-head-include-scripts nil)
;; HTML settings
(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-html-doctype "html5")

(setq make-backup-files nil)

(defvar site-attachments
  (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                "ico" "cur" "css" "js" "woff" "html" "pdf"))
  "File types that are published as static files.")

(defvar blog-exclude-files
  (regexp-opt '("index.org" "rss.org"))
  "File that are not exported in .blog. .")

(setq postamble (with-temp-buffer
                  (insert-file-contents "templates/postamble.html")
                  (buffer-string)))
(setq preamble (with-temp-buffer
                  (insert-file-contents "templates/preamble.html")
                  (buffer-string)))
(setq header (with-temp-buffer
                  (insert-file-contents "templates/header.html")
		  (buffer-string)))
(setq disqus (with-temp-buffer
                  (insert-file-contents "templates/disqus.html") ;; 设置 评论平台
		  (buffer-string)))
(setq blogmeta (with-temp-buffer
                  (insert-file-contents "templates/blog.org") ;;设置 blog index 页面部分
		  (buffer-string)))

(defvar my-website-url "https://example.com")
(defvar my-blog-url "https://example.com/blog")

(defun lujok/blog-index (title list)
  (mapconcat
   'identity
   (list
    (concat "#+TITLE: " title "\n" blogmeta)
    (org-list-to-subtree list nil '(:istart "** ")))
   "\n\n"))

(defun lujok/sitemap-format-entry (entry style project)
  (format "
    [[file:%s][%s]]
    #+begin_article-info
    #+begin_date
    %s
    #+end_date
    #+begin_tags
    %s
    #+end_tags
    #+end_article-info
    #+begin_description
    %s
    #+end_description"
	    entry
            (org-publish-find-title entry project)
	    (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	    (org-publish-find-property entry :keywords project 'html)
	    (org-publish-find-property entry :description project 'html)))

(defun lujok/org-publish-sitemap--valid-entries (entries)
  "Filter ENTRIES that are not valid or skipped by the sitemap entry function."
  (-filter (lambda (x) (car x)) entries))

(defun lujok/latest-posts-sitemap-function (title sitemap)
  "posts.org generation. Only publish the latest 6 posts from SITEMAP (https://orgmode.org/manual/Sitemap.html).  Skips TITLE."
  (let* ((posts (cdr sitemap))
         (posts (lujok/org-publish-sitemap--valid-entries posts))
         (last-five (seq-subseq posts 0 (min (length posts) 6))))
    (org-list-to-org (cons (car sitemap) last-five))))

(defun lujok/archive-sitemap-format-entry (entry style project)
  "Format sitemap entry for ENTRY STYLE PROJECT."
  (cond ((not (directory-name-p entry))
         (format "%s » [[file:%s][%s]]"
                 (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'list)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun lujok/format-rss-feed (title list)
  "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list 50 '(:icount "" :istart ""))))

(defun lujok/org-rss-publish-to-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun lujok/format-rss-feed-entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
		(title (org-publish-find-title entry project))
		(description (org-publish-find-property entry :description project 'html))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" entry title))
             (org-set-property "RSS_PERMALINK" link)
	     (org-set-property "RSS_TITLE" title)
             (org-set-property "PUBDATE" date)
	     (org-set-property "DESCRIPTION" description)
             (insert-file-contents file)
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

;; from https://github.com/aang7/aang7.github.io/blob/master/publish.el
(defun add-content-before-tag (tag content)
  "You have to write the exact string of the tag to add before it.
This function only works for html tags, that means that tags has to
 be wrapped with '<' and '>'
TAG: Tag to modify.
CONTENT: string to add."
  ;; (interactive "sTag:\nsContent:")
  (goto-char (point-min)) ; go to the start of the file
  (condition-case nil
      (progn
        (search-forward tag nil t) ;; this always will return nil
	  (search-backward "<" nil t)	  
	  (insert content)	 	
	  (indent-for-tab-command))
    (error nil)))

(defun lujok/org-html-publish-to-html (plist filename pub-dir)
  "Same as `org-html-publish-to-html' but modifies html before finishing."
  (let ((file-path (org-html-publish-to-html plist filename pub-dir)))
    (with-current-buffer (find-file-noselect file-path)
      (when (and (string-match "content/blog" filename) (not (string-match "index" filename)))
	(add-content-before-tag "</main" (concat disqus))) ;;使用的评论平台，这里用disqus，可以在templates里改     
      (save-buffer)
      (kill-buffer))
    file-path))

;; Set project
(setq org-publish-project-alist
      `(("blog"
	 :base-directory "./content/blog/"
	 :base-extension "org"
	 :publishing-directory "./public/blog"
         :publishing-function ignore
         :exclude ,blog-exclude-files
	 :recursive t
	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "My Blog"
	 :sitemap-function lujok/blog-index
	 :sitemap-format-entry lujok/sitemap-format-entry
	 :sitemap-style list
	 :sitemap-sort-files anti-chronologically
	 )
	("content"
         :base-directory "./content/"
         :base-extension "org"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :headline-level 4
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
         :recursive t
	 :auto-sitemap nil
	 :html-head ,header
         :html-preamble ,preamble
         :html-postamble ,postamble)
        ("assets"
         :base-directory "./assets/"
         :base-extension ,site-attachments
	 :include ("CNAME")
         :publishing-directory "./public"
         :publishing-function org-publish-attachment
         :recursive t
	 :exclude "public/")
        ("website" :components ("blog" "content" "assets"))))

(provide 'publish)
