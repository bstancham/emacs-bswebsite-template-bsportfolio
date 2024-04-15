;;;;;;;; THE FOLLOWING SHOULD BE REDEFINED IN YOUR SETUP FILE ;;;;;;;;

;; path to archive database
(setf bswebsite-archive-database-file nil)

(defun bswebsite-header-menu-links ()
  "Should return a list of lists defining items to appear in the header-menu, to
appear at the top of every page of the website.

Each item is defined as a list of two string elements:

(\"Label Text\" \"page_link.html\")
"
  nil)

(defun bswebsite-gallery-pages ()
  "Should return a list of lists defining homepage links to gallery-pages. These
will appear as an image-grid on the homepage, with an accompanying line of text
beneath each image.

Each item is defined as a list of three string elements:

(\"source_file.el\" \"image.jpg\" \"text beneath\")

NOTE: give image filenames without path - bswebsite assumes that images will be
found in src/resrc.
"
  nil)



;;;;;;;;;;;;;;;;;;;; REDEFINED FROM bswebsite.el ;;;;;;;;;;;;;;;;;;;;;

(defun bswebsite-insert-page-header ()
  (insert "<header class=\"site-header\">\n")
  (bswebsite-make-menu-bar)
  (bswebsite-make-gallery-pagination)
  (insert "</header>\n"))



;;;;;;;;;;;;;;;;;;; TEMPLATE DEFINITION CONTINUES ;;;;;;;;;;;;;;;;;;;;

(defun bswebsite-insert-menu-bar-item (text url)
  (insert (format "<li class=\"header-menu-item\"><a href=\"%s\">%s</a></li>\n"
                  url
                  text)))

(defun bswebsite-make-menu-bar ()
  "Makes menu bar for the website"
  (insert "<div class='menu-bar'>\n")
  (insert "<span class='site-header-menu'>\n")
  (insert (format "<a href=\"index.html\"><span class=\"site-title\">%s</span></a>\n" bswebsite-title))
  (let ((menu-items (bswebsite-header-menu-links)))
    (while menu-items
      (let ((item (pop menu-items)))
        (bswebsite-insert-menu-bar-item (pop item) (pop item)))))
  (insert "</span>\n")
  (insert "</div>\n"))

(defun bswebsite-get-pagination-link-urls ()
  "Gets PREV and NEXT urls for gallery page pagination links,
returning them in a list of the form (prev next).

Returns nil if current source file is not a gallery page."
  (let ((filename bswebsite-current-source-file)
        (pages (bswebsite-gallery-pages))
        (prev nil)
        (current nil)
        (next nil)
        (matched nil))
    ;; iterate through list of gallery pages until current source files is matched
    (while (and pages
                (not matched))
      (setf prev current
            current (car (pop pages)))
      ;; stop iterating if match found
      (when (string-equal current filename)
        (setq matched t)))

    ;; if there are any pages left, get NEXT page
    (when pages (setf next (car (pop pages))))
    
    (if matched
        (progn
          (when prev (setq prev (file-name-with-extension prev "html")))
          (when next (setq next (file-name-with-extension next "html")))
          (list prev next))
      nil)))

(defun bswebsite-make-gallery-pagination ()
  (let ((link-urls (bswebsite-get-pagination-link-urls)))
    ;; (insert (format "<p>pagination link urls: %s</p>\n" link-urls))
    (when link-urls
      (let ((prev-url (pop link-urls))
            (next-url (pop link-urls)))
        (insert "<div class=\"gallery-pagination\">\n")
        (insert "<span class=\"site-header-content\"><span class=\"gallery-pagination-inactive\">WORKS:</span>\n")
        (if prev-url
            (insert (format "<span class=\"gallery-pagination-link\"><a href=\"%s\" rel=\"next\">PREV</a>&nbsp;</span>\n" prev-url))
          (insert "<span class=\"gallery-pagination-inactive\">PREV&nbsp;</span>\n"))
        (insert "|\n")
        (if next-url
            (insert (format "<span class=\"gallery-pagination-link\"><a href=\"%s\" rel=\"prev\">NEXT</a>&nbsp;</span>\n" next-url))
          (insert "<span class=\"gallery-pagination-inactive\">NEXT&nbsp;</span>\n"))
        ;; script for arrow keys navigation
        (insert "<script type=\"text/javascript\">\n")
        (insert "<!--\n")
        (insert " /*\n")
        (insert "  * Script for enabling cursor key navigation taken from here: https://helloacm.com/how-to-use-keyboard-arrow-keys-for-wordpress-posts-navigation/\n")
        (insert "  */\n")
        (insert "  document.onkeydown = function (e) {\n")
        (insert "    var e = e || event,\n")
        (insert "    keycode = e.which || e.keyCode;\n")
        (when prev-url
          (insert "    if (keycode == 37)\n")
          (insert (format "      location = \"%s\";\n" prev-url))
        )
        (when next-url
          (insert "    if (keycode == 39)\n")
          (insert (format "      location = \"%s\";\n" next-url))
        )
        (insert "  }\n")
        (insert "-->\n")
        (insert "</script>\n")
        (insert "</span>\n")
        (insert "</div> <!-- end posts-pagination -->\n")))))

(defun bswebsite-make-homepage-items ()
  "Makes image-grid gallery-page links.

Gets the list of gallery pages, images and links from bswebsite-gallery-pages"
  (let ((gallery-pages (bswebsite-gallery-pages)))
    (while gallery-pages
      (let* ((item (pop gallery-pages))
             (link-url (file-name-with-extension (pop item) "html"))
             (image-url (bswebsite-resize-image-thumbnail
                         (file-name-concat "resrc" (pop item))))
             (text (pop item)))
        (insert "<div class=\"image-grid\">\n")
        (insert (format "<a href='%s'>\n" link-url))
        (insert (format "<img src=\"%s\"/>\n" image-url))
        (insert "</a>\n")
        (insert "<br/>")
        (insert (format "%s\n" text))
        (insert "</div>\n")))))

(defun bswebsite-artwork-get (db field artwork-id)
  "Get value of FIELD for artwork with uid matching ARTWORK-ID.

The first value found is returned. This should not be a problem
so long as there are no duplicate UIDs in the database."
  (let ((output (sqlite-select db (format
                                   "SELECT %s FROM artwork WHERE uid='%s';"
                                   field
                                   artwork-id))))
    (when output
      (setq output (car (car output))))
    output))

(defun bswebsite-artwork-insert-info (image-filename)
  (let* ((image-url (file-name-concat "resrc" image-filename))
         (archive-id (bsarchive-exif-get-uids (file-name-concat
                                               (bswebsite-src-dir)
                                               image-url)))
         (db (sqlite-open bswebsite-archive-database-file)))
    (insert "<p>\n")
    ;; (insert (format "archive uid: %s<br/>\n" archive-id))
    (let ((artwork-id (sqlite-select db (concat "SELECT artwork_ids FROM documents WHERE rowid=" (car archive-id) ";"))))
      (when artwork-id
        (setq artwork-id (car (car artwork-id)))
        ;; (insert (format "artwork uid: %s<br/>\n" artwork-id))
        (let ((value (bswebsite-artwork-get db "title" artwork-id)))
          (when (not (string-empty-p value))
            (insert (format "<span class=\"artwork-title\">%s</span><br/>\n" value))))
        (let ((value (bswebsite-artwork-get db "date" artwork-id)))
          (when (not (string-empty-p value))
            (insert (format "%s<br/>\n" value))))
        (let ((value (bswebsite-artwork-get db "dimensions" artwork-id)))
          (when (not (string-empty-p value))
            (insert (format "%s<br/>\n" value))))
        (let ((value (bswebsite-artwork-get db "media" artwork-id)))
          (when (not (string-empty-p value))
            (insert (format "%s<br/>\n" value))))))
    (insert "</p>\n")
    (sqlite-close db)))

(defun bswebsite-make-gallery-image (image-url)
  "Insert image (default size), with standard artwork info underneath."
  (insert "<p>\n")
  (insert (format "<img src=\"%s\"/>\n"
                  (bswebsite-resize-image-default (file-name-concat "resrc" image-url))))
  (insert "<\p>\n")
  (bswebsite-artwork-insert-info image-url))
