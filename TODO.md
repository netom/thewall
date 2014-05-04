TODO List
=========

1.0
---

* Fix js wall look difference bug
* Add non-obtrusive AJAX form posting
* Improve tests to really test GC and new expiration logic separately
* Try to figure out how to test javascript (hs-webdriver?)
* Review the random string and QR-Code generating process
** Improve efficiency. The link generations is naïve at best.
** Review the types and reduce conversion overhead where needed and can be.
** Look for a pure haskell implementation of QR encoding
** Same for PNG

1.1
---

* Hilite links
* Make wall refresh to do exponential back-off
* Wall refresh should return only the difference
* Write a completely JS-free version using frames and browser-based refresh
* Figure out how to do proper TLS (w/o ngingx / openSSL, maybe warp-tls? toWaiApp?)

1.2
---

* Develop a full JSON API to all functions:
** Get new wall link
** Post
** Get full wall, no wait
** Get differences to current version, long poll
* Write a good documentation on how to implement 3rd party apps


