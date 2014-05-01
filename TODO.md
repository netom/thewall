TODO List
=========

1.0b
----

* Add better expiration logic: make walls expire upon read / write ops.

1.0
---

* Refactor the post AForm into an MForm with proper structure, design
* Refactor the functions in State.hs, no C+P please.
* Add non-obtrusive AJAX form posting
* Make the header MUCH smaller
* Make the chat header collapsible
* Generate random wall links at the server side
* Generate QR codes at the server side

1.1
---

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


