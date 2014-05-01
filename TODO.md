TODO List
=========

1.0b
----

* Add tests
* Add better expiration logic: make walls expire upon read / write ops.

1.0
---

* Add non-obtrusive AJAX form posting
* Make the chat header collapsible
* Generate random wall links at the server side
* Figure out how to do proper TLS

1.1
---

* Refactor the post AForm into an MForm with proper structure, design
* Make wall refresh to do exponential back-off
* Wall refresh should return only the difference

1.2
---

* Develop a full JSON API to all functions:
** Get new wall link
** Post
** Get full wall, no wait
** Get differences to current version, long poll


