The Wall
========

The Wall is a small and efficient web service to provide instant
messaging-like functionality on the web. The point is that you don't
have to install anything beside a web browser (which is very likely
already present even on mobiles).

The website provides no user accounts, and therefore no registration is
necessary.

The site also can function without cookies and Javascript, so it is
especially useful when strict security measures are in place.

Posts are not stored on any kind of persistent storage, they're just
being kept in memory. This is safe, because if the server is stolen,
broken or confiscated, the contents are lost on shutdown, reboot or
sever crash.

The service is designed to be fast. It can be installed on a small
private server with little RAM and CPU power. (Benchmarks are still to
come. Since the functionality is far from complete, it would make
little sense to do actual measurements at this moment.)

Installation
============

You will need cabal to install the service. Just 'cd' into the root
directory of the project, and issue

    stack install

Running
=======

Cabal will install an executable called 'thewall'. You can run the service
with the following command:

    thewall Production --port 3000

After this, the server can be reached on port 3000 on all interfaces.

