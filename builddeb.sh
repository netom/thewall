#!/bin/bash

PROJECTNAME=thewall
MAJOR=1
MINOR=10
PKREV=3

BUILDDIR=.build/${PROJECTNAME}_${MAJOR}.${MINOR}-${PKREV}

#stack setup
#stack clean
stack build

if [ ! -d "$BUILDDIR" ]; then
  mkdir -p $BUILDDIR
fi

mkdir $BUILDDIR/DEBIAN
tee $BUILDDIR/DEBIAN/control <<EOF
Package: $PROJECTNAME
Version: $MAJOR.$MINOR-$PKREV
Section: base
Priority: optional
Architecture: amd64
Depends: libc6, libgmp10, libffi8, libstdc++6, libgcc-s1, libqrencode4
Maintainer: Fábián Tamás László <giganetom@gmail.com>
Description: The Wall - anonymous forgetfull web IM
 Shows a web page that can be used as an instant messaging interface.
 Works in memory, stores a limited number of messages for a limited time.
EOF

tee $BUILDDIR/DEBIAN/postinst <<EOF
addgroup --system thewall
adduser --system --ingroup thewall --no-create-home --home /opt/thewall thewall
chown -R thewall:thewall /opt/thewall
EOF

chmod 0755 $BUILDDIR/DEBIAN/postinst

mkdir -p $BUILDDIR/opt/thewall/bin
cp `stack path --local-install-root`/bin/thewall $BUILDDIR/opt/thewall/bin

mkdir -p $BUILDDIR/opt/thewall/config
cp config/* $BUILDDIR/opt/thewall/config

rm -rf static/tmp/*

mkdir -p $BUILDDIR/opt/thewall/static
cp -r static/* $BUILDDIR/opt/thewall/static

mkdir -p $BUILDDIR/etc/init
tee $BUILDDIR/etc/init/thewall.conf <<EOF
description "The Wall - anonymous forgetful web IM"
author "Fábián Tamás László <giganetom@gmail.com>"

start on runlevel [2345]
stop on runlevel [!2345]

respawn
respawn limit 100 5

chdir /opt/thewall

script
  exec sudo -u thewall ./bin/thewall /opt/thewall/config/settings.yml
end script
EOF

mkdir -p $BUILDDIR/usr/lib/systemd/system
tee $BUILDDIR/usr/lib/systemd/system/thewall.service <<EOF
[Unit]
Description=The Wall - anonymous forgetfull web IM
After=network.target

[Service]
User=thewall
Group=thewall
Type=simple
Restart=on-failure
RestartSec=5
WorkingDirectory=/opt/thewall
ExecStart=/opt/thewall/bin/thewall /opt/thewall/config/settings.yml

[Install]
WantedBy=multi-user.target
EOF

dpkg-deb --build $BUILDDIR

rm -rf $BUILDDIR
