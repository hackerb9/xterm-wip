#!/bin/sh
# $XFree86$
#
# Install manpages, substituting a reasonable section value since XFree86 4.x
# doesn't use constants...
#
# Parameters:
#	$1 = program to invoke as "install"
#	$2 = manpage to install
#	$3 = final installed-path
#

MINSTALL="$1"
OLD_FILE="$2"
END_FILE="$3"

suffix=`echo "$END_FILE" | sed -e 's/^[^.]*.//'`
NEW_FILE=temp$$

sed	-e 's/__vendorversion__/"X Window System"/' \
	-e s/__miscmansuffix__/$suffix/ \
	$OLD_FILE >$NEW_FILE

echo "$MINSTALL $OLD_FILE $END_FILE"
eval "$MINSTALL $NEW_FILE $END_FILE"

rm -f $NEW_FILE
