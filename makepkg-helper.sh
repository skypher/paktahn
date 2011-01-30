unset carch
unset pkgdest
unset pkgext
source "/etc/makepkg.conf" &> /dev/null

echo carch
echo ${CARCH}

echo pkgdest
echo ${PKGDEST}

echo pkgext
echo ${PKGEXT}
