
unset pkgname pkgver pkgrel arch pkgdesc provides url source install md5sums \
      depends makedepends conflicts replaces \
      _svntrunk _svnmod _cvsroot _cvsmod _hgroot \
      _hgrepo	_gitroot _gitname _darcstrunk _darcsmod _bzrtrunk _bzrmod 

source "/etc/makepkg.conf" &> /dev/null
source "$1" &> /dev/null

# TODO loop
echo pkgname
echo ${pkgname}

echo pkgver
echo ${pkgver}

echo pkgrel
echo ${pkgrel}

echo arch
echo ${arch[@]}

echo depends
echo ${depends[@]}

echo makedepends
echo ${makedepends[@]}

