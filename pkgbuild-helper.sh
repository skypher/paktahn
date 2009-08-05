
unset pkgname pkgver pkgrel arch pkgdesc provides url source install md5sums \
      depends makedepends conflicts replaces _svntrunk _svnmod _cvsroot _cvsmod _hgroot \
      _hgrepo	_gitroot _gitname _darcstrunk _darcsmod _bzrtrunk _bzrmod 

source "$1" &> /dev/null

echo ${depends[@]}
echo ${makedepends[@]}

