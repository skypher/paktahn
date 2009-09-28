source ./PKGBUILD
tar cvfj paktahn-$pkgver.tar.bz2 \
  {alpm,aur,main,pkgbuild,util,readline,term,pyconfig-fix,cache}.lisp \
  pkgbuild-helper.sh makepkg-helper.sh
