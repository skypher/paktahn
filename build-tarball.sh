source ./PKGBUILD
tar cvfj paktahn-$pkgver.tar.bz2 \
  {alpm,aur,main,pkgbuild,util,readline,term,pyconfig-fix,cache,checksums,packages,customizepkg}.lisp \
  paktahn.asd \
  pkgbuild-helper.sh makepkg-helper.sh
