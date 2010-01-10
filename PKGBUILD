# Contributor: Brit Butler <redline6561@gmail.com>
# Contributor: Leslie P. Polzer <polzer@gnu.org>
# Maintainer: Leslie P. Polzer <polzer@gnu.org>

pkgname=paktahn
pkgver=0.8.2
libver=0.8
pkgrel=2
pkgdesc="The kick-ass package manager for Arch Linux!"
arch=('i686' 'x86_64')
depends=('pacman' 'readline' 'sudo' 'svn')
makedepends=('sbcl>=1.0.32')
license=('GPL')
url="http://github.com/skypher/paktahn"
options=(!strip)
install="paktahn.install"
source=("http://redlinernotes.com/docs/paktahn-makedeps-$libver.tbz2"
#        "http://viridian-project.de/~sky/paktahn/paktahn-${pkgver}.tar.bz2")
	 "http://redlinernotes.com/docs/paktahn-${pkgver}.tar.bz2")

build() {
  cd "$srcdir"

  # compile
  SBCL=sbcl

  $SBCL \
       --noinform --lose-on-corruption --end-runtime-options \
       --no-userinit --no-sysinit \
       --eval "(pushnew :paktahn-deploy *features*)" \
       --eval "(require :asdf)" \
       --eval "(setf asdf:*central-registry*
                    '(\"$srcdir/\" \"$srcdir/clbuild.paktahn/systems/\"))" \
       --eval "(asdf:oos 'asdf:load-op 'split-sequence)" \
       --eval "(asdf:oos 'asdf:load-op 'paktahn)" \
       --eval "(pak::build-core :forkp nil)"

  # install binary
  mkdir -p $pkgdir/usr/bin
  install -m755 paktahn $pkgdir/usr/bin

  # install helper scripts
  mkdir -p $pkgdir/usr/lib/paktahn
  for helper in pkgbuild-helper.sh makepkg-helper.sh; do
    install -m755 "$helper" $pkgdir/usr/lib/paktahn
  done

  # create convenience symlink
  cd $pkgdir/usr/bin
  ln -s paktahn pak
}
md5sums=('f7d1a6d9a1a0107f09158216f1f9ea50'
	 'da9bbc70b52802ce392973951aed14bf')
sha1sums=('422e05d9f2dee7bd686377779b9655d828278518'
	  '65c5d3c6c1860ebc5b586ba1910bc817bb6221c2')
md5sums=('f7d1a6d9a1a0107f09158216f1f9ea50'
         'ae7f513b21d28d1f7324a9cb3052f13f')
sha1sums=('422e05d9f2dee7bd686377779b9655d828278518'
          '8f952159e348eeab4f1955a54052cb58fe8ad0cd')
md5sums=('f7d1a6d9a1a0107f09158216f1f9ea50'
         '80d2055540fe5e6e99706d73b41a7a4b')
sha1sums=('422e05d9f2dee7bd686377779b9655d828278518'
          'ee4648cda5f136bdf6838b357f5d449c32e10830')
