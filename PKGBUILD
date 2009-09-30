# Contributor: Leslie P. Polzer <polzer@gnu.org>
# Maintainer: Leslie P. Polzer <polzer@gnu.org>

### IMPORTANT ###
#
# Current SBCL versions require a patch to make Pacman
# and interactive editors work fully.
#
# I've submitted a patch to upstream but it's not in yet.
#
# For now install sbcl-run-program-fix to compile Paktahn.
#

pkgname=paktahn
pkgver=0.8.1
libver=0.8
pkgrel=1
pkgdesc="The kick-ass package manager for Arch Linux!"
arch=('i686' 'x86_64')
depends=('pacman' 'readline' 'sudo')
license=('GPL')
url="http://blog.viridian-project.de/2009/09/19/announcement-paktahn-a-successor-to-yaourt/"
makedepends=('sbcl-run-program-fix')
options=(!strip)
install="paktahn.install"
source=("http://viridian-project.de/~sky/paktahn/paktahn-makedeps-$libver.tbz2"
#        "http://viridian-project.de/~sky/paktahn/paktahn-${pkgver}.tar.bz2")
        "paktahn-${pkgver}.tar.bz2")

build() {
  cd "$srcdir"

  # compile
  SBCL=sbcl

  $SBCL \
       --noinform --lose-on-corruption --end-runtime-options \
       --eval "(pushnew :paktahn-deploy *features*)" \
       --eval "(require :asdf)" \
       --eval "(setf asdf:*central-registry*
                    '(\"$srcdir/clbuild.paktahn/systems/\"))" \
       --load main.lisp \
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
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '84a403a0ac53d6df6018573da691945d')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          'f85e191cbd86c7cd201d59cabadc7534864395f5')
