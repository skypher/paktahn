# Contributor: Brit Butler <redline6561@gmail.com>
# Contributor: Leslie P. Polzer <polzer@gnu.org>
# Maintainer: Leslie P. Polzer <polzer@gnu.org>

pkgname=paktahn
pkgver=0.9
libver=0.8.3
pkgrel=1
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

sha1sums=('a89068dadbf18871db721ac32acbcb8c897291f6'
          '8a4d265521db5a0cf84a7f53fbf8aa184266771d')
md5sums=('dcbcbee6d532ebb10742e883a3d1ad2e'
         '9162ba76aef1a805a2125582afb1d021')
md5sums=('dcbcbee6d532ebb10742e883a3d1ad2e'
         '26c9a382331b9fc73591c7be146e0301')
sha1sums=('a89068dadbf18871db721ac32acbcb8c897291f6'
          '8041a5277baf17aa9beaeda06968b4a9eed9eabc')
