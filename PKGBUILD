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
	 'bf9db675985ff06b7a868e52024032d9')
sha1sums=('422e05d9f2dee7bd686377779b9655d828278518'
	  'e0c49e115e3c6a95cbfbce0bea4524f796b67726')md5sums=('dff66d307c758c09c083fa052d51e50a'
         '2bea1ec462814143169b8fdb37445c20')
sha1sums=('a1dbc5f34255a31a5666f8acff7fc7572a473fad'
          '95d56eedc8074f612dc9722d6a448d2821ec9f95')
