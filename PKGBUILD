# Contributor: Leslie P. Polzer <polzer@gnu.org>

pkgname=paktahn
pkgver=0.8
pkgrel=1
pkgdesc="Package manager similar to yaourt and tupac"
arch=('i686' 'x86_64')
depends=('pacman' 'readline')
license=('GPL')
url="http://gitorious.org/paktahn"
makedepends=('sbcl')
source=(${pkgname}-${pkgver}.tar.bz2)

build() {
  cd $srcdir

  sbcl --noinform --lose-on-corruption --end-runtime-options \
       --load main.lisp \
       --eval "(pak::build-core :forkp nil)"

  mkdir -p $pkgdir/usr/bin
  install -m755 paktahn $pkgdir/usr/bin

  mkdir -p $pkgdir/usr/lib/paktahn
  install -m755 pkgbuild-helper.sh $pkgdir/usr/lib/paktahn

  cd $pkgdir/usr/bin
  ln -s paktahn pak
}
md5sums=('255a09779158bcaba4b5cb2e6a9223dc')
