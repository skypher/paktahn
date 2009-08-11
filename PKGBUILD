# Contributor: Leslie P. Polzer <polzer@gnu.org>

pkgname=paktahn
pkgver=0.8
pkgrel=1
pkgdesc="Package manager similar to yaourt and tupac"
arch=('i686' 'x86_64')
depends=('pacman' 'readline' 'sudo')
license=('GPL')
url="http://gitorious.org/paktahn"
makedepends=('sbcl')
options=(!strip)
source=(${pkgname}-${pkgver}.tar.bz2)

build() {
  cd $srcdir

  if [ -d "/home/sky/sbcl.git.dev" ]; then
    SBCL="sh /home/sky/sbcl.git.dev/run-sbcl.sh"
  else
    SBCL=sbcl
  fi

  $SBCL \
       --noinform --lose-on-corruption --end-runtime-options \
       --load main.lisp \
       --eval "(pak::build-core :forkp nil)"

  mkdir -p $pkgdir/usr/bin
  install -m755 paktahn $pkgdir/usr/bin

  mkdir -p $pkgdir/usr/lib/paktahn
  install -m755 pkgbuild-helper.sh $pkgdir/usr/lib/paktahn

  cd $pkgdir/usr/bin
  ln -s paktahn pak
}
md5sums=('214c6b2991fded15e05a7e04499bf5ad')
md5sums=('546490c1a9337fd6a9587548f13e8f52')
md5sums=('f89cc70f2d8d15f072a222960b4c9fd7')
md5sums=('742dcfd8c2dc94a6ef70f2d3ce6a1bf2')
md5sums=('812e911a0224439f30e555868282a269')
