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
md5sums=('c7da0fd0e05a9df4da1adf7fb721f8e8')
