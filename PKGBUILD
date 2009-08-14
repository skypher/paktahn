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
md5sums=('58f78e6645a3ef88ea328fc35310c6bc')
md5sums=('333e3fa90c1737243ebc1dbd2a9bf771')
md5sums=('d16787255220e5d5d8b7ec7eb54536c8')
md5sums=('7e05deec97eddb7b6b0db41949b0b18e')
md5sums=('7e05deec97eddb7b6b0db41949b0b18e')
md5sums=('60d6a72b6ff7cd68df726dd2e8a63aa3')
md5sums=('a79f372ff9cf204bd929facbfabe94c6')
