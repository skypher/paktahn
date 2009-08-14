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
       --eval "(pushnew :paktahn-deploy *features*)" \
       --load main.lisp \
       --eval "(pak::build-core :forkp nil)"

  mkdir -p $pkgdir/usr/bin
  install -m755 paktahn $pkgdir/usr/bin

  mkdir -p $pkgdir/usr/lib/paktahn
  install -m755 pkgbuild-helper.sh $pkgdir/usr/lib/paktahn

  cd $pkgdir/usr/bin
  ln -s paktahn pak
}
md5sums=('1e9fb1bfb963d73ad741b6209afdf046')
md5sums=('b8644caee7cf7a801ee062cc5db89a7a')
md5sums=('0417044d9d8ac99f383455dce02cc59f')
md5sums=('df6e9895773df11bf14451ab26923e0d')
md5sums=('75208039772c28590626738943fecab0')
md5sums=('7416012e3fb59e390d0b7246b0847e77')
