# Contributor: Leslie P. Polzer <polzer@gnu.org>
# Maintainer: Leslie P. Polzer <polzer@gnu.org>

### IMPORTANT ###
#
# current SBCL versions require a patch to make Pacman
# and interactive editors work fully. Find it in
# sbcl.patched.
#
# I've submitted a patch to upstream but it's not in yet.
# Additionally the current SBCL PKGBUILD is broken so I
# can't provide a patched PKGBUILD either. :/
#
# Somewhat good news: you can build Paktahn without a
# patched SBCL; it will use Pacman in non-interactive
# mode and use `cat' instead of an editor.
#

pkgname=paktahn
pkgver=0.8
pkgrel=1
pkgdesc="Package manager similar to yaourt and tupac"
arch=('i686' 'x86_64')
depends=('pacman' 'readline' 'sudo')
license=('GPL')
url="http://gitorious.org/paktahn"
makedepends=('sbcl-run-program-fix')
options=(!strip)
install="paktahn.install"
source=("http://viridian-project.de/~sky/paktahn/paktahn-makedeps-$pkgver.tbz2"
        "http://viridian-project.de/~sky/paktahn/paktahn-${pkgver}.tar.bz2")

build() {
  cd "$srcdir"

  SBCL=sbcl

  $SBCL \
       --noinform --lose-on-corruption --end-runtime-options \
       --eval "(pushnew :paktahn-deploy *features*)" \
       --eval "(require :asdf)" \
       --eval "(setf asdf:*central-registry*
                    '(\"$srcdir/clbuild.paktahn/systems/\"))" \
       --load main.lisp \
       --eval "(pak::build-core :forkp nil)"

  mkdir -p $pkgdir/usr/bin
  install -m755 paktahn $pkgdir/usr/bin

  mkdir -p $pkgdir/usr/lib/paktahn
  install -m755 pkgbuild-helper.sh $pkgdir/usr/lib/paktahn

  cd $pkgdir/usr/bin
  ln -s paktahn pak
}
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '917b43168bf1c99e0904693d4d9141e7')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '2f8393ee50da2131c634a26de3daec6e')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '744ba299db8711a59b89a50c51da2299eedf4e5c')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '2f8393ee50da2131c634a26de3daec6e')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '744ba299db8711a59b89a50c51da2299eedf4e5c')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '2f8393ee50da2131c634a26de3daec6e')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '744ba299db8711a59b89a50c51da2299eedf4e5c')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '2f8393ee50da2131c634a26de3daec6e')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '744ba299db8711a59b89a50c51da2299eedf4e5c')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '2f8393ee50da2131c634a26de3daec6e')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '744ba299db8711a59b89a50c51da2299eedf4e5c')
