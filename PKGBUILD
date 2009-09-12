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
source=("http://viridian-project.de/~sky/paktahn-makedeps.tbz2"
        "${pkgname}-${pkgver}.tar.bz2")

build() {
  cd "$srcdir"

  # put your custom patched SBCL here!
  if [ -d "/home/sky/sbcl.git.dev" ]; then
    SBCL="sh /home/sky/sbcl.git.dev/run-sbcl.sh"
  else
    SBCL=sbcl
  fi

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
