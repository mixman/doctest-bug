set -ex

STEP=$1
BUILD=$2

CONCURRENCY=-j2

timed () {
    JOB_CURR_TIME=$(date +%s)
    JOB_DURR=$((JOB_START_TIME + 4800 - JOB_CURR_TIME))
    echo time to run $JOB_DURR
    if [ $JOB_DURR -lt 600 ]; then
        echo "Less than 10 minutes to go, aborting"
        exit 1
    else
        timeout "$JOB_DURR" "$@"
    fi
}

case $STEP in
prepare)
    echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
    mkdir -p ~/.local/bin

    case $BUILD in
    cabal)
        cabal --version

        cabal update -v
        sed -i 's/^jobs:/-- jobs:/' "$HOME/.cabal/config"
        rm -fv cabal.project.local
        rm -fv cabal.project.freeze
        ;;

    esac
    ;;

install)

    # common data
    make copy-samples

    case $BUILD in
    cabal)
        # Install doctest
		(cd /tmp && echo "" | cabal new-repl --build-dep fail)
		(cd /tmp && cabal new-install doctest --constraint='doctest^>=0.16.3' --installdir="$HOME/.local/bin") || exit 1
        doctest --version

        # Generate cabal.project.local file, with -Werror
        timed cabal new-build --enable-tests $CONCURRENCY --dry
        ;;

    esac
    ;;

build)

    case $BUILD in
    cabal)
        timed cabal v2-build --write-ghc-environment-files=always --enable-tests $CONCURRENCY all
        timed cabal v2-test --enable-tests all

        # Prepare environment
        for envfile in .ghc.environment.*; do
            mv $envfile .ghc.environment.tmp
            grep -vE 'package-id base-compat-[0-9]' .ghc.environment.tmp > $envfile
        done

        # Run doctest on selected packages
        make doctest
        ;;

    esac
    ;;
esac
