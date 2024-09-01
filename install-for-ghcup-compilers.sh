#!/usr/bin/env bash

#
# Ask the user for confirmation before we proceed.
#
confirm() {
    if ${interactive}; then
        echo -n "Continue? [y/N] "
        while read -r input; do
            case "${input}" in
                'y'|'Y')
                    break
                    ;;
                ''|'n'|'N')
                    info "Aborted by user."
                    exit 0
                    ;;
                *)
                    error \
                        "Invalid input: ${input}" \
                        'Enter "y" to proceed and "n" to abort.'
                    echo -n "Continue? [y/N] "
            esac
        done
    fi
}

#
# Print a (multiline) message at log-level 'info'.
#
info() {
    if ! ${quiet}; then
        printf '%s\n' "${@}"
    fi
}

#
# Print a (multiline) message at log-level 'error'.
#
error() {
    printf '%s\n' "${@}" >&2
}

#
# Print an error and exit.
#
die() {
    error "${@}"
    exit 1
}

#
# Run a command line or just print; Dependending on the passed options.
#
run() {
    local cli=( "${@}" )
    if ${dry_run}; then
        echo "${cli[@]}"
    else
        if ! ${quiet}; then
            info "### Running: ${cli[*]}"
        fi
        "${cli[@]}"
        echo
    fi
}

#
# Print usage information and exit.
#
usage() {
    die \
        'install-for-ghcup-compilers.sh [--debug] [--dry-run] [--interactive] [--quiet] [-- CABAL_OPTIONS...]' \
        '' \
        '    --debug         Print debug output (set -x)' \
        '    --dry-run       Just print the commands that would be run to install print-api; Do not perform the installation' \
        '    --interactive   Ask for comfirmation before doing anything' \
        '    --quiet         Print less output'
}

################################################################################
# Command line parser
################################################################################

cabal_options=()
debug=false
dry_run=false
interactive=false
quiet=false

while [[ "${1}" != '' ]]; do
    case "${1}" in
        '--')
            shift
            cabal_options=( "${@}" )
            break
            ;;
        '--debug')
            debug=true
            ;;
        '--dry-run')
            dry_run=true
            ;;
        '--interactive')
            interactive=true
            ;;
        '--quiet')
            quiet=true
            ;;
        *)
            error "Invalid argument: ${1}"
            usage
    esac
    shift
done

################################################################################
# Main program
################################################################################

set -u

if ${debug}; then
    set -x
fi

readarray -t ghcs < <(ghcup --offline list --raw-format --tool ghc --show-criteria installed | cut -d ' ' -f 2)

info "We are going to install versions of print-api for the following versions of GHC: ${ghcs[*]}"
confirm

for version in "${ghcs[@]}"; do
    run cabal install "print-api:exe:print-api-${version}" --with-compiler "ghc-${version}" "${cabal_options[@]}"
done
run cabal install "print-api:exe:print-api" "${cabal_options[@]}"
