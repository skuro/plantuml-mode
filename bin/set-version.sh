#!/bin/bash

# Retrieves the version from the changelog in plantuml-mode.el
function grab_version(){
    grep ';; version' plantuml-mode.el | \
        head -n1 |                       \
        cut -f3 -d' ' |                  \
        tr -d ','
}

# Updates the version in-place
function update_version(){
    NEW_VERSION="${1}"
    sed -i -E "s/plantuml-mode-version \"[0-9\.]+\"/plantuml-mode-version \"${1}\"/" plantuml-mode.el
}

case "$(git rev-parse --abbrev-ref HEAD)" in
    'master')
        VERSION="$(grab_version)"
        update_version "${VERSION}"
        git add plantuml-mode.el
    ;;

    'develop')
        VERSION="$(TZ='UTC' date '+%Y%m%d.%-H%M')" # MELPA style
        update_version "${VERSION}"
        git add plantuml-mode.el
    ;;

    *)
        ## do nothing
    ;;
esac
