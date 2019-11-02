#!/bin/sh

# Where the script is executed
CURRENT_PATH="$(dirname "$0")"

# Where to download the file
OUTPUT_PATH="${CURRENT_PATH}/plantuml.jar"

# Retrieve the list of versions, in XML format, only one result (the latest)
VERSIONS_URL='https://search.maven.org/solrsearch/select?q=g:net.sourceforge.plantuml+AND+a:plantuml&core=gav&start=0&rows=1&wt=xml'

# Only match the contents of the version (name="v") XML tag
LATEST_VERSION="$(curl -s "${VERSIONS_URL}" | grep '<str name="v">'| sed 's/.*<str name="v">//g'|sed 's/<\/str>.*//')"

# Compose the download link
DOWNLOAD_URL="https://search.maven.org/remotecontent?filepath=net/sourceforge/plantuml/plantuml/${LATEST_VERSION}/plantuml-${LATEST_VERSION}.jar"

# finally, download the JAR file
echo "Downloading PlantUML v${LATEST_VERSION} into ${OUTPUT_PATH}"
curl -so "${OUTPUT_PATH}" "${DOWNLOAD_URL}" 2>/dev/null
