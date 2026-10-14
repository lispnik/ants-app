#!/bin/bash
#
# testflight.sh -- build an App Store .ipa, check it, validate it and upload it.
#
#     tools/testflight.sh                # build number from the clock
#     ANTS_BUILD=7 tools/testflight.sh   # a build number of your own
#     tools/testflight.sh --validate     # build and validate, upload nothing
#
# Signing is personal, so it lives in local.env, which is not committed:
#
#     IOS_DISTRIBUTION_IDENTITY="Apple Distribution: Your Name (TEAMID1234)"
#     IOS_DEVELOPMENT_TEAM=TEAMID1234
#     IOS_DISTRIBUTION_PROFILE="/path/to/an App Store.mobileprovision"
#     ASC_KEY_ID=XXXXXXXXXX
#     ASC_ISSUER_ID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
#
# altool reads the key's .p8 from ~/.appstoreconnect/private_keys.

set -euo pipefail
cd "$(dirname "$0")/.."

upload=yes
if [ "${1:-}" = --validate ]; then upload=no; fi

if [ -f local.env ]; then . ./local.env; fi
for var in IOS_DISTRIBUTION_IDENTITY IOS_DEVELOPMENT_TEAM IOS_DISTRIBUTION_PROFILE ASC_KEY_ID ASC_ISSUER_ID; do
  test -n "${!var:-}" || { echo "set $var in local.env or the environment" >&2; exit 1; }
done
# Every upload must raise the build number; the clock always does.
: "${ANTS_BUILD:=$(date -u +%Y%m%d%H%M)}"

ipa=build/Ants.ipa

echo "== building build $ANTS_BUILD for the App Store"
# From a clean build/iphoneos: a stale bundle can lack the compiled icon.
rm -rf build/iphoneos "$ipa"
ANTS_DISTRIBUTION=1 ANTS_BUILD="$ANTS_BUILD" \
IOS_SIGNING_IDENTITY="$IOS_DISTRIBUTION_IDENTITY" \
IOS_DEVELOPMENT_TEAM="$IOS_DEVELOPMENT_TEAM" \
IOS_PROVISIONING_PROFILE="$IOS_DISTRIBUTION_PROFILE" \
  ecl --eval '(asdf:load-system "asdf-ios-app")' \
      --eval "(asdf-ios-app:export-ipa (first (asdf-ios-app:make-app \"ants-app\" :platforms (list :device))) :output (merge-pathnames \"$ipa\" (uiop:getcwd)))" \
      --eval '(quit)' 2>&1 | grep -v -i certificate | grep -E '^; |rror' || true
test -f "$ipa" || { echo "no $ipa was made" >&2; exit 1; }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
unzip -q "$ipa" -d "$work"
app=$(echo "$work"/Payload/*.app)

echo "== checking what was built"
plutil -p "$app/Info.plist" | grep -E 'CFBundleIdentifier|CFBundleVersion|CFBundleShortVersionString|CFBundleIconName'
signature=$(codesign -dvv "$app" 2>&1)
echo "$signature" | grep -E '^Authority=Apple|^TeamIdentifier'
echo "$signature" | grep -q '^Authority=Apple Distribution' \
  || { echo "not signed by an Apple Distribution identity" >&2; exit 1; }
entitlements=$(codesign -d --entitlements - --xml "$app" 2>/dev/null | plutil -p -)
if echo "$entitlements" | grep -q '"get-task-allow" => true'; then
  echo "get-task-allow is in the signature; App Store Connect refuses that." >&2
  exit 1
fi
echo "$entitlements" | grep -q 'beta-reports-active' \
  || { echo "the profile does not grant beta-reports-active: not an App Store profile?" >&2; exit 1; }

echo "== validating with App Store Connect"
xcrun altool --validate-app -f "$ipa" -t ios --apiKey "$ASC_KEY_ID" --apiIssuer "$ASC_ISSUER_ID"

if [ "$upload" = no ]; then
  echo "== validated; not uploading"
  exit 0
fi

echo "== uploading"
xcrun altool --upload-app -f "$ipa" -t ios --apiKey "$ASC_KEY_ID" --apiIssuer "$ASC_ISSUER_ID"
echo
echo "Uploaded build $ANTS_BUILD. App Store Connect takes a few minutes to process"
echo "it; it then appears under TestFlight, and internal testers can install it."
