# Threadnote MVP

This repository now supports two ways to run Threadnote on macOS:

- `swift run` for the existing SwiftPM demo loop
- `xcodebuild` / Xcode for the formal app target and `.app` bundle

## App Shape

Current foundation:

- SwiftUI app shell with keyboard-triggered quick capture window
- Formal macOS app project generated from `project.yml`
- Shared source tree under `Sources/`
- Local snapshot persistence for demo data

## Run With SwiftPM

```bash
cd /Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp
swift run
```

## Generate the Xcode Project

```bash
cd /Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp
xcodegen generate
```

This creates:

- `Threadnote.xcodeproj`

## Build the macOS App Bundle

```bash
cd /Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp
xcodebuild \
  -project Threadnote.xcodeproj \
  -scheme Threadnote \
  -configuration Debug \
  -derivedDataPath .xcodebuild/DerivedData \
  CODE_SIGNING_ALLOWED=NO \
  build
```

The built app bundle will be at:

`/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/.xcodebuild/DerivedData/Build/Products/Debug/Threadnote.app`

## Persistence

The app persists local demo data to:

`~/Library/Application Support/ThreadnoteMVP/snapshot.json`
