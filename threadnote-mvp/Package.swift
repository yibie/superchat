// swift-tools-version: 6.2
import PackageDescription

let package = Package(
    name: "ThreadnoteMVP",
    platforms: [
        .macOS(.v14)
    ],
    products: [
        .executable(name: "ThreadnoteMVP", targets: ["ThreadnoteMVP"])
    ],
    targets: [
        .executableTarget(
            name: "ThreadnoteMVP",
            path: "Sources"
        )
    ]
)
