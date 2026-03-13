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
    dependencies: [
        .package(url: "https://github.com/teunlao/swift-ai-sdk.git", from: "0.17.3"),
        .package(url: "https://github.com/groue/GRDB.swift.git", from: "7.0.0")
    ],
    targets: [
        .executableTarget(
            name: "ThreadnoteMVP",
            dependencies: [
                .product(name: "SwiftAISDK", package: "swift-ai-sdk"),
                .product(name: "AnthropicProvider", package: "swift-ai-sdk"),
                .product(name: "OpenAIProvider", package: "swift-ai-sdk"),
                .product(name: "GoogleProvider", package: "swift-ai-sdk"),
                .product(name: "GroqProvider", package: "swift-ai-sdk"),
                .product(name: "DeepSeekProvider", package: "swift-ai-sdk"),
                .product(name: "XAIProvider", package: "swift-ai-sdk"),
                .product(name: "OpenAICompatibleProvider", package: "swift-ai-sdk"),
                .product(name: "GRDB", package: "GRDB.swift"),
            ],
            path: "Sources"
        ),
        .testTarget(
            name: "ThreadnoteMVPTests",
            dependencies: ["ThreadnoteMVP"],
            path: "Tests/ThreadnoteMVPTests"
        )
    ]
)
