import Foundation

actor AITaskQueue {
    enum Priority: Int, Comparable, Sendable {
        case routing = 10
        case synthesis = 5

        static func < (lhs: Priority, rhs: Priority) -> Bool {
            lhs.rawValue < rhs.rawValue
        }
    }

    struct Lease: Sendable {
        fileprivate let id: UUID
    }

    private struct Waiter {
        let id: UUID
        let priority: Priority
        let label: String
        let continuation: CheckedContinuation<Lease, Error>
    }

    private var maxConcurrent: Int
    private var runningCount = 0
    private var pending: [Waiter] = []

    init(maxConcurrent: Int = 2) {
        self.maxConcurrent = max(1, maxConcurrent)
    }

    func acquire(priority: Priority, label: String) async throws -> Lease {
        if runningCount < maxConcurrent {
            runningCount += 1
            return Lease(id: UUID())
        }

        let waiterID = UUID()
        return try await withTaskCancellationHandler {
            try await withCheckedThrowingContinuation { continuation in
                pending.append(
                    Waiter(
                        id: waiterID,
                        priority: priority,
                        label: label,
                        continuation: continuation
                    )
                )
                pending.sort {
                    if $0.priority == $1.priority {
                        return $0.label < $1.label
                    }
                    return $0.priority > $1.priority
                }
            }
        } onCancel: {
            Task {
                await self.cancelPending(id: waiterID)
            }
        }
    }

    func release(_ lease: Lease) {
        _ = lease.id
        while let next = pending.first {
            pending.removeFirst()
            next.continuation.resume(returning: Lease(id: next.id))
            return
        }
        runningCount = max(0, runningCount - 1)
    }

    func setMaxConcurrent(_ value: Int) {
        maxConcurrent = max(1, value)
        drainPendingIfCapacityAllows()
    }

    var queueDepth: Int {
        pending.count
    }

    private func cancelPending(id: UUID) {
        guard let index = pending.firstIndex(where: { $0.id == id }) else { return }
        let waiter = pending.remove(at: index)
        waiter.continuation.resume(throwing: CancellationError())
    }

    private func drainPendingIfCapacityAllows() {
        while runningCount < maxConcurrent, let next = pending.first {
            pending.removeFirst()
            runningCount += 1
            next.continuation.resume(returning: Lease(id: next.id))
        }
    }
}
