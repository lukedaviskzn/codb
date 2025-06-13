#Users.range<id>(
    struct { id: int32 } {
        id: - 0i32 10_i32 // 0 - 10 instead of just -10 as negation operator isn't finished
    },
    struct { id: int32 } {
        id: 10_i32
    }
)
