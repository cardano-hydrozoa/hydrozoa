# docs/spec/

Design and reference documentation for Hydrozoa/Gummiworm: how the code is
organized and how its subsystems actually work, keyed to the current sources.

These describe the system **as built**, complementing the
[Gummiworm whitepaper](https://gummiworm.net/whitepaper/introduction) (the protocol
spec). The repo's top-level `design/` directory is a scratchpad for in-flight feature specs, which
graduate here once built. For project-wide conventions, start with the style guide.

| Doc | Summary |
|---|---|
| [performance-tuning.md](performance-tuning.md) | How a head's performance is measured and tuned: the optimization targets and the round-trip ceiling model, the instruments that show each one, the measurement method, and the settings that move them. |
