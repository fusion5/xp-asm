Develop:
nix develop

Generate or update .cabal after package.yaml changes
hpack

Repl:
cabal repl xp-asm

Test: 
feedback -- cabal test xp-asm-test --enable-tests

2024-05-24

2024-05-25

Remove polymrphic parameter 'address' and replace with Label in 'Reference'
Define 'SolvedReference' type with appropriate types
    (RefForwardOffsetVA would store an Integer, signed value)

2024-05-27

Solve type errors in unit test (MainTest.hs:47) -- allow the definition of
    the encoding of addresses in programs by means of Encodable?
    OK

Complete undefined `encodeSolved` function in ASM.hs
    OK

Refactor 'Reference' type
    OK

*** Activity stack

