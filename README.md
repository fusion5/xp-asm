Develop:
nix develop

Generate or update .cabal after package.yaml changes
hpack

Repl:
cabal repl xp-asm

Test: 
feedback -- cabal test xp-asm-test --enable-tests

History: 

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

Add tests with prefix
    OK

Add/test objects that add to RVA but not to IA
    OK

2024-05-30

- Use AddressInfo to keep track of position in StateLabelScan
  OK

- Add relative (signed) offsets from the current position to a label + tests
  OK

- Rename SEW, what does it mean???
  OK

- Get rid of Seq and use ByteString for encoding/decoding...
  OK

- Stop summing up stuff to keep the VA and the RVA, sum only RVA and 
  obtain VA by adding the image base address
  OK

- The Address class should have encodeAbsolute and encodeRelative functions
  NOK, we get into trouble with wrappers and other complications. When dealing
  with the encoding of addresses we should just use custom functions, type 
  classes don't add much value:
  -- class (...) => Address a where
  --   encodeAbsolute
  --     :: SolvedReference a -> Either AssemblyError BS.ByteString
  --   encodeRelative
  --     :: Address b
  --     => PositionInfo b
  --     -> SolvedReference a
  --     -> Either AssemblyError BS.ByteString

- Try to make Encodable f :: * -> * and with "f address" 
  OK this works well, and also Atom required the same refactoring

2024-06-14

- Merge SolvedReference and Reference
    OK

- sizeIA and sizeRVA should be in Encodable
    OK

- Try PositionInfo with Naturals as offsets
    OK

- Some addresses in tests should be word8, add overflow tests
    OK

2024-06-21

- Make shouldBeBytes take a bytestring
    OK

- Abstract absolute reference tests
    OK

- Abstract label position code
    OK

- Remove IAOffset and VAOffset opcodes. Replace with Zeroes opcode
    OK

2024-06-22

- Redesign position and address types and define safe operations for them
    OK

- Don't test all possible unhappy paths, only test the base functions
    OK

- Refactor tests 
    OK

- If sizeRVA and sizeIA are always equal then why do we have two functions?
    (Because they could be used to implement alignment at the opcode level,
    but the library is going to do that.)
    OK

- Should size return a Position?
    No, let's use Natural because it's more widespread.

- Add alignment feature, AAlignIA, AAlignVA
    OK

- Wrap Natural in a new type and call it Position
    OK

- Refactoring idea, maybe: merge encodable and bytesized into a single class
  with a single type that returns bytes (if the address is resolved) and ia/va sizes (always)
    OK


2024-11-09

- Eliminate ALabel and let any AOp emit the label(s).
    OK

- Define types that would be needed for linkable/executable object formats
    OK

*** Activity stack

- Write a test for TestLinkableObject
- assemble should work on any Functor?
- Opcodes need labels as well. Within an opcode there should be references to
  labels that were defined within that opcode. This is needed to support ELF 
  fields. The assembler currently takes a sequence of opcodes, but it should 
  be able to assemble a tree. From a tree node there could be a reference to 
  any other node of the final tree. How do we name nodes? Maybe with 
  "node.subnode.subsubnode.count" syntax or maybe
  "node.subnode.subsubnode.startAddr"
  "node.subnode.subsubnode.endAddr"
  Or to avoid overusing names, when we solve references, we can solve them by 
  means of a function that also takes the tree of unsolved references and 
  extracts the needed information from that tree.
  Given that the tree shape could be given by data types, there could be 
  a type class to support the construction of labels.
- Rename piIA and piRelativeVA to piImage and piRelativeVirtual etc. Also in 
  Encodable. Rename Encodable to Assemblable?
