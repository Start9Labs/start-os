## Header

### Magic

2B: `0x3b3b`

### Version

varint: `0x02`

### Pubkey

32B: ed25519 pubkey

### TOC

- number of sections (varint)
- FOREACH section
  - sig (32B: ed25519 signature of BLAKE-3 of rest of section)
  - name (varstring)
  - TYPE (varint)
    - TYPE=FILE (`0x01`)
      - mime (varstring)
      - pos (32B: u64 BE)
      - len (32B: u64 BE)
      - hash (32B: BLAKE-3 of file contents)
    - TYPE=TOC (`0x02`)
      - recursively defined
