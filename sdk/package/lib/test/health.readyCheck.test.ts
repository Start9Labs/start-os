import { containsAddress } from "../health/checkFns/checkPortListening"

describe("Health ready check", () => {
  it("Should be able to parse an example information", () => {
    let input = `
  
  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode                                                     
   0: 00000000:1F90 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 21634478 1 0000000000000000 100 0 0 10 0                  
   1: 00000000:0050 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 21634477 1 0000000000000000 100 0 0 10 0                  
   2: 0B00007F:9671 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 21635458 1 0000000000000000 100 0 0 10 0                  
   3: 00000000:0D73 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 21634479 1 0000000000000000 100 0 0 10 0   
  `

    expect(containsAddress(input, 80)).toBe(true)
    expect(containsAddress(input, 1234)).toBe(false)
  })
})
