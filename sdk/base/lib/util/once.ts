export function once<B>(fn: () => B): () => B {
  let result: [B] | [] = []
  return () => {
    if (!result.length) {
      result = [fn()]
    }
    return result[0]
  }
}
