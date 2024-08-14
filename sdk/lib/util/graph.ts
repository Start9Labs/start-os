import { boolean } from "ts-matches"

export type Vertex<VMetadata = void, EMetadata = void> = {
  metadata: VMetadata
  edges: Array<Edge<EMetadata, VMetadata>>
}

export type Edge<EMetadata = void, VMetadata = void> = {
  metadata: EMetadata
  from: Vertex<VMetadata, EMetadata>
  to: Vertex<VMetadata, EMetadata>
}

export class Graph<VMetadata = void, EMetadata = void> {
  private readonly veritces: Array<Vertex<VMetadata, EMetadata>> = []
  constructor() {}
  addVertex(
    metadata: VMetadata,
    fromEdges: Array<Omit<Edge<EMetadata, VMetadata>, "to">>,
    toEdges: Array<Omit<Edge<EMetadata, VMetadata>, "from">>,
  ): Vertex<VMetadata, EMetadata> {
    const vertex: Vertex<VMetadata, EMetadata> = {
      metadata,
      edges: [],
    }
    for (let edge of fromEdges) {
      const vEdge = {
        metadata: edge.metadata,
        from: edge.from,
        to: vertex,
      }
      edge.from.edges.push(vEdge)
      vertex.edges.push(vEdge)
    }
    for (let edge of toEdges) {
      const vEdge = {
        metadata: edge.metadata,
        from: vertex,
        to: edge.to,
      }
      edge.to.edges.push(vEdge)
      vertex.edges.push(vEdge)
    }
    this.veritces.push(vertex)
    return vertex
  }
  findVertex(
    predicate: (vertex: Vertex<VMetadata, EMetadata>) => boolean,
  ): Generator<Vertex<VMetadata, EMetadata>, void> {
    const veritces = this.veritces
    function* gen() {
      for (let vertex of veritces) {
        if (predicate(vertex)) {
          yield vertex
        }
      }
    }
    return gen()
  }
  addEdge(
    metadata: EMetadata,
    from: Vertex<VMetadata, EMetadata>,
    to: Vertex<VMetadata, EMetadata>,
  ): Edge<EMetadata, VMetadata> {
    const edge = {
      metadata,
      from,
      to,
    }
    edge.from.edges.push(edge)
    edge.to.edges.push(edge)
    return edge
  }
  bfs(
    from:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
  ): Generator<Vertex<VMetadata, EMetadata>, void> {
    const visited: Array<Vertex<VMetadata, EMetadata>> = []
    function* rec(
      vertex: Vertex<VMetadata, EMetadata>,
    ): Generator<Vertex<VMetadata, EMetadata>, void> {
      if (visited.includes(vertex)) {
        return
      }
      visited.push(vertex)
      yield vertex
      let generators = vertex.edges
        .filter((e) => e.from === vertex)
        .map((e) => rec(e.to))
      while (generators.length) {
        let prev = generators
        generators = []
        for (let gen of prev) {
          const next = gen.next()
          if (!next.done) {
            generators.push(gen)
            yield next.value
          }
        }
      }
    }

    if (from instanceof Function) {
      let generators = this.veritces.filter(from).map(rec)
      return (function* () {
        while (generators.length) {
          let prev = generators
          generators = []
          for (let gen of prev) {
            const next = gen.next()
            if (!next.done) {
              generators.push(gen)
              yield next.value
            }
          }
        }
      })()
    } else {
      return rec(from)
    }
  }
  reverseBfs(
    to:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
  ): Generator<Vertex<VMetadata, EMetadata>, void> {
    const visited: Array<Vertex<VMetadata, EMetadata>> = []
    function* rec(
      vertex: Vertex<VMetadata, EMetadata>,
    ): Generator<Vertex<VMetadata, EMetadata>, void> {
      if (visited.includes(vertex)) {
        return
      }
      visited.push(vertex)
      yield vertex
      let generators = vertex.edges
        .filter((e) => e.to === vertex)
        .map((e) => rec(e.from))
      while (generators.length) {
        let prev = generators
        generators = []
        for (let gen of prev) {
          const next = gen.next()
          if (!next.done) {
            generators.push(gen)
            yield next.value
          }
        }
      }
    }

    if (to instanceof Function) {
      let generators = this.veritces.filter(to).map(rec)
      return (function* () {
        while (generators.length) {
          let prev = generators
          generators = []
          for (let gen of prev) {
            const next = gen.next()
            if (!next.done) {
              generators.push(gen)
              yield next.value
            }
          }
        }
      })()
    } else {
      return rec(to)
    }
  }
  shortestPath(
    from:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
    to:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
  ): Array<Edge<EMetadata, VMetadata>> | void {
    const isDone =
      to instanceof Function
        ? to
        : (v: Vertex<VMetadata, EMetadata>) => v === to
    const path: Array<Edge<EMetadata, VMetadata>> = []
    const visited: Array<Vertex<VMetadata, EMetadata>> = []
    function* check(
      vertex: Vertex<VMetadata, EMetadata>,
      path: Array<Edge<EMetadata, VMetadata>>,
    ): Generator<undefined, Array<Edge<EMetadata, VMetadata>> | undefined> {
      if (isDone(vertex)) {
        return path
      }
      if (visited.includes(vertex)) {
        return
      }
      visited.push(vertex)
      yield
      let generators = vertex.edges
        .filter((e) => e.from === vertex)
        .map((e) => check(e.to, [...path, e]))
      while (generators.length) {
        let prev = generators
        generators = []
        for (let gen of prev) {
          const next = gen.next()
          if (next.done === true) {
            if (next.value) {
              return next.value
            }
          } else {
            generators.push(gen)
            yield
          }
        }
      }
    }

    if (from instanceof Function) {
      let generators = this.veritces.filter(from).map((v) => check(v, []))
      while (generators.length) {
        let prev = generators
        generators = []
        for (let gen of prev) {
          const next = gen.next()
          if (next.done === true) {
            if (next.value) {
              return next.value
            }
          } else {
            generators.push(gen)
          }
        }
      }
    } else {
      const gen = check(from, [])
      while (true) {
        const next = gen.next()
        if (next.done) {
          return next.value
        }
      }
    }
  }
}
