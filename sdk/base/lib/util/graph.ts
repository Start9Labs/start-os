import { ExtendedVersion } from '../exver'

/**
 * A vertex (node) in a directed graph, holding metadata and a list of connected edges.
 * @typeParam VMetadata - The type of metadata stored on vertices
 * @typeParam EMetadata - The type of metadata stored on edges
 */
export type Vertex<VMetadata = null, EMetadata = null> = {
  metadata: VMetadata
  edges: Array<Edge<EMetadata, VMetadata>>
}

/**
 * A directed edge connecting two vertices, with its own metadata.
 * @typeParam EMetadata - The type of metadata stored on edges
 * @typeParam VMetadata - The type of metadata stored on the connected vertices
 */
export type Edge<EMetadata = null, VMetadata = null> = {
  metadata: EMetadata
  from: Vertex<VMetadata, EMetadata>
  to: Vertex<VMetadata, EMetadata>
}

/**
 * A directed graph data structure supporting vertex/edge management and graph traversal algorithms
 * including breadth-first search, reverse BFS, and shortest path computation.
 *
 * @typeParam VMetadata - The type of metadata stored on vertices
 * @typeParam EMetadata - The type of metadata stored on edges
 */
export class Graph<VMetadata = null, EMetadata = null> {
  private readonly vertices: Array<Vertex<VMetadata, EMetadata>> = []
  constructor() {}
  /**
   * Serializes the graph to a JSON string for debugging.
   * @param metadataRepr - Optional function to transform metadata values before serialization
   * @returns A pretty-printed JSON string of the graph structure
   */
  dump(
    metadataRepr: (metadata: VMetadata | EMetadata) => any = (a) => a,
  ): string {
    const seen = new WeakSet()

    return JSON.stringify(
      this.vertices,
      (k, v) => {
        if (k === 'metadata') return metadataRepr(v)
        if (k === 'from') return metadataRepr(v.metadata)
        if (k === 'to') return metadataRepr(v.metadata)
        return v
      },
      2,
    )
  }
  /**
   * Adds a new vertex to the graph, optionally connecting it to existing vertices via edges.
   * @param metadata - The metadata to attach to the new vertex
   * @param fromEdges - Edges pointing from existing vertices to this new vertex
   * @param toEdges - Edges pointing from this new vertex to existing vertices
   * @returns The newly created vertex
   */
  addVertex(
    metadata: VMetadata,
    fromEdges: Array<Omit<Edge<EMetadata, VMetadata>, 'to'>>,
    toEdges: Array<Omit<Edge<EMetadata, VMetadata>, 'from'>>,
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
    this.vertices.push(vertex)
    return vertex
  }
  /**
   * Returns a generator that yields all vertices matching the predicate.
   * @param predicate - A function to test each vertex
   * @returns A generator of matching vertices
   */
  findVertex(
    predicate: (vertex: Vertex<VMetadata, EMetadata>) => boolean,
  ): Generator<Vertex<VMetadata, EMetadata>, null> {
    const veritces = this.vertices
    function* gen() {
      for (let vertex of veritces) {
        if (predicate(vertex)) {
          yield vertex
        }
      }
      return null
    }
    return gen()
  }
  /**
   * Adds a directed edge between two existing vertices.
   * @param metadata - The metadata to attach to the edge
   * @param from - The source vertex
   * @param to - The destination vertex
   * @returns The newly created edge
   */
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
  /**
   * Performs a breadth-first traversal following outgoing edges from the starting vertex or vertices.
   * @param from - A starting vertex, or a predicate to select multiple starting vertices
   * @returns A generator yielding vertices in BFS order
   */
  breadthFirstSearch(
    from:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
  ): Generator<Vertex<VMetadata, EMetadata>, null> {
    const visited: Array<Vertex<VMetadata, EMetadata>> = []
    function* rec(
      vertex: Vertex<VMetadata, EMetadata>,
    ): Generator<Vertex<VMetadata, EMetadata>, null> {
      if (visited.includes(vertex)) {
        return null
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
      return null
    }

    if (from instanceof Function) {
      let generators = this.vertices.filter(from).map(rec)
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
        return null
      })()
    } else {
      return rec(from)
    }
  }
  /**
   * Performs a reverse breadth-first traversal following incoming edges from the starting vertex or vertices.
   * @param to - A starting vertex, or a predicate to select multiple starting vertices
   * @returns A generator yielding vertices in reverse BFS order
   */
  reverseBreadthFirstSearch(
    to:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
  ): Generator<Vertex<VMetadata, EMetadata>, null> {
    const visited: Array<Vertex<VMetadata, EMetadata>> = []
    function* rec(
      vertex: Vertex<VMetadata, EMetadata>,
    ): Generator<Vertex<VMetadata, EMetadata>, null> {
      if (visited.includes(vertex)) {
        return null
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
      return null
    }

    if (to instanceof Function) {
      let generators = this.vertices.filter(to).map(rec)
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
        return null
      })()
    } else {
      return rec(to)
    }
  }
  /**
   * Finds the shortest path (by edge count) between two vertices using BFS.
   * @param from - The starting vertex, or a predicate to select starting vertices
   * @param to - The target vertex, or a predicate to identify target vertices
   * @returns An array of edges forming the shortest path, or `null` if no path exists
   */
  shortestPath(
    from:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
    to:
      | Vertex<VMetadata, EMetadata>
      | ((vertex: Vertex<VMetadata, EMetadata>) => boolean),
  ): Array<Edge<EMetadata, VMetadata>> | null {
    const isDone =
      to instanceof Function
        ? to
        : (v: Vertex<VMetadata, EMetadata>) => v === to
    const path: Array<Edge<EMetadata, VMetadata>> = []
    const visited: Array<Vertex<VMetadata, EMetadata>> = []
    function* check(
      vertex: Vertex<VMetadata, EMetadata>,
      path: Array<Edge<EMetadata, VMetadata>>,
    ): Generator<undefined, Array<Edge<EMetadata, VMetadata>> | null> {
      if (isDone(vertex)) {
        return path
      }
      if (visited.includes(vertex)) {
        return null
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
      return null
    }

    if (from instanceof Function) {
      let generators = this.vertices.filter(from).map((v) => check(v, []))
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
    return null
  }
}
