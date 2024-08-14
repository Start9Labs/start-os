import { Graph } from "../util/graph"

describe("graph", () => {
  {
    {
      test("findVertex", () => {
        const graph = new Graph<string, string>()
        const foo = graph.addVertex("foo", [], [])
        const bar = graph.addVertex(
          "bar",
          [{ from: foo, metadata: "foo-bar" }],
          [],
        )
        const baz = graph.addVertex(
          "baz",
          [{ from: bar, metadata: "bar-baz" }],
          [],
        )
        const qux = graph.addVertex(
          "qux",
          [{ from: baz, metadata: "baz-qux" }],
          [],
        )
        const match = Array.from(graph.findVertex((v) => v.metadata === "qux"))
        expect(match).toHaveLength(1)
        expect(match[0]).toBe(qux)
      })
      test("shortestPathA", () => {
        const graph = new Graph<string, string>()
        const foo = graph.addVertex("foo", [], [])
        const bar = graph.addVertex(
          "bar",
          [{ from: foo, metadata: "foo-bar" }],
          [],
        )
        const baz = graph.addVertex(
          "baz",
          [{ from: bar, metadata: "bar-baz" }],
          [],
        )
        const qux = graph.addVertex(
          "qux",
          [{ from: baz, metadata: "baz-qux" }],
          [],
        )
        graph.addEdge("foo-qux", foo, qux)
        expect(graph.shortestPath(foo, qux) || []).toHaveLength(1)
      })
      test("shortestPathB", () => {
        const graph = new Graph<string, string>()
        const foo = graph.addVertex("foo", [], [])
        const bar = graph.addVertex(
          "bar",
          [{ from: foo, metadata: "foo-bar" }],
          [],
        )
        const baz = graph.addVertex(
          "baz",
          [{ from: bar, metadata: "bar-baz" }],
          [],
        )
        const qux = graph.addVertex(
          "qux",
          [{ from: baz, metadata: "baz-qux" }],
          [],
        )
        graph.addEdge("bar-qux", bar, qux)
        expect(graph.shortestPath(foo, qux) || []).toHaveLength(2)
      })
      test("shortestPathC", () => {
        const graph = new Graph<string, string>()
        const foo = graph.addVertex("foo", [], [])
        const bar = graph.addVertex(
          "bar",
          [{ from: foo, metadata: "foo-bar" }],
          [],
        )
        const baz = graph.addVertex(
          "baz",
          [{ from: bar, metadata: "bar-baz" }],
          [],
        )
        const qux = graph.addVertex(
          "qux",
          [{ from: baz, metadata: "baz-qux" }],
          [{ to: foo, metadata: "qux-foo" }],
        )
        expect(graph.shortestPath(foo, qux) || []).toHaveLength(3)
      })
      test("bfs", () => {
        const graph = new Graph<string, string>()
        const foo = graph.addVertex("foo", [], [])
        const bar = graph.addVertex(
          "bar",
          [{ from: foo, metadata: "foo-bar" }],
          [],
        )
        const baz = graph.addVertex(
          "baz",
          [{ from: bar, metadata: "bar-baz" }],
          [],
        )
        const qux = graph.addVertex(
          "qux",
          [
            { from: foo, metadata: "foo-qux" },
            { from: baz, metadata: "baz-qux" },
          ],
          [],
        )
        const bfs = Array.from(graph.breadthFirstSearch(foo))
        expect(bfs).toHaveLength(4)
        expect(bfs[0]).toBe(foo)
        expect(bfs[1]).toBe(bar)
        expect(bfs[2]).toBe(qux)
        expect(bfs[3]).toBe(baz)
      })
      test("reverseBfs", () => {
        const graph = new Graph<string, string>()
        const foo = graph.addVertex("foo", [], [])
        const bar = graph.addVertex(
          "bar",
          [{ from: foo, metadata: "foo-bar" }],
          [],
        )
        const baz = graph.addVertex(
          "baz",
          [{ from: bar, metadata: "bar-baz" }],
          [],
        )
        const qux = graph.addVertex(
          "qux",
          [
            { from: foo, metadata: "foo-qux" },
            { from: baz, metadata: "baz-qux" },
          ],
          [],
        )
        const bfs = Array.from(graph.reverseBreadthFirstSearch(qux))
        expect(bfs).toHaveLength(4)
        expect(bfs[0]).toBe(qux)
        expect(bfs[1]).toBe(foo)
        expect(bfs[2]).toBe(baz)
        expect(bfs[3]).toBe(bar)
      })
    }
  }
})
