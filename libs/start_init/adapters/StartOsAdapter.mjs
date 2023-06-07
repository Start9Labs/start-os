// @ts-check

import { todo } from "../todo.mjs";

export class StartOsAdapter {
  /** 
    @param {string} method
    @param {any[]} params
     */
  async rpc(method, params) {
    return todo("Do the rpc https://en.wikipedia.org/wiki/JSON-RPC");
  }
}
