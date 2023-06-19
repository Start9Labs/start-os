'use strict';

var net = require('net');
var tsMatches = require('ts-matches');

function _interopNamespaceDefault(e) {
    var n = Object.create(null);
    if (e) {
        Object.keys(e).forEach(function (k) {
            if (k !== 'default') {
                var d = Object.getOwnPropertyDescriptor(e, k);
                Object.defineProperty(n, k, d.get ? d : {
                    enumerable: true,
                    get: function () { return e[k]; }
                });
            }
        });
    }
    n.default = e;
    return Object.freeze(n);
}

var net__namespace = /*#__PURE__*/_interopNamespaceDefault(net);

// @ts-check
const idType = tsMatches.some(tsMatches.string, tsMatches.number);
const path = "/start9/sockets/rpc.sock";
const inputCommandType = tsMatches.object({
    id: idType,
    method: tsMatches.literal("command"),
    input: tsMatches.object({
        gid: tsMatches.number,
        command: tsMatches.string,
        args: tsMatches.array(tsMatches.string),
        output: tsMatches.literals("inherit", "collect"),
    }, ["gid"]),
});
const inputLogType = tsMatches.object({
    id: idType,
    method: tsMatches.literal("log"),
    input: tsMatches.some(tsMatches.object({ gid: tsMatches.number, trace: tsMatches.string }, ["gid"]), tsMatches.object({ gid: tsMatches.number, warn: tsMatches.string }, ["gid"]), tsMatches.object({ gid: tsMatches.number, error: tsMatches.string }, ["gid"]), tsMatches.object({ gid: tsMatches.number, info: tsMatches.string }, ["gid"]), tsMatches.object({ gid: tsMatches.number, debug: tsMatches.string }, ["gid"])),
});
const inputOutputType = tsMatches.object({
    id: idType,
    method: tsMatches.literal("output"),
    input: tsMatches.object({ pid: tsMatches.number }),
});
const inputSignalType = tsMatches.object({
    id: idType,
    method: tsMatches.literal("signal"),
    input: tsMatches.object({ pid: tsMatches.number, signal: tsMatches.number }),
});
const inputSignalGroupType = tsMatches.object({
    id: idType,
    method: tsMatches.literal("signalGroup"),
    input: tsMatches.object({ gid: tsMatches.number, signal: tsMatches.number }),
});
/**
 *
 * @param {unknown} input
 * @returns {Promise<unknown>}
 */
const dealWithInput = async (input) => tsMatches.matches(input)
    .when(inputCommandType, (command) => {
    console.log("input");
    console.log(JSON.stringify({ command }, null, 2));
})
    .when(inputLogType, () => { })
    .when(inputOutputType, () => { })
    .when(inputSignalType, () => { })
    .when(inputSignalGroupType, () => { })
    .defaultToLazy(() => {
    console.warn(`Coudln't parse the following input ${input}`);
});
const jsonParse = (x) => JSON.parse(x.toString());
class Runtime {
    unixSocketServer = net__namespace.createServer(async (server) => { });
    constructor() {
        this.unixSocketServer.listen(path, () => {
            console.log("now listening");
        });
        this.unixSocketServer.on("connection", (s) => {
            console.log("got connection!");
            s.on("data", (a) => {
                Promise.resolve(a).then(jsonParse).then(dealWithInput).catch();
            });
            s.write("hello world");
            // s.end()
        });
    }
}

new Runtime();
/**

So, this is going to be sent into a running comtainer along with any of the other node modules that are going to be needed and used.

Once the container is started, we will go into a loading/ await state.
This is the init system, and it will always be running, and it will be waiting for a command to be sent to it.

Each command will be a stopable promise. And an example is going to be something like an action/ main/ or just a query into the types.

A command will be sent an object which are the effects, and the effects will be things like the file system, the network, the process, and the os.


 */
// So OS Adapter
// ==============
/**
* Why: So when the we call from the os we enter or leave here?
    
 */
/**
Command: This is a command that the

There are
 */
/**
TODO:
Should I seperate those adapter in/out?
 */
