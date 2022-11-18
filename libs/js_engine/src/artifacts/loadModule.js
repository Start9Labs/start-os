import Deno from "/deno_global.js";
import * as mainModule from "/embassy.js";

function requireParam(param) {
  throw new Error(`Missing required parameter ${param}`);
}

/**
 * This is using the simplified json pointer spec, using no escapes and arrays
 * @param {object} obj
 * @param {string} pointer
 * @returns
 */
function jsonPointerValue(obj, pointer) {
  const paths = pointer.substring(1).split("/");
  for (const path of paths) {
    if (obj == null) {
      return null;
    }
    obj = (obj || {})[path];
  }
  return obj;
}

function maybeDate(value) {
  if (!value) return value;
  return new Date(value);
}
const writeFile = (
  {
    path = requireParam("path"),
    volumeId = requireParam("volumeId"),
    toWrite = requireParam("toWrite"),
  } = requireParam("options"),
) => Deno.core.opAsync("write_file", volumeId, path, toWrite);

const readFile = (
  { volumeId = requireParam("volumeId"), path = requireParam("path") } = requireParam("options"),
) => Deno.core.opAsync("read_file", volumeId, path);



const runDaemon = (
  { command = requireParam("command"), args = [] } = requireParam("options"),
) => {
  let id = Deno.core.opAsync("start_command", command, args, "inherit", null);
  let processId = id.then(x => x.processId)
  let waitPromise = null;
  return {
    processId,
    async wait() {
      waitPromise = waitPromise || Deno.core.opAsync("wait_command", await processId)
      return waitPromise
    },
    async term(signal = 15) {
      return Deno.core.opAsync("send_signal", await processId, 15)
    }
  }
};
const runCommand = async (
  { command = requireParam("command"), args = [], timeoutMillis = 30000 } = requireParam("options"),
) => {
  let id = Deno.core.opAsync("start_command", command, args, "collect", timeoutMillis);
  let pid = id.then(x => x.processId)
  return Deno.core.opAsync("wait_command", await pid)
};
const signalGroup = async (
  { gid = requireParam("gid"), signal = requireParam("signal") } = requireParam("gid and signal")
) => {
  return Deno.core.opAsync("signal_group", gid, signal);
};
const sleep = (timeMs = requireParam("timeMs"),
) => Deno.core.opAsync("sleep", timeMs);

const rename = (
  {
    srcVolume = requireParam("srcVolume"),
    dstVolume = requirePapram("dstVolume"),
    srcPath = requireParam("srcPath"),
    dstPath = requireParam("dstPath"),
  } = requireParam("options"),
) => Deno.core.opAsync("rename", srcVolume, srcPath, dstVolume, dstPath);
const metadata = async (
  { volumeId = requireParam("volumeId"), path = requireParam("path") } = requireParam("options"),
) => {
  const data = await Deno.core.opAsync("metadata", volumeId, path);
  return {
    ...data,
    modified: maybeDate(data.modified),
    created: maybeDate(data.created),
    accessed: maybeDate(data.accessed),
  };
};
const removeFile = (
  { volumeId = requireParam("volumeId"), path = requireParam("path") } = requireParam("options"),
) => Deno.core.opAsync("remove_file", volumeId, path);
const isSandboxed = () => Deno.core.opSync("is_sandboxed");

const writeJsonFile = (
  {
    volumeId = requireParam("volumeId"),
    path = requireParam("path"),
    toWrite = requireParam("toWrite"),
  } = requireParam("options"),
) =>
  writeFile({
    volumeId,
    path,
    toWrite: JSON.stringify(toWrite),
  });
const readJsonFile = async (
  { volumeId = requireParam("volumeId"), path = requireParam("path") } = requireParam("options"),
) => JSON.parse(await readFile({ volumeId, path }));
const createDir = (
  { volumeId = requireParam("volumeId"), path = requireParam("path") } = requireParam("options"),
) => Deno.core.opAsync("create_dir", volumeId, path);
const removeDir = (
  { volumeId = requireParam("volumeId"), path = requireParam("path") } = requireParam("options"),
) => Deno.core.opAsync("remove_dir", volumeId, path);
const trace = (whatToTrace = requireParam('whatToTrace')) => Deno.core.opSync("log_trace", whatToTrace);
const warn = (whatToTrace = requireParam('whatToTrace')) => Deno.core.opSync("log_warn", whatToTrace);
const error = (whatToTrace = requireParam('whatToTrace')) => Deno.core.opSync("log_error", whatToTrace);
const debug = (whatToTrace = requireParam('whatToTrace')) => Deno.core.opSync("log_debug", whatToTrace);
const info = (whatToTrace = requireParam('whatToTrace')) => Deno.core.opSync("log_info", whatToTrace);
const fetch = async (url = requireParam ('url'), options = null) => {
  const { body, ...response } = await Deno.core.opAsync("fetch", url, options);
  const textValue = Promise.resolve(body);
  return {
    ...response,
    text() {
      return textValue;
    },
    json() {
      return textValue.then((x) => JSON.parse(x));
    },
  };
};

const runRsync = (
  { 
    srcVolume = requireParam("srcVolume"),
    dstVolume = requireParam("dstVolume"),
    srcPath = requireParam("srcPath"),
    dstPath = requireParam("dstPath"),
    options = requireParam("options"),
 } = requireParam("options"),
) => {
  let id = Deno.core.opAsync("rsync", srcVolume, srcPath, dstVolume, dstPath, options);
  let waitPromise = null;
  return {
    async id() {
      return id
    },
    async wait() {
      waitPromise = waitPromise || Deno.core.opAsync("rsync_wait", await id)
      return waitPromise
    },
    async progress() {
      return Deno.core.opAsync("rsync_progress", await id)
    }
  }
};

const currentFunction = Deno.core.opSync("current_function");
const input = Deno.core.opSync("get_input");
const variable_args = Deno.core.opSync("get_variable_args");
const setState = (x) => Deno.core.opSync("set_value", x);
const effects = {
  writeFile,
  readFile,
  writeJsonFile,
  readJsonFile,
  error,
  warn,
  debug,
  trace,
  info,
  isSandboxed,
  fetch,
  removeFile,
  createDir,
  removeDir,
  metadata,
  rename,
  runCommand,
  sleep,
  runDaemon,
  signalGroup,
  runRsync
};

const defaults = {
  "handleSignal": (effects, { gid, signal }) => {
    return effects.signalGroup({ gid, signal })
  }
}

const runFunction = jsonPointerValue(mainModule, currentFunction) || jsonPointerValue(defaults, currentFunction);
(async () => {
  if (typeof runFunction !== "function") {
    error(`Expecting ${currentFunction} to be a function`);
    throw new Error(`Expecting ${currentFunction} to be a function`);
  }
  const answer = await runFunction(effects, input, ...variable_args);
  setState(answer);
})();
