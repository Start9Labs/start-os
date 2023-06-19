'use strict';

var net = require('net');
var Start9Package = require('service');

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
var Start9Package__namespace = /*#__PURE__*/_interopNamespaceDefault(Start9Package);

const isObject = (x) => typeof x === "object" && x != null;
const isFunctionTest = (x) => typeof x === "function";
const isNumber = (x) => typeof x === "number";
const isString = (x) => typeof x === "string";
const booleanOnParse = {
    parsed(_) {
        return true;
    },
    invalid(_) {
        return false;
    },
};

class GuardParser {
    constructor(checkIsA, typeName, description = {
        name: "Guard",
        children: [],
        extras: [typeName],
    }) {
        Object.defineProperty(this, "checkIsA", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: checkIsA
        });
        Object.defineProperty(this, "typeName", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: typeName
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (this.checkIsA(a)) {
            return onParse.parsed(a);
        }
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

/**
 * Tries and run the stringify, if that fails just return the toString
 * @param x Could be anything, including a recursive object
 */
function saferStringify(x) {
    try {
        return JSON.stringify(x);
    }
    catch (e) {
        return "" + x;
    }
}

class AnyParser {
    constructor(description = {
        name: "Any",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        return onParse.parsed(a);
    }
}

class ArrayParser {
    constructor(description = {
        name: "Array",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (Array.isArray(a))
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

class BoolParser {
    constructor(description = {
        name: "Boolean",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (a === true || a === false)
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

class ConcatParsers {
    constructor(parent, otherParser, description = {
        name: "Concat",
        children: [parent, otherParser],
        extras: [],
    }) {
        Object.defineProperty(this, "parent", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parent
        });
        Object.defineProperty(this, "otherParser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: otherParser
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    static of(parent, otherParser) {
        if (parent.unwrappedParser().description.name === "Any") {
            return otherParser;
        }
        if (otherParser.unwrappedParser().description.name === "Any") {
            return parent;
        }
        return new ConcatParsers(parent, otherParser);
    }
    parse(a, onParse) {
        const parent = this.parent.enumParsed(a);
        if ("error" in parent) {
            return onParse.invalid(parent.error);
        }
        const other = this.otherParser.enumParsed(parent.value);
        if ("error" in other) {
            return onParse.invalid(other.error);
        }
        return onParse.parsed(other.value);
    }
}

class DefaultParser {
    constructor(parent, defaultValue, description = {
        name: "Default",
        children: [parent],
        extras: [defaultValue],
    }) {
        Object.defineProperty(this, "parent", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parent
        });
        Object.defineProperty(this, "defaultValue", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: defaultValue
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        // deno-lint-ignore no-this-alias
        const parser = this;
        const defaultValue = this.defaultValue;
        if (a == null) {
            return onParse.parsed(defaultValue);
        }
        const parentCheck = this.parent.enumParsed(a);
        if ("error" in parentCheck) {
            parentCheck.error.parser = parser;
            return onParse.invalid(parentCheck.error);
        }
        return onParse.parsed(parentCheck.value);
    }
}

class FunctionParser {
    constructor(description = {
        name: "Function",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (isFunctionTest(a))
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

class MappedAParser {
    constructor(parent, map, mappingName = map.name, description = {
        name: "Mapped",
        children: [parent],
        extras: [mappingName],
    }) {
        Object.defineProperty(this, "parent", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parent
        });
        Object.defineProperty(this, "map", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: map
        });
        Object.defineProperty(this, "mappingName", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: mappingName
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        const map = this.map;
        const result = this.parent.enumParsed(a);
        if ("error" in result) {
            return onParse.invalid(result.error);
        }
        return onParse.parsed(map(result.value));
    }
}

class MaybeParser {
    constructor(parent, description = {
        name: "Maybe",
        children: [parent],
        extras: [],
    }) {
        Object.defineProperty(this, "parent", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parent
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (a == null) {
            return onParse.parsed(null);
        }
        // deno-lint-ignore no-this-alias
        const parser = this;
        const parentState = this.parent.enumParsed(a);
        if ("error" in parentState) {
            const { error } = parentState;
            error.parser = parser;
            return onParse.invalid(error);
        }
        return onParse.parsed(parentState.value);
    }
}

class NamedParser {
    constructor(parent, name, description = {
        name: "Named",
        children: [parent],
        extras: [name],
    }) {
        Object.defineProperty(this, "parent", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parent
        });
        Object.defineProperty(this, "name", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: name
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        // deno-lint-ignore no-this-alias
        const parser = this;
        const parent = this.parent.enumParsed(a);
        if ("error" in parent) {
            const { error } = parent;
            error.parser = parser;
            return onParse.invalid(error);
        }
        return onParse.parsed(parent.value);
    }
}
function parserName(name, parent) {
    return new Parser(new NamedParser(parent, name));
}

class NilParser {
    constructor(description = {
        name: "Null",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (a === null || a === undefined)
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

class NumberParser {
    constructor(description = {
        name: "Number",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (isNumber(a))
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

class ObjectParser {
    constructor(description = {
        name: "Object",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (isObject(a))
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

class OrParsers {
    constructor(parent, otherParser, description = {
        name: "Or",
        children: [parent, otherParser],
        extras: [],
    }) {
        Object.defineProperty(this, "parent", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parent
        });
        Object.defineProperty(this, "otherParser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: otherParser
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        // deno-lint-ignore no-this-alias
        const parser = this;
        const parent = this.parent.enumParsed(a);
        if ("value" in parent) {
            return onParse.parsed(parent.value);
        }
        const other = this.otherParser.enumParsed(a);
        if ("error" in other) {
            const { error } = other;
            error.parser = parser;
            return onParse.invalid(error);
        }
        return onParse.parsed(other.value);
    }
}

// deno-lint-ignore-file no-explicit-any ban-types
/**
 * Given an object, we want to make sure the key exists and that the value on
 * the key matches the parser
 */
class ShapeParser {
    constructor(parserMap, isPartial, parserKeys = Object.keys(parserMap), description = {
        name: isPartial ? "Partial" : "Shape",
        children: parserKeys.map((key) => parserMap[key]),
        extras: parserKeys,
    }) {
        Object.defineProperty(this, "parserMap", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parserMap
        });
        Object.defineProperty(this, "isPartial", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: isPartial
        });
        Object.defineProperty(this, "parserKeys", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parserKeys
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        // deno-lint-ignore no-this-alias
        const parser = this;
        if (!object.test(a)) {
            return onParse.invalid({
                value: a,
                keys: [],
                parser,
            });
        }
        const { parserMap, isPartial } = this;
        const value = { ...a };
        if (Array.isArray(a)) {
            value.length = a.length;
        }
        for (const key in parserMap) {
            if (key in value) {
                const parser = parserMap[key];
                const state = parser.enumParsed(a[key]);
                if ("error" in state) {
                    const { error } = state;
                    error.keys.push(saferStringify(key));
                    return onParse.invalid(error);
                }
                const smallValue = state.value;
                value[key] = smallValue;
            }
            else if (!isPartial) {
                return onParse.invalid({
                    value: "missingProperty",
                    parser,
                    keys: [saferStringify(key)],
                });
            }
        }
        return onParse.parsed(value);
    }
}
const isPartial = (testShape) => {
    return new Parser(new ShapeParser(testShape, true));
};
/**
 * Good for duck typing an object, with optional values
 * @param testShape Shape of validators, to ensure we match the shape
 */
const partial = isPartial;
/**
 * Good for duck typing an object
 * @param testShape Shape of validators, to ensure we match the shape
 */
const isShape = (testShape) => {
    return new Parser(new ShapeParser(testShape, false));
};
function shape(testShape, optionals, optionalAndDefaults) {
    if (optionals) {
        const defaults = optionalAndDefaults || {};
        const entries = Object.entries(testShape);
        const optionalSet = new Set(Array.from(optionals));
        return every(partial(Object.fromEntries(entries
            .filter(([key, _]) => optionalSet.has(key))
            .map(([key, parser]) => [key, parser.optional()]))), isShape(Object.fromEntries(entries.filter(([key, _]) => !optionalSet.has(key))))).map((ret) => {
            for (const key of optionalSet) {
                const keyAny = key;
                if (!(keyAny in ret) && keyAny in defaults) {
                    ret[keyAny] = defaults[keyAny];
                }
            }
            return ret;
        });
    }
    return isShape(testShape);
}

class StringParser {
    constructor(description = {
        name: "String",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (isString(a))
            return onParse.parsed(a);
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}

function unwrapParser(a) {
    if (a instanceof Parser)
        return unwrapParser(a.parser);
    return a;
}
const enumParsed = {
    parsed(value) {
        return { value };
    },
    invalid(error) {
        return { error };
    },
};
/**
 * A Parser is usually a function that takes a value and returns a Parsed value.
 * For this class we have that as our main reason but we want to be able to have other methods
 * including testing and showing text representations.
 *
 * The main function unsafeCast which will take in a value A (usually unknown) and will always return a B. If it cannot
 * it will throw an error.
 *
 * The parse function is the lower level function that will take in a value and a dictionary of what to do with success and failure.
 */
class Parser {
    constructor(parser, description = {
        name: "Wrapper",
        children: [parser],
        extras: [],
    }) {
        Object.defineProperty(this, "parser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parser
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
        /// This is a hack to get the type of what the parser is going to return.
        // deno-lint-ignore no-explicit-any
        Object.defineProperty(this, "_TYPE", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: null
        });
        /**
         * Use this when you want to decide what happens on the succes and failure cases of parsing
         * @param a
         * @param onParse
         * @returns
         */
        Object.defineProperty(this, "parse", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (a, onParse) => {
                return this.parser.parse(a, onParse);
            }
        });
        /**
         * This is the most useful parser, it assumes the happy path and will throw an error if it fails.
         * @param value
         * @returns
         */
        Object.defineProperty(this, "unsafeCast", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (value) => {
                const state = this.enumParsed(value);
                if ("value" in state)
                    return state.value;
                const { error } = state;
                throw new TypeError(`Failed type: ${Parser.validatorErrorAsString(error)} given input ${saferStringify(value)}`);
            }
        });
        /**
         * This is the like the unsafe parser, it assumes the happy path and will throw and return a failed promise during failure.
         * @param value
         * @returns
         */
        Object.defineProperty(this, "castPromise", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (value) => {
                const state = this.enumParsed(value);
                if ("value" in state)
                    return Promise.resolve(state.value);
                const { error } = state;
                return Promise.reject(new TypeError(`Failed type: ${Parser.validatorErrorAsString(error)} given input ${saferStringify(value)}`));
            }
        });
        /**
         * When we want to get the error message from the input, to know what is wrong
         * @param input
         * @returns Null if there is no error
         */
        Object.defineProperty(this, "errorMessage", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (input) => {
                const parsed = this.parse(input, enumParsed);
                if ("value" in parsed)
                    return;
                return Parser.validatorErrorAsString(parsed.error);
            }
        });
        /**
         * Use this that we want to do transformations after the value is valid and parsed.
         * A use case would be parsing a string, making sure it can be parsed to a number, and then convert to a number
         * @param fn
         * @param mappingName
         * @returns
         */
        Object.defineProperty(this, "map", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (fn, mappingName) => {
                return new Parser(new MappedAParser(this, fn, mappingName));
            }
        });
        /**
         * Use this when you want to combine two parsers into one. This will make sure that both parsers will run against the same value.
         * @param otherParser
         * @returns
         */
        Object.defineProperty(this, "concat", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (otherParser) => {
                // deno-lint-ignore no-explicit-any
                return new Parser(ConcatParsers.of(this, new Parser(otherParser)));
            }
        });
        /**
         * Use this to combine parsers into one. This will make sure that one or the other parsers will run against the value.
         * @param otherParser
         * @returns
         */
        Object.defineProperty(this, "orParser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (otherParser) => {
                return new Parser(new OrParsers(this, new Parser(otherParser)));
            }
        });
        /**
         * Use this as a guard clause, useful for escaping during the error cases.
         * https://www.typescriptlang.org/docs/handbook/advanced-types.html#type-guards-and-differentiating-types
         * @param value
         * @returns
         */
        Object.defineProperty(this, "test", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (value) => {
                return this.parse(value, booleanOnParse);
            }
        });
        /**
         * When we want to make sure that we handle the null later on in a monoid fashion,
         * and this ensures we deal with the value
         * https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#optional-chaining
         */
        Object.defineProperty(this, "optional", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (_name) => {
                return new Parser(new MaybeParser(this));
            }
        });
        /**
         * There are times that we would like to bring in a value that we know as null or undefined
         * and want it to go to a default value
         */
        Object.defineProperty(this, "defaultTo", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (defaultValue) => {
                return new Parser(new DefaultParser(new Parser(new MaybeParser(this)), defaultValue));
            }
        });
        /**
         * We want to test value with a test eg isEven
         */
        Object.defineProperty(this, "validate", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (isValid, otherName) => {
                return new Parser(ConcatParsers.of(this, new Parser(new GuardParser(isValid, otherName))));
            }
        });
        /**
         * We want to refine to a new type given an original type, like isEven, or casting to a more
         * specific type
         */
        Object.defineProperty(this, "refine", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (refinementTest, otherName = refinementTest.name) => {
                return new Parser(ConcatParsers.of(this, new Parser(new GuardParser(refinementTest, otherName))));
            }
        });
        /**
         * Use this when we want to give the parser a name, and we want to be able to use the name in the error messages.
         * @param nameString
         * @returns
         */
        Object.defineProperty(this, "rename", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (nameString) => {
                return parserName(nameString, this);
            }
        });
        /**
         * This is another type of parsing that will return a value that is a discriminated union of the success and failure cases.
         * https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html#discriminated-unions
         * @param value
         * @returns
         */
        Object.defineProperty(this, "enumParsed", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (value) => {
                // deno-lint-ignore no-explicit-any
                return this.parse(value, enumParsed);
            }
        });
        /**
         * Return the unwrapped parser/ IParser
         * @returns
         */
        Object.defineProperty(this, "unwrappedParser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: () => {
                // deno-lint-ignore no-this-alias no-explicit-any
                let answer = this;
                while (true) {
                    const next = answer.parser;
                    if (next instanceof Parser) {
                        answer = next;
                    }
                    else {
                        return next;
                    }
                }
            }
        });
    }
    /**
     * This is a constructor helper that can use a predicate tester in the form of a guard function,
     * and will return a parser that will only parse if the predicate returns true.
     * https://www.typescriptlang.org/docs/handbook/advanced-types.html#type-guards-and-differentiating-types
     * @param checkIsA
     * @param name
     * @returns
     */
    static isA(checkIsA, name) {
        return new Parser(new GuardParser(checkIsA, name));
    }
    /**
     * Trying to convert the parser into a string representation
     * @param parserComingIn
     * @returns
     */
    static parserAsString(parserComingIn) {
        const parser = unwrapParser(parserComingIn);
        const { description: { name, extras, children }, } = parser;
        if (parser instanceof ShapeParser) {
            return `${name}<{${parser.description.children
                .map((subParser, i) => `${String(parser.description.extras[i]) || "?"}:${Parser.parserAsString(subParser)}`)
                .join(",")}}>`;
        }
        if (parser instanceof OrParsers) {
            const parent = unwrapParser(parser.parent);
            const parentString = Parser.parserAsString(parent);
            if (parent instanceof OrParsers)
                return parentString;
            return `${name}<${parentString},...>`;
        }
        if (parser instanceof GuardParser) {
            return String(extras[0] || name);
        }
        if (parser instanceof StringParser ||
            parser instanceof ObjectParser ||
            parser instanceof NumberParser ||
            parser instanceof BoolParser ||
            parser instanceof AnyParser) {
            return name.toLowerCase();
        }
        if (parser instanceof FunctionParser) {
            return name;
        }
        if (parser instanceof NilParser) {
            return "null";
        }
        if (parser instanceof ArrayParser) {
            return "Array<unknown>";
        }
        const specifiers = [
            ...extras.map(saferStringify),
            ...children.map(Parser.parserAsString),
        ];
        const specifiersString = `<${specifiers.join(",")}>`;
        return `${name}${specifiersString}`;
    }
}
/**
 * This is the line of code that could be over written if
 * One would like to have a custom error as any shape
 */
Object.defineProperty(Parser, "validatorErrorAsString", {
    enumerable: true,
    configurable: true,
    writable: true,
    value: (error) => {
        const { parser, value, keys } = error;
        const keysString = !keys.length ? "" : keys
            .map((x) => `[${x}]`)
            .reverse()
            .join("");
        return `${keysString}${Parser.parserAsString(parser)}(${saferStringify(value)})`;
    }
});

class UnknownParser {
    constructor(description = {
        name: "Unknown",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        return onParse.parsed(a);
    }
}

/**
 * Create a custom type guard
 * @param test A function that will determine runtime if the value matches
 * @param testName A name for that function, useful when it fails
 */
function guard(test, testName) {
    return Parser.isA(test, testName || test.name);
}
const any = new Parser(new AnyParser());
const unknown = new Parser(new UnknownParser());
const number = new Parser(new NumberParser());
const isNill = new Parser(new NilParser());
const natural = number.refine((x) => x >= 0 && x === Math.floor(x));
const isFunction = new Parser(new FunctionParser());
const boolean = new Parser(new BoolParser());
const objectMatcher = new Parser(new ObjectParser());
// deno-lint-ignore ban-types
const object = Object.assign(
// deno-lint-ignore no-explicit-any
function objectOf(...args) {
    // deno-lint-ignore no-explicit-any
    return shape(...args);
}, objectMatcher);
const isArray = new Parser(new ArrayParser());
const string = new Parser(new StringParser());
const instanceOf = (classCreator) => guard((x) => x instanceof classCreator, `is${classCreator.name}`);
const regex = (tester) => string.refine(function (x) {
    return tester.test(x);
}, tester.toString());

/**
 * Union is a good tool to make sure that the validated value
 * is in the union of all the validators passed in. Basically an `or`
 * operator for validators.
 */
function some(...parsers) {
    if (parsers.length <= 0) {
        return any;
    }
    const first = parsers.splice(0, 1)[0];
    return parsers.reduce((left, right) => left.orParser(right), first);
}

// deno-lint-ignore-file no-explicit-any
/**
 * Intersection is a good tool to make sure that the validated value
 * is in the intersection of all the validators passed in. Basically an `and`
 * operator for validators
 */
function every(...parsers) {
    const filteredParsers = parsers.filter((x) => x !== any);
    if (filteredParsers.length <= 0) {
        return any;
    }
    const first = filteredParsers.splice(0, 1)[0];
    return filteredParsers.reduce((left, right) => {
        return left.concat(right);
    }, first);
}

// deno-lint-ignore-file no-explicit-any ban-types
class DictionaryParser {
    constructor(parsers, description = {
        name: "Dictionary",
        children: parsers.reduce((acc, [k, v]) => {
            acc.push(k, v);
            return acc;
        }, []),
        extras: [],
    }) {
        Object.defineProperty(this, "parsers", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parsers
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        const { parsers } = this;
        // deno-lint-ignore no-this-alias
        const parser = this;
        const entries = Object.entries(a);
        for (const entry of entries) {
            const [key, value] = entry;
            const found = findOrError(parsers, key, value, parser);
            if (found == undefined)
                return onParse.parsed(a);
            if ("error" in found)
                return onParse.invalid(found.error);
            entry[0] = found[0].value;
            entry[1] = found[1].value;
        }
        const answer = Object.fromEntries(entries);
        return onParse.parsed(answer);
    }
}
const dictionary = (...parsers) => {
    return object.concat(new DictionaryParser([...parsers]));
};
function findOrError(parsers, key, value, parser) {
    let foundError;
    for (const [keyParser, valueParser] of parsers) {
        const enumState = keyParser.enumParsed(key);
        const valueState = valueParser.enumParsed(value);
        if ("error" in enumState) {
            if (!foundError) {
                const { error } = enumState;
                error.parser = parser;
                error.keys.push("" + key);
                foundError = { error };
            }
            continue;
        }
        const newKey = enumState.value;
        if ("error" in valueState) {
            if (!foundError) {
                const { error } = valueState;
                error.keys.push("" + newKey);
                foundError = { error };
            }
            continue;
        }
        return [enumState, valueState];
    }
    return foundError;
}

// deno-lint-ignore-file no-explicit-any
class TupleParser {
    constructor(parsers, lengthMatcher = literal(parsers.length), description = {
        name: "Tuple",
        children: parsers,
        extras: [],
    }) {
        Object.defineProperty(this, "parsers", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parsers
        });
        Object.defineProperty(this, "lengthMatcher", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: lengthMatcher
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(input, onParse) {
        const tupleError = isArray.enumParsed(input);
        if ("error" in tupleError)
            return onParse.invalid(tupleError.error);
        const values = input;
        const stateCheck = this.lengthMatcher.enumParsed(values.length);
        if ("error" in stateCheck) {
            stateCheck.error.keys.push(saferStringify("length"));
            return onParse.invalid(stateCheck.error);
        }
        const answer = new Array(this.parsers.length);
        for (const key in this.parsers) {
            const parser = this.parsers[key];
            const value = values[key];
            const result = parser.enumParsed(value);
            if ("error" in result) {
                const { error } = result;
                error.keys.push(saferStringify(key));
                return onParse.invalid(error);
            }
            answer[key] = result.value;
        }
        return onParse.parsed(answer);
    }
}
function tuple(...parsers) {
    return new Parser(new TupleParser(parsers));
}

// deno-lint-ignore-file no-explicit-any
/**
 * Given an object, we want to make sure the key exists and that the value on
 * the key matches the parser
 * Note: This will mutate the value sent through
 */
class ArrayOfParser {
    constructor(parser, description = {
        name: "ArrayOf",
        children: [parser],
        extras: [],
    }) {
        Object.defineProperty(this, "parser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: parser
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (!Array.isArray(a)) {
            return onParse.invalid({
                value: a,
                keys: [],
                parser: this,
            });
        }
        const values = [...a];
        for (let index = 0; index < values.length; index++) {
            const result = this.parser.enumParsed(values[index]);
            if ("error" in result) {
                result.error.keys.push("" + index);
                return onParse.invalid(result.error);
            }
            else {
                values[index] = result.value;
            }
        }
        return onParse.parsed(values);
    }
}
/**
 * We would like to validate that all of the array is of the same type
 * @param validator What is the validator for the values in the array
 */
function arrayOf(validator) {
    return new Parser(new ArrayOfParser(validator));
}

class LiteralsParser {
    constructor(values, description = {
        name: "Literal",
        children: [],
        extras: values,
    }) {
        Object.defineProperty(this, "values", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: values
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
    }
    parse(a, onParse) {
        if (this.values.indexOf(a) >= 0) {
            return onParse.parsed(a);
        }
        return onParse.invalid({
            value: a,
            keys: [],
            parser: this,
        });
    }
}
function literal(isEqualToValue) {
    return new Parser(new LiteralsParser([isEqualToValue]));
}
function literals(firstValue, ...restValues) {
    return new Parser(new LiteralsParser([firstValue, ...restValues]));
}

/**
 * This parser is used when trying to create parsers that
 * user their own definitions in their types, like interface Tree<Leaf> {
 *   [key: string]: Tree<Leaf> | Leaf;
 * }
 */
class RecursiveParser {
    constructor(recursive, description = {
        name: "Recursive",
        children: [],
        extras: [recursive],
    }) {
        Object.defineProperty(this, "recursive", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: recursive
        });
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
        Object.defineProperty(this, "parser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
    }
    static create(fn) {
        const parser = new RecursiveParser(fn);
        parser.parser = fn(new Parser(parser));
        return parser;
    }
    parse(a, onParse) {
        if (!this.parser) {
            return onParse.invalid({
                value: "Recursive Invalid State",
                keys: [],
                parser: this,
            });
        }
        return this.parser.parse(a, onParse);
    }
}
/**
 * Must pass the shape that we expect since typescript as of this point
 * can't infer with recursive functions like this.
 * @param fn This should be a function that takes a parser, basically the self in a type recursion, and
 * return a parser that is the combination of the recursion.
 * @returns
 */
function recursive(fn) {
    fn(any);
    const created = RecursiveParser
        .create(fn);
    return new Parser(created);
}

/**
 * This is needed when the typescript has a recursive, mutual types
 * type Things = string | [OtherThings]
 * type OtherThings = {type: 'other', value:Things }
 */
class DeferredParser {
    constructor(description = {
        name: "Deferred",
        children: [],
        extras: [],
    }) {
        Object.defineProperty(this, "description", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: description
        });
        Object.defineProperty(this, "parser", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
    }
    static create() {
        return new DeferredParser();
    }
    setParser(parser) {
        this.parser = new Parser(parser);
        return this;
    }
    parse(a, onParse) {
        if (!this.parser) {
            return onParse.invalid({
                value: "Not Set Up",
                keys: [],
                parser: this,
            });
        }
        return this.parser.parse(a, onParse);
    }
}
/**
 * Must pass the shape that we expect since typescript as of this point
 * can't infer with recursive like structures like this.
 * @returns [Parser, setParser] Use the setParser to set the parser later
 */
function deferred() {
    const deferred = DeferredParser.create();
    function setParser(parser) {
        deferred.setParser(parser);
    }
    return [new Parser(deferred), setParser];
}

class Matched {
    constructor(value) {
        Object.defineProperty(this, "value", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: value
        });
        Object.defineProperty(this, "when", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: ((..._args) => {
                // deno-lint-ignore no-explicit-any
                return this;
                // deno-lint-ignore no-explicit-any
            })
        });
        Object.defineProperty(this, "unwrap", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (() => {
                return this.value;
                // deno-lint-ignore no-explicit-any
            })
        });
    }
    defaultTo(_defaultValue) {
        return this.value;
    }
    defaultToLazy(_getValue) {
        return this.value;
    }
}
class MatchMore {
    constructor(a) {
        Object.defineProperty(this, "a", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: a
        });
        Object.defineProperty(this, "when", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: ((...args) => {
                const [outcome, ...matchers] = args.reverse();
                // deno-lint-ignore no-this-alias
                const me = this;
                const parser = matches.some(...matchers.map((matcher) => 
                // deno-lint-ignore no-explicit-any
                matcher instanceof Parser ? matcher : literal(matcher)));
                const result = parser.enumParsed(this.a);
                if ("error" in result) {
                    // deno-lint-ignore no-explicit-any
                    return me;
                }
                const { value } = result;
                if (outcome instanceof Function) {
                    // deno-lint-ignore no-explicit-any
                    return new Matched(outcome(value));
                }
                // deno-lint-ignore no-explicit-any
                return new Matched(outcome);
                // deno-lint-ignore no-explicit-any
            })
        });
        Object.defineProperty(this, "unwrap", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: (() => {
                throw new Error("Expecting that value is matched");
                // deno-lint-ignore no-explicit-any
            })
        });
    }
    defaultTo(value) {
        return value;
    }
    defaultToLazy(getValue) {
        return getValue();
    }
}
const array = Object.assign(
// deno-lint-ignore no-explicit-any
function arrayOfWrapper(...args) {
    // deno-lint-ignore no-explicit-any
    return arrayOf(...args);
}, isArray);
/**
 * Want to be able to bring in the declarative nature that a functional programming
 * language feature of the pattern matching and the switch statement. With the destructors
 * the only thing left was to find the correct structure then move move forward.
 * Using a structure in chainable fashion allows for a syntax that works with typescript
 * while looking similar to matches statements in other languages
 *
 * Use: matches('a value').when(matches.isNumber, (aNumber) => aNumber + 4).defaultTo('fallback value')
 */
const matches = Object.assign(function matchesFn(value) {
    return new MatchMore(value);
}, {
    array,
    arrayOf,
    some,
    tuple,
    regex,
    number,
    natural,
    isFunction,
    object,
    string,
    shape,
    partial,
    literal,
    every,
    guard,
    unknown,
    any,
    boolean,
    dictionary,
    literals,
    nill: isNill,
    instanceOf,
    Parse: Parser,
    parserName,
    recursive,
    deferred,
});

// @ts-check
const idType = some(string, number);
const path = "/start9/sockets/rpc.sock";
const runType = object({
    id: idType,
    method: literal("run"),
    input: object({
        methodName: string,
        methodArgs: array,
    }),
});
const dealWithInput = async (input) => matches(input)
    .when(runType, async ({ id, input: { methodName, methodArgs } }) => Promise.resolve(Start9Package__namespace[methodName])
    .then((x) => (typeof x === "function" ? x(...methodArgs) : x))
    .then(JSON.stringify)
    .then((result) => ({ id, result }))
    .catch((error) => ({
    id,
    error: { message: error?.message ?? String(error) },
})))
    .defaultToLazy(() => {
    console.warn(`Coudln't parse the following input ${input}`);
});
const jsonParse = (x) => JSON.parse(x.toString());
class Runtime {
    unixSocketServer = net__namespace.createServer(async (server) => { });
    constructor() {
        this.unixSocketServer.listen(path);
        this.unixSocketServer.on("connection", (s) => {
            s.on("data", (a) => Promise.resolve(a)
                .then(jsonParse)
                .then(dealWithInput)
                .then((x) => s.write(String(s)))
                // .catch(x => ({id: x.id, error: x.message})}))
                .finally(() => void s.end()));
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
