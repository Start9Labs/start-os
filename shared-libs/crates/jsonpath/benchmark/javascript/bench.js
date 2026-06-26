let json = {
    'store': {
        'book': [
            {
                'category': 'reference',
                'author': 'Nigel Rees',
                'title': 'Sayings of the Century',
                'price': 8.95,
            },
            {
                'category': 'fiction',
                'author': 'Evelyn Waugh',
                'title': 'Sword of Honour',
                'price': 12.99,
            },
            {
                'category': 'fiction',
                'author': 'Herman Melville',
                'title': 'Moby Dick',
                'isbn': '0-553-21311-3',
                'price': 8.99,
            },
            {
                'category': 'fiction',
                'author': 'J. R. R. Tolkien',
                'title': 'The Lord of the Rings',
                'isbn': '0-395-19395-8',
                'price': 22.99,
            },
        ],
        'bicycle': {
            'color': 'red',
            'price': 19.95,
        },
    },
    'expensive': 10,
};
let jsonStr = JSON.stringify(json);

function getJson() {
    return JSON.parse(jsonStr);
}
const path = '$..book[?(@.price<30 && @.category=="fiction")]';
const jp = require('jsonpath');
const jpw = require('jsonpath-wasm');

function jsonpath() {
    for (var i = 0; i < iter; i++) {
        let _ = jp.query(getJson(), path);
    }
}

function wasmSelector() {
    let selector = jpw.selector(getJson());
    for (var i = 0; i < iter; i++) {
        let _ = selector(path);
    }
}

function wasmCompile() {
    let template = jpw.compile(path);
    for (var i = 0; i < iter; i++) {
        let _ = template(getJson());
    }
}

function wasmSelect() {
    for (var i = 0; i < iter; i++) {
        let _ = jpw.select(getJson(), path);
    }
}

function wasmSelectorClass() {
    let selector = new jpw.Selector();
    for (var i = 0; i < iter; i++) {
        selector.path(path);
        selector.value(jsonStr);
        let _ = selector.select();
    }
}

const functionName = process.argv[2];
const iter = parseInt(process.argv[3], 10);
eval(functionName + "()");