import { displayUniqueBy } from '../src/app/app-config/config-cursor'

function assert (predicate: boolean, message: string) {
    if (!predicate) {
        throw new Error('Assertion Failed: ' + message)
    }
}

function assertEq (a: any, b: any, message: string) {
    assert(a === b, message)
}

function test () {
    assertEq(
        displayUniqueBy(
            'foo',
            {
                type: 'object',
                name: 'Object',
                spec: {
                    'foo': {
                        type: 'string',
                        name: 'Foo',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                },
                nullByDefault: false,
                nullable: false,
                uniqueBy: 'foo',
            },
            {
                foo: 'foo-val',
            },
        ),
        'Foo',
        'base string uses name mapping',
    )
    assertEq(
        displayUniqueBy(
            {
                any: [
                    'foo',
                    'bar',
                ],
            },
            {
                type: 'object',
                name: 'Object',
                spec: {
                    'foo': {
                        type: 'string',
                        name: 'Foo',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                    'bar': {
                        type: 'string',
                        name: 'Bar',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                },
                nullByDefault: false,
                nullable: false,
                uniqueBy: {
                    any: [
                        'foo',
                        'bar',
                    ],
                },
            },
            {
                foo: 'foo-val',
                bar: 'bar-val',
            },
        ),
        'Foo and Bar',
        '`any` must be joined with `and`',
    )
    assertEq(
        displayUniqueBy(
            {
                all: [
                    'foo',
                    'bar',
                ],
            },
            {
                type: 'object',
                name: 'Object',
                spec: {
                    'foo': {
                        type: 'string',
                        name: 'Foo',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                    'bar': {
                        type: 'string',
                        name: 'Bar',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                },
                nullByDefault: false,
                nullable: false,
                uniqueBy: {
                    all: [
                        'foo',
                        'bar',
                    ],
                },
            },
            {
                foo: 'foo-val',
                bar: 'bar-val',
            },
        ),
        'Foo or Bar',
        '`all` must be joined with `or`',
    )
    assertEq(
        displayUniqueBy(
            {
                any: [
                    'foo',
                    {
                        all: [
                            'bar',
                            'baz',
                        ],
                    },
                ],
            },
            {
                type: 'object',
                name: 'Object',
                spec: {
                    'foo': {
                        type: 'string',
                        name: 'Foo',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                    'bar': {
                        type: 'string',
                        name: 'Bar',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                    'baz': {
                        type: 'string',
                        name: 'Baz',
                        nullable: true,
                        copyable: false,
                        masked: false,
                    },
                },
                nullByDefault: false,
                nullable: false,
                uniqueBy: {
                    any: [
                        'foo',
                        {
                            all: [
                                'bar',
                                'baz',
                            ],
                        },
                    ],
                },
            },
            {
                foo: 'foo-val',
                bar: 'bar-val',
                baz: 'baz-val',
            },
        ),
        'Foo and (Bar or Baz)',
        '`any` of `all` is correct',
    )
    assertEq(
        displayUniqueBy(
            {
                any: [
                    'foo',
                    {
                        all: [
                            'bar',
                            'baz',
                        ],
                    },
                ],
            },
            {
                type: 'union',
                name: 'Union',
                tag: {
                    id: 'variant',
                    name: 'Variant',
                    variantNames: {
                        'variant-a': 'Variant A',
                        'variant-b': 'Variant B',
                    },
                },
                variants: {
                    'variant-a': {
                        'foo': {
                            type: 'string',
                            name: 'Foo',
                            nullable: true,
                            copyable: false,
                            masked: false,
                        },
                        'bar': {
                            type: 'string',
                            name: 'Bar',
                            nullable: true,
                            copyable: false,
                            masked: false,
                        },
                        'baz': {
                            type: 'string',
                            name: 'Baz',
                            nullable: true,
                            copyable: false,
                            masked: false,
                        },
                    },
                    'variant-b': { },
                },
                uniqueBy: {
                    any: [
                        'foo',
                        {
                            all: [
                                'bar',
                                'baz',
                            ],
                        },
                    ],
                },
                default: 'variant-a',
            },
            {
                variant: 'variant-a',
                foo: 'foo-val',
                bar: 'bar-val',
                baz: 'baz-val',
            },
        ),
        'Foo and (Bar or Baz)',
        'union is correct',
    )
}
