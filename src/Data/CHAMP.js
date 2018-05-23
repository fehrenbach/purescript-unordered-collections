"use strict";

/** @constructor */
function Node(datamap, nodemap, content) {
    this.datamap = datamap;
    this.nodemap = nodemap;
    this.content = content;
}

Node.prototype.getKey = function (index) {
    return this.content[index * 2];
}
Node.prototype.getValue = function (index) {
    return this.content[index * 2 + 1];
}
Node.prototype.getNode = function (index) {
    return this.content[this.content.length - 1 - index];
}
Node.prototype.lookup = lookup;
Node.prototype.insert = insert;
Node.prototype.delet = delet;
Node.prototype.toArrayBy = function (f, res) {
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        var k = this.content[i++];
        var v = this.content[i++];
        res.push(f(k)(v));
    }
    for (; i < this.content.length; i++)
        this.content[i].toArrayBy(f, res);
}

/** @constructor */
function Collision(keys, values) {
    this.keys = keys;
    this.values = values;
}

Collision.prototype.lookup = function collisionLookup(Nothing, Just, keyEquals, key, keyHash, shift) {
    for (var i = 0; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            return Just(this.values[i]);
    return Nothing;
};

Collision.prototype.insert = function collisionInsert(keyEquals, hashFunction, key, keyHash, value, shift) {
    var i = 0;
    for (; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            break;
    return new Collision(copyAndOverwriteOrExtend1(this.keys, i, key),
                         copyAndOverwriteOrExtend1(this.values, i, value));
};

Collision.prototype.delet = function collisionDelete(keyEquals, key, keyHash, shift) {
    throw "TODO collisionDelete"
}


Collision.prototype.toArrayBy = function (f, res) {
    for (var i = 0; i < this.keys.length; i++)
        res.push(f(this.keys[i])(this.values[i]));
}

function mask(keyHash, shift) {
    return 1 << ((keyHash >>> shift) & 31);
}

function index(map, bit) {
    return popCount(map & (bit - 1));
}

function lookup(Nothing, Just, keyEquals, key, keyHash, shift) {
    var bit = mask(keyHash, shift);
    if ((this.datamap & bit) !== 0) {
        var i = index(this.datamap, bit);
        // TODO compare hashes first?
        if (keyEquals(key, this.getKey(i))) {
            return Just(this.getValue(i));
        }
        return Nothing;
    }
    if ((this.nodemap & bit) !== 0) {
        return this.getNode(index(this.nodemap, bit)).lookup(Nothing, Just, keyEquals, key, keyHash, shift + 5);
    }
    return Nothing;
}

function popCount (n) {
    n = n - ((n >> 1) & 0x55555555);
    n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
    return ((n + (n >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
}

function binaryNode(k1, kh1, v1, k2, kh2, v2, s) {
    if (s >= 32) return new Collision([k1, k2], [v1, v2]);

    var b1 = (kh1 >>> s) & 31;
    var b2 = (kh2 >>> s) & 31;
    
    if (b1 !== b2) return new Node((1 << b1) | (1 << b2), 0, (b1 >>> 0) < (b2 >>> 0) ? [k1, v1, k2, v2] : [k2, v2, k1, v1]);

    return new Node(0, 1 << b1, [binaryNode(k1, kh1, v1, k2, kh2, v2, s + 5)]);
}

function overwriteTwoElements(a, index, v1, v2) {
    var res = a.slice();
    res[index] = v1;
    res[index+1] = v2;
    return res;
}

// TODO benchmark some alternative implementations (manual copy, slice
// left + copy?, slice both + concat?, ...)
function remove2(a, index) {
    var res = a.slice();
    res.splice(index, 2);
    return res;
}

// I think this is always called with a node index? Therefore, the
// left will often be larger than the right. Does that help?
function remove1(a, index) {
    var res = a.slice();
    res.splice(index, 1);
    return res;
}

// Make a copy while overwriting the element at index, or adding one element if index == a.length
function copyAndOverwriteOrExtend1(a, index, v) {
    var res = a.slice();
    res[index] = v;
    return res;
}

function insert(keyEquals, hashFunction, key, keyHash, value, shift) {
    var bit = mask(keyHash, shift);
    var i = index(this.datamap, bit);
    if ((this.datamap & bit) !== 0) {
        if (keyEquals(this.getKey(i))(key)) {
            return new Node(this.datamap, this.nodemap, overwriteTwoElements(this.content, i*2, key, value));
        } else {
            var newNode = binaryNode(this.getKey(i), hashFunction(this.getKey(i)), this.getValue(i), key, keyHash, value, shift + 5);
            var newLength = this.content.length - 1;
            var newContent = new Array(newLength);
            var newNodeIndex = newLength - index(this.nodemap, bit) - 1; // old length - 2 - nodeindex
            var j = 0;
            for (; j < i * 2; j++) newContent[j] = this.content[j];
            for (; j < newNodeIndex; j++) newContent[j] = this.content[j+2];
            newContent[j++] = newNode;
            for (; j < newLength; j++) newContent[j] = this.content[j+1];
            return new Node(this.datamap ^ bit, this.nodemap | bit, newContent);
        }
    }
    if ((this.nodemap & bit) !== 0) {
        var nodeIndex = index(this.nodemap, bit);
        /*const*/ newNode = (this.getNode(nodeIndex)).insert(keyEquals, hashFunction, key, keyHash, value, shift + 5);
        /*const*/ newContent = this.content.slice();
        newContent[newContent.length - nodeIndex - 1] = newNode;
        return new Node(this.datamap, this.nodemap, newContent);
    }
    /*const*/ newContent = new Array(this.content.length + 2);
    for (var k = 0; k < i * 2; k++) newContent[k] = this.content[k];
    newContent[k++] = key;
    newContent[k++] = value;
    for (; k < newContent.length; k++) newContent[k] = this.content[k - 2];
    return new Node(this.datamap | bit, this.nodemap, newContent);
}

function delet(keyEquals, key, keyHash, shift) {
    var bit = mask(keyHash, shift);
    if ((this.datamap & bit) !== 0) {
        var dataIndex = index(this.datamap, bit);
        if (keyEquals(this.getKey(i))(key)) {
            var newDatamap = this.datamap ^ dataIndex;
            if (newDatamap === 0 && this.nodemap === 0)
                return empty;
            return new Node(newDatamap, this.nodemap, remove2(this.content, dataIndex * 2));
        }
        return this;
    }
    if ((this.nodemap & bit) !== 0) {
        var nodeIndex = index(this.nodemap,bit);
        var recRes = this.getNode(nodeIndex).delet(keyEquals, key, keyHash, shift + 5);
        var nodemapWithout = this.nodemap ^ nodeIndex;
        throw "TODO"
        // if (popCount(nodemapWithout) == 0) {
        //     // 
        // }
        // return new Node(this.datamap, this.nodemap ^ nodeIndex, remove1(this.content, this.content.length - nodeIndex - 1));
    }
}

function l(k, m) {
    return m.lookup(null, function (a) { return a; }, function(a) { return function (b) { return a == b; } },
                    k, k, 0);
}

function i(k, v, m) {
    return m.insert(function(a) { return function (b) { return a == b; } },
                    function(a) { return a % 100; },
                    k, k, v, 0);
}

var empty = new Node(0,0,[],[]);

exports.empty = empty;
exports.lookupPurs = function (Nothing) {
    return function (Just) {
        return function (keyEquals) {
            return function (key) {
                return function (keyHash) {
                    return function (m) {
                        return m.lookup(Nothing, Just, keyEquals, key, keyHash, 0);
                    };
                };
            };
        };
    };
};

exports.insertPurs = function (keyEquals) {
    return function (hashFunction) {
        return function (key) {
            return function (value) {
                return function (m) {
                    return m.insert(keyEquals, hashFunction, key, hashFunction(key), value, 0);
                };
            };
        };
    };
};

exports.deletePurs = function (keyEquals) {
    return function (key) {
        return function (keyHash) {
            return function (m) {
                return m.delet(keyEquals, key, keyHash, 0);
            };
        };
    };
};

exports.toArrayBy = function (f) {
    return function (m) {
        var res = [];
        m.toArrayBy(f, res);
        return res;
    };
};
