"use strict";

/** @constructor */
function Node(datamap, nodemap, keyhashes, content) {
    this.datamap = datamap;
    this.nodemap = nodemap;
    // TODO try storing inline with content? [k1, h1, v1, k2, h2, v2, ..., n1, ...]
    this.keyhashes = keyhashes;
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

/** @constructor */
function Collision(keys, values) {
    this.keys = keys;
    this.values = values;
}

Collision.prototype.lookup = function collisionLookup(Nothing, Just, keyEquals, key, keyHash, shift) {
    for (var i = 0; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            return Just(this.values[i]);
};

Collision.prototype.insert = function collisionInsert(keyEquals, key, keyHash, value, shift) {
    var i = 0;
    for (; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            break;
    return new Collision(this.keys.slice()[i] = key, this.values.slice()[i] = value);
};

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
    if (s >= 32) {
        return new Collision([k1, k2], [v1, v2]);
    }

    var b1 = (kh1 >>> s) & 31;
    var b2 = (kh2 >>> s) & 31;
    
    if (b1 !== b2) {
        var datamap = (1 << b1) | (1 << b2);
        if ((b1 >>> 0) < (b2 >>> 0)) {
            return new Node(datamap, 0, [kh1, kh2], [k1, v1, k2, v2]);
        } else {
            return new Node(datamap, 0, [kh2, kh1], [k2, v2, k1, v1]);
        }
    }

    var node = binaryNode(k1, kh1, v1, k2, kh2, v2, s + 5);
    return new Node(0, 1 << b1, [], [node]);
}

function overwriteTwoElements(a, index, v1, v2) {
    var res = a.slice();
    res[index] = v1;
    res[index+1] = v2;
    return res;
}

function insert(keyEquals, key, keyHash, value, shift) {
    var bit = mask(keyHash, shift);
    var i = index(this.datamap, bit);
    if ((this.datamap & bit) !== 0) {
        // TODO compare hashes first?!
        if (keyEquals(this.getKey(i))(key)) {
            return new Node(this.datamap, this.nodemap, this.keyhashes, overwriteTwoElements(this.content, i*2, key, value));
        } else {
            var newNode = binaryNode(this.getKey(i), this.keyhashes[i], this.getValue(i), key, keyHash, value, shift + 5);
            var newLength = this.content.length - 1;
            var newContent = new Array(newLength);
            var newNodeIndex = newLength - index(this.nodemap, bit) - 1; // old length - 2 - nodeindex
            var j = 0;
            for (; j < i * 2; j++) newContent[j] = this.content[j];
            for (; j < newNodeIndex; j++) newContent[j] = this.content[j+2];
            newContent[j++] = newNode;
            for (; j < newLength; j++) newContent[j] = this.content[j+1];
            // TODO remove splice
            var newKeyhashes = this.keyhashes.slice();
            newKeyhashes.splice(i, 1);
            return new Node(this.datamap ^ bit, this.nodemap | bit, newKeyhashes, newContent);
        }
    }
    if ((this.nodemap & bit) !== 0) {
        var nodeIndex = index(this.nodemap, bit);
        /*const*/ newNode = (this.getNode(nodeIndex)).insert(keyEquals, key, keyHash, value, shift + 5);
        /*const*/ newContent = this.content.slice();
        newContent[newContent.length - nodeIndex - 1] = newNode;
        return new Node(this.datamap, this.nodemap, this.keyhashes, newContent);
    }
    /*const*/ newContent = new Array(this.content.length + 2);
    for (var k = 0; k < i * 2; k++) newContent[k] = this.content[k];
    newContent[k++] = key;
    newContent[k++] = value;
    for (; k < newContent.length; k++) newContent[k] = this.content[k - 2];
    /*const*/ newKeyhashes = this.keyhashes.slice();
    // TODO remove splice
    newKeyhashes.splice(i, 0, keyHash);
    return new Node(this.datamap | bit, this.nodemap, newKeyhashes, newContent);
}

function l(k, m) {
    return m.lookup(null, function (a) { return a; }, function(a) { return function (b) { return a == b; } },
                    k, k, 0);
}

function i(k, v, m) {
    return m.insert(function(a) { return function (b) { return a == b; } },
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
    return function (key) {
        return function (keyHash) {
            return function (value) {
                return function (m) {
                    return m.insert(keyEquals, key, keyHash, value, 0);
                };
            };
        };
    };
};

// l(-868019,i(-868019,16574,i(-204499,-358325,i(-676116,21098,i(641360,773947,empty)))))
// l(-868019,i(641360,773947,i(-676116,21098,i(-204499,-358325,i(-868019,16574,empty)))))
// l(-868019,i(-868019,16574,i(641360,773947,i(-676116,21098,i(-204499,-358325,empty)))))


// console.log(i( -8766, 339042, 
//      i( 210080, -617132, 
//         i( 362834, -32859, 
//            i( -457558, 1714, 
//               i( -222372, 541251, 
//                  i( 23922, -787932,
//                     empty)))))))

// console.log(
// i(-695086, -811390,
//   i( -8766, 339042, 
//      i( 210080, -617132, 
//         i( 362834, -32859, 
//            i( -457558, 1714, 
//               i( -222372, 541251, 
//                  i( 23922, -787932,
//                     empty))))))))
  

// console.log("empty content", empty.content)
// console.log("0 b content", i(0,'b', empty).content)
// console.log("0 b 8 a content", i(8,'a', i(0,'b', empty)).content)

// console.log(i(8,'a',i(0,'b',empty)))
// console.log(l(8,i(8,'a',i(0,'b',empty))))

// console.log(l(0,i(8, 'eight',i(32,'thirtytwo',i(0,'z',empty)))))
// console.log(l(8,i(8, 'eight',i(32,'thirtytwo',i(0,'z',empty)))))
// console.log(l(32,i(8, 'eight',i(32,'thirtytwo',i(0,'z',empty)))))
