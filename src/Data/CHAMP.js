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
        return Just(this.getNode(index(this.nodemap, bit)).lookup(Nothing, Just, keyEquals, key, keyHash, shift + 5));
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
        throw "TODO collision"
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

function insert(keyEquals, key, keyHash, value, shift) {
    var bit = mask(keyHash, shift);
    var i = index(this.datamap, bit);
    if ((this.datamap & bit) !== 0) {
        // TODO compare hashes first?!
        if (keyEquals(this.getKey(i))(key)) {
            // TODO remove splice
            var newContent = this.content.slice();
            // newContent[i * 2] = key;
            // newContent[i * 2 + 1] = value;
            newContent.splice(i * 2, 2, key, value);
            return new Node(this.datamap, this.nodemap, this.keyhashes, newContent);
        } else {
            var newNode = binaryNode(this.getKey(i), this.keyhashes[i], this.getValue(i), key, keyHash, value, shift + 5);
            var newLength = this.content.length - 1;
            /*const*/ newContent = new Array(newLength);
            var newNodeIndex = newLength - i - 1;
            var j = 0;
            for (; j < i * 2; j++) newContent[j] = this.content[j];
            j = j+2;
            for (; j < newNodeIndex; j++) newContent[j] = this.content[j];
            newContent[newNodeIndex] = newNode;
            j = newNodeIndex + 1;
            for (; j < newNodeIndex; j++) newContent[j] = this.content[j+1];
            // TODO remove splice
            var newKeyhashes = this.keyhashes.slice();
            newKeyhashes.splice(i, 1);
            return new Node(this.datamap ^ bit, this.nodemap | bit, newKeyhashes, newContent);
        }
    }
    if ((this.nodemap & bit) !== 0) {
        /*const*/ newNode = this.getNode(index(this.nodemap, bit)).insert(keyEquals, key, keyHash, value, shift);
        /*const*/ newContent = this.content.slice();
        newContent[newContent.length - i - 1] = newNode;
        return new Node(this.datamap, this.nodemap, this.keyhashes, newContent);
    }
    /*const*/newContent = this.content.slice();
    // TODO apparently splice is slow, replace by something faster
    newContent.splice(i * 2, 0, key, value);
    /*const*/ newKeyhashes = this.keyhashes.slice();
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

// console.log("empty content", empty.content)
// console.log("0 b content", i(0,'b', empty).content)
// console.log("0 b 8 a content", i(8,'a', i(0,'b', empty)).content)

// console.log(i(8,'a',i(0,'b',empty)))
console.log(l(8,i(8,'a',i(0,'b',empty))))

console.log(l(0,i(8, 'eight',i(32,'thirtytwo',i(0,'z',empty)))))
console.log(l(8,i(8, 'eight',i(32,'thirtytwo',i(0,'z',empty)))))
console.log(l(32,i(8, 'eight',i(32,'thirtytwo',i(0,'z',empty)))))
