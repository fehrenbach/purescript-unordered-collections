// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

"use strict";

// These are used in lookup. This is of course highly dependent on
// PureScript codegen. It improves lookup performance by 25% though.
var Data_Maybe = require("../Data.Maybe/index.js");
// I'd like to use these, but purs bundle "optimises" them away :/
// var Just = Data_Maybe.Just.create;
// var Nothing = Data_Maybe.Nothing.value;

/** @constructor */
function MapNode(datamap, nodemap, content) {
    this.datamap = datamap;
    this.nodemap = nodemap;
    this.content = content;
}

MapNode.prototype.lookup = function lookup(keyEquals, key, keyHash, shift) {
    var bit = mask(keyHash, shift);
    if ((this.datamap & bit) !== 0) {
        var i = index(this.datamap, bit);
        if (keyEquals(key)(this.content[i * 2]))
            return new Data_Maybe.Just(this.content[i * 2 + 1]);
        return Data_Maybe.Nothing.value;
    }
    if ((this.nodemap & bit) !== 0) {
        return this.content[this.content.length - 1 - index(this.nodemap, bit)].lookup(keyEquals, key, keyHash, shift + 5);
    }
    return Data_Maybe.Nothing.value;
}

MapNode.prototype.insert = function insert(keyEquals, hashFunction, key, keyHash, value, shift) {
    var bit = mask(keyHash, shift);
    var i = index(this.datamap, bit);
    if ((this.datamap & bit) !== 0) {
        var k = this.content[i * 2];
        if (keyEquals(k)(key))
            return new MapNode(this.datamap, this.nodemap, overwriteTwoElements(this.content, i*2, key, value));
        var newNode = binaryNode(k, hashFunction(k), this.content[i*2+1], key, keyHash, value, shift + 5);
        return new MapNode(this.datamap ^ bit, this.nodemap | bit, remove2insert1(this.content, i * 2, this.content.length - index(this.nodemap, bit) - 2, newNode));
    }
    if ((this.nodemap & bit) !== 0) {
        var n = this.content.length - 1 - index(this.nodemap, bit);
        return new MapNode(this.datamap, this.nodemap,
                           copyAndOverwrite(this.content, n,
                                            this.content[n].insert(keyEquals, hashFunction, key, keyHash, value, shift + 5)));
    }
    return new MapNode(this.datamap | bit, this.nodemap, insert2(this.content, i*2, key, value));
}

MapNode.prototype.insertWith = function insertWith(keyEquals, hashFunction, f, key, keyHash, value, shift) {
    var bit = mask(keyHash, shift);
    var i = index(this.datamap, bit);
    if ((this.datamap & bit) !== 0) {
        var k = this.content[i * 2];
        if (keyEquals(k)(key))
            return new MapNode(this.datamap, this.nodemap, overwriteTwoElements(this.content, i*2, key, f(this.content[i*2+1])(value)));
        var newNode = binaryNode(k, hashFunction(k), this.content[i*2+1], key, keyHash, value, shift + 5);
        return new MapNode(this.datamap ^ bit, this.nodemap | bit, remove2insert1(this.content, i * 2, this.content.length - index(this.nodemap, bit) - 2, newNode));
    }
    if ((this.nodemap & bit) !== 0) {
        var n = this.content.length - 1 - index(this.nodemap, bit);
        return new MapNode(this.datamap, this.nodemap,
                           copyAndOverwrite(this.content, n,
                                            this.content[n].insertWith(keyEquals, hashFunction, f, key, keyHash, value, shift + 5)));
    }
    return new MapNode(this.datamap | bit, this.nodemap, insert2(this.content, i*2, key, value));
}

MapNode.prototype.delet = function delet(keyEquals, key, keyHash, shift) {
    var bit = mask(keyHash, shift);
    if ((this.datamap & bit) !== 0) {
        var dataIndex = index(this.datamap, bit);
        if (keyEquals(this.content[dataIndex*2])(key)) {
            if (this.nodemap === 0 && this.content.length === 2) return empty;
            return new MapNode(this.datamap ^ bit, this.nodemap, remove2(this.content, dataIndex * 2));
        }
        return this;
    }
    if ((this.nodemap & bit) !== 0) {
        var nodeIndex = index(this.nodemap,bit);
        var recNode = this.content[this.content.length - 1 - nodeIndex];
        var recRes = recNode.delet(keyEquals, key, keyHash, shift + 5);
        if (recNode === recRes) return this;
        if (recRes.isSingleton()) {
            if (this.content.length === 1) {
                recRes.datamap = this.nodemap;
                return recRes;
            }
            return new MapNode(this.datamap | bit, this.nodemap ^ bit,
                               insert2remove1(this.content, 2 * index(this.datamap, bit), recRes.content[0], recRes.content[1], this.content.length - 1 - nodeIndex));
        }
        return new MapNode(this.datamap, this.nodemap, copyAndOverwrite(this.content, this.content.length - 1 - nodeIndex, recRes));
    }
    return this;
}

MapNode.prototype.toArrayBy = function (f, res) {
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        var k = this.content[i++];
        var v = this.content[i++];
        res.push(f(k)(v));
    }
    for (; i < this.content.length; i++)
        this.content[i].toArrayBy(f, res);
}

MapNode.prototype.isSingleton = function () {
    return this.nodemap === 0 && this.content.length === 2;
}

MapNode.prototype.eq = function(kf, vf, that) {
    if (this === that) return true;
    if (this.constructor !== that.constructor || this.nodemap !== that.nodemap || this.datamap !== that.datamap) return false;
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        if (kf(this.content[i])(that.content[i])) i++; else return false;
        if (vf(this.content[i])(that.content[i])) i++; else return false;
    }
    for (; i < this.content.length; i++)
        if (!this.content[i].eq(kf, vf, that.content[i])) return false;
    return true;
}

MapNode.prototype.hash = function (vhash) {
    var h = this.datamap;
    for (var i = 0; i < popCount(this.datamap); i++)
        h = (h * 31 + vhash(this.content[i * 2 + 1])) | 0;
    for (var j = 0; j < popCount(this.nodemap); j++)
        h = (h * 31 + this.content[this.content.length - j - 1].hash(vhash)) | 0;
    return h;
}

MapNode.prototype.size = function () {
    var res = popCount(this.datamap);
    for (var i = res * 2; i < this.content.length; i++) res += this.content[i].size();
    return res;
}

MapNode.prototype.imap = function (f) {
    var newContent = this.content.slice();
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        var k = this.content[i++];
        var v = this.content[i++];
        newContent[i-2] = k;
        newContent[i-1] = f(k)(v);
    }
    for (; i < this.content.length; i++)
        newContent[i] = this.content[i].imap(f);
    return new MapNode(this.datamap, this.nodemap, newContent);
}

MapNode.prototype.ifoldMap = function (m, mappend, f) {
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        var k = this.content[i++];
        var v = this.content[i++];
        m = mappend(m)(f(k)(v));
    }
    for (; i < this.content.length; i++)
        m = this.content[i].ifoldMap(m, mappend, f);
    return m;
}

MapNode.prototype.unionWith = function (eq, hash, f, that, shift) {
    if (this.constructor === that.constructor) {
        var datamap = 0;
        var nodemap = 0;
        var data = [];
        var nodes = [];
        var skipmap = this.datamap | this.nodemap | that.datamap | that.nodemap;
        // This goes through the 32 bits in the resulting
        // (data|node)maps one by one. It's not pretty, but it allows
        // us to handle the 9 cases (in/notin + left/right + data/node)
        // fairly uniformly. I'd like to use bitops to calculate the
        // maps, but sometimes we move stuff from the datamaps to the
        // nodemap, which makes that difficult.
        for (var i = 0; i < 32; i++) {
            var bit = 1 << i;
            if ((skipmap & bit) === 0) continue;
            var thisDataIndex = index(this.datamap, bit);
            var thatDataIndex = index(that.datamap, bit);
            var thisNodeIndex = index(this.nodemap, bit);
            var thatNodeIndex = index(that.nodemap, bit);
            if ((this.datamap & bit) !== 0) {
                if ((that.datamap & bit) !== 0) {
                    // in both data => combine or merge into node
                    if (eq(this.content[thisDataIndex * 2])(that.content[thatDataIndex * 2])) {
                        // equal, merge with f
                        datamap |= bit;
                        data.push(this.content[thisDataIndex * 2], f(this.content[thisDataIndex * 2 + 1])(that.content[thatDataIndex * 2 + 1]));
                    } else {
                        // key hashes equal at this level, merge into node
                        nodemap |= bit;
                        nodes.push(binaryNode(
                            this.content[thisDataIndex * 2],
                            hash(this.content[thisDataIndex * 2]),
                            this.content[thisDataIndex*2+1],
                            that.content[thatDataIndex * 2],
                            hash(that.content[thatDataIndex * 2]),
                            that.content[thatDataIndex*2+1],
                            shift + 5));
                    }
                } else {
                    if ((that.nodemap & bit) !== 0) {
                        // merge thisData into thatNode
                        // This sucks a bit because we don't have insertWith
                        var k = this.content[thisDataIndex * 2];
                        var hk = hash(k);
                        var res = that.content[that.content.length - thatNodeIndex - 1].lookup(eq, k, hk, shift + 5);
                        nodemap |= bit;
                        nodes.push(
                            that.content[that.content.length - thatNodeIndex - 1]
                                .insert(eq, hash, k, hk,
                                        (res === Data_Maybe.Nothing.value) ? this.content[thisDataIndex * 2 + 1] : f(this.content[thisDataIndex * 2 + 1])(res.value0),
                                        shift + 5));
                    } else {
                        // add thisData
                        datamap |= bit;
                        data.push(this.content[thisDataIndex * 2], this.content[thisDataIndex * 2 + 1]);
                    }
                }
            }
            else {
                if ((this.nodemap & bit) !== 0) {
                    if ((that.datamap & bit) !== 0) {
                        // merge thatData into thisNode
                        // This sucks a bit because we don't have insertWith
                        var k = that.content[thatDataIndex * 2];
                        var hk = hash(k);
                        var res = this.content[this.content.length - thisNodeIndex - 1].lookup(eq, k, hk, shift + 5);
                        nodemap |= bit;
                        nodes.push(
                            this.content[this.content.length - thisNodeIndex - 1]
                                .insert(eq, hash, k, hk,
                                        (res === Data_Maybe.Nothing.value) ? that.content[thatDataIndex * 2 + 1] : f(res.value0)(that.content[thatDataIndex * 2 + 1]),
                                        shift + 5));
                    } else {
                        if ((that.nodemap & bit) !== 0) {
                            // recursively merge nodes
                            nodemap |= bit;
                            nodes.push(
                                this.content[this.content.length - thisNodeIndex - 1].unionWith(eq, hash, f, that.content[that.content.length - thatNodeIndex - 1], shift + 5));
                        } else {
                            // add this node
                            nodemap |= bit;
                            nodes.push(this.content[this.content.length - thisNodeIndex - 1]);
                        }
                    }
                } else {
                    if ((that.datamap & bit) !== 0) {
                        // add that data
                        datamap |= bit;
                        data.push(that.content[thatDataIndex * 2], that.content[thatDataIndex * 2 + 1]);
                    } else {
                        if ((that.nodemap & bit) !== 0) {
                            // add that node
                            nodemap |= bit;
                            nodes.push(that.content[that.content.length - thatNodeIndex - 1]);
                        } else {
                            // not anywhere
                            // do nothing
                            // this is unreachable because we skip these bits early
                        }
                    }
                }
            }
        }
        return new MapNode(datamap, nodemap, data.concat(nodes.reverse()));
    }
    throw "Trying to union a MapNode with something else";
}

MapNode.prototype.intersectionWith = function (eq, hash, f, that, shift) {
    var datamap = 0;
    var nodemap = 0;
    var data = [];
    var nodes = [];
    var skipmap = (this.datamap | this.nodemap) & (that.datamap | that.nodemap);
    // This goes through the 32 bits in the resulting (data|node)maps
    // one by one. I'd like to use bitops to calculate the maps, but
    // sometimes we move stuff from the nodemap into the datamap, and
    // sometimes hashes collide, which makes that difficult.
    for (var i = 0; i < 32; i++) {
        var bit = 1 << i;
        if ((skipmap & bit) === 0) continue;

        var inThisData = (this.datamap & bit) !== 0;
        var inThatData = (that.datamap & bit) !== 0;

        if (inThisData) {
            var thisDataIndex = index(this.datamap, bit);
            if (inThatData) {
                var thatDataIndex = index(that.datamap, bit);
                if (eq(this.content[thisDataIndex * 2])(that.content[thatDataIndex * 2])) {
                    datamap |= bit;
                    data.push(this.content[thisDataIndex * 2], f(this.content[thisDataIndex * 2 + 1])(that.content[thatDataIndex * 2 + 1]));
                }
            } else {
                var thatNodeIndex = index(that.nodemap, bit);
                var k = this.content[thisDataIndex * 2];
                var hk = hash(k);
                var res = that.content[that.content.length - thatNodeIndex - 1].lookup(eq, k, hk, shift + 5);
                if (res !== Data_Maybe.Nothing.value) {
                    datamap |= bit;
                    data.push(this.content[thisDataIndex * 2], f(this.content[thisDataIndex * 2 + 1])(res.value0));
                }
            }
        } else {
            var thisNodeIndex = index(this.nodemap, bit);
            if (inThatData) {
                var thatDataIndex = index(that.datamap, bit);
                var k = that.content[thatDataIndex * 2];
                var hk = hash(k);
                var res = this.content[this.content.length - thisNodeIndex - 1].lookup(eq, k, hk, shift + 5);
                if (res !== Data_Maybe.Nothing.value) {
                    datamap |= bit;
                    data.push(that.content[thatDataIndex * 2], f(res.value0)(that.content[thatDataIndex * 2 + 1]));
                }
            } else {
                var thatNodeIndex = index(that.nodemap, bit);
                var recRes = this.content[this.content.length - thisNodeIndex - 1].intersectionWith(eq, hash, f, that.content[that.content.length - thatNodeIndex - 1], shift + 5);
                if (isEmpty(recRes)) continue;
                if (recRes.isSingleton()) {
                    datamap |= bit;
                    data.push(recRes.content[0], recRes.content[1]);
                } else {
                    nodemap |= bit;
                    nodes.push(recRes);
                }
            }
        }
    }
    return new MapNode(datamap, nodemap, data.concat(nodes.reverse()));
}


// This builds an n-ary curried function that all values and all
// subnodes as arguments and places them in a copy of the hashmap
// preserving the keys, datamap, and nodemap.  Basically, a (Hashmap k
// v) with s key-value pairs and t nodes turns into a function:
//
// k_0 -> .. -> k_s -> HashMap_0 k v -> .. -> HashMap_t k v -> HashMap k v
//
// Indices here are to be understood as count.
//
// The main use for this is the as the partial hashmap constructor in
// place of the hole in this concept of an implementation of
// traverseWithKey:
//
// pure ?here <*> f k1 v1 <*> f k2 v2 <*> traverseWithKey f n1 <*> traverseWithKey f n2
MapNode.prototype.travHelper = function () {
    // TODO could have two helpers that basically switch mode from
    // setting values to setting nodes. That way branches would be
    // more predictable. Because the value branch is essentially
    // unpredictable.
    function go(vi, vm, ni, nm, copy) {
        if (vi < vm)
            return function (v) {
                return go(vi + 1, vm, ni, nm, function () { var res = copy();
                                                            res.content[vi * 2 + 1] = v;
                                                            return res;
                                                          });
            }
        if (ni < nm)
            return function (n) {
                return go(vi, vm, ni+1, nm, function () { var res = copy();
                                                          // order of parameters must match stored order
                                                          res.content[vm*2 + ni] = n;
                                                          return res;
                                                        });
            }
        return copy();
    }
    var vm = popCount(this.datamap);
    var self = this;
    return go(0, vm, 0, this.content.length - vm * 2, function () { return new MapNode(self.datamap, self.nodemap, self.content.slice()); });
}

MapNode.prototype.ifoldMap = function (m, mappend, f) {
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        var k = this.content[i++];
        var v = this.content[i++];
        m = mappend(m)(f(k)(v));
    }
    for (; i < this.content.length; i++)
        m = this.content[i].ifoldMap(m, mappend, f);
    return m;
}

MapNode.prototype.itraverse = function (pure, apply, f) {
    var m = pure(this.travHelper());
    for (var i = 0; i < popCount(this.datamap) * 2;) {
        var k = this.content[i++];
        var v = this.content[i++];
        m = apply(m)(f(k)(v));
    }
    for (; i < this.content.length; i++)
        m = apply(m)(this.content[i].itraverse(pure, apply, f));
    return m;
}

/** @constructor */
function Collision(keys, values) {
    this.keys = keys;
    this.values = values;
}

Collision.prototype.lookup = function collisionLookup(keyEquals, key, keyHash, shift) {
    for (var i = 0; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            return new Data_Maybe.Just(this.values[i]);
    return Data_Maybe.Nothing.value;
};

Collision.prototype.insert = function collisionInsert(keyEquals, hashFunction, key, keyHash, value, shift) {
    var i = 0;
    for (; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            break;
    return new Collision(copyAndOverwriteOrExtend1(this.keys, i, key),
                         copyAndOverwriteOrExtend1(this.values, i, value));
};

Collision.prototype.insertWith = function collisionInsert(keyEquals, hashFunction, f, key, keyHash, value, shift) {
    var i = 0;
    for (; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            return new Collision(copyAndOverwrite(this.keys, i, key),
                                 copyAndOverwrite(this.values, i, f(this.values[i])(value)));
    return new Collision(copyAndOverwriteOrExtend1(this.keys, i, key),
                         copyAndOverwriteOrExtend1(this.values, i, value));
};

Collision.prototype.delet = function collisionDelete(keyEquals, key, keyHash, shift) {
    var i = 0;
    for (; i < this.keys.length; i++)
        if (keyEquals(key)(this.keys[i]))
            break;
    if (i === this.keys.length) return this;
    if (this.keys.length === 2)
        return new MapNode(1 << (keyHash & 31), 0, [this.keys[1 - i], this.values[1 - i]]);
    return new Collision(remove1(this.keys, i), remove1(this.values, i));
}

Collision.prototype.toArrayBy = function (f, res) {
    for (var i = 0; i < this.keys.length; i++)
        res.push(f(this.keys[i])(this.values[i]));
}

Collision.prototype.isSingleton = function () { return false; }

Collision.prototype.eq = function(kf, vf, that) {
    if (this.constructor !== that.constructor || this.keys.length !== that.keys.length) return false;
    outer:
    for (var i = 0; i < this.keys.length; i++) {
        for (var j = 0; j < that.keys.length; j++) {
            if (kf(this.keys[i])(that.keys[j])) {
                if (vf(this.values[i])(that.values[j]))
                    continue outer;
                else
                    return false;
            }
        }
    }
    return true;
}

Collision.prototype.hash = function (vhash) {
    // We ignore keys because they have all the same hash anyways
    // (we're in a collision node!)
    var h = 0;
    // We use just + here, not multiply&add, because order in
    // collision nodes is undefined. A commutative combining operation
    // allows us to simply ignore the order.
    for (var i = 0; i < this.values.length; i++)
        h += vhash(this.values[i]);
    return h;
}

Collision.prototype.size = function () {
    return this.keys.length;
}

Collision.prototype.imap = function (f) {
    var newValues = this.values.slice();
    for (var i = 0; i < this.values.length; i++)
        newValues[i] = f(this.keys[i])(this.values[i]);
    return new Collision(this.keys, newValues);
}

Collision.prototype.ifoldMap = function (m, mappend, f) {
    for (var i = 0; i < this.keys.length; i++)
        m = mappend(m)(f(this.keys[i])(this.values[i]));
    return m;
}

Collision.prototype.travHelper = function () {
    function go(i, m, copy) {
        if (i < m)
            return function (v) {
                return go(i + 1, m, function () { var res = copy();
                                                  res.values[i] = v;
                                                  return res;
                                                });
            }
        return copy();
    }
    var self = this;
    return go(0, this.keys.length, function () { return new Collision(self.keys, self.values.slice()); });
}

Collision.prototype.itraverse = function (pure, apply, f) {
    var m = pure(this.travHelper());
    for (var i = 0; i < this.keys.length; i++)
        m = apply(m)(f(this.keys[i])(this.values[i]));
    return m;
}

Collision.prototype.unionWith = function (eq, hash, f, that, shift) {
    if (that.constructor !== Collision)
        throw "Trying to union a Collision with something else";
    var keys = [];
    var values = [];
    var added = Array(that.keys.length).fill(false);
    outer:
    for (var i = 0; i < this.keys.length; i++) {
        for (var j = 0; j < that.keys.length; j++) {
            if (eq(this.keys[i])(that.keys[j])) {
                keys.push(this.keys[i]);
                values.push(f(this.values[i])(that.values[j]));
                added[j] = true;
                continue outer;
            }
        }
        keys.push(this.keys[i]);
        values.push(this.values[i]);
        added[j] = true;
    }
    for (var k = 0; k < that.keys.length; k++) {
        if (!added[k]) {
            keys.push(that.keys[k]);
            values.push(that.values[k]);
        }
    }
    return new Collision(keys, values);
}

Collision.prototype.intersectionWith = function (eq, hash, f, that, shift) {
    if (that.constructor !== Collision)
        throw "Trying to intersect a Collision with something else";
    var keys = [];
    var values = [];
    outer:
    for (var i = 0; i < this.keys.length; i++) {
        for (var j = 0; j < that.keys.length; j++) {
            if (eq(this.keys[i])(that.keys[j])) {
                keys.push(this.keys[i]);
                values.push(f(this.values[i])(that.values[j]));
                continue outer;
            }
        }
    }
    if (keys.length === 0)
        return empty;
    // This is a bit dodgy. We return a fake MapNode (wrong datamap
    // (WHICH CANNOT BE 0, OTHERWISE isEmpty THINKS IT'S EMPTY!) and
    // nodemap), but it's okay, because we will immediately
    // deconstruct it in the MapNode.intersectionWith.
    if (keys.length === 1)
        return new MapNode(1, 0, [keys[0], values[0]]);
    return new Collision(keys, values);
}

function mask(keyHash, shift) {
    return 1 << ((keyHash >>> shift) & 31);
}

function index(map, bit) {
    return popCount(map & (bit - 1));
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

    if (b1 !== b2) return new MapNode((1 << b1) | (1 << b2), 0, (b1 >>> 0) < (b2 >>> 0) ? [k1, v1, k2, v2] : [k2, v2, k1, v1]);

    return new MapNode(0, 1 << b1, [binaryNode(k1, kh1, v1, k2, kh2, v2, s + 5)]);
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

var copyAndOverwrite = copyAndOverwriteOrExtend1;

function remove2insert1(a, removeIndex, insertIndex, v1) {
    var res = new Array(a.length - 1);
    for (var i = 0; i < removeIndex; i++) res[i] = a[i];
    for (; i < insertIndex; i++) res[i] = a[i+2];
    res[i++] = v1;
    for (; i < res.length; i++) res[i] = a[i+1];
    return res;
}

function insert2(a, index, v1, v2) {
    var res = new Array(a.length + 2);
    for (var i = 0; i < index; i++) res[i] = a[i];
    res[i++] = v1;
    res[i++] = v2;
    for (; i < res.length; i++) res[i] = a[i - 2];
    return res;
}

function insert2remove1(a, insertIndex, v1, v2, removeIndex) {
    var res = new Array(a.length + 1);
    for (var i = 0; i < insertIndex; i++) res[i] = a[i];
    res[i++] = v1;
    res[i++] = v2;
    for (; i < removeIndex + 2; i++) res[i] = a[i-2];
    for (; i < res.length; i++) res[i] = a[i-1];
    return res;
}

var empty = new MapNode(0,0,[]);

exports.empty = empty;
exports.lookupPurs = function (keyEquals) {
    return function (key) {
        return function (keyHash) {
            return function (m) {
                return m.lookup(keyEquals, key, keyHash, 0);
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

exports.insertWithPurs = function (keyEquals) {
    return function (hashFunction) {
        return function (f) {
            return function (key) {
                return function (value) {
                    return function (m) {
                        return m.insertWith(keyEquals, hashFunction, f, key, hashFunction(key), value, 0);
                    };
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

exports.unionWithPurs = function (eq) {
    return function (hash) {
        return function (f) {
            return function (l) {
                return function (r) {
                    return l.unionWith(eq, hash, f, r, 0);
                };
            };
        };
    };
};

exports.intersectionWithPurs = function (eq) {
    return function (hash) {
        return function (f) {
            return function (l) {
                return function (r) {
                    return l.intersectionWith(eq, hash, f, r, 0);
                };
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

exports.debugShow = function (m) {
    return JSON.stringify(m);
}

exports.singletonPurs = function (k) {
    return function (keyHash) {
        return function (v) {
            return new MapNode(1 << (keyHash & 31), 0, [k, v]);
        };
    };
};

exports.eqPurs = function (kf) {
    return function (vf) {
        return function (a) {
            return function (b) {
                return a.eq(kf, vf, b);
            };
        };
    };
};

function isEmpty (m) {
    return m.datamap === 0 && m.nodemap === 0;
}

exports.isEmpty = isEmpty;

exports.size = function (m) { return m.size(); }

exports.mapWithIndexPurs = function (f) {
    return function (m) {
        return m.imap(f);
    };
};

exports.foldMapWithIndexPurs = function (mempty) {
    return function (mappend) {
        return function (f) {
            return function (m) {
                return m.ifoldMap(mempty, mappend, f);
            };
        };
    };
};

exports.traverseWithIndexPurs = function (pure) {
    return function (apply) {
        return function (f) {
            return function (m) {
                return isEmpty(m) ? pure(empty) : m.itraverse(pure, apply, f);
            };
        };
    };
};

exports.hashPurs = function (vhash) {
    return function (m) {
        return m.hash(vhash);
    };
};

// TODO ideally this would use a mutable HashSet instead
exports.nubHashPurs = function (eq) {
    return function (hash) {
        return function (a) {
            var m = empty;
            var r = [];
            for (var i = 0; i < a.length; i++) {
                var x = a[i];
                var hx = hash(x);
                if (m.lookup(eq, x, hx, 0) !== Data_Maybe.Nothing.value)
                    continue;
                m = m.insert(eq, hash, x, hx, null, 0);
                r.push(x);
            }
            return r;
        };
    };
};
