"use strict";

exports.popCount = function (n) {
    n = n - ((n >> 1) & 0x55555555);
    n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
    return ((n + (n >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
}

// unsafeArrayIndex :: forall a. Array a -> Int -> a
exports.unsafeArrayIndex = function (a) {
    return function(i) {
        return a[i];
    };
};

// unsafeInsertAt :: forall a. Int -> a -> Array a -> Array a
exports.unsafeInsertAt = function (i) {
    return function (a) {
        return function (l) {
            var l1 = l.slice();
            l1.splice(i, 0, a);
            return l1;
        };
    };
};

// unsafeUpdateAt :: forall a. Int -> a -> Array a -> Array a
exports.unsafeUpdateAt = function (i) {
    return function (a) {
        return function (l) {
            var l1 = l.slice();
            l1[i] = a;
            return l1;
        };
    };
};

// unsafeDeleteAt :: forall a. Int -> Array a -> Array a
exports.unsafeDeleteAt = function (i) {
    return function (l) {
        var l1 = l.slice();
        l1.splice(i, 1);
        return l1;
    };
};
