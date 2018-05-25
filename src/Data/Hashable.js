// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

"use strict";

exports.hashNumber = function (o) {
    if (o !== o || o === Infinity) {
        return 0;
    }
    var h = o | 0;
    if (h !== o) {
        h ^= o * 0xffffffff;
    }
    while (o > 0xffffffff) {
        o /= 0xffffffff;
        h ^= o;
    }
    return h;
};

exports.hashString = function (s) {
    var h = 0;
    for (var i = 0; i < s.length; i++) {
        h = (31 * h + s.charCodeAt(i)) | 0;
    }
    return h;
};
