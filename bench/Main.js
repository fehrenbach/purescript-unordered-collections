exports.table = function (t) {
    return function () {
        console.table(t);
        return {};
    };
};

exports.shuffle = function (input) {
    return function () {
        var a = input.slice();
        var j, x, i;
        for (i = a.length - 1; i > 0; i--) {
            j = Math.floor(Math.random() * (i + 1));
            x = a[i];
            a[i] = a[j];
            a[j] = x;
        }
        return a;
    };
}

exports.bulkLoadStringKeysObj = function (arr) {
    var m = {};
    for (var i = 0; i < arr.length; i++)
        m[arr[i]+""] = i*10;
    return m;
}
